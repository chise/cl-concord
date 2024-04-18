(in-package :cl-user)
(eval-when (:execute :compile-toplevel :load-toplevel)
  (require 'cl-redis))

(defpackage :concord
  (:use :cl)
  (:export
   :genre
   :genre-name :genre-ds
   :object :decode-object
   :object-genre :object-id
   :object-put :object-get
   :define-object :object-spec
   :object-p
   :some-in-feature
   :metadata-feature-name-p
   :id-feature-name-p :relation-feature-name-p
   :make-reversed-relation-feature-name
   :sequence-list-p :association-list-p))

(in-package :concord)

(defun sequence-list-p (object)
  (cond ((null object))
	((consp object)
	 (sequence-list-p (cdr object)))))

(defun association-list-p (object)
  (cond ((null object))
	((and (consp object)
	      (consp (car object)))
	 (association-list-p (cdr object)))))
  
(defvar *default-ds* nil "The default data-store of Concord.")

(defclass data-store ()
  ((location :initform nil :accessor ds-location)
   (genres :initform (make-hash-table))
   ))

(defmethod ds-find-genre ((ds data-store) genre-name)
  (gethash genre-name (slot-value ds 'genres)))

(defmethod ds-register-genre ((ds data-store) genre-name gobj)
  (setf (gethash genre-name (slot-value ds 'genres)) gobj))

(defmethod ds-make-genre ((ds data-store) genre-name)
  (let ((gobj (ds-find-genre ds genre-name)))
    (unless gobj
      (setq gobj (make-instance 'genre :name genre-name))
      (ds-register-genre ds genre-name gobj))
    gobj))

(defun default-ds ()
  (unless *default-ds*
    (setq *default-ds* (make-instance 'redis-ds :db-number 3)))
  *default-ds*)

(defun genre (genre-name &key ds)
  (unless ds
    (setq ds (default-ds)))
  (if (keywordp genre-name)
      (setq genre-name (read-from-string (format nil "~a" genre-name))))
  (ds-make-genre ds genre-name))


(defclass location ()())


(defclass redis-location (location)
  ((connection :accessor redis-location-connection)
   (db-number :initform 0 :accessor redis-location-db-number)
   ))

(defmethod initialize-instance :after ((location redis-location)
				       &key (host #(127 0 0 1)) (port 6379) auth
					 (db-number 0))
  (setf (redis-location-connection location)
	(redis:connect :host host :port port :auth auth))
  (setf (redis-location-db-number location) db-number))

(defmethod (setf redis-location-db-number) :after (db-number
						   (location redis-location))
  (red:select db-number))


(defclass redis-ds (data-store)())

(defmethod initialize-instance :after ((ds redis-ds)
				       &key (host #(127 0 0 1)) (port 6379) auth
					 (db-number 0))
  (setf (ds-location ds)
	(make-instance 'redis-location
		       :host host :port port :auth auth
		       :db-number db-number)))

(defmethod redis-ds-host ((ds redis-ds))
  (redis::conn-host (redis-location-connection (ds-location ds))))

(defmethod redis-ds-port ((ds redis-ds))
  (redis::conn-port (redis-location-connection (ds-location ds))))

(defmethod redis-ds-auth ((ds redis-ds))
  (redis::conn-auth (redis-location-connection (ds-location ds))))

(defmethod ds-get-atom ((ds redis-ds) key &optional default-value)
  (let ((ret (red:get key)))
    (if ret
	(read-from-string ret)
	default-value)))

(defmethod ds-set-atom ((ds redis-ds) key value)
  (when (string= (red:set key (format nil "~S" value)) "OK")
    value))

(defmethod ds-del ((ds redis-ds) key)
  (red:del key))

(defmethod ds-rpush ((ds redis-ds) key value)
  (red:rpush key value))

(defmethod ds-set-list ((ds redis-ds) key value)
  (let (ret)
    (red:del key)
    (when (integerp (setq ret (apply #'red:rpush key
				     (mapcar (lambda (unit)
					       (format nil "~S" unit))
					     value))))
      (values value ret))))

(defmethod ds-get-list ((ds redis-ds) key)
  (mapcar #'read-from-string (red:lrange key 0 -1)))

(defmethod ds-get ((ds redis-ds) key &optional default-value)
  (cond ((string= (red:type key) "list")
	 (ds-get-list ds key)
	 )
	(t
	 (ds-get-atom ds key default-value)
	 )))

(defmethod ds-get-object-spec ((ds redis-ds) genre-name id)
  (let ((pat (format nil "~a:obj:~a;" genre-name id))
	len)
    (setq len (length pat))
    (mapcar (lambda (key)
	      (cons (read-from-string (subseq key len))
		    (ds-get ds key)))
	    (red:keys (format nil "~a*" pat)))))

(defmethod ds-some-in-feature ((ds redis-ds) func genre-name feature-name)
  (let ((pat (format nil "~a:obj:*;~a" genre-name feature-name)))
    (some func (red:keys pat))))

(defun some-in-feature (func feature-name &key genre ds)
  (unless genre
    (setq genre 'default))
  (unless ds
    (setq ds *default-ds*))
  (let (pos end id obj)
    (ds-some-in-feature
     ds
     (lambda (key)
       (setq end (position #\; key :from-end t))
       (setq pos (position #\: key :from-end t :end end))
       (setq id (read-from-string (subseq key (1+ pos) end)))
       (funcall func
		(object genre id :ds ds)
		(ds-get ds key)))
     genre  feature-name)))

(defclass genre ()
  ((name :accessor genre-name :initform 'default :initarg :name)
   (ds :accessor genre-ds :initarg :ds)
   (objects :initform (make-hash-table))))

(defmethod initialize-instance :after ((g genre) &key (ds nil))
  (unless ds
    (setq ds (default-ds)))
  (setf (slot-value g 'ds) ds)
  (ds-register-genre ds (slot-value g 'name) g))

(defmethod genre-find-object ((g genre) id)
  (gethash id (slot-value g 'objects)))

(defmethod genre-register-object ((g genre) id obj)
  (setf (gethash id (slot-value g 'objects)) obj))

(defmethod genre-make-object ((g genre) id)
  (let ((obj (genre-find-object g id)))
    (unless obj
      (setq obj (make-instance 'object :id id :genre g))
      (genre-register-object g id obj))
    obj))

(defmethod generate-object-id ((g genre))
  (let* ((next-id (format nil "~a:sys:next-id" (genre-name g)))
	 (ret (ds-get-atom (genre-ds g) next-id
			   (if (eq (genre-name g) 'character)
			       #xF0000
			       0)))
	 status)
    (setq status (red:set next-id (1+ ret)))
    (if (string= status "OK")
	ret)))

(defun object (genre-name id &key ds)
  (genre-make-object (genre genre-name :ds ds) id))

(defun decode-object (id-feature id &key genre)
  (cond ((null genre)
	 (setq genre (genre :default))
	 )
	((symbolp genre)
	 (setq genre (genre genre))
	 ))
  (let ((index (format nil "~a:idx:~a;~(~a~)"
		       (genre-name genre) id-feature id)))
    (ds-get-atom (genre-ds genre) index)))
  

(defclass object ()
  ((genre :accessor object-genre :initarg :genre)
   (id :accessor object-id :initarg :id)))

(defmethod initialize-instance :after ((obj object) &key (genre nil)(id nil)
						      (ds nil))
  (let (gobj genre-name)
    (setq gobj
	  (cond (genre
		 (cond ((symbolp genre)
			(setq genre-name genre)
			(genre genre-name :ds ds)
			)
		       (t genre))
		 )
		(t
		 (ds-make-genre (or ds *default-ds*) 'default)
		 )))
    (setf (object-genre obj) gobj)
    (unless id
      (setq id (generate-object-id gobj)))
    (setf (object-id obj) id)
    (genre-register-object gobj id obj)
    (object-put obj '=_id id)))

(defun object-p (obj)
  (typep obj 'object))

(defmethod print-object ((obj object) out)
  (format out "#.(concord:object :~a ~a)"
	  (genre-name (object-genre obj))
	  (object-id obj)))

(defun metadata-feature-name-p (feature-name)
  (if (symbolp feature-name)
      (setq feature-name (symbol-name feature-name)))
  (search "*" feature-name))

(defun id-feature-name-p (feature-name)
  (and (not (metadata-feature-name-p feature-name))
       (progn
	 (if (symbolp feature-name)
	     (setq feature-name (format nil "~a" feature-name)))
	 (eql (elt feature-name 0) #\=))
       (not (let ((pos (search "decomposition" feature-name)))
	      (and pos
		   (or (= pos 1)
		       (and (= pos 2)
			    (eql (elt feature-name 1) #\>))))))))

(defun structure-feature-name-p (feature-name)
  (and (not (metadata-feature-name-p feature-name))
       (progn
	 (if (symbolp feature-name)
	     (setq feature-name (format nil "~a" feature-name)))
	 (eql (search "ideographic-structure" feature-name) 0))))

(defun relation-feature-name-p (feature-name)
  (and (not (metadata-feature-name-p feature-name))
       (progn
	 (if (symbolp feature-name)
	     (setq feature-name (format nil "~a" feature-name)))
	 (or (eql (search "<-" feature-name) 0)
	     (eql (search "->" feature-name) 0)))))

(defun make-reversed-relation-feature-name (feature-name)
  (and (not (metadata-feature-name-p feature-name))
       (progn
	 (if (symbolp feature-name)
	     (setq feature-name (format nil "~a" feature-name)))
	 (cond ((eql (search "<-" feature-name) 0)
		(read-from-string
		 (format nil "->~a"
			 (subseq feature-name 2)))
		)
	       ((eql (search "->" feature-name) 0)
		(read-from-string
		 (format nil "<-~a"
			 (subseq feature-name 2)))
		)))))

(defun define-object (genre object-spec &key id)
  (if (symbolp genre)
      (setq genre (genre (or genre 'default))))
  (unless id
    (setq id (cdr (assoc '=_id object-spec))))
  (let (ret obj)
    (unless id
      (when (setq ret (assoc '=id object-spec))
	(setq id (cdr ret))))
    (cond (id
	   ;; (format t "Defining ~s: id=~x~%"
	   ;;    (genre-name genre) id)
	   (setq obj (genre-make-object genre id))
	   )
	  ((find-if (lambda (feature-pair)
		      (and (id-feature-name-p (car feature-pair))
			   (setq ret (decode-object (car feature-pair)
						    (cdr feature-pair)
						    :genre genre))))
		    object-spec)
	   (setq obj ret)
	   )
	  ((setq id (generate-object-id genre))
	   (setq obj (genre-make-object genre id))
	   ))
    (when obj
      (dolist (feature-pair object-spec)
	(object-put obj (car feature-pair)(cdr feature-pair))))
    obj))

(defun normalize-object-representation (object-rep &key genre ds)
  (cond
    ((association-list-p object-rep)
     (define-object 'character object-rep)
     )
    ((and (consp object-rep)
	  (symbolp (car object-rep))
	  (association-list-p (cdr object-rep)))
     (define-object (genre (car object-rep) :ds ds)
	 (cdr object-rep))
     )
    (t
     object-rep)))
     
(defmethod object-put ((obj object) feature value)
  (let* ((genre (object-genre obj))
	 (ds (genre-ds genre))
	 (key (format nil "~a:obj:~a;~a"
		      (genre-name genre)
		      (object-id obj)
		      feature))
	 index rep-list rev-feature rev-key)
    (cond ((id-feature-name-p feature)
	   (setq index (format nil "~a:idx:~a;~a"
			       (genre-name genre) feature value))
	   (when (ds-set-atom ds key value)
	     (when (ds-set-atom ds index obj)
	       value))
	   )
	  ((structure-feature-name-p feature)
	   (setq rep-list (mapcar #'normalize-object-representation
				  value))
	   (ds-set-list ds key rep-list)
	   )
	  ((setq rev-feature (make-reversed-relation-feature-name feature))
	   (setq rep-list (mapcar #'normalize-object-representation
				  value))
	   (dolist (rev-item rep-list)
	     (cond ((object-p rev-item)
		    (setq rev-key (format nil "~a:obj:~a;~a"
					  (genre-name (object-genre rev-item))
					  (object-id rev-item)
					  rev-feature))
		    (unless (member obj (ds-get-list ds rev-key))
		      (ds-rpush ds rev-key obj))
		    )
		   ((characterp rev-item)
		    (setq rev-key (format nil "character:obj:~a;~a"
					  (char-code rev-item)
					  rev-feature))
		    (unless (member obj (ds-get-list ds rev-key))
		      (ds-rpush ds rev-key obj))
		    )))
	   (ds-set-list ds key rep-list)
	   )
	  ((and value (sequence-list-p value))
	   (ds-set-list ds key value)
	   )
	  (t
	   (ds-set-atom ds key value)
	   ))))

(defmethod object-get ((obj object) feature &optional default-value)
  (let* ((genre (object-genre obj))
	 (key (format nil "~a:obj:~a;~a"
		      (genre-name genre)
		      (object-id obj)
		      feature)))
    (cond ((string= (red:type key) "list")
	   (ds-get-list (genre-ds genre) key)
	   )
	  (t
	   (ds-get-atom (genre-ds genre) key default-value)
	   ))))

(defmethod object-spec ((obj object))
  (let* ((genre (object-genre obj))
	 (ds (genre-ds genre)))
    (ds-get-object-spec ds (genre-name genre) (object-id obj))))

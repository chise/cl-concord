(in-package :cl-user)

(defpackage :concord
  (:use :cl)
  (:export
   :default-ds
   :genre
   :genre-name :genre-ds
   :feature :find-feature
   :object :decode-object
   :object-genre :object-id
   :object-put :object-get
   :object-adjoin
   :define-object :find-object :object-spec
   :object-spec-to-id-spec
   :object-p
   :some-in-feature :intersection-in-feature :union-in-feature
   :store-union-in-feature
   :metadata-feature-name-p
   :id-feature-name-p :decomposition-feature-name-p
   :structure-feature-name-p
   :relation-feature-name-p
   :make-reversed-relation-feature-name
   :expand-feature-name
   :sequence-list-p :association-list-p
   :while
   :ipld-put
   :*use-ipld-based-object-id*
   :=id :=_id
   :hypernymy :<-denotational :<-subsumptive
   :hyponymy  :->denotational :->subsumptive
   :relations
   :=ucs :=>ucs
   :phonemic-values :sound
   :kangxi :shuowen
   :ideographic-strokes :total-strokes
   :structure :ideographic-structure
   :misc
   :encode-json))

(in-package :concord)

(defmacro while (test &body body)
  `(loop while ,test do ,@body))

(defun json-encode-vector-with-sort (vec)
  (let ((len (length vec))
	item dest)
    (cond
      ((> len 0)
       (setq item (aref vec 0))
       (cond
	 ((numberp item)
	  (setq vec (sort vec #'<))
	  )
	 ((stringp item)
	  (setq vec (sort vec #'string<))
	  ))
       (setq dest
	     (apply #'concatenate
		    'string
		    (map 'list
			 (lambda (cell)
			   (if (symbolp cell)
			       (format nil ",\"~a\"" cell)
			       (format nil ",~S" cell)))
			 vec)))
       (setf (aref dest 0) #\[)
       (concatenate 'string dest "]")
       ))))

(defun json-encode-bare-ccs-spec (spec)
  (let (dest)
    (setq dest
	  (apply #'concatenate
		 'string
		 (mapcar (lambda (pair)
			   (format
			    nil
			    ",\"~a\":~a"
			    (car pair)
			    (json-encode-vector-with-sort (cdr pair))))
			 spec)))
    (setf (aref dest 0) #\{)
    (concatenate 'string dest "}")))       

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

(defmethod ds-adjoin ((ds redis-ds) key value)
  (unless (string= (red:type key) "set")
    (red:del key))
  (red:sadd key value))

(defmethod ds-set-members ((ds redis-ds) key value)
  (let (ret)
    (red:del key)
    (cond ((null value)
	   (ds-set-atom ds key value)
	   )
	  (t
	   (when (integerp (setq ret (apply #'red:sadd key
					    (mapcar (lambda (unit)
						      (format nil "~S" unit))
						    value))))
	     (values value ret))
	   ))))

(defmethod ds-get-members ((ds redis-ds) key)
  (mapcar #'read-from-string (red:smembers key)))

(defmethod ds-intersection ((ds redis-ds) &rest keys)
  (mapcar #'read-from-string (apply #'red:sinter keys)))

(defmethod ds-union ((ds redis-ds) &rest keys)
  (mapcar #'read-from-string (apply #'red:sunion keys)))

(defmethod ds-store-union ((ds redis-ds) dest-key &rest keys)
  (apply #'red:sunionstore dest-key keys))

(defmethod ds-get ((ds redis-ds) key &optional default-value)
  (cond ((string= (red:type key) "list")
	 (ds-get-list ds key)
	 )
	((string= (red:type key) "set")
	 (ds-get-members ds key)
	 )
	(t
	 (ds-get-atom ds key default-value)
	 )))

(defmethod ds-get-object-feature-names ((ds redis-ds) genre-name id
					&key (require-system-features nil))
  (let ((pat (format nil "~(~a~):obj:~a;" genre-name id))
	len fname dest)
    (setq len (length pat))
    (dolist (key (red:keys (format nil "~a*" pat)))
      (setq fname (subseq key len))
      (when (or require-system-features
		(and (not (eq (search "$_" fname) 0))
		     (not (eq (search "=_" fname) 0))))
	(setq dest (cons fname dest))))
    dest))

(defmethod ds-get-object-spec ((ds redis-ds) genre-name id)
  (let ((pat (format nil "~(~a~):obj:~a;" genre-name id))
	len)
    (setq len (length pat))
    (mapcar (lambda (key)
	      (cons (read-from-string (subseq key len))
		    (ds-get ds key)))
	    (red:keys (format nil "~a*" pat)))))

(defmethod ds-some-in-feature ((ds redis-ds) func genre-name feature-name)
  (let ((pat (format nil "~(~a~):obj:*;~a" genre-name feature-name)))
    (some func (red:keys pat))))

(defun some-in-feature (func feature-name &key genre ds)
  (unless genre
    (setq genre 'default))
  (unless ds
    (setq ds *default-ds*))
  (let (pos end id obj ret)
    (cond
      ((or (eq feature-name 'ideographic-structure)
	   (equal feature-name "ideographic-structure"))
       (ds-some-in-feature
	ds
	(lambda (key)
	  (setq end (position #\; key :from-end t))
	  (setq pos (position #\: key :from-end t :end end))
	  (setq id (read-from-string (subseq key (1+ pos) end)))
	  (setq obj (object genre id :ds ds))
	  (setq ret (gethash
		     obj
		     *ideographic-structure-feature-hash*
		     'unload))
	  (if (eq ret 'unload)
	      (setq ret (ds-get ds key)))
	  (funcall func obj ret))
	genre  feature-name)
       )
      (t
       (ds-some-in-feature
	ds
	(lambda (key)
	  (setq end (position #\; key :from-end t))
	  (setq pos (position #\: key :from-end t :end end))
	  (setq id (read-from-string (subseq key (1+ pos) end)))
	  (funcall func
		   (object genre id :ds ds)
		   (ds-get ds key)))
	genre  feature-name)
       ))))

(defun intersection-in-feature (feature-name &rest objects)
  (let (genre ds)
    (when (and (object-p (car objects))
	       (setq genre (object-genre (car objects)))
	       (setq ds (genre-ds genre)))
      (apply #'ds-intersection
	     ds (mapcar (lambda (obj)
			  (format nil "~a:obj:~a;~a"
				  (genre-name genre)
				  (object-id obj)
				  feature-name))
			objects)))))

(defun union-in-feature (feature-name &rest objects)
  (let (genre ds)
    (when (and (object-p (car objects))
	       (setq genre (object-genre (car objects)))
	       (setq ds (genre-ds genre)))
      (apply #'ds-union
	     ds (mapcar (lambda (obj)
			  (format nil "~a:obj:~a;~a"
				  (genre-name genre)
				  (object-id obj)
				  feature-name))
			objects)))))

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
  (let ((index (format nil "~(~a~):idx:~a;~(~a~)"
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
  (format out "#.(concord:object :~(~a~) ~a)"
	  (genre-name (object-genre obj))
	  (let ((id (object-id obj)))
	    (if (symbolp id)
		(format nil "'~a" id)
		id))))

(defun metadata-feature-name-p (feature-name)
  (if (symbolp feature-name)
      (setq feature-name (symbol-name feature-name)))
  (search "*" feature-name))

(defun id-feature-name-p* (feature-name)
  (and (progn
	 (if (symbolp feature-name)
	     (setq feature-name (format nil "~a" feature-name)))
	 (eql (elt feature-name 0) #\=))
       (not (let ((pos (search "decomposition" feature-name)))
	      (and pos
		   (or (= pos 1)
		       (and (= pos 2)
			    (eql (elt feature-name 1) #\>))))))))

(defun id-feature-name-p (feature-name)
  (and (not (metadata-feature-name-p feature-name))
       (id-feature-name-p* feature-name)))

(defun structure-feature-name-p (feature-name)
  (and (not (metadata-feature-name-p feature-name))
       (progn
	 (if (symbolp feature-name)
	     (setq feature-name (format nil "~a" feature-name)))
	 (eql (search "ideographic-structure" feature-name) 0))))

(defun decomposition-feature-name-p (feature-name)
  (and (not (metadata-feature-name-p feature-name))
       (progn
	 (if (symbolp feature-name)
	     (setq feature-name (format nil "~a" feature-name)))
	 (eql (search "=decomposition" feature-name) 0))))

(defun products-feature-name-p (feature-name)
  (and (not (metadata-feature-name-p feature-name))
       (progn
	 (if (symbolp feature-name)
	     (setq feature-name (format nil "~a" feature-name)))
	 (eql (search "ideographic-products" feature-name) 0))))

(defun relation-feature-name-p* (feature-name)
  (if (symbolp feature-name)
      (setq feature-name (format nil "~a" feature-name)))
  (or (eql (search "<-" feature-name) 0)
      (eql (search "->" feature-name) 0)))

(defun relation-feature-name-p (feature-name)
  (and (not (metadata-feature-name-p feature-name))
       (relation-feature-name-p* feature-name)))

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

(defun split-metadata-feature-name (feature-name)
  (if (symbolp feature-name)
      (setq feature-name (format nil "~a" feature-name)))
  (let (pos istr ibeg base ibase meta idx)
    (when (and (setq pos (search "*" feature-name))
	       (< 0 pos)
	       (< pos (1- (length feature-name))))
      (setq base (subseq feature-name 0 pos)
	    meta (read-from-string (format nil ":~a" (subseq feature-name (1+ pos)))))
      (cond
	((and (setq ibeg (search "$_" base))
	      (setq istr (subseq base (+ ibeg 2) pos)
		    ibase (subseq base 0 ibeg))
	      (multiple-value-bind (val iend)
		  (read-from-string istr)
		(and (numberp val)
		     (= iend (length istr))
		     (setq idx val))))
	 (list (read-from-string ibase) idx meta)
	 )
	(t
	 (list (read-from-string base) nil meta))))))

(defun split-feature-name-with-domain (feature-name)
  (if (symbolp feature-name)
      (setq feature-name (format nil "~a" feature-name)))
  (let (pos)
    (when (setq pos (search "@" feature-name))
      (cons (read-from-string (subseq feature-name 0 pos))
	    (read-from-string (subseq feature-name (1+ pos)))))))

(defun split-ccs-feature-name (feature-name)
  (if (eq feature-name '=ucs)
      (values "ucs" 'abstract-character 10)
      (let ((name (format nil "~a" feature-name)))
	(cond
	  ((eql (search "==>" name) 0)
	   (values (subseq name 3) 'super-abstract-character 0)
	   )
	  ((eql (search "=+>" name) 0)
	   (values (subseq name 3) 'unified-glyph 15)
	   )
	  ((eql (search "=>>" name) 0)
	   (values (subseq name 3) 'detailed-glyph 24)
	   )
	  ((eql (search "===" name) 0)
	   (values (subseq name 3) 'glyph-image 40)
	   )
	  ((eql (search "=>" name) 0)
	   (values (subseq name 2) 'abstract-character 10)
	   )
	  ((eql (search "==" name) 0)
	   (values (subseq name 2) 'abstract-glyph-form 30)
	   )
	  ((eql (search "=" name) 0)
	   (values (subseq name 1) 'abstract-glyph 20)
	   )))))

(defun expand-feature-name (feature domain)
  (if domain
      (format nil "~a@~a" feature domain)
    feature))

(defun feature (feature-name &key ds)
  (object :feature feature-name :ds ds))

(defun find-feature (feature-name)
  (genre-find-object (genre :feature) feature-name))

(defun find-object (genre object-spec)
  (let (ret)
    (if (find-if (lambda (feature-pair)
		   (and (id-feature-name-p (car feature-pair))
			(setq ret (decode-object (car feature-pair)
						 (cdr feature-pair)
						 :genre genre))))
		 object-spec)
	ret)))

(defun object-spec-to-id-spec (object-spec)
  (let (dest)
    (dolist (cell object-spec)
      (cond ((eq (car cell) '=_id)
	     )
	    ((concord:id-feature-name-p (car cell))
	     (setq dest (adjoin cell dest :test #'equal))
	     )
	    ((member (format nil "~a" (car cell))
		     '( ; "name" "name*"
		       "=>iwds-1*note")
		     :test #'equal)
	     (setq dest (adjoin cell dest :test #'equal))
	     )))
    dest))

(defun sort-value-list (value)
  (if (cdr value)
      (sort value #'<)
      value))

(defun register-combined-feature-value (combined-feature-alist base domain value)
  (let (ret0 ret i val)
    (cond ((setq ret0 (assoc base combined-feature-alist))
	   (cond ((setq ret (assoc domain (cdr ret0)))
		  (cond ((and (setq val (getf (cdr ret) :value))
			      (vectorp val))
			 (if (< (length val)(length value))
			     (setq val (adjust-array val (length value)
						     :initial-element nil)))
			 (setf (getf (cdr ret) :value) val)
			 (setq i 0)
			 (dolist (item value)
			   (setf (getf (aref val i) :value) item)
			   (setq i (1+ i)))
			 )
			(t
			 (setf (getf (cdr ret) :value) value)
			 ))
		  )
		 (t
		  (setf (cdr ret0)
			(cons (cons domain `(:value ,value))
			      (cdr ret0)))
		  ))
	   combined-feature-alist)
	  (t
	   (setf combined-feature-alist
		 (cons (cons base (list (cons domain
					      `(:value ,value))))
		       combined-feature-alist))
	   ))))

(defun register-combined-item-metadata (combined-feature-alist base domain idx meta value)
  (let (ret0 ret val)
    (cond ((setq ret0 (assoc base combined-feature-alist))
	   (cond ((setq ret (assoc domain (cdr ret0)))
		  (cond ((setq val (getf (cdr ret) :value))
			 (when (listp val)
			   (setq val (apply #'vector (mapcar (lambda (item)
							       (list :value item))
							     val))))
			 (if (< (length val) idx)
			     (setq val (adjust-array val idx :initial-element nil)))
			 )
			(t
			 (setq val (make-array idx :initial-element nil))
			 ))
		  (setf (getf (cdr ret) :value) val)
		  )
		 (t
		  (setq val (make-array idx :initial-element nil))
		  (setf (cdr ret0)
			(cons (cons domain (list :value val))
			      (cdr ret0)))
		  ))
	   )
	  (t
	   (setq val (make-array idx :initial-element nil))
	   (setq combined-feature-alist
		 (cons (cons base (list (cons domain
					      (list :value val))))
		       combined-feature-alist))
	   ))
    (setf (getf (aref val (1- idx)) meta) value)
    combined-feature-alist))

(defun register-combined-feature-metadata (combined-feature-alist base domain meta value)
  (let (ret0 ret)
    (cond ((setq ret0 (assoc base combined-feature-alist))
	   (cond ((setq ret (assoc domain (cdr ret0)))
		  (setf (getf (cdr ret) meta) value)
		  combined-feature-alist)
		 (t
		  (setf (cdr ret0)
			(cons (cons domain
 				    (list meta value))
			      (cdr ret0)))
		  ))
	   )
	  (t
	   (setf combined-feature-alist
		 (cons (cons base (list (cons domain
					      (list meta value))))
		       combined-feature-alist))
	   ))
    combined-feature-alist))

(defun ideographic-structure-granularity (structure)
  (let ((granularity-rank -1)
	granularity ret)
    (dolist (comp (cdr structure))
      (cond
	((and (association-list-p comp)
	      (setq ret (assoc 'ideographic-structure comp)))
	 (multiple-value-bind (gname rank)
	     (ideographic-structure-granularity (cdr ret))
	   (if (< granularity-rank rank)
	       (setq granularity-rank rank 
		     granularity gname)))
	 )
	(t
	 (multiple-value-bind (g-spec gname rank)
	     (object-spec-to-grain-spec
	      (object-spec
	       (cond ((characterp comp)
		      (object :character (char-code comp))
		      )
		     (t comp))))
	   (if (< granularity-rank rank)
	       (setq granularity-rank rank 
		     granularity gname))))))
    (values granularity granularity-rank)))

(defun object-spec-to-grain-spec (object-spec)
  (let ((granularity-rank -1)
	granularity ret dest
	structure-alist base domain structure)
    (dolist (cell object-spec)
      (cond ((eq (car cell) '=_id)
	     )
	    ((eq (car cell) '=>ucs)
	     )
	    ((null (cdr cell))
	     )
	    ((id-feature-name-p (car cell))
	     (multiple-value-bind (name gname rank)
		 (split-ccs-feature-name (car cell))
	       (if (< granularity-rank rank)
		   (setq granularity-rank rank 
			 granularity gname))
	       (setq name (read-from-string name))
	       (cond
		 ((setq ret (assoc name dest))
		  (unless (member (cdr cell) ret)
		    (setf (cdr (assoc name dest))
			  (cons (cdr cell) (cdr ret))))
		  )
		 (t
		  (setq dest (cons (list name (cdr cell))
				   dest))
		  )))
	     )
	    ((member (format nil "~a" (car cell))
		     '( ; "name" "name*"
		       "=>iwds-1*note")
		     :test #'equal)
	     (setq dest (adjoin cell dest :test #'equal))
	     )
	    ((structure-feature-name-p (car cell))
	     (if (setq ret (split-feature-name-with-domain (car cell)))
		 (setq base (car ret)
		       domain (cdr ret))
		 (setq base (car cell)))
	     (setq structure-alist
	      	   (register-combined-feature-value
	     	    structure-alist base domain (cdr cell)))
	     )))
    (cond (dest
	   (setq dest
		 (mapcar (lambda (cell)
			   (if (consp (cdr cell))
			       (cons (car cell)
				     (apply #'vector (sort-value-list (cdr cell))))
			       cell))
			 dest))
	   )
	  (structure-alist
	   (setq ret (car (cdr (assoc 'ideographic-structure structure-alist))))
	   (setq domain (car ret)
		 structure (getf (cdr ret) :value))
	   (multiple-value-bind (gname rank)
	       (ideographic-structure-granularity structure)
	     (setq granularity gname
		   granularity-rank rank))
	   ))
    (values dest granularity granularity-rank structure-alist)))

(defun separate-object-spec (object-spec)
  (let ((granularity-rank -1)
	granularity ret dest
	id-meta-list
	fname base domain
	fname-str type pos
	radical-str
	meta-spec
	hypernymy-alist
	hyponymy-alist
	radical-strokes-alist
	phonemic-values-alist
	structure-alist structure
	misc-alist
	relations-alist)
    (dolist (cell object-spec)
      (setq fname (car cell))
      (cond ((eq fname '=_id)
	     )
	    ((eq fname '=>ucs)
	     (setq misc-alist
	      	   (register-combined-feature-value
		    misc-alist fname nil (cdr cell)))
	     )
	    ((null (cdr cell))
	     (if (setq ret (split-feature-name-with-domain fname))
		 (setq base (car ret)
		       domain (cdr ret))
		 (setq base fname
		       domain nil))
	     (setq misc-alist
	      	   (register-combined-feature-value
		    misc-alist base domain (cdr cell)))
	     )
	    ((setq meta-spec (split-metadata-feature-name fname))
	     (cond ((id-feature-name-p* (car meta-spec))
		    (setq id-meta-list (cons cell id-meta-list))
		    )
		   (t
		    (if (setq ret (split-feature-name-with-domain (car meta-spec)))
			(setq base (car ret)
			      domain (cdr ret))
			(setq base (car meta-spec)
			      domain nil))
		    (setq fname-str (format nil "~a" fname))
		    (cond
		      ((eq base 'ideographic-structure)
		       (setq structure-alist
			     (cond
 			       ((null (nth 1 meta-spec))
				(register-combined-feature-metadata
				 structure-alist
				 base domain (nth 2 meta-spec) (cdr cell))
				)
			       (t
				(register-combined-item-metadata
				 structure-alist
				 base domain (nth 1 meta-spec)
				 (nth 2 meta-spec) (cdr cell))
				)))
		       )
		      ((eq base 'sound)
		       (setq phonemic-values-alist
			     (cond ((null (nth 1 meta-spec))
				    (register-combined-feature-metadata
				     phonemic-values-alist
				     base domain (nth 2 meta-spec) (cdr cell))
				    )
				   (t
				    (register-combined-item-metadata
				     phonemic-values-alist
				     base domain (nth 1 meta-spec)
				     (nth 2 meta-spec) (cdr cell))
				    )))
		       )
		      ((eq base 'total-strokes)
		       (setq radical-strokes-alist
			     (register-combined-feature-metadata
			      radical-strokes-alist
			      nil domain
			      (read-from-string
			       (format nil ":total-strokes*~a" (nth 2 meta-spec)))
			      (cdr cell)))
		       )
		      ((setq pos (search "-radical" fname-str))
		       (setq type (subseq fname-str 0 pos))
		       (setq type (if (string= type "ideographic")
				      'kangxi
				      (read-from-string type)))
		       (register-combined-feature-metadata
			radical-strokes-alist
			type domain
			(read-from-string
			 (format nil ":radical-number*~a" (nth 2 meta-spec)))
			(cdr cell))
		       )
		      ((setq pos (search "-strokes" fname-str))
		       (setq type (subseq fname-str 0 pos))
		       (setq type (if (string= type "ideographic")
				      'kangxi
				      (read-from-string type)))
		       (setq radical-strokes-alist
			     (register-combined-feature-metadata
			      radical-strokes-alist
			      type domain
			      (read-from-string
			       (format nil ":body-strokes*~a" (nth 2 meta-spec)))
			      (cdr cell)))
		       )
		      ((relation-feature-name-p* base)
		       (cond ((member base '(<-denotational <-subsumptive))
			      (setq hypernymy-alist
				    (cond
 				      ((null (nth 1 meta-spec))
				       (register-combined-feature-metadata
					hypernymy-alist
					base domain (nth 2 meta-spec) (cdr cell))
				       )
				      (t
	      			       (register-combined-item-metadata
					hypernymy-alist
					base domain (nth 1 meta-spec)
					(nth 2 meta-spec) (cdr cell))
				       )))
			      )
			     ((member base '(->denotational ->subsumptive))
			      (setq hyponymy-alist
				    (cond
 				      ((null (nth 1 meta-spec))
				       (register-combined-feature-metadata
					hyponymy-alist
					base domain (nth 2 meta-spec) (cdr cell))
				       )
				      (t
				       (register-combined-item-metadata
					hyponymy-alist
					base domain (nth 1 meta-spec)
					(nth 2 meta-spec) (cdr cell))
				       )))
			      )
			     (t
			      (setq relations-alist
				    (cond
 				      ((null (nth 1 meta-spec))
				       (register-combined-feature-metadata
					relations-alist
					base domain (nth 2 meta-spec) (cdr cell))
				       )
				      (t
				       (register-combined-item-metadata
					relations-alist
					base domain (nth 1 meta-spec)
					(nth 2 meta-spec) (cdr cell)))
				      ))
			      ))
		       )
		      ((null (nth 1 meta-spec))
		       (setq misc-alist
			     (register-combined-feature-metadata
			      misc-alist
			      base domain (nth 2 meta-spec) (cdr cell)))
		       )
		      (t
		       (setq misc-alist
			     (register-combined-item-metadata
			      misc-alist
			      base domain (nth 1 meta-spec)
			      (nth 2 meta-spec) (cdr cell)))
		       ))))
	     )
	    ((id-feature-name-p* fname)
	     (multiple-value-bind (name gname rank)
		 (split-ccs-feature-name fname)
	       (if (< granularity-rank rank)
		   (setq granularity-rank rank 
			 granularity gname))
	       (setq name (read-from-string name))
	       (cond
		 ((setq ret (assoc name dest))
		  (unless (member (cdr cell) ret)
		    (setf (cdr (assoc name dest))
			  (cons (cdr cell) (cdr ret))))
		  )
		 (t
		  (setq dest (cons (list name (cdr cell))
				   dest))
		  )))
	     )
	    ((relation-feature-name-p* fname)
	     (if (setq ret (split-feature-name-with-domain fname))
		 (setq base (car ret)
		       domain (cdr ret))
		 (setq base fname
		       domain nil))
	     (cond ((member base '(<-denotational <-subsumptive))
		    (setq hypernymy-alist
	      		  (register-combined-feature-value
	     		   hypernymy-alist base domain (cdr cell)))
		    )
		   ((member base '(->denotational ->subsumptive))
		    (setq hyponymy-alist
	      		  (register-combined-feature-value
	     		   hyponymy-alist base domain (cdr cell)))
		    )
		   (t
		    (setq relations-alist
	      		  (register-combined-feature-value
	     		   relations-alist base domain (cdr cell)))
		    ))
	     )
	    ((member (format nil "~a" fname)
		     '( ; "name" "name*"
		       "=>iwds-1*note")
		     :test #'equal)
	     (setq dest (adjoin cell dest :test #'equal))
	     )
	    (t
	     (if (setq ret (split-feature-name-with-domain fname))
		 (setq base (car ret)
		       domain (cdr ret))
		 (setq base fname
		       domain nil))
	     (setq fname-str (format nil "~a" fname))
	     (cond ((eq base 'ideographic-structure)
		    (setq structure-alist
	      		  (register-combined-feature-value
	     		   structure-alist base domain (cdr cell)))
		    )
		   ((eq base 'sound)
		    (setq phonemic-values-alist
	      		  (register-combined-feature-value
	     		   phonemic-values-alist base domain (cdr cell)))
		    )
		   ((eq base 'total-strokes)
		    (setq radical-strokes-alist
			  (register-combined-feature-value
			   radical-strokes-alist
			   base domain
			   (cdr cell)))
		    )
		   ((setq pos (search "-radical" fname-str))
		    (setq type (subseq fname-str 0 pos))
		    (setq type (if (string= type "ideographic")
				   'kangxi
				   (read-from-string type)))
		    (setq radical-strokes-alist
			  (register-combined-feature-metadata
			   radical-strokes-alist
			   type domain
			   :radical-number
			   (cdr cell)))
		    (setq radical-str
			  (cond
			    ((eq type 'kangxi)
			     (format nil "~a" (code-char (+ #x2EFF (cdr cell))))
			     )
			    ((eq type 'shuowen)
			     (format nil "~a" (aref shuowen-radicals
						    (1- (cdr cell))))
			     )
			    ))
		    (when radical-str
		      (setq radical-strokes-alist
			    (register-combined-feature-metadata
			     radical-strokes-alist
			     type domain
			     :radical radical-str)))
		    )
		   ((setq pos (search "-strokes" fname-str))
		    (setq type (subseq fname-str 0 pos))
		    (setq type (if (string= type "ideographic")
				   'kangxi
				   (read-from-string type)))
		    (setq radical-strokes-alist
			  (register-combined-feature-metadata
			   radical-strokes-alist
			   type domain
			   :body-strokes (cdr cell)))
		    )
		   (t
		    (setq misc-alist
	      		  (register-combined-feature-value
	     		   misc-alist base domain (cdr cell)))
		    ))
	     )))
    (cond (dest
	   (setq dest
		 (mapcar (lambda (cell)
			   (if (consp (cdr cell))
			       (cons (car cell)
				     (apply #'vector (sort-value-list (cdr cell))))
			       cell))
			 dest))
	   )
	  (structure-alist
	   (setq ret (car (cdr (assoc 'ideographic-structure structure-alist))))
	   (setq domain (car ret)
		 structure (getf (cdr ret) :value))
	   (multiple-value-bind (gname rank)
	       (ideographic-structure-granularity structure)
	     (setq granularity gname
		   granularity-rank rank))
	   ))
    (values dest granularity granularity-rank id-meta-list
	    structure-alist
	    (nconc
	     (and hypernymy-alist
		  (list (cons 'hypernymy
			      hypernymy-alist)))
	     (and radical-strokes-alist
		  (list (cons 'radical-and-strokes
			      radical-strokes-alist)))
	     (and phonemic-values-alist
		  (list (cons 'phonemic-values
			      phonemic-values-alist)))
	     (and misc-alist
		  (list (cons 'misc misc-alist)))
	     (and hyponymy-alist
		  (list (cons 'hyponymy
			      hyponymy-alist))))
	    relations-alist)))

(defun define-object (genre object-spec &key id)
  (if (symbolp genre)
      (setq genre (genre (or genre 'default))))
  (unless id
    (setq id (cdr (assoc '=_id object-spec))))
  (let (ret obj)
    (unless id
      (when (setq ret (assoc '=id object-spec))
	(setq id (cdr ret))))
    (unless id
      (when (eql (genre-name genre) 'character)
	(setq id (cdr (assoc '=ucs object-spec)))))
    (cond (id
	   (setq obj (genre-make-object genre id))
	   )
	  ((setq obj (find-object genre object-spec))
	   )
	  ((and *use-ipld-based-object-id*
		(setq id (generate-object-cid genre object-spec)))
	   (setq obj (genre-make-object genre id))
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
    ((symbolp object-rep)
     (let ((name (format nil "~a" object-rep)))
       (if (and (eql (aref name 0) #\?)
		(= (length name) 2))
	   (aref name 1)
	   object-rep))
     )
    ((association-list-p object-rep)
     (if genre
      	 (or (find-object genre object-rep)
	     (if (some (lambda (pair)
			 (id-feature-name-p (car pair)))
		       object-rep)
		 (define-object genre object-rep)
		 object-rep))
     	 (let* ((ucs (cdr (assoc '=ucs object-rep)))
     		(obj (find-object 'character object-rep)))
	   (cond (obj
		  (if (and (integerp (object-id obj))
     			   (< (object-id obj) #xF0000))
		      (code-char (object-id obj))
     		      obj)
		  )
		 (ucs
		  (code-char ucs)
		  )
		 ((some (lambda (pair)
			  (id-feature-name-p (car pair)))
			object-rep)
		  (define-object 'character object-rep)
		  )
		 (t
		  object-rep)
		 )))
     )
    ((and (consp object-rep)
	  (symbolp (car object-rep))
	  (association-list-p (nth 1 object-rep)))
     (define-object (genre (car object-rep) :ds ds)
	 (nth 1 object-rep))
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
	  ((decomposition-feature-name-p feature)
	   (setq rep-list (mapcar #'normalize-object-representation
				  value))
	   (ds-set-list ds key rep-list)
	   )
	  ((structure-feature-name-p feature)
	   (setq rep-list (mapcar #'normalize-object-representation
				  value))
	   (if (or (eq feature 'ideographic-structure)
		   (equal feature "ideographic-structure"))
	       (setf (gethash obj *ideographic-structure-feature-hash*)
		     rep-list))
	   (ds-set-list ds key rep-list)
	   )
	  ((products-feature-name-p feature)
	   (setq rep-list (mapcar #'normalize-object-representation
				  value))
	   (ds-set-members ds key rep-list)
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

(defmethod object-adjoin ((obj object) feature item)
  (if (products-feature-name-p feature)
      (let* ((genre (object-genre obj))
	     (ds (genre-ds genre))
	     (key (format nil "~a:obj:~a;~a"
			  (genre-name genre)
			  (object-id obj)
			  feature)))
	(ds-adjoin ds key (normalize-object-representation item)))
      (let ((ret (object-get obj feature)))
	(unless (member obj ret)
	  (object-put obj feature (cons obj ret))))))

(defmethod store-union-in-feature (feature-name (dest-obj object) &rest objects)
  (let (genre ds)
    (setq genre (object-genre dest-obj)
	  ds (genre-ds genre))
    (apply #'ds-store-union
	   ds
 	   (format nil "~a:obj:~a;~a"
		   (genre-name genre)
		   (object-id dest-obj)
		   feature-name)
	   (mapcar (lambda (obj)
		     (format nil "~a:obj:~a;~a"
			     (genre-name genre)
			     (if (characterp obj)
				 (char-code obj)
				 (object-id obj))
			     feature-name))
		   objects))))

(defmethod object-get ((obj object) feature &optional default-value
		       &key (recursive nil))
  (let* ((genre (object-genre obj))
	 (key (format nil "~a:obj:~a;~a"
		      (genre-name genre)
		      (object-id obj)
		      feature))
	 (unbound (gensym))
	 ret)
    (cond ((or (eq feature 'ideographic-structure)
	       (equal feature "ideographic-structure"))
	   (setq ret (gethash
		      obj
		      *ideographic-structure-feature-hash*
		      'unload))
	   (cond
	     ((eq ret 'unload)
	      (setq ret
		    (if (string= (red:type key) "list")
			(ds-get-list (genre-ds genre) key)
			(ds-get-atom (genre-ds genre) key)))
	      (setf (gethash obj *ideographic-structure-feature-hash*)
		    ret)
	      ret)
	     (t
	      ret))
	   )
	  ((string= (red:type key) "list")
	   (ds-get-list (genre-ds genre) key)
	   )
	  ((string= (red:type key) "set")
	   (ds-get-members (genre-ds genre) key)
	   )
	  (t
	   (setq ret (ds-get-atom (genre-ds genre) key unbound))
	   (if (eq ret unbound)
	       (or (if recursive
		       (or (dolist (parent (object-get obj "<-subsumptive"))
			     (setq ret (object-get parent feature
						   unbound :recursive t))
			     (unless (eq ret unbound)
			       (return ret)))
			   (dolist (parent (object-get obj "<-denotational"))
			     (setq ret (object-get parent feature
						   unbound :recursive t))
			     (unless (eq ret unbound)
			       (return ret)))))
		   default-value)
	       ret)))))

(defmethod object-get ((obj character) feature &optional default-value
		       &key (recursive nil))
  (object-get (concord:object :character (char-code obj))
	      feature default-value
	      :recursive recursive))

(defmethod object-spec ((obj object) &key (require-system-features nil))
  (let* ((genre (object-genre obj))
	 (ds (genre-ds genre))
	 dest)
    (dolist (fname (ds-get-object-feature-names
		    ds (genre-name genre) (object-id obj)
		    :require-system-features require-system-features))
      (when (or require-system-features
		(not (member fname '("ideographic-products") :test #'equal)))
	(setq dest (cons (cons (read-from-string fname)
			       (concord:object-get obj fname))
			 dest))))
    dest))

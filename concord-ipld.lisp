(in-package :concord)

(defvar *use-ipld-based-object-id* nil)

(defun ipld-put (data &key pin json-input)
  (let ((in (flexi-streams:make-in-memory-input-stream
	     (map 'vector #'char-code
 		  (if json-input
		      data
		      (let ((json:*lisp-identifier-name-to-json* #'identity)
			    (s (make-string-output-stream)))
			(encode-json data s)
			(get-output-stream-string s)))))))
    (read-from-string
     (ipfs::ipfs-call "dag/put" `(("pin" ,pin))
		      :parameters `((:stream ,in))))))

(defun ipld-get (cid)
  (trivial-utf-8:utf-8-bytes-to-string (map 'vector #'char-code (ipfs::ipfs-call "dag/get" `(("arg" ,cid))))))

(defmethod generate-object-cid ((g genre) spec)
  (cond
    ((eql (genre-name g) 'character)
     (multiple-value-bind (g-spec granularity granularity-rank structure-spec)
	 (object-spec-to-grain-spec spec)
       (let ((u-cid (ipld-put
		     (if g-spec
			 (json-encode-bare-ccs-spec g-spec)
			 (let ((json:*lisp-identifier-name-to-json* #'identity)
			       (s (make-string-output-stream)))
			   (encode-json-feature-spec structure-spec s)
			   (get-output-stream-string s)))
		     :json-input t)))
	 (ipld-put
	  (format nil
		  "{\"granularity\": \"~a\",\"spec\":{\"/\":\"~a\"}}"
		  granularity u-cid)
	  :json-input t)))
     )
    (t
     (generate-object-id g)
     )))

(defun symbol-base32-cid-p (symbol)
  (and (symbolp symbol)
       (= (length (symbol-name symbol)) 59)
       (eql (aref (symbol-name symbol) 0) #\B)
       (eql (aref (symbol-name symbol) 1) #\A)))

(defmethod set-object-cid ((obj object))
  (let ((id (object-id obj))
	(spec (object-spec obj))
	(genre (object-genre obj))
	u-cid ref-cid node-cid node-json-spec)
    (multiple-value-bind (g-spec granularity granularity-rank
			  id-meta-list structure-spec node-spec rel-spec)
	(separate-object-spec spec)
      (cond
	((symbol-base32-cid-p id)
	 (setq ref-cid id)
	 )
	(t
	 (setq u-cid
	       (cond
		 (g-spec
		  (ipld-put (json-encode-bare-ccs-spec g-spec)
			    :json-input t)
		  )
		 (structure-spec
		  (ipld-put (let ((json:*lisp-identifier-name-to-json* #'identity)
				  (s (make-string-output-stream)))
			      (encode-json-feature-spec structure-spec s)
			      (get-output-stream-string s))
			    :json-input t)
		  )
		 (node-spec
		  (ipld-put (let ((json:*lisp-identifier-name-to-json* #'identity)
				  (s (make-string-output-stream)))
			      (concord::encode-json-category-spec node-spec s)
			      (get-output-stream-string s))
			    :json-input t)
		  )
		 (t
		  (ipld-put `((=_id . ,id)))
		  )))
	 (setq ref-cid (ipld-put
			(format nil
				"{\"granularity\": \"~a\",\"spec\":{\"/\":\"~a\"}}"
				granularity u-cid)
			:json-input t))
	 ))
      (when structure-spec
	(setq node-spec
	      (cons (cons 'structure structure-spec)
		    node-spec)))
      (when node-spec
	(setq node-json-spec
	      (format nil
		      "{\"genre\":\"~a\",\"id\":\"~a\",\"ref\":{\"/\":\"~a\"},~a"
		      (genre-name genre)
		      id
		      ref-cid
		      (subseq
		       (let ((json:*lisp-identifier-name-to-json* #'identity)
			     (s (make-string-output-stream)))
			 (json:encode-json
			  (mapcar (lambda (cell)
				    (cons
				     (car cell)
				     (list
				      (cons "/"
					    (ipld-put
					     (let ((fss (make-string-output-stream)))
					       (encode-json-feature-spec (cdr cell) fss)
					       (get-output-stream-string fss))
					     :json-input t)))))
				  node-spec)
			  s)
			 (get-output-stream-string s))
		       1)))
	(setq node-cid (ipld-put node-json-spec :json-input t))
	(object-put obj "=_node-cid" node-cid)
	)
      node-cid)))

(defun setup-object-cid-in-genre (genre)
  (some-in-feature (lambda (obj val)
		     (set-object-cid obj)
		     nil)
		   '=_id :genre genre))

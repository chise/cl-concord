(in-package :concord)

(defmethod json:encode-json ((obj concord:object) &optional (stream json:*json-output*))
  (let ((genre (concord:genre-name (concord:object-genre obj)))
	(id (concord:object-id obj)))
    (json:with-object (stream)
      (json:encode-object-member "genre" genre stream)
      (cond ((symbol-base32-cid-p id)
	     (json:as-object-member ("ref" stream)
	       (json:with-object (stream)
		 (json:encode-object-member "/" id stream)))
	     )
	    (t
	     (json:encode-object-member "id" id stream)
	     )))
    stream))

(defmethod json:encode-json ((s symbol) &optional (stream json:*json-output*))
  "Write the JSON representation of the symbol S to STREAM (or to
*JSON-OUTPUT*).  If S is boolean, a boolean literal is written.
Otherwise, the name of S is passed to *LISP-IDENTIFIER-NAME-TO-JSON*
and the result is written as String."
  (let ((mapped (car (rassoc s json::+json-lisp-symbol-tokens+))))
    (if mapped
        (progn (write-string mapped stream) nil)
        (let ((s (funcall json:*lisp-identifier-name-to-json* (format nil "~a" s))))
          (json::write-json-string s stream)))))

(defun encode-identifier-name-to-json (str)
  (if (or (eql (aref str 0) #\=)
	  (eql (search "<-" str) 0)
	  (eql (search "->" str) 0))
      (identity str)
      (json:lisp-to-camel-case str)))

(defun encode-json (obj &optional (stream json:*json-output*))
  (let ((json:*lisp-identifier-name-to-json* #'encode-identifier-name-to-json))
    (json:encode-json obj stream)))

(defun encode-json-feature-value-list (obj &optional (stream json:*json-output*))
  (json:with-array (stream)
    (dolist (cell obj)
      (json:as-array-member (stream)
	(encode-json-feature-value cell stream)))))

(defun encode-json-feature-value-vector (obj &optional (stream json:*json-output*))
  (let ((len (length obj))
	cell)
    (json:with-array (stream)
      (loop for i from 0
	    while (< i len)
	    do
	       (setq cell (aref obj i))
	       (json:as-array-member (stream)
		 (encode-json-feature-value cell stream))))))

(defun encode-json-feature-value (obj &optional (stream json:*json-output*))
  (cond ((stringp obj)
	 (encode-json obj stream)
	 )
	((vectorp obj)
	 (encode-json-feature-value-vector obj stream)
	 )
	((listp obj)
	 (cond ((association-list-p obj)
		(encode-json obj stream)
		)
	       ((keywordp (car obj))
		(encode-json-feature-value-plist obj stream)
		)
	       (t
		(encode-json-feature-value-list obj stream)
		))
	 )
	(t
	 (encode-json obj stream)
	 )))

(defun encode-json-feature-value-plist (obj &optional (stream json:*json-output*))
  (let (ret)
    (json:with-object (stream)
      (loop (unless (setq ret (pop obj))
	      (return))
	    (json:as-object-member (ret stream)
	      (encode-json-feature-value (pop obj) stream))))))

(defun encode-json-feature-domains (obj &optional (stream json:*json-output*))
  (json:with-object (stream)
    (dolist (cell obj)
      (json:as-object-member ((car cell) stream)
	(encode-json-feature-value (cdr cell) stream)))))

(defun encode-json-feature-spec (obj &optional (stream json:*json-output*))
  (let ((json:*lisp-identifier-name-to-json* #'identity))
    (json:with-object (stream)
      (dolist (cell obj)
	(json:as-object-member ((car cell) stream)
	  (encode-json-feature-domains (cdr cell) stream))))))

(defun encode-json-category-spec (obj &optional (stream json:*json-output*))
  (let ((json:*lisp-identifier-name-to-json* #'identity))
    (json:with-object (stream)
      (dolist (cell obj)
	(json:as-object-member ((car cell) stream)
	  (encode-json-feature-spec (cdr cell) stream))))))

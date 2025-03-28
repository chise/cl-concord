(in-package :concord)

(require 'cl-json)

(defmethod json:encode-json ((obj concord:object) &optional (stream json:*json-output*))
  (let ((genre (concord:genre-name (concord:object-genre obj)))
	(id (concord:object-id obj)))
    (json:encode-json
     (list (cons "genre" genre)
	   (cons "ref"
		 (if (and (symbolp id)
			  (= (length (symbol-name id)) 59)
			  (eql (aref (symbol-name id) 0) #\B)
			  (eql (aref (symbol-name id) 1) #\A))
		     (list (cons "/" (format nil "~a" id)))
		     id))))))

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

(defun encode-json (obj &optional (stream json:*json-output*))
  (let ((json:*lisp-identifier-name-to-json*
	  (lambda (str)
	    (if (or (eql (aref str 0) #\=)
		    (eql (search "<-" str) 0)
		    (eql (search "->" str) 0))
		(identity str)
		(json:lisp-to-camel-case str)))))
    (json:encode-json obj stream)))

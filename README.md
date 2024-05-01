# cl-concord
A Common Lisp based implementation of Concord.
Concord is a framework to handle prototype based Object Oriented
Database, which is used as the backend of the CHISE system.


## Installation

0. Install Redis or Valkey server

1. Clone this repository in a ql:*local-project-directories*

```
* ql:*local-project-directories*
(#P"/home/me/quicklisp/local-projects/")
* (quit)

% cd ~/quicklisp/local-projects/

% git clone https://gitlab.chise.org/CHISE/cl-concord.git
```

2. Register it

```
% sbcl

* (ql:register-local-projects)
NIL
* (quit)
```

3. Load it

```
% sbcl

* (ql:quickload :cl-concord)
```


## Usage

```
(setf (readtable-case *readtable*) :invert)
:invert

(setq test-obj (concord:define-object :test '((=foo . test-id-1)(name . "test-1"))))
-> #.(concord:object :test 0)

(concord:object-get test-obj 'name)
-> test-1

(concord:object-get test-obj '=foo)
-> test-id-1

(concord:decode-object "=foo" 'test-id-1 :genre 'test)
-> #.(concord:object :test 0)

(eq test-obj (concord:decode-object '=foo 'test-id-1 :genre 'test))
-> t

(concord:object-put test-obj 'note "This is sample.")
-> "This is sample."

(concord:object-spec test-obj)
-> ((=_id . 0) (note . "This is sample.") (name . "test-1") (=foo . test-id-1))

(setq test-obj2 (concord:define-object :test '((=foo . test-id-2)(name . "test-2"))))
-> #.(concord:object :test 1)

(concord:object-put test-obj '<-rel (list test-obj2))
-> (#.(concord:object :test 1))

(concord:object-spec test-obj)
-> ((=_id . 0) (note . "This is sample.") (name . "test-1")
    (<-rel #.(concord:object :test 1)) (=foo . test-id-1))

(concord:object-spec test-obj2)
-> ((=foo . test-id-2) (name . "test-2") (=_id . 1)
    (->rel #.(concord:object :test 0)))

(concord:some-in-feature
	(lambda (obj val)
	  (format t "~a : ~a~%"
		  obj val)
	  nil)
	"name" :genre 'test)
->
#.(concord:object :test 0) : test-1
#.(concord:object :test 1) : test-2
nil

(concord:some-in-feature
	(lambda (obj val)
	  (format t "~a : ~a~%"
		  obj val)
	  nil)
	"=foo" :genre 'test)
#.(concord:object :test 1) : test-id-2
#.(concord:object :test 0) : test-id-1
nil

(concord:some-in-feature
	(lambda (obj val)
	  (format t "~a : ~a~%"
		  obj val)
	  t)
	"=foo" :genre 'test)
#.(concord:object :test 1) : test-id-2
t
```

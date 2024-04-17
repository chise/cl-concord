# cl-concord
A Common Lisp based implementation of Concord.
Concord is a framework to handle prototype based Object Oriented
Database, which is used as the backend of the CHISE system.


## Usage

```
(setq test-obj (concord:define-object :test '((=foo . test-id-1)(name . "test-1"))))
-> #.(concord:object :test 0)

(concord:object-get test-obj 'name)
-> test-1

(concord:object-get test-obj '=foo)
-> test-id-1

(concord:decode-object '=foo 'test-id-1 :genre 'test)
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
(concord:object-spec test-obj)
-> ((=_id . 0) (note . "This is sample.") (name . "test-1")
    (<-rel #.(concord:object :test 1)) (=foo . test-id-1))

(concord:object-spec test-obj2)
-> ((=foo . test-id-2) (name . "test-2") (=_id . 1)
    (->rel #.(concord:object :test 0)))
```

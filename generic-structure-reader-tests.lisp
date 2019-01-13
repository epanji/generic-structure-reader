;;;; generic-structure-reader-tests.lisp

(defpackage #:generic-structure-reader/tests
  (:use #:common-lisp #:generic-structure-reader #:fiveam)
  (:export #:run-suite-tests))
(in-package #:generic-structure-reader/tests)

(defmacro with-dummy-data (ignored &body body)
  (defstruct foo a b)
  (defstruct bar b c)
  (defstruct baz z)
  `(let ((foo (make-foo :a 1 :b 2))
         (bar (make-bar :b 11 :c 22)))
     (declare (ignore ,@ignored))
     (handler-bind ((warning (lambda (c)
                               (declare (ignore c))
                               (muffle-warning))))
       (define-generic-structure-reader foo-a (foo))
       (define-generic-structure-reader bar-b (bar))
       ,@body)))

(defun run-suite-tests ()
  (run! 'generic-structure-reader-suite))

(def-suite generic-structure-reader-suite)

(in-suite generic-structure-reader-suite)

(test replace-structure-reader-with-generic-function
  (with-dummy-data (foo bar)
    (is (typep (function foo-a) 'standard-generic-function))
    (is (typep (function bar-b) 'standard-generic-function))
    (is (typep (structure-reader-function 'foo-a) 'function))
    (is (typep (structure-reader-function 'bar-b) 'function))))

(test adding-method-for-structure-reader
  (with-dummy-data ()
    (defmethod foo-a ((object null)) 100)
    (defmethod bar-b ((object null)) 200)
    (is (= (foo-a nil) 100))
    (is (= (foo-a foo) 1))
    (is (= (bar-b nil) 200))
    (is (= (bar-b bar) 11))))

(test access-unreplaced-structure-reader
  (with-dummy-data ()
    (is (= (foo-b foo) 2))
    (is (= (bar-c bar) 22))))

(test still-able-to-change-values
  (with-dummy-data ()
    (is (setf (foo-a foo) 1))
    (is (setf (foo-b foo) 2))
    (is (setf (bar-b bar) 3))
    (is (setf (bar-c bar) 4))))

(test mismatch-between-structure-and-reader
  (with-dummy-data (foo bar)
    (let ((baz (make-baz :z 3)))
      ;; Expecting error because slot z does not exists in bar.
      (ignore-errors (define-generic-structure-reader baz-z (bar)))
      (is (typep (structure-reader-function 'baz-z) 'function))
      (define-generic-structure-reader baz-z (baz))
      (is (typep (function baz-z) 'standard-generic-function))
      (is (= (baz-z baz) 3)))))

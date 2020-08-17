;;;; generic-structure-reader-tests.lisp

(defpackage #:generic-structure-reader/tests
  (:use #:common-lisp #:fiveam #:generic-structure-reader)
  (:export #:run-suite-tests))
(in-package #:generic-structure-reader/tests)

(defstruct foo a b)
(defstruct bar b c)
(defstruct baz z)

(defmacro with-suppress-warning (&body body)
  `(handler-bind ((warning (lambda (condition)
                             (declare (ignore condition))
                             (muffle-warning))))
     ,@body))

(def-fixture dummy-data ()
  (let ((foo (make-foo :a 1 :b 2))
        (bar (make-bar :b 11 :c 22))
        (baz (make-baz :z 111)))
    (declare (ignorable foo bar baz))
    (&body)))

(defun run-suite-tests ()
  (run! 'generic-structure-reader-suite))

(def-suite generic-structure-reader-suite)

(in-suite generic-structure-reader-suite)

(test replace-structure-reader-with-generic-function
  (with-suppress-warning
    (define-generic-structure-reader foo-a (foo))
    (define-generic-structure-reader bar-b (bar)))
  (is (typep (function foo-a) 'standard-generic-function))
  (is (typep (function bar-b) 'standard-generic-function))
  (is (typep (structure-reader-function 'foo-a) 'function))
  (is (typep (structure-reader-function 'bar-b) 'function)))

(test (adding-method-for-structure-reader
       :depends-on replace-structure-reader-with-generic-function)
  (with-suppress-warning
    (defmethod foo-a ((object null)) 100)
    (defmethod bar-b ((object null)) 200))
  (with-fixture dummy-data ()
    (is (= (foo-a nil) 100))
    (is (= (foo-a foo) 1))
    (is (= (bar-b nil) 200))
    (is (= (bar-b bar) 11))))

(test (access-unreplaced-structure-reader
       :depends-on adding-method-for-structure-reader)
  (with-fixture dummy-data ()
    (is (= (foo-b foo) 2))
    (is (= (bar-c bar) 22))))

(test (able-to-change-values
       :depends-on adding-method-for-structure-reader)
  (with-fixture dummy-data ()
    (is (setf (foo-a foo) 1))
    (is (setf (foo-b foo) 2))
    (is (setf (bar-b bar) 3))
    (is (setf (bar-c bar) 4))))

(test (able-to-use-with-slots-and-accessors
       :depends-on adding-method-for-structure-reader)
  (with-fixture dummy-data ()
    (with-slots (a b) foo
      (is (= a 1))
      (is (= b 2)))
    (with-accessors ((b bar-b) (c bar-c)) bar
      (is (= b 11))
      (is (= c 22)))))

(test mismatch-between-structure-and-reader
  (with-fixture dummy-data ()
    (with-suppress-warning
      ;; Expecting error because slot z does not exists in bar.
      (signals error (define-generic-structure-reader baz-z (bar)))
      (define-generic-structure-reader baz-z (baz)))
    (is (typep (function baz-z) 'standard-generic-function))
    (is (typep (structure-reader-function 'baz-z) 'function))
    (is (= (baz-z baz) 111))))

;;; It failed when evaluate at :load-toplevel
;;; Don't even bother to test #'revoke-generic-structure-readers
;;; It is work as expected. But, it is not recommended to be used
(eval-when (:compile-toplevel :execute)
  (defstruct quux a)
  (define-generic-structure-reader quux-a (quux)))
(test revoke-generic-structure-reader
  (revoke-generic-structure-reader 'quux-a)
  (is (typep (function quux-a) 'function))
  (is-false (nth-value 1 (structure-reader-function 'quux-a))))

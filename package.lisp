;;;; package.lisp

(defpackage #:generic-structure-reader
  (:use #:common-lisp)
  (:export #:define-generic-structure-reader
           #:replace-slot-reader-error
           #:structure-reader-function))

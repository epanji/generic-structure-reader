;;;; package.lisp

(defpackage #:generic-structure-reader
  (:use #:common-lisp)
  (:export #:define-generic-structure-reader
           #:structure-reader-function))

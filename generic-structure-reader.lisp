;;;; generic-structure-reader.lisp

(in-package #:generic-structure-reader)

(defvar *structure-reader-functions* (make-hash-table))

(defmacro structure-reader-function (name)
  `(gethash ,name *structure-reader-functions*))

(defmacro define-generic-structure-reader (function-name (structure-name) &body body)
  ;; Ensure type of FUNCTION-NAME and STRUCTURE-NAME related.
  (when (and (eql (type-of (symbol-function function-name)) 'function)
             (eql (type-of (find-class structure-name nil)) 'structure-class)
             ;; Provide nil to get expected type from error.
             (eql (handler-case (funcall (symbol-function function-name) nil)
                    (type-error (c) (type-error-expected-type c)))
                  structure-name))
    ;; Register original function.
    (setf (structure-reader-function function-name)
          (symbol-function function-name))
    ;; Remove function definition.
    (fmakunbound function-name))
  ;; Create or [Re]define generic function.
  `(defgeneric ,function-name (,structure-name)
     (:method ((instance ,structure-name))
       (funcall (structure-reader-function ',function-name) instance))
     ,@body))

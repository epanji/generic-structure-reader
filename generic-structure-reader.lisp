;;;; generic-structure-reader.lisp

(in-package #:generic-structure-reader)

(defvar *structure-reader-functions* (make-hash-table))

(defun slot-reader-expected-type (function)
  (handler-case (funcall (typecase function
                           (function function)
                           (symbol (symbol-function function))
                           (t nil))
                         nil)
    (type-error (c) (type-error-expected-type c))
    (undefined-function ())))

(define-condition replace-slot-reader-error (error)
  ((function :initarg :function :reader replace-slot-reader-error-function)
   (structure :initarg :structure :reader replace-slot-reader-error-structure))
  (:report (lambda (condition stream)
             (let ((function-name (replace-slot-reader-error-function condition))
                   (structure-name (replace-slot-reader-error-structure condition)))
               (format stream "Invalid ~(~a~) ~a for ~(~a~) ~a."
                       (type-of (symbol-function function-name)) function-name
                       (type-of (find-class structure-name nil)) structure-name)))))

(defmacro structure-reader-function (name)
  `(gethash ,name *structure-reader-functions*))

(defmacro define-generic-structure-reader (function-name (structure-name) &body body)
  ;; Ensure type of FUNCTION-NAME and STRUCTURE-NAME related.
  (when (and (eql (type-of (symbol-function function-name)) 'function)
             (eql (type-of (find-class structure-name nil)) 'structure-class)
             (eql (slot-reader-expected-type function-name) structure-name))
    ;; Register original function.
    (setf (structure-reader-function function-name)
          (symbol-function function-name))
    ;; Remove function definition.
    (fmakunbound function-name))
  ;; Define or redefine generic function.
  ;; Since slot-reader-expected-type did not work for method,
  ;; STRUCTURE-NAME is needed for redefine generic function.
  (if (eql (slot-reader-expected-type (structure-reader-function function-name)) structure-name)
      `(defgeneric ,function-name (,structure-name)
         (:method ((instance ,structure-name))
           (funcall (structure-reader-function ',function-name) instance))
         ,@body)
      (error 'replace-slot-reader-error :function function-name :structure structure-name)))

;;;; generic-structure-reader.lisp

(in-package #:generic-structure-reader)

;;; Supported Common Lisp Implementation.
#-(or sbcl ecl abcl)
(warn "GENERIC-STRUCTURE-READER: This system might be un-supported for ~S." (lisp-implementation-type))

(defvar *structure-reader-functions* (make-hash-table))

(defun slot-reader-expected-type (function)
  (handler-case (funcall (typecase function
                           (function function)
                           (symbol (symbol-function function))
                           (t nil #|undefined-function|#))
                         nil)
    (type-error (c) (type-error-expected-type c))
    (undefined-function () nil)
    (simple-condition () nil)))

(define-condition replace-slot-reader-error (error)
  ((function :initarg :function :reader replace-slot-reader-error-function)
   (structure :initarg :structure :reader replace-slot-reader-error-structure))
  (:report (lambda (condition stream)
             (let ((function-name (replace-slot-reader-error-function condition))
                   (structure-name (replace-slot-reader-error-structure condition)))
               (format stream "Invalid ~(~A~) ~A for ~(~A~) ~A."
                       (type-of (symbol-function function-name)) function-name
                       (type-of (find-class structure-name nil)) structure-name)))))

(defmacro structure-reader-function (name)
  `(gethash ,name *structure-reader-functions*))

(defmacro define-generic-structure-reader (function-name (structure-name) &body body)
  ;; Ensure type of FUNCTION-NAME and STRUCTURE-NAME related.
  (when (and (fboundp function-name)
             (not (eql (type-of (symbol-function function-name)) 'standard-generic-function)) ; Exclude generic-function
             (eql (type-of (find-class structure-name nil)) 'structure-class)
             (eql (slot-reader-expected-type function-name) #-abcl structure-name #+abcl 'structure-object))
    ;; Register original function.
    (setf (structure-reader-function function-name)
          (symbol-function function-name))
    ;; Remove function definition.
    (fmakunbound function-name))
  ;; Define or redefine generic function.
  ;; Since slot-reader-expected-type did not work for method,
  ;; STRUCTURE-NAME is needed for redefine generic function.
  (if (eql (slot-reader-expected-type (structure-reader-function function-name)) #-abcl structure-name #+abcl 'structure-object)
      `(defgeneric ,function-name (,structure-name)
         (:method ((instance ,structure-name))
           (funcall (structure-reader-function ',function-name) instance))
         ,@body)
      (error 'replace-slot-reader-error :function function-name :structure structure-name)))

(defun revoke-generic-structure-reader (symbol)
  "Revoke generic function definition from SYMBOL and restore the original function.
Only SYMBOL registered in `*structure-reader-functions*' will be affected by this.
As a consideration, generic function might be unrevertable after revoked."
  (multiple-value-bind (origin existp) (structure-reader-function symbol)
    (when existp
      (let ((generic (symbol-function symbol)))
        (setf (fdefinition symbol) origin)
        (remhash symbol *structure-reader-functions*)
        (values origin generic)))))

(defun revoke-generic-structure-readers (package)
  "Revoke generic functions by calling `revoke-generic-structure-reader' multiple times.
Only specified PACKAGE in `*structure-reader-functions*' will be affected by this."
  (loop for name being the hash-key of *structure-reader-functions*
        when (eql (symbol-package name) (find-package package))
          collect (multiple-value-list (revoke-generic-structure-reader name))))

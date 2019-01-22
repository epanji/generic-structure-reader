;;;; generic-structure-reader.asd

(asdf:defsystem #:generic-structure-reader
  :description "Replace structure slot reader with generic function."
  :author "Panji Kusuma <epanji@gmail.com>"
  :license  "Public Domain"
  :version "0.0.2"
  :serial t
  :components ((:file "package")
               (:file "generic-structure-reader"))
  :in-order-to ((test-op (load-op #:generic-structure-reader/tests))))

(defmethod perform ((o asdf:test-op)
                    (c (eql (asdf:find-system :generic-structure-reader))))
  (funcall (intern "RUN-SUITE-TESTS" :generic-structure-reader/tests)))

(asdf:defsystem #:generic-structure-reader/tests
  :defsystem-depends-on (:fiveam)
  :depends-on (:generic-structure-reader)
  :components ((:file "generic-structure-reader-tests")))

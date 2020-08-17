;;;; generic-structure-reader.asd

(asdf:defsystem #:generic-structure-reader
  :description "Replace structure slot reader with generic function."
  :author "Panji Kusuma <epanji@gmail.com>"
  :license  "Public Domain"
  :version "0.0.4"
  :serial t
  :components ((:file "package")
               (:file "generic-structure-reader"))
  :in-order-to ((test-op (load-op "generic-structure-reader/tests")))
  :perform (test-op (o c) (symbol-call :generic-structure-reader/tests
                                       :run-suite-tests)))

(asdf:defsystem #:generic-structure-reader/tests
  :depends-on ("fiveam" "generic-structure-reader")
  :components ((:file "generic-structure-reader-tests")))

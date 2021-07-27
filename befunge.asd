(asdf:defsystem "befunge"
  :version "0.1.0"
  :author "Me :)"
  :license "MIT"
  :depends-on (;; "croatoan"
               "uiop"
               "array-operations"
               "alexandria"
               "resettable-class")
  :components ((:file "befunge"))
  :description ""
  :in-order-to ((test-op (test-op "befunge/tests"))))

(asdf:defsystem "befunge/tests"
  :author "Me :)"
  :license "MIT"
  :depends-on ("befunge"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for befunge"
  :perform (test-op (op c) (symbol-call :rove :run c)))

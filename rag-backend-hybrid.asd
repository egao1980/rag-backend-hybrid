(defsystem "rag-backend-hybrid"
  :version "0.1.1"
  :description "In-process Okapi BM25 + RRF/linear hybrid store for rag-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ("rag-protocol")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "backend"))
  :in-order-to ((test-op (test-op "rag-backend-hybrid/tests"))))

(defsystem "rag-backend-hybrid/tests"
  :depends-on ("rag-backend-hybrid" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))

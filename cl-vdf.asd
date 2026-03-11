;;;; cl-vdf.asd - Verifiable Delay Functions Library

(asdf:defsystem #:cl-vdf
  :description "Standalone Verifiable Delay Functions (VDF) implementation"
  :version "1.0.0"
  :author "CLPIC Project"
  :license "BSD-3-Clause"
  :depends-on ()
  :serial t
  :components ((:file "package")
               (:module "src"
                :serial t
                :components ((:file "crypto")
                             (:file "types")
                             (:file "class-groups")
                             (:file "wesolowski")))))

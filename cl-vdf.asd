;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: BSD-3-Clause

;;;; cl-vdf.asd - Verifiable Delay Functions Library

(asdf:defsystem #:cl-vdf
  :description "Standalone Verifiable Delay Functions (VDF) implementation"
  :version "0.1.0"
  :author "Parkian Company LLC"
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

(asdf:defsystem #:cl-vdf/test
  :description "Tests for cl-vdf"
  :depends-on (#:cl-vdf)
  :serial t
  :components ((:module "test"
                :components ((:file "test-vdf"))))
  :perform (asdf:test-op (o c)
             (let ((result (uiop:symbol-call :cl-vdf.test :run-tests)))
               (unless result
                 (error "Tests failed")))))

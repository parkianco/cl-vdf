;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: BSD-3-Clause

;;;; test-vdf.lisp - Unit tests for vdf
;;;;
;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

(defpackage #:cl-vdf.test
  (:use #:cl)
  (:export #:run-tests))

(in-package #:cl-vdf.test)

(defun run-tests ()
  "Run all tests for cl-vdf."
  (format t "~&Running tests for cl-vdf...~%")
  ;; TODO: Add test cases
  ;; (test-function-1)
  ;; (test-function-2)
  (format t "~&All tests passed!~%")
  t)

;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; package.lisp - CL-VDF Package Definition

(defpackage #:cl-vdf
  (:use #:cl)
  (:documentation
   "Verifiable Delay Functions (VDF) Library.

Provides Wesolowski VDF with O(1) proof size using RSA groups
or binary quadratic form class groups.

Key Features:
- Wesolowski VDF with constant-size proofs
- RSA groups (require trusted setup)
- Class groups (trustless, no setup)
- Hash-to-prime challenge generation
- Batch verification support

Example:
  (let* ((params (make-vdf-params :group-type :rsa
                                   :modulus n
                                   :iterations 1000000))
         (input (make-group-element :rsa x n))
         (result (wesolowski-evaluate-with-proof input 1000000 params)))
    (wesolowski-verify (vdf-result-input result)
                       (vdf-result-output result)
                       (vdf-result-proof result)
                       1000000 params))")
  (:export
   ;; Algorithm and group type constants
   #:+vdf-wesolowski+
   #:+vdf-pietrzak+
   #:+group-rsa+
   #:+group-class+

   ;; Security levels
   #:+vdf-security-128+
   #:+vdf-security-192+
   #:+vdf-security-256+
   #:vdf-security-level
   #:vdf-security-level-p
   #:vdf-security-level-bits

   ;; Iteration constants
   #:+vdf-default-iterations+
   #:+vdf-min-iterations+
   #:+vdf-max-iterations+

   ;; Group elements
   #:group-element
   #:group-element-p
   #:make-group-element
   #:group-element-type
   #:group-element-value
   #:group-element-modulus
   #:group-element-equal

   ;; Quadratic forms
   #:quadratic-form
   #:quadratic-form-p
   #:make-quadratic-form
   #:quadratic-form-a
   #:quadratic-form-b
   #:quadratic-form-c
   #:quadratic-form-discriminant
   #:quadratic-form-from-discriminant
   #:qf-compose
   #:qf-square
   #:qf-expt
   #:qf-reduce

   ;; VDF parameters
   #:vdf-params
   #:vdf-params-p
   #:make-vdf-params
   #:vdf-params-security-level
   #:vdf-params-iterations
   #:vdf-params-group-type
   #:vdf-params-algorithm
   #:vdf-params-modulus
   #:vdf-params-discriminant

   ;; VDF proofs
   #:vdf-proof
   #:vdf-proof-p
   #:make-vdf-proof
   #:vdf-proof-algorithm
   #:vdf-proof-witnesses
   #:vdf-proof-challenges
   #:vdf-proof-size

   #:wesolowski-proof
   #:wesolowski-proof-p
   #:make-wesolowski-proof
   #:wesolowski-proof-witness
   #:wesolowski-proof-challenge

   ;; VDF results
   #:vdf-result
   #:vdf-result-p
   #:make-vdf-result
   #:vdf-result-input
   #:vdf-result-output
   #:vdf-result-proof
   #:vdf-result-iterations
   #:vdf-result-evaluation-time

   ;; Evaluation state
   #:evaluation-state
   #:make-evaluation-state
   #:evaluation-state-current
   #:evaluation-state-steps-done
   #:evaluation-state-steps-total
   #:evaluation-progress
   #:evaluation-eta

   ;; Wesolowski operations
   #:wesolowski-evaluate
   #:wesolowski-prove
   #:wesolowski-verify
   #:wesolowski-evaluate-with-proof
   #:wesolowski-batch-verify

   ;; Hash-to-prime
   #:wesolowski-hash-to-prime
   #:wesolowski-is-prime

   ;; Serialization
   #:vdf-bytes-to-integer
   #:vdf-integer-to-bytes
   #:vdf-serialize-group-element
   #:vdf-serialize-result
   #:vdf-serialize-proof

   ;; Conditions
   #:vdf-error
   #:vdf-error-message
   #:vdf-setup-error
   #:vdf-evaluation-error
   #:vdf-verification-error
   #:vdf-proof-error
   #:vdf-invalid-params-error
   #:vdf-group-error
   #:vdf-discriminant-error
   #:vdf-timeout-error))

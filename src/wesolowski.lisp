;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: Apache-2.0

;;;; wesolowski.lisp - Wesolowski VDF Implementation

(in-package #:cl-vdf)

(declaim (optimize (speed 3) (safety 1) (debug 0)))

;;; ============================================================================
;;; Wesolowski VDF Constants
;;; ============================================================================

(defconstant +wesolowski-prime-bits+ 256
  "Number of bits for hash-to-prime challenge.")

(defconstant +wesolowski-prime-attempts+ 1000
  "Maximum attempts to find prime from hash.")

(defconstant +wesolowski-miller-rabin-rounds+ 64
  "Number of Miller-Rabin rounds for primality testing.")

;;; ============================================================================
;;; Primality Testing
;;; ============================================================================

(defun wesolowski-is-prime (n &optional (rounds +wesolowski-miller-rabin-rounds+))
  "Test if N is prime using Miller-Rabin.

   Returns T if N is probably prime, NIL if definitely composite."
  (declare (type integer n)
           (type fixnum rounds))
  (when (< n 2) (return-from wesolowski-is-prime nil))
  (when (= n 2) (return-from wesolowski-is-prime t))
  (when (evenp n) (return-from wesolowski-is-prime nil))
  ;; Small prime check
  (when (< n 341)
    (return-from wesolowski-is-prime
      (member n '(3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71
                  73 79 83 89 97 101 103 107 109 113 127 131 137 139 149
                  151 157 163 167 173 179 181 191 193 197 199 211 223 227
                  229 233 239 241 251 257 263 269 271 277 281 283 293 307
                  311 313 317 331 337))))
  ;; Write n-1 = 2^s * d with d odd
  (let* ((n-1 (1- n))
         (s 0)
         (d n-1))
    (loop while (evenp d)
          do (setf d (ash d -1))
             (incf s))
    ;; Miller-Rabin rounds
    (dotimes (_ rounds t)
      (let* ((a (+ 2 (random (- n 3))))
             (x (mod-expt a d n)))
        (unless (or (= x 1) (= x n-1))
          (let ((composite t))
            (dotimes (_ (1- s))
              (setf x (mod (* x x) n))
              (when (= x n-1)
                (setf composite nil)
                (return)))
            (when composite
              (return-from wesolowski-is-prime nil))))))))

;;; ============================================================================
;;; Hash-to-Prime
;;; ============================================================================

(defun wesolowski-hash-to-prime (x y t-val params &optional (nonce 0))
  "Generate a prime challenge via hash-to-prime.

   X: Input group element
   Y: Output group element
   T-VAL: Number of iterations
   PARAMS: VDF parameters
   NONCE: Counter for rejection sampling

   Returns (values prime nonce-used)."
  (declare (ignore params))
  (loop for attempt from nonce below (+ nonce +wesolowski-prime-attempts+)
        do (let* ((hash-input (make-array 128 :element-type '(unsigned-byte 8)
                                          :initial-element 0))
                  (pos 0))
             ;; Serialize x
             (let ((x-bytes (vdf-serialize-group-element x)))
               (replace hash-input x-bytes :start1 pos :end2 (min 32 (length x-bytes)))
               (incf pos 32))
             ;; Serialize y
             (let ((y-bytes (vdf-serialize-group-element y)))
               (replace hash-input y-bytes :start1 pos :end2 (min 32 (length y-bytes)))
               (incf pos 32))
             ;; Add T
             (let ((t-bytes (vdf-integer-to-bytes t-val 8)))
               (replace hash-input t-bytes :start1 pos)
               (incf pos 8))
             ;; Add nonce
             (let ((nonce-bytes (vdf-integer-to-bytes attempt 8)))
               (replace hash-input nonce-bytes :start1 pos)
               (incf pos 8))
             ;; Hash to get candidate
             (let* ((hash-result (sha256 hash-input))
                    (candidate (vdf-bytes-to-integer hash-result)))
               ;; Ensure candidate is in correct range and odd
               (setf candidate (logior candidate (ash 1 (1- +wesolowski-prime-bits+))))
               (setf candidate (logior candidate 1))
               ;; Check primality
               (when (wesolowski-is-prime candidate)
                 (return-from wesolowski-hash-to-prime (values candidate attempt)))))
        finally (error 'vdf-error
                       :message "Failed to generate prime after maximum attempts")))

;;; ============================================================================
;;; Group Operations
;;; ============================================================================

(defun wesolowski-group-square (element params)
  "Square a group element."
  (declare (ignore params))
  (ecase (group-element-type element)
    (:rsa
     (let* ((x (group-element-value element))
            (n (group-element-modulus element))
            (x-squared (mod (* x x) n)))
       (make-group-element :rsa x-squared n)))
    (:class-group
     (let* ((qf (group-element-value element))
            (qf-squared (qf-square qf)))
       (make-group-element :class-group qf-squared)))))

(defun wesolowski-group-expt (element exponent params)
  "Raise a group element to an integer power."
  (declare (ignore params))
  (ecase (group-element-type element)
    (:rsa
     (let* ((x (group-element-value element))
            (n (group-element-modulus element))
            (result (mod-expt x exponent n)))
       (make-group-element :rsa result n)))
    (:class-group
     (let* ((qf (group-element-value element))
            (result (qf-expt qf exponent)))
       (make-group-element :class-group result)))))

(defun wesolowski-group-multiply (a b params)
  "Multiply two group elements."
  (declare (ignore params))
  (unless (eq (group-element-type a) (group-element-type b))
    (error 'vdf-group-error
           :message "Cannot multiply elements from different groups"))
  (ecase (group-element-type a)
    (:rsa
     (let* ((xa (group-element-value a))
            (xb (group-element-value b))
            (n (group-element-modulus a))
            (product (mod (* xa xb) n)))
       (make-group-element :rsa product n)))
    (:class-group
     (let* ((qfa (group-element-value a))
            (qfb (group-element-value b))
            (product (qf-compose qfa qfb)))
       (make-group-element :class-group product)))))

;;; ============================================================================
;;; Core Wesolowski Evaluation
;;; ============================================================================

(defun wesolowski-repeated-squaring (element iterations params)
  "Perform repeated squaring T times.

   Returns y = x^(2^T)."
  (declare (type group-element element)
           (type iteration-count iterations))
  (let ((current element))
    (dotimes (_ iterations)
      (setf current (wesolowski-group-square current params)))
    current))

(defun wesolowski-evaluate (input iterations params)
  "Evaluate the Wesolowski VDF.

   INPUT: Starting group element x
   ITERATIONS: Number of sequential squarings T
   PARAMS: VDF parameters

   Returns y = x^(2^T).

   Time: O(T) sequential squarings."
  (wesolowski-repeated-squaring input iterations params))

;;; ============================================================================
;;; Wesolowski Proof Generation
;;; ============================================================================

(defun wesolowski-compute-quotient-exponent (t-val l)
  "Compute floor(2^T / l) for proof generation."
  (floor (ash 1 t-val) l))

(defun wesolowski-compute-remainder (t-val l)
  "Compute 2^T mod l for verification."
  (mod (ash 1 t-val) l))

(defun wesolowski-prove (input output iterations params)
  "Generate a Wesolowski proof for y = x^(2^T).

   INPUT: Starting element x
   OUTPUT: Result y = x^(2^T)
   ITERATIONS: Number of iterations T
   PARAMS: VDF parameters

   Returns a WESOLOWSKI-PROOF structure."
  (let* ((l (wesolowski-hash-to-prime input output iterations params))
         (q (wesolowski-compute-quotient-exponent iterations l))
         (pi-witness (wesolowski-group-expt input q params)))
    (make-wesolowski-proof pi-witness l)))

(defun wesolowski-evaluate-with-proof (input iterations params)
  "Evaluate Wesolowski VDF and generate proof.

   INPUT: Starting group element
   ITERATIONS: Number of sequential squarings
   PARAMS: VDF parameters

   Returns a VDF-RESULT with output and proof."
  (let ((start-time (get-internal-real-time)))
    (let* ((output (wesolowski-evaluate input iterations params))
           (proof (wesolowski-prove input output iterations params))
           (generic-proof (make-vdf-proof
                           :wesolowski
                           (list (wesolowski-proof-witness proof))
                           :challenges (list (wesolowski-proof-challenge proof))))
           (elapsed-ms (round (* 1000 (/ (- (get-internal-real-time) start-time)
                                         internal-time-units-per-second)))))
      (make-vdf-result input output generic-proof
                       iterations
                       (vdf-params-group-type params)
                       :wesolowski
                       elapsed-ms))))

;;; ============================================================================
;;; Wesolowski Verification
;;; ============================================================================

(defun wesolowski-verify (input output proof iterations params)
  "Verify a Wesolowski VDF proof.

   INPUT: Claimed input x
   OUTPUT: Claimed output y
   PROOF: The Wesolowski proof (pi, l)
   ITERATIONS: Claimed number of iterations T
   PARAMS: VDF parameters

   Returns T if the proof is valid, NIL otherwise.

   Verification checks: x^r * pi^l = y
   where r = 2^T mod l and pi is the proof witness."
  (handler-case
      (let* ((pi-witness (if (wesolowski-proof-p proof)
                             (wesolowski-proof-witness proof)
                             (first (vdf-proof-witnesses proof))))
             (l (if (wesolowski-proof-p proof)
                    (wesolowski-proof-challenge proof)
                    (first (vdf-proof-challenges proof)))))
        ;; Verify challenge derivation
        (let ((expected-l (wesolowski-hash-to-prime input output iterations params)))
          (unless (= l expected-l)
            (return-from wesolowski-verify nil)))
        ;; Compute r = 2^T mod l
        (let ((r (wesolowski-compute-remainder iterations l)))
          ;; Verify: x^r * pi-witness^l = y
          (let* ((x-to-r (wesolowski-group-expt input r params))
                 (pi-to-l (wesolowski-group-expt pi-witness l params))
                 (product (wesolowski-group-multiply x-to-r pi-to-l params)))
            (group-element-equal product output))))
    (error (e)
      (declare (ignore e))
      nil)))

;;; ============================================================================
;;; Batch Verification
;;; ============================================================================

(defun wesolowski-batch-verify (proofs params)
  "Batch verify multiple Wesolowski proofs.

   PROOFS: List of (input output proof iterations) tuples
   PARAMS: VDF parameters

   Returns T if all proofs are valid, NIL otherwise."
  (when (null proofs)
    (return-from wesolowski-batch-verify t))
  ;; Verify each proof individually
  ;; Full batch verification would use random linear combinations
  (loop for (input output proof iterations) in proofs
        always (wesolowski-verify input output proof iterations params)))

;;; ============================================================================
;;; RSA Group Setup
;;; ============================================================================

(defun generate-rsa-modulus (bit-length)
  "Generate an RSA modulus N = p * q for VDF.

   BIT-LENGTH: Total bit length of N (e.g., 2048)

   Returns N (the modulus).
   Note: For proper security, p and q should be generated via MPC ceremony
   or other trusted setup to ensure factorization is unknown."
  (let* ((prime-bits (/ bit-length 2))
         (p (generate-random-prime prime-bits))
         (q (generate-random-prime prime-bits)))
    ;; Ensure p and q are distinct
    (loop while (= p q)
          do (setf q (generate-random-prime prime-bits)))
    (* p q)))

(defun generate-random-prime (bit-length)
  "Generate a random prime of approximately BIT-LENGTH bits."
  (loop
    (let* ((bytes (get-random-bytes (ceiling bit-length 8)))
           (candidate (vdf-bytes-to-integer bytes)))
      ;; Set high bit to ensure length
      (setf candidate (logior candidate (ash 1 (1- bit-length))))
      ;; Make odd
      (setf candidate (logior candidate 1))
      (when (wesolowski-is-prime candidate)
        (return candidate)))))

;;; ============================================================================
;;; VDF Convenience Functions
;;; ============================================================================

(defun create-rsa-vdf-params (modulus iterations &key (security-level +vdf-security-128+))
  "Create VDF parameters for RSA-based Wesolowski VDF."
  (make-vdf-params :security-level security-level
                   :iterations iterations
                   :group-type :rsa
                   :algorithm :wesolowski
                   :modulus modulus))

(defun create-class-group-vdf-params (discriminant iterations &key (security-level +vdf-security-128+))
  "Create VDF parameters for class-group-based Wesolowski VDF."
  (make-vdf-params :security-level security-level
                   :iterations iterations
                   :group-type :class-group
                   :algorithm :wesolowski
                   :discriminant discriminant))

(defun vdf-random-input (params)
  "Generate a random input element for VDF evaluation."
  (ecase (vdf-params-group-type params)
    (:rsa
     (let* ((n (vdf-params-modulus params))
            (bytes (get-random-bytes (ceiling (integer-length n) 8)))
            (value (mod (vdf-bytes-to-integer bytes) n)))
       (when (zerop value) (setf value 1))
       (make-group-element :rsa value n)))
    (:class-group
     (let* ((d (vdf-params-discriminant params))
            (qf (quadratic-form-from-discriminant d)))
       (make-group-element :class-group qf)))))

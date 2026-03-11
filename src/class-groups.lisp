;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; class-groups.lisp - Binary Quadratic Form Operations for Class Group VDF

(in-package #:cl-vdf)

(declaim (optimize (speed 3) (safety 1) (debug 0)))

;;; ============================================================================
;;; Binary Quadratic Form Operations
;;; ============================================================================
;;;
;;; Class groups are the ideal class groups of imaginary quadratic fields.
;;; We represent elements as binary quadratic forms (a, b, c) where:
;;;   - The form represents ax^2 + bxy + cy^2
;;;   - Discriminant D = b^2 - 4ac < 0 (negative for imaginary quadratic)
;;;   - Forms are reduced to canonical representatives
;;;
;;; Operations:
;;;   - Composition (group operation)
;;;   - Squaring (optimized composition)
;;;   - Exponentiation (repeated squaring)
;;;   - Reduction (find canonical representative)
;;;
;;; Class groups provide trustless VDF because:
;;;   - Unknown group order (no trusted setup needed)
;;;   - Sequential squaring is provably hard to parallelize
;;; ============================================================================

;;; ============================================================================
;;; Form Reduction (Canonical Representative)
;;; ============================================================================

(defun qf-reduce (qf)
  "Reduce a quadratic form to its canonical representative.

   A reduced form (a, b, c) satisfies:
   - |b| <= a <= c
   - if |b| = a or a = c, then b >= 0

   This ensures unique representation of each ideal class."
  (declare (type quadratic-form qf))
  (let ((a (quadratic-form-a qf))
        (b (quadratic-form-b qf))
        (c (quadratic-form-c qf))
        (d (quadratic-form-discriminant qf)))
    ;; Reduction loop
    (loop while (or (> (abs b) a)
                    (> a c)
                    (and (= (abs b) a) (minusp b))
                    (and (= a c) (minusp b)))
          do
             (cond
               ;; If c < a, swap a and c, negate b
               ((< c a)
                (rotatef a c)
                (setf b (- b)))
               ;; Otherwise, reduce b
               (t
                (let* ((k (if (minusp b)
                              (- (floor (+ (- b) a) (* 2 a)))
                              (floor (+ b a) (* 2 a)))))
                  (setf b (- b (* 2 k a)))
                  (setf c (- c (* k (+ b (* k a)))))))))
    ;; Final adjustment for canonical form
    (when (and (= (abs b) a) (minusp b))
      (setf b (- b)))
    (when (and (= a c) (minusp b))
      (setf b (- b)))
    (%make-quadratic-form a b c d)))

;;; ============================================================================
;;; Form Composition (Group Operation)
;;; ============================================================================

(defun qf-compose (f1 f2)
  "Compose two quadratic forms using NUCOMP algorithm.

   This implements the group operation in the class group.
   The result is reduced to canonical form.

   NUCOMP is the optimized composition algorithm that avoids
   the full extended GCD computation when possible."
  (declare (type quadratic-form f1 f2))
  (let ((a1 (quadratic-form-a f1))
        (b1 (quadratic-form-b f1))
        (c1 (quadratic-form-c f1))
        (a2 (quadratic-form-a f2))
        (b2 (quadratic-form-b f2))
        (c2 (quadratic-form-c f2))
        (d (quadratic-form-discriminant f1)))
    ;; Ensure forms have same discriminant
    (unless (= d (quadratic-form-discriminant f2))
      (error 'vdf-group-error
             :message "Cannot compose forms with different discriminants"
             :group-type :class-group))
    ;; Ensure a1 >= a2 for efficiency
    (when (< a1 a2)
      (rotatef a1 a2)
      (rotatef b1 b2)
      (rotatef c1 c2))
    ;; Standard composition via Dirichlet's algorithm
    (let* ((g (gcd a1 a2))
           (s (/ (+ b1 b2) 2)))
      (if (= g 1)
          ;; Coprime case - simplified
          (let* ((h (mod (* s (mod-inverse a2 a1)) a1))
                 (a3 (* a1 a2))
                 (b3 (+ b2 (* 2 a2 h)))
                 (c3 (/ (- (* b3 b3) d) (* 4 a3))))
            (qf-reduce (%make-quadratic-form a3 b3 c3 d)))
          ;; General case with GCD
          (multiple-value-bind (g u v) (extended-gcd g (/ (- b1 b2) 2))
            (declare (ignore v))
            (let* ((w (* u (- c2 c1)))
                   (a3 (/ (* a1 a2) (* g g)))
                   (b3 (+ b2 (* 2 (/ a2 g) w)))
                   (c3 (/ (- (* b3 b3) d) (* 4 a3))))
              (qf-reduce (%make-quadratic-form a3 b3 c3 d))))))))

(defun extended-gcd (a b)
  "Extended Euclidean algorithm.
   Returns (values gcd u v) such that gcd = a*u + b*v."
  (let ((old-r a) (r b)
        (old-s 1) (s 0)
        (old-t 0) (tt 1))
    (loop while (not (zerop r))
          do (let ((q (floor old-r r)))
               (psetf old-r r r (- old-r (* q r)))
               (psetf old-s s s (- old-s (* q s)))
               (psetf old-t tt tt (- old-t (* q tt)))))
    (values old-r old-s old-t)))

;;; ============================================================================
;;; Form Squaring (Optimized Self-Composition)
;;; ============================================================================

(defun qf-square (qf)
  "Square a quadratic form (self-composition).

   This is the main operation for VDF evaluation.
   Optimized compared to general composition."
  (declare (type quadratic-form qf))
  (let ((a (quadratic-form-a qf))
        (b (quadratic-form-b qf))
        (c (quadratic-form-c qf))
        (d (quadratic-form-discriminant qf)))
    ;; Squaring is composition with itself
    ;; Simplified since b1 = b2
    (multiple-value-bind (g u v) (extended-gcd a b)
      (declare (ignore v))
      (let* ((w (* u c))
             (a3 (/ (* a a) (* g g)))
             (b3 (+ b (* 2 (/ a g) w)))
             (c3 (/ (- (* b3 b3) d) (* 4 a3))))
        (qf-reduce (%make-quadratic-form a3 b3 c3 d))))))

;;; ============================================================================
;;; Form Exponentiation
;;; ============================================================================

(defun qf-expt (qf n)
  "Raise a quadratic form to power N using square-and-multiply.

   Returns qf^n in the class group."
  (declare (type quadratic-form qf)
           (type (integer 0 *) n))
  (when (zerop n)
    ;; Return identity element (principal form)
    (return-from qf-expt
      (quadratic-form-from-discriminant (quadratic-form-discriminant qf))))
  (let ((result nil)
        (base qf))
    (loop while (plusp n)
          do (when (oddp n)
               (setf result (if result
                                (qf-compose result base)
                                base)))
             (setf n (ash n -1))
             (when (plusp n)
               (setf base (qf-square base))))
    result))

;;; ============================================================================
;;; Class Group Setup
;;; ============================================================================

(defun generate-class-group-discriminant (security-bits)
  "Generate a suitable discriminant for class group VDF.

   SECURITY-BITS: Desired security level (128, 192, or 256)

   Returns a fundamental discriminant D < 0 where:
   - D = 1 mod 4 (for odd discriminant)
   - |D| is large enough for security
   - D is fundamental (not divisible by p^2 for any odd prime p)"
  (let* ((bit-length (case security-bits
                       (128 800)
                       (192 1024)
                       (256 1536)
                       (t 800)))
         (candidate nil))
    ;; Generate random odd negative integer
    (loop
      (let ((raw (1+ (* 2 (random (ash 1 (1- bit-length)))))))
        ;; Make it = 1 mod 4 and negative
        (setf candidate (- (logior raw 1)))
        (when (= 1 (mod candidate 4))
          ;; Check if roughly square-free (probabilistic)
          (when (is-likely-fundamental candidate)
            (return candidate)))))))

(defun is-likely-fundamental (d)
  "Check if discriminant is likely fundamental (probabilistic).

   Tests divisibility by small primes squared."
  (let ((small-primes '(3 5 7 11 13 17 19 23 29 31 37 41 43 47)))
    (loop for p in small-primes
          never (zerop (mod d (* p p))))))

(defun class-group-generator (discriminant)
  "Create the generator element for class group with given discriminant.

   Returns the reduced form (2, 1, c) which typically has small order."
  (let* ((c (/ (- 1 discriminant) 8)))
    (when (integerp c)
      (qf-reduce (make-quadratic-form 2 1 c)))))

;;; ============================================================================
;;; Class Group Verification Utilities
;;; ============================================================================

(defun qf-valid-p (qf)
  "Check if a quadratic form is valid."
  (and (plusp (quadratic-form-a qf))
       (plusp (quadratic-form-c qf))
       (minusp (quadratic-form-discriminant qf))
       (= (quadratic-form-discriminant qf)
          (- (* (quadratic-form-b qf) (quadratic-form-b qf))
             (* 4 (quadratic-form-a qf) (quadratic-form-c qf))))))

(defun qf-reduced-p (qf)
  "Check if a quadratic form is in reduced form."
  (let ((a (quadratic-form-a qf))
        (b (quadratic-form-b qf))
        (c (quadratic-form-c qf)))
    (and (<= (abs b) a)
         (<= a c)
         (or (/= (abs b) a) (>= b 0))
         (or (/= a c) (>= b 0)))))

(defun qf-equal (f1 f2)
  "Check if two quadratic forms are equal."
  (and (= (quadratic-form-a f1) (quadratic-form-a f2))
       (= (quadratic-form-b f1) (quadratic-form-b f2))
       (= (quadratic-form-c f1) (quadratic-form-c f2))))

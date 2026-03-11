;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; types.lisp - VDF Core Types and Structures

(in-package #:cl-vdf)

(declaim (optimize (speed 3) (safety 1) (debug 0)))

;;; ============================================================================
;;; Algorithm Identifiers
;;; ============================================================================

(defconstant +vdf-pietrzak+ :pietrzak
  "Pietrzak VDF algorithm (logarithmic proof size).")

(defconstant +vdf-wesolowski+ :wesolowski
  "Wesolowski VDF algorithm (constant proof size).")

(defconstant +group-rsa+ :rsa
  "RSA group type: Z/NZ* where N = p*q.")

(defconstant +group-class+ :class-group
  "Class group type: ideal class group of imaginary quadratic field.")

;;; ============================================================================
;;; Security Level Constants
;;; ============================================================================

(defstruct (vdf-security-level
            (:constructor %make-vdf-security-level (bits name modulus-bits))
            (:copier nil)
            (:predicate vdf-security-level-p))
  "VDF security level specification."
  (bits 128 :type (member 128 192 256) :read-only t)
  (name :standard :type keyword :read-only t)
  (modulus-bits 2048 :type (integer 1024 8192) :read-only t))

(defparameter +vdf-security-128+
  (%make-vdf-security-level 128 :standard 2048)
  "128-bit security level.")

(defparameter +vdf-security-192+
  (%make-vdf-security-level 192 :high 3072)
  "192-bit security level.")

(defparameter +vdf-security-256+
  (%make-vdf-security-level 256 :paranoid 4096)
  "256-bit security level.")

;;; ============================================================================
;;; Iteration Constants
;;; ============================================================================

(defconstant +vdf-default-iterations+ 1000000
  "Default number of sequential iterations.")

(defconstant +vdf-min-iterations+ 1000
  "Minimum allowed iterations.")

(defconstant +vdf-max-iterations+ (expt 2 40)
  "Maximum allowed iterations.")

;;; ============================================================================
;;; Type Declarations
;;; ============================================================================

(deftype iteration-count ()
  `(integer 0 ,+vdf-max-iterations+))

(deftype algorithm-type ()
  '(member :pietrzak :wesolowski))

(deftype group-type ()
  '(member :rsa :class-group))

;;; ============================================================================
;;; Condition Types
;;; ============================================================================

(define-condition vdf-error (error)
  ((message :initarg :message
            :reader vdf-error-message
            :initform "VDF error"))
  (:report (lambda (condition stream)
             (format stream "VDF error: ~A" (vdf-error-message condition)))))

(define-condition vdf-setup-error (vdf-error)
  ((parameter :initarg :parameter
              :reader vdf-setup-error-parameter
              :initform nil))
  (:report (lambda (condition stream)
             (format stream "VDF setup error~@[ for ~A~]: ~A"
                     (vdf-setup-error-parameter condition)
                     (vdf-error-message condition)))))

(define-condition vdf-evaluation-error (vdf-error)
  ((step :initarg :step
         :reader vdf-evaluation-error-step
         :initform 0))
  (:report (lambda (condition stream)
             (format stream "VDF evaluation error at step ~D: ~A"
                     (vdf-evaluation-error-step condition)
                     (vdf-error-message condition)))))

(define-condition vdf-verification-error (vdf-error)
  ((expected :initarg :expected
             :reader vdf-verification-error-expected
             :initform nil)
   (actual :initarg :actual
           :reader vdf-verification-error-actual
           :initform nil))
  (:report (lambda (condition stream)
             (format stream "VDF verification failed: ~A"
                     (vdf-error-message condition)))))

(define-condition vdf-proof-error (vdf-error)
  ()
  (:documentation "Error in VDF proof generation or structure."))

(define-condition vdf-invalid-params-error (vdf-error)
  ((parameter :initarg :parameter
              :reader vdf-invalid-params-error-parameter
              :initform nil)
   (value :initarg :value
          :reader vdf-invalid-params-error-value
          :initform nil))
  (:report (lambda (condition stream)
             (format stream "Invalid VDF parameter~@[ ~A~]: ~A"
                     (vdf-invalid-params-error-parameter condition)
                     (vdf-error-message condition)))))

(define-condition vdf-group-error (vdf-error)
  ((group-type :initarg :group-type
               :reader vdf-group-error-type
               :initform nil))
  (:report (lambda (condition stream)
             (format stream "VDF group error~@[ in ~A~]: ~A"
                     (vdf-group-error-type condition)
                     (vdf-error-message condition)))))

(define-condition vdf-discriminant-error (vdf-error)
  ((discriminant :initarg :discriminant
                 :reader vdf-discriminant-error-value
                 :initform nil))
  (:report (lambda (condition stream)
             (format stream "Invalid discriminant~@[ ~A~]: ~A"
                     (vdf-discriminant-error-value condition)
                     (vdf-error-message condition)))))

(define-condition vdf-timeout-error (vdf-error)
  ((timeout :initarg :timeout :reader vdf-timeout-error-timeout :initform nil)
   (elapsed :initarg :elapsed :reader vdf-timeout-error-elapsed :initform nil))
  (:report (lambda (condition stream)
             (format stream "VDF operation timed out after ~A seconds"
                     (vdf-timeout-error-elapsed condition)))))

;;; ============================================================================
;;; Group Element Structure
;;; ============================================================================

(defstruct (group-element
            (:constructor %make-group-element)
            (:copier nil)
            (:predicate group-element-p))
  "Generic group element for VDF operations."
  (type :rsa :type group-type :read-only t)
  (value 0 :type t :read-only t)
  (modulus nil :type (or null integer) :read-only t))

(defun make-group-element (type value &optional modulus)
  "Create a new group element with validation."
  (ecase type
    (:rsa
     (unless modulus
       (error 'vdf-invalid-params-error
              :message "RSA group elements require a modulus"
              :parameter :modulus))
     (unless (and (integerp value) (plusp value) (< value modulus))
       (error 'vdf-invalid-params-error
              :message "RSA element must be positive integer less than modulus"
              :parameter :value))
     (%make-group-element :type type :value value :modulus modulus))
    (:class-group
     (unless (quadratic-form-p value)
       (error 'vdf-invalid-params-error
              :message "Class group elements must be quadratic forms"
              :parameter :value))
     (%make-group-element :type type :value value :modulus nil))))

(defun group-element-equal (a b)
  "Test equality of two group elements."
  (and (eq (group-element-type a) (group-element-type b))
       (equal (group-element-value a) (group-element-value b))
       (eql (group-element-modulus a) (group-element-modulus b))))

;;; ============================================================================
;;; Binary Quadratic Form Structure
;;; ============================================================================

(defstruct (quadratic-form
            (:constructor %make-quadratic-form (a b c discriminant))
            (:copier nil)
            (:predicate quadratic-form-p)
            (:print-function print-quadratic-form))
  "Binary quadratic form (a, b, c) representing ax^2 + bxy + cy^2."
  (a 1 :type (integer 1 *) :read-only t)
  (b 0 :type integer :read-only t)
  (c 1 :type (integer 1 *) :read-only t)
  (discriminant -4 :type (integer * -1) :read-only t))

(defun print-quadratic-form (qf stream depth)
  (declare (ignore depth))
  (format stream "#<QF (~D, ~D, ~D) D=~D>"
          (quadratic-form-a qf)
          (quadratic-form-b qf)
          (quadratic-form-c qf)
          (quadratic-form-discriminant qf)))

(defun make-quadratic-form (a b c)
  "Create a binary quadratic form with validation."
  (unless (plusp a)
    (error 'vdf-invalid-params-error
           :message "Quadratic form coefficient 'a' must be positive"
           :parameter :a :value a))
  (unless (plusp c)
    (error 'vdf-invalid-params-error
           :message "Quadratic form coefficient 'c' must be positive"
           :parameter :c :value c))
  (let ((discriminant (- (* b b) (* 4 a c))))
    (unless (minusp discriminant)
      (error 'vdf-discriminant-error
             :message "Discriminant must be negative for imaginary quadratic field"
             :discriminant discriminant))
    (%make-quadratic-form a b c discriminant)))

(defun quadratic-form-from-discriminant (discriminant &optional (a 1))
  "Create a principal form for the given discriminant."
  (unless (minusp discriminant)
    (error 'vdf-discriminant-error
           :message "Discriminant must be negative"
           :discriminant discriminant))
  (cond
    ((zerop (mod discriminant 4))
     (let ((c (/ (- discriminant) (* 4 a))))
       (unless (integerp c)
         (error 'vdf-discriminant-error
                :message "Cannot create form with given a and discriminant"
                :discriminant discriminant))
       (%make-quadratic-form a 0 c discriminant)))
    ((= 1 (mod discriminant 4))
     (let ((c (/ (- 1 discriminant) (* 4 a))))
       (unless (integerp c)
         (error 'vdf-discriminant-error
                :message "Cannot create form with given a and discriminant"
                :discriminant discriminant))
       (%make-quadratic-form a 1 c discriminant)))
    (t
     (error 'vdf-discriminant-error
            :message "Discriminant must be 0 or 1 mod 4"
            :discriminant discriminant))))

;;; ============================================================================
;;; VDF Parameters Structure
;;; ============================================================================

(defstruct (vdf-params
            (:constructor %make-vdf-params)
            (:copier nil)
            (:predicate vdf-params-p))
  "Parameters for VDF evaluation and verification."
  (security-level +vdf-security-128+ :type vdf-security-level :read-only t)
  (iterations +vdf-default-iterations+ :type iteration-count :read-only t)
  (group-type :rsa :type group-type :read-only t)
  (algorithm :wesolowski :type algorithm-type :read-only t)
  (modulus nil :type (or null integer) :read-only t)
  (discriminant nil :type (or null integer) :read-only t)
  (generator nil :type (or null group-element) :read-only t))

(defun make-vdf-params (&key (security-level +vdf-security-128+)
                             (iterations +vdf-default-iterations+)
                             (group-type :rsa)
                             (algorithm :wesolowski)
                             modulus
                             discriminant
                             generator)
  "Create VDF parameters with validation."
  (unless (typep iterations 'iteration-count)
    (error 'vdf-invalid-params-error
           :message (format nil "Iterations must be between ~D and ~D"
                            +vdf-min-iterations+ +vdf-max-iterations+)
           :parameter :iterations :value iterations))
  (ecase group-type
    (:rsa
     (unless modulus
       (error 'vdf-invalid-params-error
              :message "RSA group requires modulus"
              :parameter :modulus)))
    (:class-group
     (unless discriminant
       (error 'vdf-invalid-params-error
              :message "Class group requires discriminant"
              :parameter :discriminant))
     (unless (minusp discriminant)
       (error 'vdf-discriminant-error
              :message "Discriminant must be negative"
              :discriminant discriminant))))
  (%make-vdf-params
   :security-level security-level
   :iterations iterations
   :group-type group-type
   :algorithm algorithm
   :modulus modulus
   :discriminant discriminant
   :generator generator))

;;; ============================================================================
;;; VDF Proof Structures
;;; ============================================================================

(defstruct (vdf-proof
            (:constructor %make-vdf-proof)
            (:copier nil)
            (:predicate vdf-proof-p))
  "Generic VDF proof structure."
  (algorithm :wesolowski :type algorithm-type :read-only t)
  (witnesses nil :type list :read-only t)
  (challenges nil :type list :read-only t)
  (auxiliary nil :type list :read-only t))

(defun make-vdf-proof (algorithm witnesses &key challenges auxiliary)
  "Create a VDF proof with the given components."
  (%make-vdf-proof
   :algorithm algorithm
   :witnesses witnesses
   :challenges challenges
   :auxiliary auxiliary))

(defun vdf-proof-size (proof)
  "Calculate the size of a VDF proof in bytes."
  (let ((size 8))
    (dolist (w (vdf-proof-witnesses proof))
      (incf size (if (integerp w)
                     (ceiling (integer-length w) 8)
                     256)))
    (dolist (c (vdf-proof-challenges proof))
      (incf size (if (integerp c)
                     (ceiling (integer-length c) 8)
                     32)))
    (incf size (* 32 (length (vdf-proof-auxiliary proof))))
    size))

(defstruct (wesolowski-proof
            (:constructor %make-wesolowski-proof)
            (:copier nil)
            (:predicate wesolowski-proof-p))
  "Wesolowski VDF proof with single witness."
  (witness nil :type t :read-only t)
  (challenge nil :type (or null integer) :read-only t))

(defun make-wesolowski-proof (witness challenge)
  "Create a Wesolowski proof from witness and challenge."
  (unless (and witness challenge)
    (error 'vdf-proof-error
           :message "Wesolowski proof requires witness and challenge"))
  (%make-wesolowski-proof :witness witness :challenge challenge))

;;; ============================================================================
;;; VDF Result Structure
;;; ============================================================================

(defstruct (vdf-result
            (:constructor %make-vdf-result)
            (:copier nil)
            (:predicate vdf-result-p))
  "Complete VDF evaluation result."
  (input nil :type (or null group-element) :read-only t)
  (output nil :type (or null group-element) :read-only t)
  (proof nil :type (or null vdf-proof) :read-only t)
  (iterations 0 :type iteration-count :read-only t)
  (group-type :rsa :type group-type :read-only t)
  (algorithm :wesolowski :type algorithm-type :read-only t)
  (evaluation-time 0 :type (integer 0 *) :read-only t))

(defun make-vdf-result (input output proof iterations group-type algorithm
                        &optional (evaluation-time 0))
  "Create a VDF result structure."
  (%make-vdf-result
   :input input
   :output output
   :proof proof
   :iterations iterations
   :group-type group-type
   :algorithm algorithm
   :evaluation-time evaluation-time))

;;; ============================================================================
;;; Evaluation State Structure
;;; ============================================================================

(defstruct (evaluation-state
            (:constructor %make-evaluation-state)
            (:copier nil)
            (:predicate evaluation-state-p))
  "State for incremental VDF evaluation."
  (current nil :type (or null group-element))
  (steps-done 0 :type iteration-count)
  (steps-total +vdf-default-iterations+ :type iteration-count :read-only t)
  (checkpoints nil :type list)
  (start-time 0 :type (integer 0 *))
  (algorithm :wesolowski :type algorithm-type :read-only t)
  (params nil :type (or null vdf-params) :read-only t))

(defun make-evaluation-state (initial-element total-iterations algorithm params)
  "Create an evaluation state for incremental VDF computation."
  (%make-evaluation-state
   :current initial-element
   :steps-done 0
   :steps-total total-iterations
   :checkpoints (list (cons 0 initial-element))
   :start-time (get-universal-time)
   :algorithm algorithm
   :params params))

(defun evaluation-progress (state)
  "Calculate progress as a fraction from 0.0 to 1.0."
  (if (zerop (evaluation-state-steps-total state))
      1.0
      (/ (float (evaluation-state-steps-done state))
         (float (evaluation-state-steps-total state)))))

(defun evaluation-eta (state)
  "Estimate time remaining in seconds."
  (let ((done (evaluation-state-steps-done state))
        (total (evaluation-state-steps-total state))
        (elapsed (- (get-universal-time)
                    (evaluation-state-start-time state))))
    (when (and (plusp done) (plusp elapsed))
      (let ((rate (/ done elapsed)))
        (when (plusp rate)
          (ceiling (/ (- total done) rate)))))))

;;; ============================================================================
;;; Serialization Utilities
;;; ============================================================================

(defun vdf-bytes-to-integer (bytes)
  "Convert byte array to integer (big-endian)."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
  (let ((result 0)
        (len (length bytes)))
    (dotimes (i len result)
      (setf result (logior (ash result 8) (aref bytes i))))))

(defun vdf-integer-to-bytes (integer &optional byte-length)
  "Convert integer to byte array (big-endian)."
  (declare (type integer integer))
  (unless (>= integer 0)
    (error 'vdf-error :message "Cannot convert negative integer to bytes"))
  (let* ((min-length (max 1 (ceiling (integer-length integer) 8)))
         (length (or byte-length min-length))
         (bytes (make-array length :element-type '(unsigned-byte 8)
                            :initial-element 0)))
    (when (< length min-length)
      (error 'vdf-error :message "Byte length too small for integer"))
    (loop for i from (1- length) downto 0
          for shift from 0 by 8
          do (setf (aref bytes i) (ldb (byte 8 shift) integer)))
    bytes))

(defun vdf-serialize-group-element (element)
  "Serialize a group element to bytes."
  (ecase (group-element-type element)
    (:rsa
     (let* ((value (group-element-value element))
            (modulus (group-element-modulus element))
            (mod-bytes (ceiling (integer-length modulus) 8)))
       (vdf-integer-to-bytes value mod-bytes)))
    (:class-group
     (let ((qf (group-element-value element)))
       (let* ((a-bytes (vdf-integer-to-bytes (quadratic-form-a qf)))
              (b-int (quadratic-form-b qf))
              (b-sign (if (minusp b-int) 1 0))
              (b-bytes (vdf-integer-to-bytes (abs b-int)))
              (result (make-array (+ 4 (length a-bytes) 1 (length b-bytes))
                                  :element-type '(unsigned-byte 8))))
         (setf (aref result 0) (ldb (byte 8 8) (length a-bytes)))
         (setf (aref result 1) (ldb (byte 8 0) (length a-bytes)))
         (replace result a-bytes :start1 2)
         (let ((offset (+ 2 (length a-bytes))))
           (setf (aref result offset) b-sign)
           (setf (aref result (1+ offset)) (length b-bytes))
           (replace result b-bytes :start1 (+ offset 2)))
         result)))))

(defun vdf-serialize-result (result)
  "Serialize a VDF result to bytes."
  (let ((parts nil))
    (push (vector (ecase (vdf-result-algorithm result)
                    (:pietrzak 0)
                    (:wesolowski 1))
                  (ecase (vdf-result-group-type result)
                    (:rsa 0)
                    (:class-group 1)))
          parts)
    (push (vdf-integer-to-bytes (vdf-result-iterations result) 8) parts)
    (when (vdf-result-input result)
      (push (vdf-serialize-group-element (vdf-result-input result)) parts))
    (when (vdf-result-output result)
      (push (vdf-serialize-group-element (vdf-result-output result)) parts))
    (let* ((total-length (reduce #'+ parts :key #'length))
           (output (make-array total-length :element-type '(unsigned-byte 8)))
           (pos 0))
      (dolist (part (nreverse parts) output)
        (replace output part :start1 pos)
        (incf pos (length part))))))

(defun vdf-serialize-proof (proof)
  "Serialize a VDF proof to bytes."
  (declare (type vdf-proof proof))
  (let ((alg-byte (ecase (vdf-proof-algorithm proof)
                    (:pietrzak 0)
                    (:wesolowski 1))))
    (ecase (vdf-proof-algorithm proof)
      (:wesolowski
       (let ((witness (first (vdf-proof-witnesses proof)))
             (challenge (first (vdf-proof-challenges proof))))
         (let* ((w-bytes (if (integerp witness)
                             (vdf-integer-to-bytes witness)
                             (vdf-serialize-group-element witness)))
                (c-bytes (vdf-integer-to-bytes challenge))
                (result (make-array (+ 1 4 (length w-bytes) 4 (length c-bytes))
                                    :element-type '(unsigned-byte 8))))
           (setf (aref result 0) alg-byte)
           (let ((pos 1))
             (setf (aref result pos) (ldb (byte 8 24) (length w-bytes)))
             (setf (aref result (+ pos 1)) (ldb (byte 8 16) (length w-bytes)))
             (setf (aref result (+ pos 2)) (ldb (byte 8 8) (length w-bytes)))
             (setf (aref result (+ pos 3)) (ldb (byte 8 0) (length w-bytes)))
             (replace result w-bytes :start1 (+ pos 4))
             (let ((cpos (+ pos 4 (length w-bytes))))
               (setf (aref result cpos) (ldb (byte 8 24) (length c-bytes)))
               (setf (aref result (+ cpos 1)) (ldb (byte 8 16) (length c-bytes)))
               (setf (aref result (+ cpos 2)) (ldb (byte 8 8) (length c-bytes)))
               (setf (aref result (+ cpos 3)) (ldb (byte 8 0) (length c-bytes)))
               (replace result c-bytes :start1 (+ cpos 4))))
           result)))
      (:pietrzak
       (let* ((witnesses (vdf-proof-witnesses proof))
              (challenges (vdf-proof-challenges proof))
              (n (length witnesses)))
         (let ((parts (list (vector alg-byte (ldb (byte 8 0) n)))))
           (dolist (w witnesses)
             (push (if (integerp w)
                       (vdf-integer-to-bytes w)
                       (vdf-serialize-group-element w))
                   parts))
           (dolist (c challenges)
             (push (vdf-integer-to-bytes c 32) parts))
           (let* ((total (reduce #'+ parts :key #'length))
                  (result (make-array total :element-type '(unsigned-byte 8)))
                  (pos 0))
             (dolist (p (nreverse parts) result)
               (replace result p :start1 pos)
               (incf pos (length p))))))))))

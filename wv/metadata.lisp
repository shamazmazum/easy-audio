;; Copyright (c) 2012-2014, Vasily Postnicov
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met: 

;; 1. Redistributions of source code must retain the above copyright notice, this
;;   list of conditions and the following disclaimer. 
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;   this list of conditions and the following disclaimer in the documentation
;;   and/or other materials provided with the distribution. 

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :easy-audio.wv)

(defvar *wvx-buffers* nil
  "Works with @c(make-output-buffers) to reduce consing.
Bind this variable to wvx buffers when you read multiple
block in a loop to reduce consing.")

;; Metadata body readers
(defmethod read-metadata-body ((metadata metadata) reader)
  (let ((data (make-array (list (metadata-actual-size metadata))
                          :element-type '(ub 8))))
    (setf (metadata-data metadata)
          (read-octet-vector data reader)))

  (warn 'unknown-metadata :metadata metadata)
  metadata)

(defmethod read-metadata-body :after ((metadata metadata) reader)
  (if (/= (metadata-size metadata) (metadata-actual-size metadata))
      (read-octet reader)))

(defmethod read-metadata-body :before ((metadata metadata-decorr) reader)
  (let ((decorr-passes (block-decorr-passes *current-block*))
        (data-length (metadata-actual-size metadata)))

    (setf (metadata-decorr-passes metadata)
          (if decorr-passes decorr-passes
              (setf (block-decorr-passes *current-block*)
                    (loop repeat data-length collect (make-decorr-pass)))))))

(defmethod read-metadata-body ((metadata metadata-decorr-terms) reader)
  ;; NB: in wavpack lib data is filled in backward order. We do not do this.
  (let ((data-length (metadata-actual-size metadata)))
    (if (or (>  data-length 16)
            (/= data-length (length (metadata-decorr-passes metadata))))
        (error 'block-error
               :format-control "Size of metadata sub-block ~a is incorrect"
               :format-arguments (list metadata))))

  (loop for decorr-pass in (metadata-decorr-passes metadata) do
       (let* ((octet (read-octet reader))
              (term (- (logand octet #x1f) 5))
              (delta (logand (ash octet -5) #x7)))
         (if (or (= term 0)
                 (< term -3)
                 (and (> term 8) (< term 17))
                 (> term 18))
             (error 'block-error
                    :format-control "Invalid term in metadata sub-block ~a"
                    :format-arguments (list metadata)))

         (setf (decorr-pass-term decorr-pass) term
               (decorr-pass-delta decorr-pass) delta)))
  metadata)

(defmethod read-metadata-body ((metadata metadata-decorr-weights) reader)
  (let* ((data-size (metadata-actual-size metadata))
         (channels (block-channels *current-block*))
         (term-number (ash data-size (- 1 channels))))

    (if (> term-number
           (length (metadata-decorr-passes metadata)))
        (error 'block-error
               :format-control "Size of metadata sub-block ~a is too big"
               :format-arguments (list metadata)))

    (flet ((restore-weight (weight)
             (if (< weight #x80)
                 (let ((val (ash weight 3)))
                   (+ val (ash (+ val 64) -7)))
                 (- (ash (- #x100 weight) 3)))))

      (loop for decorr-pass in (metadata-decorr-passes metadata)
            repeat term-number do
           (loop for channel below channels do
                (setf (aref (decorr-pass-weight decorr-pass) channel)
                      (restore-weight (read-octet reader)))))))
  metadata)

(defmethod read-metadata-body ((metadata metadata-decorr-samples) reader)
  (if (and (= (block-version *current-block*) #x402)
           (flag-set-p *current-block* +flags-hybrid-mode+))
      (error 'block-error :format-control "Hybrid encoding is not supported"))

  (let ((first-pass (first (metadata-decorr-passes metadata))))
    (if first-pass
        (let ((channels (block-channels *current-block*))
              (first-term (decorr-pass-term first-pass))
              (bytes-read 0))

          (if (and (< first-term 0)
                   (= channels 1))
              (error 'block-error :format-control "decorrelation term < 0 and mono audio"))

          (let ((decorr-samples
                 (cond
                   ((> first-term 8)
                    (let ((decorr-samples (loop repeat channels collect
                                                (make-array (list 2) :element-type '(sb 32)))))
                      (loop for samples in decorr-samples do
                           (setf (aref samples 0)
                                 (exp2s (read-octets 2 reader :endianness :little))
                                 (aref samples 1)
                                 (exp2s (read-octets 2 reader :endianness :little)))
                           (incf bytes-read 4))
                      decorr-samples))

                   ((< first-term 0)
                    (loop for i below channels do (incf bytes-read 2) collect
                         (exp2s (read-octets 2 reader :endianness :little))))

                   (t
                    (let ((decorr-samples (loop repeat channels collect
                                               (make-array (list first-term) :element-type '(sb 32)))))
                      (loop for i below first-term do
                           (loop for samples in decorr-samples do
                                (setf (aref samples i) (exp2s (read-octets 2 reader :endianness :little)))
                                (incf bytes-read 2)))
                      decorr-samples)))))

            (if (/= bytes-read (metadata-actual-size metadata))
                (error 'block-error
                       :format-control "Size of metadata sub-block ~a is invalid"
                       :format-arguments (list metadata)))

            (setf (metadata-decorr-samples metadata) decorr-samples
                  (block-decorr-samples *current-block*) decorr-samples)))))
  metadata)

(defmethod read-metadata-body ((metadata metadata-entropy) reader)
  (let ((data-size (metadata-actual-size metadata))
        (channels (block-channels *current-block*)))

    (if (/= data-size (* 6 channels))
        (error 'block-error
               :format-control "Size of metadata sub-block ~a is invalid"
               :format-arguments (list metadata)))

    (setf (metadata-entropy-median metadata)
          (loop repeat channels collect
               (let ((median (make-array 3 :element-type '(ub 32))))
                 (loop for i below 3 do
                      (setf (aref median i)
                            (exp2s (read-octets 2 reader :endianness :little))))
                 median))

          (block-entropy-median *current-block*)
          (metadata-entropy-median metadata)))
  metadata)

(defmethod read-metadata-body ((metadata metadata-int32-info) reader)
  (let ((data-size (metadata-actual-size metadata)))
    (if (/= data-size 4)
        (error 'block-error
               :format-control "Size of metadata sub-block ~a is invalid"
               :format-arguments (list metadata))))
  (setf (metadata-sent-bits metadata) (read-octet reader)
        (metadata-zeros metadata) (read-octet reader)
        (metadata-ones metadata) (read-octet reader)
        (metadata-dups metadata) (read-octet reader)
        (block-int32-info *current-block*) metadata)
  metadata)

(defmethod read-metadata-body ((metadata metadata-wvx-bits) reader)
  (let ((int32-info (find 'metadata-int32-info (block-metadata *current-block*) :key #'type-of)))
    (if (not int32-info)
        (error 'block-error :format-control "No int32-info prior to wvx-bitstream"))
    (let* ((block-samples (block-block-samples *current-block*))
           (channels (block-channels *current-block*))
           (sent-bits (metadata-sent-bits int32-info))
           (size (metadata-actual-size metadata))
           (bits-wasted (- (* 8 size) (* channels block-samples sent-bits))))
      (if (< bits-wasted 0)
          (error 'block-error :format-control "wvx-bitstream is too small"))
      (let ((bits
             (or *wvx-buffers*
                 (loop repeat channels collect
                      (make-array (list block-samples) :element-type '(sb 32))))))
        (loop for i below block-samples do
             (loop for j below channels do
                  (setf (aref (nth j bits) i)
                        (read-bits (metadata-sent-bits int32-info) reader :endianness :little))))
        (setf (metadata-bits metadata) bits
              ;; Make a copy for easy access
              (block-wvx-bits *current-block*) bits))

      (read-bits bits-wasted reader))) ;; Why there are wasted bits anyway?
  metadata)

(defmethod read-metadata-body :around ((metadata metadata-ignorable) reader)
  (declare (ignore reader))
  (handler-bind
      ((unknown-metadata #'muffle-warning))
    (call-next-method)))

;; We do not have primary reader method for residual block, as we cannot do
;; anything with it at the moment when READ-METADATA-BODY is called. We
;; only create additional residual reader in the following method.
(defmethod read-metadata-body :after ((metadata metadata-residual) reader)
  (declare (ignore reader))
  (setf (metadata-residual-reader metadata)
        (make-reader-from-buffer (metadata-data metadata))))

;; Metadata reader
(defreader (read-metadata%) ((make-instance 'metadata) metadata)
  (metadata-id (:octets 1))
  (metadata-size (:octets (if (bit-set-p (metadata-id metadata)
                                         +meta-id-large-block+) 3 1))
                 :endianness :little
                 :function (lambda (x) (ash x 1))))

(defun read-metadata (reader)
  (let ((metadata (read-metadata% reader)))
    (setf (metadata-actual-size metadata)
          (let ((size (metadata-size metadata)))
            (if (bit-set-p (metadata-id metadata) +meta-id-data-length--1+)
                (1- size) size)))

    (let* ((id (metadata-id metadata))
           (useless (bit-set-p id +meta-id-useless-for-decoder+))
           (useful (not useless))
           (function (logand id (logior +meta-id-useless-for-decoder+
                                        +meta-id-function+))))
      (change-class
       metadata
       (cond
         ((and useful  (= function +meta-id-decorr-terms+)) 'metadata-decorr-terms)
         ((and useful  (= function +meta-id-decorr-weights+)) 'metadata-decorr-weights)
         ((and useful  (= function +meta-id-decorr-samples+)) 'metadata-decorr-samples)
         ((and useful  (= function +meta-id-entropy-vars+)) 'metadata-entropy)
         ((and useful  (= function +meta-id-wv-bitstream+)) 'metadata-wv-residual)
         ((and useful  (= function +meta-id-int32-info+)) 'metadata-int32-info)
         ((and useful  (= function +meta-id-wvx-bitstream+)) 'metadata-wvx-bits)
         ((and useless (= function +meta-id-riff-header+)) 'metadata-riff-header)
         ((and useless (= function +meta-id-riff-trailer+)) 'metadata-riff-trailer)
         (t 'metadata))))
    (read-metadata-body metadata reader)))

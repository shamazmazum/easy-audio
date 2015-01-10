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

;; Metadata body readers
(defmethod read-metadata-body ((metadata metadata) reader)
  (let ((data (make-array (list (metadata-actual-size metadata))
                          :element-type '(ub 8))))
    (read-octet-vector data reader)
    (setf (metadata-data metadata) data))

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
        (error 'block-error :message
               (format nil "Size of metadata sub-block ~a is incorrect" metadata))))

  (loop for decorr-pass in (metadata-decorr-passes metadata) do
       (let* ((octet (read-octet reader))
              (term (- (logand octet #x1f) 5))
              (delta (logand (ash octet -5) #x7)))
         (if (or (= term 0)
                 (< term -3)
                 (and (> term 8) (< term 17))
                 (> term 18))
             (error 'wv-block :message
                    (format nil "Invalid term in metadata sub-block ~a" metadata)))

         (setf (decorr-pass-term decorr-pass) term
               (decorr-pass-delta decorr-pass) delta)))
  metadata)

(defmethod read-metadata-body ((metadata metadata-decorr-weights) reader)
  (let* ((data-size (metadata-actual-size metadata))
         (mono (bit-set-p (block-flags *current-block*) +flags-mono-output+))
         (term-number (if mono data-size (ash data-size -1)))
         (channels (if mono 1 2)))

    (if (> term-number
           (length (metadata-decorr-passes metadata)))
        (error 'block-error :message
               (format nil "Size of metadata sub-block ~a is too big" metadata)))

    (flet ((restore-weight (weight)
             (let ((res (ash weight 3)))
               (if (> res 0) (+ res (ash (+ res 64) -7)) res))))
      (loop for decorr-pass in (metadata-decorr-passes metadata)
            repeat term-number do
           (loop for channel below channels do
                (setf (aref (decorr-pass-weight decorr-pass) channel)
                      (restore-weight (read-octet reader)))))))
  metadata)

(defmethod read-metadata-body ((metadata metadata-decorr-samples) reader)
  (if (and (= (block-version *current-block*) #x402)
           (bit-set-p (block-flags *current-block*) +flags-hybrid-mode+))
      (error 'block-error "Hybrid encoding is not supported"))

  (let ((channels (if (bit-set-p (block-flags *current-block*) +flags-mono-output+) 1 2))
        (first-term (decorr-pass-term (first (metadata-decorr-passes metadata))))
        (bytes-read 0)
        decorr-samples)

    (cond
      ((> first-term 8)
       (setq decorr-samples (make-array (list 2 2) :element-type '(sb 32)))
       (loop for i below channels do
            (setf (aref decorr-samples 0 i)
                  (exp2s (read-octets 2 reader :endianness :little))
                  (aref decorr-samples 1 i)
                  (exp2s (read-octets 2 reader :endianness :little)))
            (incf bytes-read 4)))

      ((< first-term 0)
       (if (= channels 1)
           (error 'block-error :message "decorrelation term < 0 and mono audio"))
       (setq decorr-samples (make-array (list 1 2) :element-type '(sb 32)))
       (loop for i below channels do
            (setf (aref decorr-samples 0 i) (exp2s (read-octets 2 reader :endianness :little)))
            (incf bytes-read 2)))

      (t
       (setq decorr-samples (make-array (list first-term 2) :element-type '(sb 32)))
       (loop for i below first-term do
            (loop for j below channels do
                 (setf (aref decorr-samples i j) (exp2s (read-octets 2 reader :endianness :little)))
                 (incf bytes-read 2)))))

    (if (/= bytes-read (metadata-actual-size metadata))
        (error 'block-error :message
               (format nil "Size of metadata sub-block ~a is invalid" metadata)))

    (setf (metadata-decorr-samples metadata) decorr-samples
          (block-decorr-samples *current-block*) decorr-samples))
  metadata)

(defmethod read-metadata-body ((metadata metadata-entropy) reader)
  (let ((data-size (metadata-actual-size metadata))
        (mono (bit-set-p (block-flags *current-block*)
                         +flags-mono-output+))
        (entropy-median (block-entropy-median *current-block*)))

    (if (/= data-size (if mono 6 12))
        (error 'block-error :message
               (format nil "Size of metadata sub-block ~a is invalid" metadata)))

    (loop for i below (if mono 1 2) do
         (loop for j below 3 do
              (setf (aref entropy-median j i)
                    (exp2s (read-octets 2 reader :endianness :little)))))

    (setf (metadata-entropy-median metadata) entropy-median))
  metadata)

;; There is nothing we can do with residuals at this moment
;; as they can be decoded all at once, so just silence warning
;; and assign buffer reader to data
(defmethod read-metadata-body :around ((metadata metadata-residual) reader)
  (handler-bind
      ((unknown-metadata #'muffle-warning))
    (call-next-method))
  (setf (metadata-residual-reader metadata)
        (make-reader-from-buffer (metadata-data metadata)))
  metadata)

;; Metadata reader
(defreader read-metadata% ((make-instance 'metadata) metadata)
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

    (if (not (bit-set-p (metadata-id metadata) +meta-id-useless-for-decoder+))
        (change-class
         metadata
         (let ((function (logand (metadata-id metadata) +meta-id-function+)))
           (cond
             ((= function +meta-id-decorr-terms+) 'metadata-decorr-terms)
             ((= function +meta-id-decorr-weights+) 'metadata-decorr-weights)
             ((= function +meta-id-decorr-samples+) 'metadata-decorr-samples)
             ((= function +meta-id-entropy-vars+) 'metadata-entropy)
             ((= function +meta-id-wv-bitstream+) 'metadata-wv-residual)
             (t 'metadata)))))
    (read-metadata-body metadata reader)))

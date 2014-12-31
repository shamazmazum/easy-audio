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
         (stereo (= (logand (block-flags *current-block*)
                            +flags-output-mask+)
                    +flags-stereo-output+))
         (term-number (if stereo (ash data-size -1) data-size)))

    (if (> term-number
           (length (metadata-decorr-passes metadata)))
        (error 'block-error :message
               (format nil "Size of metadata sub-block ~a is too big" metadata)))

    (flet ((restore-weight (weight)
             (let ((res (ash weight 3)))
               (if (> res 0) (+ res (ash (+ res 64) -7)) res))))
      (loop for decorr-pass in (metadata-decorr-passes metadata)
            repeat term-number do
           (setf (decorr-pass-weight-A decorr-pass)
                 (restore-weight (read-octet reader)))
           (if stereo (setf (decorr-pass-weight-b decorr-pass)
                            (restore-weight (read-octet reader)))))))
  metadata)

;; Metadata reader
(defun large-meta-p (metadata)
  (= (logand (metadata-id metadata)
             +meta-id-large-block+)
     +meta-id-large-block+))

(defreader read-metadata% ((make-instance 'metadata) metadata)
  (metadata-id (:octets 1))
  (metadata-size (:octets (if (large-meta-p metadata) 3 1))
                 :endianness :little
                 :function (lambda (x) (ash x 1))))

(defun read-metadata (reader)
  (let ((metadata (read-metadata% reader)))
    (setf (metadata-actual-size metadata)
          (let ((size (metadata-size metadata)))
            (if (/= (logand (metadata-id metadata) +meta-id-data-length--1+) 0)
                (1- size) size)))

    (change-class
     metadata
     (let ((function (logand (metadata-id metadata) +meta-id-function+)))
       (cond
         ((= function +meta-id-decorr-terms+) 'metadata-decorr-terms)
         ((= function +meta-id-decorr-weights+) 'metadata-decorr-weights)
         (t 'metadata))))
    (read-metadata-body metadata reader)))

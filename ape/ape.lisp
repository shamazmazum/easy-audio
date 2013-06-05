;; Copyright (c) 2012-2013, Vasily Postnicov
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

(in-package :easy-audio.ape)

;; Skip id3 tags
(define-stub skip-junk)

(defun first-frame-offset (header)
  (let ((lengths (header-blocks-length header)))
  (+ (blocks-length-junk lengths)
     (blocks-length-descriptor lengths)
     (blocks-length-header lengths)
     (blocks-length-seektable lengths)
     (blocks-length-wavheader lengths)
     (if (< (header-version header) 3810)
         (header-total-frames header) 0))))

(defun get-total-samples (header)
  (+ (* (1- (header-total-frames header))
        (header-blocks-per-frame header))
     (header-final-frame-blocks header)))

;; Skip n octets
(defun skip-octets (reader n message)
  (let ((array (make-array (list n)
                           :element-type 'u8)))
    (warn message)
    (read-octet-vector array reader)))

(defun read-ape-header-3980- (reader header)
  (let ((lengths (header-blocks-length header)))
    (setf
     (blocks-length-header lengths) 32
     (header-compression-type header) (read-bits 16 reader)
     (header-format-flags header) (read-bits 16 reader)
     (header-channels header) (read-bits 16 reader)
     (header-sample-rate header) (bitreader.le-bignum:read-bits 32 reader)
     (blocks-length-wavheader lengths) (bitreader.le-bignum:read-bits 32 reader)
     (blocks-length-wavtail lengths) (bitreader.le-bignum:read-bits 32 reader)
     (header-total-frames header) (bitreader.le-bignum:read-bits 32 reader)
     (header-final-frame-blocks header) (bitreader.le-bignum:read-bits 32 reader))
     
    (cond
     ((flag-set-p (header-format-flags header) :has-peek-level)
      (incf (blocks-length-header lengths) 4)
      (skip-octets reader 4 "Skip peak level")))

    (cond
     ((flag-set-p (header-format-flags header) :has-seek-elements)
      (incf (blocks-length-header lengths) 4)
      (setf (blocks-length-seektable lengths)
            (* (bitreader.le-bignum:read-bits 32 reader) 4)))
     (t (setf (blocks-length-seektable lengths)
              (* (header-total-frames header) 4))))

    (setf (header-bps header)
          (cond
           ((flag-set-p (header-format-flags header) :8bit) 8)
           ((flag-set-p (header-format-flags header) :24bit) 24)
           (t 16)))

    (setf (header-blocks-per-frame header)
          (cond
           ((>= (header-version header) 3950) #.(* 4 73728))
           ((or (>= (header-version header) 3900)
                (and (>= (header-version header) 3800)
                     (>= (header-compression-type header) 4000)))
            73728)
           (t 9216)))

    (if (not (flag-set-p (header-format-flags header) :create-wav-header))
        (skip-octets reader
                     (blocks-length-wavheader lengths)
                     "Skip wav header"))))

(defun read-ape-header-3980+ (reader header)
  ;; Read padding
  (read-bits 16 reader)
  (let ((lengths (header-blocks-length header))
        (descriptor-length (bitreader.le-bignum:read-bits 32 reader)))
    (setf
     (blocks-length-descriptor lengths)
     descriptor-length
     
     (blocks-length-header lengths)
     (bitreader.le-bignum:read-bits 32 reader)
     
     (blocks-length-seektable lengths)
     (bitreader.le-bignum:read-bits 32 reader)
     
     (blocks-length-wavheader lengths)
     (bitreader.le-bignum:read-bits 32 reader)
     
     (blocks-length-audiodata lengths)
     (bitreader.le-bignum:read-bits 32 reader)
     
     (blocks-length-audiodata-high lengths)
     (bitreader.le-bignum:read-bits 32 reader)
    
     (blocks-length-wavtail lengths)
     (bitreader.le-bignum:read-bits 32 reader))
    
    (let ((md5 (make-array 16 :element-type 'u8)))
      (read-octet-vector md5 reader)
      (setf (header-md5 header) md5))

    (if (> descriptor-length 52) (skip-octets reader
                                              (- descriptor-length 52)
                                              "Skip rest of descriptor in ape file"))
    
    (setf (header-compression-type header) (read-bits 16 reader)
          (header-format-flags header) (read-bits 16 reader)
          (header-blocks-per-frame header) (bitreader.le-bignum:read-bits 32 reader)
          (header-final-frame-blocks header) (bitreader.le-bignum:read-bits 32 reader)
          (header-total-frames header) (bitreader.le-bignum:read-bits 32 reader)
          (header-bps header) (read-bits 16 reader)
          (header-channels header) (read-bits 16 reader)
          (header-sample-rate header) (bitreader.le-bignum:read-bits 32 reader))))

(defun open-ape (stream)
  (let* ((bitreader (make-reader :stream stream))
         (header (make-header))
         (lengths (header-blocks-length header)))
    (skip-junk bitreader)
    (if (/= (bitreader.le-bignum:read-bits 32 bitreader) +ape-id+)
        (error 'ape-error :message "Stream is not ape stream"))

    (let ((version (read-bits 16 bitreader)))
      (setf (header-version header) version)
      
      (if (or (< version +min-ape-version+)
              (> version +max-ape-version+))
          (error 'ape-error :message "Unsupported version"))

      (if (>= version 3980)
          (read-ape-header-3980+ bitreader header)
        (read-ape-header-3980- bitreader header))

      ;; Sanity checks
      (if (= (header-total-frames header) 0)
          (error 'ape-error :message "Total frame number is zero"))
      
      (if (and (/= (blocks-length-seektable lengths) 0)
               (< (/ (blocks-length-seektable lengths) 4)
                  (header-total-frames header)))
          (error 'ape-error :message "Number of seek entries is too low"))

      (if (= (blocks-length-seektable lengths) 0)
          (error 'ape-error :message "Missing seektable"))
      
      ;; Read seektable
      (let* ((seektable-elem (floor (blocks-length-seektable lengths) 4))
             (seektable (make-array (list seektable-elem)
                                    :element-type 'u32)))
        (loop for i below seektable-elem do
              (setf (aref seektable i) (bitreader.le-bignum:read-bits 32 bitreader)))
        (setf (header-seektable header) seektable)
        (if (/= (aref seektable 0)
                (first-frame-offset header))
            (error 'ape-error "Malformed seektable")))

      (if (< version 3810)
          (let ((bittable (make-array (list (header-total-frames header))
                                      :element-type 'u8)))
            (read-octet-vector bittable bitreader)
            (setf (header-bittable header) bittable))))
    
    header))

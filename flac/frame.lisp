;; Copyright (c) 2012, Vasily Postnicov
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

(in-package :easy-audio.flac)

(sera:-> get-blocking-strategy (bit)
         (values (member :fixed :variable) &optional))
(defun get-blocking-strategy (val)
  (declare (optimize (speed 3)))
  (ecase val
    (0 :fixed)
    (1 :variable)))

(sera:-> get-block-size ((unsigned-byte 4) reader)
         (values (unsigned-byte 16) &optional))
(defun get-block-size (val reader)
  (declare (optimize (speed 3)))
  (cond
    ;; 0001
    ((= val 1) 192)
    ;; 0010-0101
    ((and (<= val 5)
          (/= val 0))
     (ash 576 (- val 2)))
    ;; 0110
    ((= val 6) (1+ (read-octet reader)))
    ;; 0111
    ((= val 7) (1+ (read-octets 2 reader)))
    ;; 1000-1111
    ((and (<= val 15)
          (/= val 0))
     (ash 1 val))
    (t (error 'flac-bad-frame
              :format-control "Frame block size is invalid"))))

(sera:-> get-sample-rate ((unsigned-byte 4) reader &optional (or null streaminfo))
         (values positive-fixnum &optional))
(defun get-sample-rate (val reader &optional streaminfo)
  (declare (optimize (speed 3)))
  (cond
    ((and (/= val 0)
          (< val 12))
     (nth (1- val) +coded-sample-rates+))
    ((= val 12) (* 1000 (read-octet reader)))
    ((= val 13) (read-octets 2 reader))
    ((= val 14) (* 10 (read-octets 2 reader)))
    ((and (= val 0) streaminfo)
     (streaminfo-samplerate  streaminfo))
    (t
     (error 'flac-bad-frame
            :format-control "Frame sample rate is invalid"))))

(sera:-> get-channel-assignment ((unsigned-byte 4))
         (values (integer 1 11) &optional))
(defun get-channel-assignment (val)
  (declare (optimize (speed 3)))
  (cond ((<= val 10) (1+ val))
        (t (error 'flac-bad-frame
                  :format-control "Invalid channel assignment"))))

(sera:-> get-sample-size ((unsigned-byte 3) &optional (or null streaminfo))
         (values (integer 4 32) &optional))
(defun get-sample-size (val &optional streaminfo)
  (declare (optimize (speed 3)))
  (if (and (= val 0) streaminfo)
      (streaminfo-bitspersample streaminfo)
      (or (cdr (assoc val +coded-sample-sizes+))
          (error 'flac-bad-frame
                 :format-control "Invalid sample size"))))

;; Residual reader
(sera:-> read-residual-body
         (reader (sa-sb 32) fixnum blocksize fixnum fixnum)
         (values (sa-sb 32) &optional))
(defun read-residual-body (bit-reader out predictor-order blocksize param-len esc-code)
  (declare (optimize (speed 3)))
  (let* ((part-order (read-bits 4 bit-reader))
	 (sample-idx predictor-order)
	 (partition-samples (ash blocksize (- part-order))))
    (declare (type fixnum sample-idx))
    (loop for i below (ash 1 part-order) do
	  (let ((samples-num
		 (cond
		  ;; FIXME:: Check following lines
		  ((zerop i) (- partition-samples predictor-order))
		  (t partition-samples)))
		(rice-parameter (read-bits param-len bit-reader)))
	    (cond
	     ((/= rice-parameter esc-code)
	      (loop repeat samples-num do
		    (setf (aref out sample-idx)
			  (read-rice-signed bit-reader rice-parameter))
		    (incf sample-idx)))
	     (t
	      ;; FIXME: read unencoded signed rice
	      ;; Do we need to store bps?
	      ;; Read bps:
	      (setq rice-parameter (read-bits 5 bit-reader))
	      (read-bits-array bit-reader out rice-parameter
			       :signed t
			       :offset sample-idx)
	      (incf sample-idx samples-num)))))
    out))

(sera:-> read-residual
         (reader (sa-sb 32) fixnum blocksize)
         (values (sa-sb 32) &optional))
(defun read-residual (bit-reader out predictor-order blocksize)
  (declare (optimize (speed 3)))
  (let ((coding-method (read-bits 2 bit-reader)))
    (cond
     ((= coding-method 0) ; 00
      (read-residual-body bit-reader out predictor-order blocksize 4 #b1111))
     ((= coding-method 1) ; 01
      (read-residual-body bit-reader out predictor-order blocksize 5 #b11111))
     (t (error 'flac-bad-frame
	       :format-control "Invalid residual coding method")))))

;; Subframe reader
(sera:-> read-subframe-verbatim
         (reader subframe-header)
         (values subframe-verbatim &optional))
(defun read-subframe-verbatim (stream header)
  (declare (optimize (speed 3)))
  (let ((bps (subframe-header-actual-bps header)))
    (read-bits-array stream (subframe-header-out-buf header)
                     bps :signed t)
    (subframe-verbatim header)))

(sera:-> read-subframe-constant
         (reader subframe-header)
         (values subframe-constant &optional))
(defun read-subframe-constant (stream header)
  (declare (optimize (speed 3)))
  (let* ((actual-bps (subframe-header-actual-bps header))
         (value (unsigned-to-signed
		 (read-bits actual-bps stream)
		 actual-bps)))
    (subframe-constant header value)))

(sera:-> read-subframe-fixed
         (reader subframe-header blocksize (integer 0 4))
         (values subframe-fixed &optional))
(defun read-subframe-fixed (stream header blocksize order)
  (declare (optimize (speed 3)))
  (let ((bps (subframe-header-actual-bps header))
	(warm-up-samples order)
	(out-buf (subframe-header-out-buf header)))
    (read-bits-array stream out-buf bps :signed t :len warm-up-samples)
    (read-residual stream out-buf order blocksize)
    (subframe-fixed header order)))

(sera:-> read-subframe-lpc
         (reader subframe-header blocksize (integer 1 32))
         (values subframe-lpc &optional))
(defun read-subframe-lpc (stream header blocksize order)
  (declare (optimize (speed 3)))
  (let ((bps (subframe-header-actual-bps header))
	(warm-up-samples order)
	(out-buf (subframe-header-out-buf header)))
    (read-bits-array stream out-buf bps
                     :signed t :len warm-up-samples)
    (flet ((check-precision (precision)
             (when (= #b10000 precision)
	       (error 'flac-bad-frame
	              :format-control "lpc coefficients precision cannot be 16"))
             precision))
      (let* ((precision (check-precision (1+ (read-bits 4 stream))))
             (coeff-shift (unsigned-to-signed (read-bits 5 stream) 5))
             (predictor-coeff
              (read-bits-array
               stream (make-array warm-up-samples :element-type '(sb 32)) precision
               :signed t)))
        (read-residual stream out-buf order blocksize)
        (subframe-lpc header order precision coeff-shift predictor-coeff)))))

(sera:-> read-subframe (reader blocksize (integer 4 33))
         (values subframe &optional))
(defun read-subframe (stream blocksize actual-bps)
  (declare (optimize (speed 3)))
  (unless (zerop (read-bit stream))
    (error 'flac-bad-frame
	   :format-control "Error reading subframe"))
  (let* ((type-num (read-bits 6 stream))
         (wasted-bits
          (let ((lead-in-bit (read-bit stream)))
            (if (= lead-in-bit 1)
                (1+ (count-zeros stream)) 0)))
         (header
          (subframe-header
           wasted-bits
           (- actual-bps wasted-bits)
           (make-array
            (list blocksize)
            :element-type '(sb 32)))))
    (cond
      ;; 000000
      ((= type-num 0)
       (read-subframe-constant stream header))
      ((= type-num 1)
       ;; 000001
       (read-subframe-constant stream header))
      ;; 001000-001100
      ((and
	(>= type-num 8)
	(<= type-num 12))
       (read-subframe-fixed stream header blocksize
                            (logand type-num #b111)))
      ;; 100000-111111
      ((and
	(>= type-num 32)
	(<= type-num 63))
       (read-subframe-lpc stream header blocksize
                          (1+ (logand type-num #b11111))))
      (t (error 'flac-bad-frame
		:format-control "Error subframe type")))))

(sera:-> check-frame-crc (reader)
         (values (ub 16) &optional))
(defun check-frame-crc (stream)
  (declare (optimize (speed 3)))
  #+easy-audio-check-crc
  (let ((crc (get-crc stream)))
    (declare (type (ub 16) crc))
    (unless (= crc (read-bits 16 stream))
      (error 'flac-bad-frame
             :format-control "Frame CRC mismatch"))
    crc)
  #-easy-audio-check-crc
  (read-bits 16 stream))

(sera:-> %read-frame (reader &optional (or null streaminfo))
         (values frame &optional))
(defun %read-frame (stream &optional streaminfo)
  (declare (optimize (speed 3)))
  #+easy-audio-check-crc
  (init-crc stream)
  (when (/= +frame-sync-code+ (read-bits 14 stream))
    (error 'flac-bad-frame
           :format-control "Frame sync code is not 11111111111110"))
  (unless (zerop (read-bit stream))
    (error 'flac-bad-frame
	   :format-control "Error reading frame"))

  (let ((blocking-strategy (get-blocking-strategy (read-bit stream)))
        (block-size (read-bits 4 stream))
        (sample-rate (read-bits 4 stream))
        (assignment (get-channel-assignment (read-bits 4 stream)))
        (sample-size (get-sample-size (read-bits 3 stream) streaminfo)))

    (unless (zerop (read-bit stream))
      (error 'flac-bad-frame
             :format-control "Error reading frame"))

    (unless (eq blocking-strategy :fixed)
      (error 'flac-bad-frame
             :format-control "Variable block size not implemented yet"))
    
      (let* ((number (read-utf8-u32 stream))
             (block-size (get-block-size block-size stream))
             (sample-rate (get-sample-rate sample-rate stream streaminfo))
             (crc-8 (read-octet stream))
             (subframes
              (loop for sf fixnum below (if (<= assignment +max-channels+) assignment 2)
                    collect (read-subframe
                             stream block-size
                             ;; Do bps correction
                             (cond
                               ((or
                                 (and (= assignment +left-side+)
                                      (= sf 1))
                                 (and (= assignment +right-side+)
                                      (= sf 0))
                                 (and (= assignment +mid-side+)
                                      (= sf 1)))
                                (1+ sample-size))
                               (t sample-size))))))

        ;; Check zero padding
        (unless (zerop (read-to-byte-alignment stream))
          (error 'flac-bad-frame
                 :format-control "Padding to byte-alignment is not zero"))

        (frame blocking-strategy block-size sample-rate
               assignment sample-size number crc-8 subframes
               (check-frame-crc stream)))))

(sera:-> read-frame (reader &optional (or null streaminfo))
         (values frame &optional))
(defun read-frame (stream &optional streaminfo)
  "Read a frame from a flac stream. An additional metadata
@c(streaminfo) may be required for some files which do not support
streaming."
  (restart-case
      (%read-frame stream streaminfo)
      (skip-malformed-frame ()
        :report "Skip this frame and read the next one"
        (restore-sync stream streaminfo)
        (read-frame stream streaminfo))
      (stop-reading-frame ()
        :report "Do nothing and return dummy frame"
        (make-instance 'frame))))

;; Rather slow (and buggy) absolute sample seek
(sera:-> seek-sample (reader unsigned-byte &key
                             (:seektable  (or null seektable))
                             (:streaminfo (or null streaminfo)))
         (values unsigned-byte &optional))
(defun seek-sample (bitreader sample &key seektable streaminfo)
  "Seeks to an interchannel sample.  Sets input to new frame, which
contains this sample.  Returns position of this sample in the frame.
@c(seektable) and @c(streaminfo) are optional. Providing
@c(streaminfo) enables additional sanity checks. Currently only fixed
block size is supported."
  (with-accessors ((totalsamples streaminfo-totalsamples)) streaminfo
    (when (and streaminfo
               (> sample totalsamples)
               (/= totalsamples 0))
      (error 'flac-error
             :format-control
             "Seek error. Desired sample number is bigger than number of samples in stream")))

  ;; Reset the reader
  (reader-position bitreader 0)
  (restore-sync bitreader)
  ;; Init the boundaries where desired sample must be
  (let ((start-pos (reader-position bitreader))
	(end-pos (reader-length bitreader)))

    ;; Now, if seektable is present, correct the boundaries
    (when seektable
      (let* ((points (seektable-seekpoints seektable))
             (pos (position-if #'(lambda (samplenum) (>= samplenum sample))
                               points
                               :key #'seekpoint-samplenum))
             (upperpoint (seekpoint-offset (nth pos points)))
             (lowerpoint (if (= pos 0) 0 (seekpoint-offset (nth (1- pos) points)))))
        (cond
          ((= sample lowerpoint)
           ;; We are extremely lucky
           ;; All we need to do is set input to a new frame
           (reader-position bitreader (+ start-pos lowerpoint))
           (return-from seek-sample 0))
          (t
           (psetq start-pos (+ start-pos lowerpoint)
                  end-pos (+ start-pos upperpoint))))))

    ;; Check implementation limitations
    (when (and streaminfo
               (/= (streaminfo-minblocksize streaminfo)
                   (streaminfo-maxblocksize streaminfo)))
      (error 'flac-bad-metadata
	     :format-control "Cannot seek with variable blocksize"))

    (multiple-value-bind (needed-num remainder)
	(floor sample (if streaminfo
                          (streaminfo-minblocksize streaminfo)
                          (frame-block-size (read-frame bitreader))))
      (declare (type non-negative-fixnum needed-num remainder))

      (labels ((dichotomy-search (start end)
				 "Searches for desired frame num by
                                  dividing stream in half"
				 (let* ((first-half start)
					(second-half (floor (+ start end) 2))
					
					(firstnum (progn (reader-position bitreader first-half)
							 (restore-sync bitreader streaminfo)))
					
					(secondnum (progn (reader-position bitreader (1- second-half))
							  (restore-sync bitreader streaminfo))))

				   (when (< needed-num firstnum)
                                     (error 'flac-error :format-control "Seek error"))

				   (cond 
				    ((< secondnum needed-num)
				     (dichotomy-search second-half end))
				    ((> secondnum needed-num)
				     (dichotomy-search start second-half))
				    (t t)))))

	(dichotomy-search start-pos end-pos)
	remainder))))

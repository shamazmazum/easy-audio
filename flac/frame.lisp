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

(declaim (optimize (speed 3)))

(sera:defvar-unbound *out-buffer*
    "Output buffer for exactly one subframe")
(defvar *out-buffers* nil
  "Output buffers for stream with a fixed block size")

(defun get-blocking-strategy (val)
  (declare (type non-negative-fixnum val))
  (cond
    ((= val 0) :fixed)
    ((= val 1) :variable)
    (t (error 'flac-bad-frame
              :format-control "Blocking strategy must be 0 or 1"))))

(defun get-block-size (val &optional reader)
  (declare (type non-negative-fixnum val)
           (type (or null reader) reader))
  (cond
    ((= val 1) 192)               ; 0001
    ((and (<= val 5)              ; 0010-0101
          (/= val 0))
     (ash 576 (- val 2)))
    ((= val 6) (1+ (read-octet reader)))   ; 0110
    ((= val 7) (1+ (the (integer 0 #.(ash 1 16))
                        (read-octets 2 reader))))   ; 0111
    ((and (<= val 15)             ; 1000-1111
          (/= val 0))
     (ash 1 val))
    (t (error 'flac-bad-frame
              :format-control "Frame block size is invalid"))))

(defun get-sample-rate (val &optional reader streaminfo)
  (declare (type non-negative-fixnum val)
           (type (or null streaminfo) streaminfo)
           (type (or null reader) reader))
  (cond
    ((and (/= val 0)
          (< val 12))
     (nth (1- val) +coded-sample-rates+))
    ((= val 12) (* 1000 (read-octet reader)))
    ((= val 13) (read-octets 2 reader))
    ((= val 14) (* 10 (the (integer 0 #.(ash 1 16))
                           (read-octets 2 reader))))
    ((and (= val 0)
          streaminfo)
     (streaminfo-samplerate  streaminfo))
    (t
     (error 'flac-bad-frame
            :format-control "Frame sample rate is invalid"))))

(defun get-channel-assignment (val)
  (declare (type non-negative-fixnum val))
  (cond ((<= val 10) (1+ val))
        (t (error 'flac-bad-frame
                  :format-control "Invalid channel assignment"))))

(defun get-sample-size (val &optional streaminfo)
  (declare (type non-negative-fixnum val)
           (type (or null streaminfo) streaminfo))
  (if (and (= val 0) streaminfo)
      (streaminfo-bitspersample streaminfo)
      (or (cdr (assoc val +coded-sample-sizes+))
          (error 'flac-bad-frame
                 :format-control "Invalid sample size"))))

;; Residual reader
(defun read-residual (bit-reader subframe frame out)
  (let ((coding-method (read-bits 2 bit-reader)))
    (declare (type (ub 2) coding-method))
    (cond
     ((= coding-method 0) ; 00
      (read-residual-body bit-reader subframe frame out
                          :param-len 4
                          :esc-code #b1111))
     ((= coding-method 1) ; 01
      (read-residual-body bit-reader subframe frame out
                          :param-len 5
                          :esc-code #b11111))
     (t (error 'flac-bad-frame
	       :format-control "Invalid residual coding method")))))

(defun read-residual-body (bit-reader subframe frame out &key param-len esc-code)
  (declare (type fixnum param-len esc-code)
	   (type (sa-sb 32) out))

  (let* ((part-order (the (ub 4)
		       (read-bits 4 bit-reader)))
	 (sample-idx (subframe-order subframe))
	 (blocksize (frame-block-size frame))
	 (predictor-order (subframe-order subframe))
	 (partition-samples (ash blocksize (- part-order))))
    (declare (type fixnum sample-idx
		   blocksize predictor-order
		   partition-samples))
    
    (loop for i below (ash 1 part-order) do
	  (let ((samples-num
		 (cond
		  ;; FIXME:: Check following lines
		  ((zerop i) (- partition-samples predictor-order))
		  (t partition-samples)))
		(rice-parameter (read-bits param-len bit-reader)))
	    (declare (type non-negative-fixnum rice-parameter samples-num))
	    
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

;; Subframe reader

(defmethod read-subframe-body (bit-reader (subframe subframe-lpc) frame)
  (let* ((bps (subframe-actual-bps subframe))
	 (warm-up-samples (subframe-order subframe))
	 (out-buf (subframe-out-buf subframe))
	 (coeff-buf (make-array (list warm-up-samples)
				:element-type '(sb 32))))
    
    (read-bits-array bit-reader
		     out-buf bps :signed t :len warm-up-samples)
    
    (let ((precision (1+ (read-bits 4 bit-reader))))
      (declare (type (integer 1 16) precision))
      (when (= #b10000 precision)
	(error 'flac-bad-frame
	       :format-control "lpc coefficients precision cannot be 16"))
      (setf (subframe-lpc-precision subframe) precision
            (subframe-lpc-coeff-shift subframe)
	    (unsigned-to-signed (read-bits 5 bit-reader) 5)
            (subframe-lpc-predictor-coeff subframe)
	    (read-bits-array bit-reader
			     coeff-buf precision :signed t)))
    (read-residual bit-reader subframe frame out-buf)))

(defmethod read-subframe-body (bit-reader (subframe subframe-fixed) frame)
  (let ((bps (subframe-actual-bps subframe))
	(warm-up-samples (subframe-order subframe))
	(out-buf (subframe-out-buf subframe)))
    (read-bits-array bit-reader out-buf bps :signed t :len warm-up-samples)
    (read-residual bit-reader subframe frame out-buf)))

(defmethod read-subframe-body (bit-reader (subframe subframe-constant) frame)
  (declare (ignore frame))
  (with-slots (actual-bps) subframe
	      (setf (subframe-constant-value subframe) ;; FIXME: value is signed in original libFLAC
		    (unsigned-to-signed
		     (read-bits actual-bps bit-reader)
		     actual-bps))))

(defmethod read-subframe-body (bit-reader (subframe subframe-verbatim) frame)
  (declare (ignore frame))
  (let ((bps (subframe-actual-bps subframe)))
    (with-slots (out-buf) subframe
      (setf out-buf
	    (read-bits-array bit-reader out-buf bps :signed t)))))

(defun read-subframe (stream frame actual-bps)
  (declare (type (integer 4 33) actual-bps))
  (unless (zerop (read-bit stream))
    (error 'flac-bad-frame
	   :format-control "Error reading subframe"))
  (let* ((type-num (read-bits 6 stream))
	 (subframe
	  (cond
	    ((= type-num 0) (make-instance 'subframe-constant))         ; 000000
	    ((= type-num 1) (make-instance 'subframe-verbatim))         ; 000001
	    ((and
	      (>= type-num 8)
	      (<= type-num 12))
	     (make-instance 'subframe-fixed :order (logand type-num #b111)))     ; 001000-001100
	    ((and
	      (>= type-num 32)
	      (<= type-num 63))
	     (make-instance 'subframe-lpc :order (1+ (logand type-num #b11111)))) ; 100000-111111
	    (t (error 'flac-bad-frame
		      :format-control "Error subframe type"))))
         (wasted-bits
          (let ((lead-in-bit (read-bit stream)))
            (if (= lead-in-bit 1)
                (1+ (count-zeros stream)) 0)))
         (blocksize (frame-block-size frame)))
    (declare (type non-negative-fixnum wasted-bits))

    (setf (subframe-wasted-bps subframe) wasted-bits            
          (subframe-actual-bps subframe)
          (- actual-bps wasted-bits)
          (subframe-out-buf subframe)
          (if (and *out-buffer*
                   (= (length (the (sa-sb 32) *out-buffer*)) blocksize))
              *out-buffer*
              (make-array
               (list blocksize)
               :element-type '(sb 32))))

    (read-subframe-body stream subframe frame)
    subframe))

(defmethod read-frame :around (stream &optional streaminfo)
  (restart-case
      (call-next-method)
      (skip-malformed-frame ()
        :report "Skip this frame and read the next one"
        (restore-sync stream streaminfo)
        (read-frame stream streaminfo))
      (stop-reading-frame ()
        :report "Do nothing and return dummy frame"
        (make-instance 'frame))))

(defmethod read-frame (stream &optional streaminfo)
  (let ((frame (make-instance 'frame)))
    #+easy-audio-check-crc
    (init-crc stream)
    (when (/= +frame-sync-code+ (read-bits 14 stream))
      (error 'flac-bad-frame
             :format-control "Frame sync code is not 11111111111110"))
    (unless (zerop (read-bit stream))
      (error 'flac-bad-frame
	     :format-control "Error reading frame"))

    (with-slots (blocking-strategy
                 block-size
                 sample-rate
                 channel-assignment
                 sample-size
                 number)
        frame
      (setf blocking-strategy (get-blocking-strategy (read-bit stream))
            block-size (read-bits 4 stream)
            sample-rate (read-bits 4 stream)
            channel-assignment (get-channel-assignment (read-bits 4 stream))
            sample-size (get-sample-size (read-bits 3 stream) streaminfo))
      
      (unless (zerop (read-bit stream))
        (error 'flac-bad-frame
               :format-control "Error reading frame"))

      (unless (eq blocking-strategy :fixed)
        (error 'flac-bad-frame
               :format-control "Variable block size not implemented yet"))
      (setf number (read-utf8-u32 stream)
            block-size (get-block-size block-size stream)
            sample-rate (get-sample-rate sample-rate stream streaminfo)
            (frame-crc-8 frame) (read-octet stream)))

    (let ((assignment (frame-channel-assignment frame))
          (sample-size (frame-sample-size frame)))
      (declare (type non-negative-fixnum assignment)
               (type (integer 4 32) sample-size))
      (setf (frame-subframes frame)
            (loop for sf fixnum below (if (<= assignment +max-channels+) assignment 2)
                  collect (let ((*out-buffer* (nth sf *out-buffers*)))
                            (read-subframe
                             stream frame
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
                               (t sample-size)))))))
    
    ;; Check zero padding
    (unless (zerop (read-to-byte-alignment stream))
      (error 'flac-bad-frame
             :format-control "Padding to byte-alignment is not zero"))

    #+easy-audio-check-crc
    (let ((crc (get-crc stream)))
      (declare (type (ub 16) crc))
      (setf (frame-crc-16 frame) crc)
      (when (/= crc (read-bits 16 stream))
        (error 'flac-bad-frame
               :format-control "Frame CRC mismatch")))
    #-easy-audio-check-crc
    (setf (frame-crc-16 frame) (read-bits 16 stream))
  frame))

;; Rather slow (and buggy) absolute sample seek
(defun seek-sample (bitreader sample &key seektable streaminfo)
  "Seeks to an interchannel sample.
   Sets input to new frame, which contains this sample.
   Returns position of this sample in the frame.
   @c(seektable) and @c(streaminfo) are optional. Providing @c(streaminfo) enables
   additional sanity checks. Currently only fixed block size is supported."
  (declare (type reader bitreader)
	   (type (or null streaminfo) streaminfo)
	   (optimize (speed 0)))

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
    (if seektable
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

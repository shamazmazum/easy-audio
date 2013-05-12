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
(defvar *data*) ;; For signaling errors from functions called from body-readers (they usual requires just a stream, so no need pass data object as argument)

(defun metadata-summar-length (blocks)
  (declare (type list blocks))
  (reduce #'+ (mapcar #'(lambda (data) (metadata-length data)) blocks)))

(defun metadata-header-reader (stream)
  "Returns (values START-POSITION LAST-BLOCK-P TYPE LENGTH)"
  (values (reader-position stream)
          (if (= 0 (read-bit stream)) nil t)
          (read-bits 7 stream)
          (read-bits 24 stream)))

(defun metadata-reader (stream)
  (multiple-value-bind (start-position last-block-p type length)
      (metadata-header-reader stream)
    
    (let* ((mtype (get-metadata-type type))
           (data (make-instance mtype
                                :last-block-p last-block-p
                                :length length
                                :start-position start-position)))
      (metadata-body-reader stream data)
      data)))

(defmethod metadata-body-reader (stream (data padding))
  ;; Read zero padding bytes
  (let ((chunk (make-array (list (metadata-length data))
			   :element-type 'ub8)))
    (read-octet-vector chunk stream)
    ;; Do sanity checks
    (if (find-if-not #'zerop  chunk)
        (error 'flac-bad-metadata
               :message "Padding bytes is not zero"
               :metadata data))))

(defmethod metadata-body-reader (stream (data vorbis-comment))
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (flet ((read-comment-string (stream)
	  (let ((buffer (make-array (list #+x86_64 (bitreader.le-fixnum:read-bits 32 stream)
					  #-x86_64 (bitreader.le-bignum:read-bits 32 stream))
					   :element-type '(unsigned-byte 8))))
	    (read-octet-vector buffer stream)
	    (babel:octets-to-string buffer))))
    
    (setf (vorbis-vendor-comment data)
	  (read-comment-string stream))

    (let ((comments-num #+x86_64 (bitreader.le-fixnum:read-bits 32 stream)
			#-x86_64 (bitreader.le-bignum:read-bits 32 stream)))
      
      (setf (vorbis-user-comments data)
	    (loop for i below comments-num collect
		  (read-comment-string stream))))))
			    

(defmethod metadata-body-reader (stream (data seektable))
  (flet ((read-seekpoint (stream)
			 (let ((samplenum (bitreader.be-bignum:read-bits 64 stream)))
			   (if (/= (the (unsigned-byte 64) samplenum) +seekpoint-placeholder+)
			       (let ((offset (bitreader.be-bignum:read-bits 64 stream))
				     (samples-in-frame (read-bits 16 stream)))
				 (make-seekpoint :samplenum samplenum
						 :offset offset
						 :samples-in-frame samples-in-frame))))))
    (multiple-value-bind (seekpoints-num remainder)
	(floor (the (unsigned-byte 24) (metadata-length data)) 18)
      (if (/= remainder 0) (error 'flac-bad-metadata
				  :message "Bad seektable"
				  :metadata data))
      (setf (seektable-seekpoints data)
	    (loop for i below seekpoints-num collect
		  (read-seekpoint stream))))))

(defmethod metadata-body-reader (stream (data streaminfo))
  (setf (streaminfo-minblocksize data) (read-bits 16 stream)
	(streaminfo-maxblocksize data) (read-bits 16 stream))
	      
  (setf (streaminfo-minframesize data) (read-bits 24 stream)
	(streaminfo-maxframesize data) (read-bits 24 stream))

  (setf (streaminfo-samplerate data) (read-bits 20 stream)
	(streaminfo-channels data) (the (integer 1 8)
				     (1+ (read-bits 3 stream)))
	(streaminfo-bitspersample data) (the non-negative-fixnum
					  (1+ (read-bits 5 stream)))
	(streaminfo-totalsamples data) #+x86_64 (read-bits 36 stream)
	                               #-x86_64 (bitreader.be-bignum:read-bits 36 stream))
  
  (let ((md5 (make-array (list 16) :element-type 'u8)))
    (read-octet-vector md5 stream)
    (setf (streaminfo-md5 data) md5))
  data)

(defun read-cuesheet-string (stream length)
  (let ((buffer (make-array (list length) :element-type 'u8)))
    (read-octet-vector buffer stream)
    (let ((pos (position 0 buffer)))
      (setq buffer
	    (if pos (subseq buffer 0 pos) buffer)))
    (babel:octets-to-string buffer)))

(defun read-cuesheet-index (stream)
  (let ((index (make-cuesheet-index)))
    (setf (cuesheet-index-offset index) (bitreader.be-bignum:read-bits 64 stream))
    (setf (cuesheet-index-number index) (read-octet stream))

    (let ((reserved (bitreader.be-bignum:read-bits #.(* 3 8) stream)))
      (if (/= 0 reserved) (error 'flac-bad-metadata
				 :message "Bad cuesheet index"
				 :metadata *data*)))
    index))

(defun read-cuesheet-track (stream)
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((track (make-cuesheet-track)))
    (setf (cuesheet-track-offset track) (bitreader.be-bignum:read-bits 64 stream))
    (setf (cuesheet-track-number track) (read-octet stream))
    (setf (cuesheet-track-isrc track) (read-cuesheet-string stream 12))
    (setf (cuesheet-track-type track) (if (= 0 (read-bit stream))
					  :audio
					:non-audio))
    (setf (cuesheet-track-pre-emphasis track)
	  (if (= 0 (read-bit stream)) :no-pre-emphasis :pre-emphasis))
    
    (let ((reserved (bitreader.be-bignum:read-bits #.(+ 6 (* 8 13)) stream)))
      (if (/= 0 reserved) (error 'flac-bad-metadata
				 :message "Bad cuesheet track"
				 :metadata *data*)))
    
    (let ((number-of-indices (read-octet stream)))
      (setf (cuesheet-track-indices track)
	    (loop for track below number-of-indices collect
		  (read-cuesheet-index stream))))

    track))

(defmethod metadata-body-reader (stream (data cuesheet))
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((*data* data))
    (setf (cuesheet-catalog-id data) (read-cuesheet-string stream 128))
    (setf (cuesheet-lead-in data) (bitreader.be-bignum:read-bits 64 stream))
    (setf (cuesheet-cdp data) (if (= 1 (read-bit stream)) t nil))
    
    (let ((reserved (bitreader.be-bignum:read-bits #.(+ 7 (* 8 258)) stream)))
      (if (/= 0 reserved) (error 'flac-bad-metadata
				 :message "Bad cuesheet"
				 :metadata data)))
    
    (let ((number-of-tracks (read-octet stream)))
      (setf (cuesheet-tracks data)
	    (loop for track below number-of-tracks collect
		  (read-cuesheet-track stream))))
  data))

(defmethod metadata-body-reader (stream (data metadata-header))
  (error 'flac-bad-metadata
         :message "Unknown metadata block"
         :metadata data))

(defun metadata-find-seektable (metadata)
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (find-if #'(lambda (x) (typep x 'seektable)) metadata))

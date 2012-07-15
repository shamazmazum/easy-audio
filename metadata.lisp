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

(in-package :cl-flac)

(declaim (optimize (speed 3)))

(defun metadata-summar-length (blocks)
  (declare (type list blocks))
  (reduce #'+ (mapcar #'(lambda (data) (metadata-length data)) blocks)))

(defun metadata-header-reader (stream header)
  (with-slots (last-block-p type length) header
	      (setf last-block-p (if (= 0 (read-bit stream)) nil t)
		    type (read-bits 7 stream)
		    length (read-bits 24 stream)))
  header)

(defun metadata-reader (stream)
  (let ((data (make-instance 'metadata-header)))
    (metadata-header-reader stream data)
    
    (handler-case
     (let ((mtype (get-metadata-type (slot-value data 'type))))
       (change-class data mtype))
     (flac-bad-metadata-type () ()))

    (metadata-body-reader stream data)
    data))

(defmethod metadata-body-reader (stream (data padding))
  (declare (ignore stream))
  ;; Read length bytes
  (call-next-method)
  ;; Sanity check
  (if (find-if-not #'zerop (the (simple-array u8)
			     (slot-value data 'rawdata)))
      (error 'flac-bad-metadata
	     :message "Padding bytes is not zero"
	     :metadata data)))

(defmethod metadata-body-reader (stream (data vorbis-comment))
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (flet ((read-comment-string (stream)
	  (let ((buffer (make-array (list #+x86_64 (read-bits-le 32 stream)
					  #-x86_64 (read-bits-le-bignum 32 stream))
					   :element-type '(unsigned-byte 8))))
	    (read-octet-vector buffer stream)
	    (babel:octets-to-string buffer))))
    
    (setf (vorbis-vendor-comment data)
	  (read-comment-string stream))

    (let ((comments-num #+x86_64 (read-bits-le 32 stream)
			#-x86_64 (read-bits-le-bignum 32 stream)))
      
      (setf (vorbis-user-comments data)
	    (loop for i below comments-num collect
		  (read-comment-string stream))))))
			    

(defmethod metadata-body-reader (stream (data seektable))
  (flet ((read-seekpoint (stream)
			 (let ((samplenum (read-bits-bignum 64 stream)))
			   (if (/= (the (unsigned-byte 64) samplenum) +seekpoint-placeholder+)
			       (let ((offset (read-bits-bignum 64 stream))
				     (samples-in-frame (read-bits 16 stream)))
				 (make-seekpoint :samplenum samplenum
						 :offset offset
						 :samples-in-frame samples-in-frame))))))
    (multiple-value-bind (seekpoints-num remainder)
	(floor (the (unsigned-byte 24) (metadata-length data)) 18)
      (if (/= remainder 0) (error 'flac-bad-metadata
				  :message "Bad seektable"
				  :bits-to-read (metadata-length data)
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
	                               #-x86_64 (read-bits-bignum 36 stream))
  
  (let ((md5 (make-array (list 16) :element-type 'u8)))
    (read-octet-vector md5 stream)
    (setf (streaminfo-md5 data) md5))
  data)

(defmethod metadata-body-reader (stream (data metadata-header))
  (let ((chunk (make-array (list (slot-value data 'length))
			   :element-type 'u8)))
    (read-octet-vector chunk stream)
    (setf (slot-value data 'rawdata) chunk))) ; For debugging

(defun metadata-find-seektable (metadata)
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (find-if #'(lambda (x) (typep x 'seektable)) metadata))

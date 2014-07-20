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

;; For signaling errors from functions called from body-readers
;; (they usual requires just a stream, so no need pass data object as argument)
(easy-audio-early:defvar-unbound *data*
    "METADATA-BODY-READER bounds this var to metadata block
     it is reading at the moment")

(defun metadata-summar-length (blocks)
  "Returns length of metadata blocks given in bytes"
  (declare (type list blocks))
  (reduce #'+ (mapcar #'(lambda (data) (metadata-length data)) blocks)))

(defun metadata-header-reader (stream)
  "Returns (values START-POSITION LAST-BLOCK-P TYPE LENGTH)"
  (values (reader-position stream)
          (/= 0 (read-bit stream))
          (read-bits 7 stream)
          (read-bits 24 stream)))

(defun metadata-reader (stream)
  "Read one metadata block from STREAM"
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
			   :element-type '(ub 8))))
    (read-octet-vector chunk stream)
    ;; Do sanity checks
    (if (notevery #'zerop  chunk)
        (error 'flac-bad-metadata
               :message "Padding bytes is not zero"
               :metadata data))))

(defmethod metadata-body-reader (stream (data vorbis-comment))
  (flet ((read-comment-string (stream)
	  (let ((buffer (make-array (list (read-bits 32 stream :endianness :little))
					   :element-type '(unsigned-byte 8))))
	    (read-octet-vector buffer stream)
	    (babel:octets-to-string buffer))))
    
    (setf (vorbis-vendor-comment data)
	  (read-comment-string stream))

    (let ((comments-num (read-bits 32 stream :endianness :little)))
      
      (setf (vorbis-user-comments data)
	    (loop for i below comments-num collect
		  (read-comment-string stream))))))
			    

(defmethod metadata-body-reader (stream (data seektable))
  (flet ((read-seekpoint (stream)
			 (let ((samplenum (read-bits 64 stream)))
			   (if (/= samplenum +seekpoint-placeholder+)
			       (let ((offset (read-bits 64 stream))
				     (samples-in-frame (read-bits 16 stream)))
				 (make-seekpoint :samplenum samplenum
						 :offset offset
						 :samples-in-frame samples-in-frame))))))
    (multiple-value-bind (seekpoints-num remainder)
	(floor (metadata-length data) 18)
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
	(streaminfo-channels data) (1+ (read-bits 3 stream))
	(streaminfo-bitspersample data) (1+ (read-bits 5 stream))
	(streaminfo-totalsamples data) (read-bits 36 stream))

  (let ((md5 (make-array (list 16) :element-type '(ub 8))))
    (read-octet-vector md5 stream)
    (setf (streaminfo-md5 data) md5))
  data)

(defun read-cuesheet-string (stream length)
  (let ((buffer (make-array (list length) :element-type '(ub 8))))
    (read-octet-vector buffer stream)
    (let ((pos (position 0 buffer)))
      (setq buffer
	    (if pos (subseq buffer 0 pos) buffer)))
    (babel:octets-to-string buffer)))

(defun read-cuesheet-index (stream)
  (let ((index (make-cuesheet-index)))
    (setf (cuesheet-index-offset index) (read-bits 64 stream))
    (setf (cuesheet-index-number index) (read-octet stream))

    (let ((reserved (read-bits #.(* 3 8) stream)))
      (if (/= 0 reserved) (error 'flac-bad-metadata
				 :message "Bad cuesheet index"
				 :metadata *data*)))
    index))

(defun read-cuesheet-track (stream)
  (let ((track (make-cuesheet-track)))
    (setf (cuesheet-track-offset track) (read-bits 64 stream))
    (setf (cuesheet-track-number track) (read-octet stream))
    (setf (cuesheet-track-isrc track) (read-cuesheet-string stream 12))
    (setf (cuesheet-track-type track) (if (= 0 (read-bit stream))
					  :audio
					:non-audio))
    (setf (cuesheet-track-pre-emphasis track)
	  (if (= 0 (read-bit stream)) :no-pre-emphasis :pre-emphasis))
    
    (let ((reserved (read-bits #.(+ 6 (* 8 13)) stream)))
      (if (/= 0 reserved) (error 'flac-bad-metadata
				 :message "Bad cuesheet track"
				 :metadata *data*)))
    
    (let ((number-of-indices (read-octet stream)))
      (setf (cuesheet-track-indices track)
	    (loop for track below number-of-indices collect
		  (read-cuesheet-index stream))))

    track))

(defmethod metadata-body-reader (stream (data cuesheet))
  (let ((*data* data))
    (setf (cuesheet-catalog-id data) (read-cuesheet-string stream 128))
    (setf (cuesheet-lead-in data) (read-bits 64 stream))
    (setf (cuesheet-cdp data) (if (= 1 (read-bit stream)) t nil))
    
    (let ((reserved (read-bits #.(+ 7 (* 8 258)) stream)))
      (if (/= 0 reserved) (error 'flac-bad-metadata
				 :message "Bad cuesheet"
				 :metadata data)))
    
    (let ((number-of-tracks (read-octet stream)))
      (setf (cuesheet-tracks data)
	    (loop for track below number-of-tracks collect
		  (read-cuesheet-track stream))))
  data))

(defmethod metadata-body-reader (stream (data picture))
  (let ((picture-type (nth (read-bits 32 stream) +picture-types+)))
    (if (not (typep picture-type 'picture-type-id))
        (error 'flac-bad-metadata
               :message "Bad picture type"
               :metadata data))
    (setf (picture-type data) picture-type))
  
  (let* ((mime-type-len (read-bits 32 stream))
         (mime-type-seq (make-array (list mime-type-len)
                                    :element-type '(unsigned-byte 8))))
    (read-octet-vector mime-type-seq stream)
    (if (notevery #'(lambda (char) (and (>= char #x20)
                                        (<= char #x7e)))
                  mime-type-seq)
        (error 'flac-bad-metadata
               :message "MIME type must be an ASCII string"
               :metadata data))
    (setf (picture-mime-type data) (babel:octets-to-string mime-type-seq)))

  (let* ((description-len (read-bits 32 stream))
         (description-seq (make-array (list description-len)
                                      :element-type '(unsigned-byte 8))))
    (read-octet-vector description-seq stream)
    (setf (picture-description data) (babel:octets-to-string description-seq)))

  (setf (picture-width data) (read-bits 32 stream)
        (picture-height data) (read-bits 32 stream)
        (picture-depth data) (read-bits 32 stream)
        (picture-color-num data) (read-bits 32 stream))

  (let* ((picture-len (read-bits 32 stream))
         (picture-seq (make-array (list picture-len)
                                  :element-type '(unsigned-byte 8))))
    ;; FIXME: artifical sanity check: Picture can be less than 10 MiB
    (if (> picture-len #.(ash 10 20))
        (error 'flac-bad-metadata
               :message (format nil "It's strange, but picture size is too long (~D bytes)" picture-len)
               :metadata data))
    (setf (picture-picture data) (read-octet-vector picture-seq stream))))

(defmethod metadata-body-reader (stream (data metadata-header))
  (error 'flac-bad-metadata
         :message "Unknown metadata block"
         :metadata data))

(defun metadata-find-seektable (metadata)
  (find-if #'(lambda (x) (typep x 'seektable)) metadata))

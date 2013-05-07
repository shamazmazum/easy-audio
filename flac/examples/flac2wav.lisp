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

(in-package :flac-examples)

(defparameter +wav-chunk-id+ (babel:string-to-octets "RIFF"))
(defparameter +wav-subchunk1-id+ (babel:string-to-octets "fmt "))
(defparameter +wav-subchunk2-id+ (babel:string-to-octets "data"))
(defparameter +wave+ (babel:string-to-octets "WAVE"))
(defconstant +header-size+ (* 8 44))

(defun integer-to-array (val array)
  (declare (optimize (speed 3))
	   (type (unsigned-byte 32) val)
	   (type (simple-array (unsigned-byte 8)) array))
  (loop for i below (length array)
	for pos from 0 by 8 do
	(setf (aref array i)
	      (ldb (byte 8 pos) val)))
  array)

(defun mixchannels (out buffers)
  (declare (type list buffers)
	   (type (simple-array (signed-byte 32)) out)
	   (optimize (speed 3) (safety 0)))
  (let ((offset (length buffers))
	(size (length (the (simple-array (signed-byte 32))
			(nth 0 buffers))))
	(idx 0))
    (declare (type fixnum offset size idx))
    (dotimes (i size)
      (dotimes (j offset)
	(declare (type fixnum i j))
	(setf (aref out (+ idx j))
	      (aref (the (simple-array (signed-byte 32))
		      (nth j buffers)) i)))
      (incf idx offset))
    out))

;; Works only for 8 or 16 bps
(defun flac2wav (flac-name wav-name)
  "Decodes flac to wav. Works only for 8 or 16 bps,
   fixed block size and if total samples in stream is known"
  (with-open-flac (blocks stream
			  (open flac-name :element-type '(unsigned-byte 8)))

    (let* ((streaminfo (the streaminfo (first blocks)))
	   (buf2 (make-array 2 :element-type '(unsigned-byte 8)))
	   (buf4 (make-array 4 :element-type '(unsigned-byte 8)))
	   
	   (minblocksize (streaminfo-minblocksize streaminfo))
	   (maxblocksize (streaminfo-maxblocksize streaminfo))
	   (totalsamples (streaminfo-totalsamples streaminfo))
	   (blocksize minblocksize)
	   
	   (bps (streaminfo-bitspersample streaminfo))
	   (channels (streaminfo-channels streaminfo))
	   (samplerate (streaminfo-samplerate streaminfo)))
      
      (if (= 0 totalsamples)
	  (error "Number of total samples is unknown"))
      (if (/= minblocksize maxblocksize)
	  (error "Block size must be fixed"))
      
      (if (not (or (= 8 bps)
		   (= 16 bps)))
	  (error "Bps must be 16 or 8"))
      
      (with-open-file (out-stream wav-name
				  :direction :output
				  :if-exists :supersede
				  :if-does-not-exist :create
				  :element-type '(unsigned-byte 8))
		      ;; Fill headers
		      (write-sequence +wav-chunk-id+ out-stream)
		      (let ((size
			     (ash
			      (* bps channels totalsamples) -3)))
			
			(write-sequence (integer-to-array (+ 36 size) buf4) out-stream)
			(write-sequence +wave+ out-stream)
			
			;; Subchunk 1
			(write-sequence +wav-subchunk1-id+ out-stream)
			(write-sequence (integer-to-array 16 buf4) out-stream)
			(write-byte 1 out-stream)
			(write-byte 0 out-stream)
			
			(write-sequence (integer-to-array
					 channels buf2) out-stream)
			
			(write-sequence (integer-to-array
					 samplerate buf4) out-stream)
			
			(write-sequence (integer-to-array
					 (ash
					  (* samplerate channels bps) -3)
					 buf4) out-stream)
			
			(write-sequence (integer-to-array
					 (ash
					  (* channels bps) -3)
					 buf2) out-stream)
			
			(write-sequence (integer-to-array
					 bps buf2) out-stream)
			
			;; Subchunk 2
			(write-sequence +wav-subchunk2-id+ out-stream)
			(write-sequence (integer-to-array size
							  buf4) out-stream)))
      
      (with-open-file (out-stream wav-name
				  :direction :output
				  :if-exists :append
				  :element-type (list 'signed-byte bps))
		      (file-position out-stream
				     (/ (ash 44 3) bps))
		      
		      (let ((buf (make-array (* blocksize channels)
					     :element-type '(signed-byte 32))))
			
			(loop for i below totalsamples
			      by blocksize do
			      (write-sequence (mixchannels buf (frame-decode (frame-reader stream streaminfo)))
					      out-stream)))))))

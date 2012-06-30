(in-package :flac-examples)

(defparameter +wav-chunk-id+ (babel:string-to-octets "RIFF"))
(defparameter +wav-subchunk1-id+ (babel:string-to-octets "fmt "))
(defparameter +wav-subchunk2-id+ (babel:string-to-octets "data"))
(defparameter +wave+ (babel:string-to-octets "WAVE"))
(defconstant +header-size+ (* 8 44))

(declaim (inline integer-to-array))
(defun integer-to-array (val array &optional (size 8))
  (loop for i below (length array)
	for pos from 0 by size do
	(setf (aref array i)
	      (ldb (byte size pos) val)))
  array)

(defun mixchannels (out buffers)
  (let ((offset (length buffers))
	(size (length (nth 0 buffers))))
    (loop for i below size
	  for idx from 0 by offset do 
	  (loop for j below offset do
		(setf (aref out (+ idx j))
		      (aref (nth j buffers) i)))))
  out)

;; Works only for 8 or 16 bps
(defun flac2wav (flac-name wav-name)
  (multiple-value-bind (blocks stream)
      (flac:open-flac flac-name)
    (let ((streaminfo (first blocks))
	  (buf2 (make-array 2 :element-type '(unsigned-byte 8)))
	  (buf4 (make-array 4 :element-type '(unsigned-byte 8))))
      
      (if (= 0 (streaminfo-totalsamples streaminfo))
	  (error "Number of total samples is unknown"))
      (if (/= (streaminfo-minblocksize streaminfo)
	      (streaminfo-maxblocksize streaminfo))
	  (error "Block size must be fixed"))
      (with-open-file (out-stream wav-name
				  :direction :output
				  :if-exists :supersede
				  :if-does-not-exist :create
				  :element-type '(unsigned-byte 8))
		      ;; Fill headers
		      (write-sequence +wav-chunk-id+ out-stream)
		      (multiple-value-bind (size remainder)
			  (floor
			   (* (streaminfo-bitspersample streaminfo)
			      (streaminfo-channels streaminfo)
			      (streaminfo-totalsamples streaminfo)) 8)
			
			(if (/= remainder 0) (error "Bits per samples is not power of two"))
			(write-sequence (integer-to-array (+ 36 size) buf4) out-stream)
			(write-sequence +wave+ out-stream)
			
			;; Subchunk 1
			(write-sequence +wav-subchunk1-id+ out-stream)
			(write-sequence (integer-to-array 16 buf4) out-stream)
			(write-byte 1 out-stream)
			(write-byte 0 out-stream)

			(write-sequence (integer-to-array
					 (streaminfo-channels streaminfo)
					 buf2) out-stream)
			
			(write-sequence (integer-to-array
					 (streaminfo-samplerate streaminfo)
					 buf4) out-stream)

			(write-sequence (integer-to-array
					 (ash
					  (* (streaminfo-samplerate streaminfo)
					     (streaminfo-channels streaminfo)
					     (streaminfo-bitspersample streaminfo)) -3)
					 buf4) out-stream)

			(write-sequence (integer-to-array
					 (ash
					  (* (streaminfo-channels streaminfo)
					     (streaminfo-bitspersample streaminfo)) -3)
					 buf2) out-stream)

			(write-sequence (integer-to-array
					 (streaminfo-bitspersample streaminfo)
					 buf2) out-stream)

			;; Subchunk 2
			(write-sequence +wav-subchunk2-id+ out-stream)
			(write-sequence (integer-to-array size
					 buf4) out-stream)))
      
      (with-open-file (out-stream wav-name
				  :direction :output
				  :if-exists :append
				  :element-type (list 'signed-byte (streaminfo-bitspersample streaminfo)))
		      (file-position out-stream
				     (/ (ash 44 3) (streaminfo-bitspersample streaminfo)))

		      (let ((buf (make-array (* (streaminfo-minblocksize streaminfo)
						(streaminfo-channels streaminfo))
					     :element-type '(signed-byte 32))))

			(loop for i below (streaminfo-totalsamples streaminfo)
			      by (streaminfo-minblocksize streaminfo) do
			      (write-sequence (mixchannels buf (frame-decode (frame-reader stream streaminfo)))
					      out-stream)))))
    (close stream)))

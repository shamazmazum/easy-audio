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
  "Decodes flac to wav. Works only for 8 or 16 bps,
   fixed block size and if total samples in stream is known"
  (multiple-value-bind (blocks stream)
      (flac:open-flac flac-name)
    (let* ((streaminfo (first blocks))
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
					      out-stream)))))
    (close stream)))

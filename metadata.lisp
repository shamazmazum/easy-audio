(in-package :cl-flac)

(defun metadata-header-reader (stream header)
  (with-slots (last-block-p type length) header
	      (let ((chunk (read-to-integer stream 4)))
		
		(setf last-block-p (ldb (byte 1 #.(- 32 1)) chunk))
		(setf type (ldb (byte 7 #.(- 32 1 7)) chunk))
		(setf length (ldb (byte 24 #.(- 32 1 7 24)) chunk))))
  header)

(defun metadata-reader (stream)
  (let ((data (make-instance 'metadata-header)))
    (metadata-header-reader stream data)
    
    (handler-case
     (let ((mtype (get-metadata-type (slot-value data 'type))))
       (change-class data mtype))
     (error () ())) ; Надо обрабатывать более специфичную ошибку из get-reader

    (metadata-body-reader stream data)
    data))

(defmethod metadata-body-reader (stream (data padding))
  (declare (ignore stream))
  ;; Read length bytes
  (call-next-method)
  ;; Sanity check
  (if (find-if-not #'zerop (slot-value data 'rawdata))
      (error "Padding bytes is not zero")))

(defmethod metadata-body-reader (stream (data streaminfo))
  (let ((chunk (read-to-integer stream 18)))
    (with-slots (minblocksize maxblocksize) data
		(setf minblocksize (ldb (byte 16 #.(- 144 16)) chunk))
		(setf maxblocksize (ldb (byte 16 #.(- 144 16 16)) chunk)))

  (with-slots (minframesize maxframesize) data
		(setf minframesize (ldb (byte 16 #.(- 144 16 16 24)) chunk))
		(setf maxframesize (ldb (byte 16 #.(- 144 16 16 24 24)) chunk)))

  (with-slots (samplerate channels-1 bitspersample-1 totalsamples) data
	      (let ((sr-pos (byte 20 #.(- 64 20)))
		    (ch-pos (byte 3 #.(- 64 20 3)))
		    (bps-pos (byte 5 #.(- 64 20 3 5)))
		    (tot-pos (byte 36 #.(- 64 20 3 5 36))))
		(setf samplerate (ldb sr-pos chunk)
		      channels-1 (ldb ch-pos chunk)
		      bitspersample-1 (ldb bps-pos chunk)
		      totalsamples (ldb tot-pos chunk)))))
  
  (let ((md5 (make-array 16 :element-type 'u8)))
    (read-sequence md5 stream)
    (setf (streaminfo-md5 data) md5))
  data)

(defmethod metadata-body-reader (stream (data metadata-header))
  (let ((chunk (make-array (slot-value data 'length) :element-type 'u8)))
    (read-sequence chunk stream)
    (setf (slot-value data 'rawdata) chunk))) ; For debugging

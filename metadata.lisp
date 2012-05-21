(in-package :cl-flac)

(defun metadata-header-reader (stream header)
  (with-slots (last-block-p type length) header
	      (let ((byte (read-byte stream))
		    (last-pos (byte 1 0))
		    (type-pos (byte 7 1)))
		(setf last-block-p (ldb last-pos byte))
		(setf type (ldb type-pos byte)))
	      (setf length (read-to-integer stream 3)))
  header)

(defun metadata-reader (stream)
  (let ((data (make-instance 'metadata-header)))
    (metadata-header-reader stream data)
    
    (handler-case
     (let ((reader (get-reader (slot-value data 'type))))
       (funcall reader stream data))
     (error () ; Надо обрабатывать более специфичную ошибку из get-reader
	    (let ((array (make-array (slot-value data 'length) :element-type 'u8)))
	      (read-sequence array stream)
	      (setf (slot-value data 'rawdata) array)))) ; Необязательно к заполнению. Для отладки
    data))

(defun streaminfo-reader (stream data)
  (if (not (typep data 'streaminfo)) (change-class data 'streaminfo))
  (with-slots (minblocksize maxblocksize) data
		(setf minblocksize (read-to-integer stream 2))
		(setf maxblocksize (read-to-integer stream 2)))

  (with-slots (minframesize maxframesize) data
		(setf minframesize (read-to-integer stream 3))
		(setf maxframesize (read-to-integer stream 3)))

  (with-slots (samplerate channels-1 bitspersample-1 totalsamples) data
  (let ((chunk (read-to-integer stream 8))
	(sr-pos (byte 20 #.(- 64 20)))
	(ch-pos (byte 3 #.(- 64 20 3)))
	(bps-pos (byte 5 #.(- 64 20 3 5)))
	(tot-pos (byte 36 #.(- 64 20 3 5 36))))
    (setf samplerate (ldb sr-pos chunk)
	  channels-1 (ldb ch-pos chunk)
	  bitspersample-1 (ldb bps-pos chunk)
	  totalsamples (ldb tot-pos chunk))))
  
  (let ((md5 (make-array 16 :element-type 'u8)))
    (read-sequence md5 stream)
    (setf (slot-value data 'md5) md5))
  data)

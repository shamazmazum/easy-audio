(in-package :cl-flac)

(defun parse-flac (name)
  (with-open-file (stream name :element-type '(unsigned-byte 8))

		  ;; Checking if stream is flac stream
		  (let ((flac-header-array (make-array 4 :element-type 'u8)))
		    (read-sequence flac-header-array stream)
		    (if (not (string= "fLaC" (babel:octets-to-string flac-header-array))) (error "Stream is not flac stream")))

	  
		    (let ((metadata-blocks
			   (loop for metadata-block = (metadata-reader stream)
				 collect metadata-block
				 while (= 0 (slot-value metadata-block 'last-block-p)))))
;;		      metadata-blocks ; Store it to some object in future
		    (frame-reader stream (first metadata-blocks)))))

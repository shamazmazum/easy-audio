(in-package :cl-flac)

(defun open-flac (stream)
  ;; Checking if stream is flac stream
  (let ((flac-header-array (make-array 4 :element-type 'u8)))
    (read-sequence flac-header-array stream)
    (if (not (string= "fLaC" (babel:octets-to-string flac-header-array))) (error "Stream is not flac stream")))
  
  (let* ((bitreader (make-reader :stream stream))
	 (metadata-blocks
	  (loop for metadata-block =
		;; I am not master of conditions handling, so
		;; the following code is ugly
		(restart-case
		 (metadata-reader bitreader)
		 (skip-malformed-metadata (c)
					  (read-bits
					   (flac-bits-to-read c)
					   bitreader)
					  (metadata-last-block-p
					   (flac-metadata c))))
		
		for read-farther =
		(typecase metadata-block
		  (metadata-header
		   (not (slot-value metadata-block 'last-block-p)))
		  (t (not metadata-block)))
		
		when (typep metadata-block 'metadata-header)
		collect metadata-block
		while read-farther)))
      
    (values
     metadata-blocks
     bitreader)))

(defmacro with-open-flac ((blocks reader stream) &body body)
  `(multiple-value-bind (,blocks ,reader)
       (open-flac ,stream)
     (unwind-protect
	 (progn ,@body)
       (close-reader ,reader))))

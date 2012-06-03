(in-package :cl-flac)

(defmethod subframe-decode ((subframe subframe-constant) frame)
  (declare (type frame frame))
  (make-array (frame-block-size frame)
	      :element-type (list 'unsigned-byte (frame-sample-size frame))
	      :initial-element (subframe-constant-value subframe)))

(defmethod subframe-decode ((subframe subframe-verbatim) frame)
  (declare (type frame frame))
  (with-slots (block-size sample-size) frame
	      (let ((decoded-buf
		     (make-array block-size
				 :element-type (list 'unsigned-byte sample-size))))

		(integer-to-array (subframe-verbatim-buffer subframe) decoded-buf sample-size))))

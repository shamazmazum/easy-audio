(in-package :cl-flac)

(defmethod subframe-decode ((subframe subframe-constant) frame)
  (declare (type frame frame))
  (make-array (frame-block-size frame)
	      :element-type (list 'unsigned-byte (frame-sample-size))
	      :initial-element (subframe-constant-value subframe)))

(defmethod subframe-decode ((subframe subframe-verbatim) frame)
  (declare (ignore frame))
  (subframe-verbatim-buffer subframe))

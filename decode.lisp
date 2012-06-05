(in-package :cl-flac)

(defmethod subframe-decode ((subframe subframe-constant) frame)
  (declare (type frame frame))
  (make-array (frame-block-size frame)
	      ;; FIXME: sample is signed in original libFLAC
	      :element-type (list 'signed-byte (frame-sample-size frame))
	      :initial-element (subframe-constant-value subframe)))

(defmethod subframe-decode ((subframe subframe-verbatim) frame)
  (declare (ignore frame))
  (subframe-verbatim-buffer subframe))

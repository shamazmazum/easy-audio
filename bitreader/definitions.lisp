(in-package :easy-audio.bitreader)

(declaim (type positive-fixnum *buffer-size*))
(defparameter *buffer-size* 4096)

(define-condition bitreader-eof (error)
  ((bitreader :initarg :bitreader
              :reader bitreader-eof-bitreader)))

(defstruct reader
  (ibit      0 :type bit-counter)
  (ibyte     0 :type non-negative-fixnum)
  (end       0 :type non-negative-fixnum)
  (buffer    (make-array *buffer-size*
		         :element-type '(ub 8))
	       :type (sa-ub 8))
  (fill-buffer-fun
   #'read-buffer-from-stream
               :type function)
  #+easy-audio-check-crc
  (crc       0 :type unsigned-byte)
  #+easy-audio-check-crc
  (crc-start 0 :type non-negative-fixnum)
  #+easy-audio-check-crc
  (crc-fun #'(lambda (array accum &key start end)
               (declare (ignore array accum start end))
               0)
               :type function)
  stream)

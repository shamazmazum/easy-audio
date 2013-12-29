(in-package :easy-audio.utils)

(defun integer-to-array (val array)
  (loop for i below (length array)
	for pos from 0 by 8 do
	(setf (aref array i)
	      (ldb (byte 8 pos) val)))
  array)

(defun integer-to-array-be (val array)
  (let* ((len (length array))
         (len-bits (ash len 3)))
    (loop for i below len
          for pos from 0 by 8 do
          (setf (aref array i)
                (ldb (byte 8 (- len-bits pos 8)) val))))
  array)

(defun mixchannels (out buffers)
  "Maps a list of buffers (each one for each channel) into
   one buffer writing the first sample of the first channel when the
   first sample of second channel and so on until final channel is reached.
   When process repeats for second sample of each channel until all data is
   written"
  (declare (type list buffers)
	   (type (simple-array (signed-byte 32)) out)
	   (optimize #+easy-audio-unsafe-code
                     (safety 0) (speed 3)))
  (let ((offset (length buffers))
	(size (length (the (simple-array (signed-byte 32))
			(nth 0 buffers))))
	(idx 0))
    (declare (type fixnum offset size idx))
    (dotimes (i size)
      (dotimes (j offset)
	(declare (type fixnum i j))
	(setf (aref out (+ idx j))
	      (aref (the (simple-array (signed-byte 32))
		      (nth j buffers)) i)))
      (incf idx offset))
    out))

(defun write-pcm-wav-header (out-stream &key samplerate channels bps totalsamples)
  "Writes header of uncompressed wav into stream"
  (let ((size (ash (* bps channels totalsamples) -3))
        (buf2 (make-array 2 :element-type '(unsigned-byte 8)))
        (buf4 (make-array 4 :element-type '(unsigned-byte 8))))
    
    (write-sequence (integer-to-array-be wav:+wav-id+ buf4) out-stream)
    (write-sequence (integer-to-array (+ 36 size) buf4) out-stream)
    (write-sequence (integer-to-array-be wav:+wav-format+ buf4) out-stream)
    
                          
    ;; Subchunk 1
    (write-sequence (integer-to-array-be wav:+format-subchunk+ buf4) out-stream)
    (write-sequence #(16 0 0 0) out-stream)
    (write-sequence (integer-to-array wav:+wave-format-pcm+ buf2) out-stream)
    (write-sequence (integer-to-array channels buf2) out-stream)
    (write-sequence (integer-to-array samplerate buf4) out-stream)
    
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
    (write-sequence (integer-to-array-be wav:+data-subchunk+ buf4) out-stream)
    (write-sequence (integer-to-array size buf4) out-stream)))

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

(defun mixchannels-n (out buffers)
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

(defun mixchannels-2 (output channel1 channel2)
  (declare (optimize #+easy-audio-unsafe-code
                     (safety 0) (speed 3))
           (type (sa-sb 32) output channel1 channel2))
  (loop for i below (length channel1)
        for j from 0 by 2 do
       (setf (aref output j)
             (aref channel1 i)
             (aref output (1+ j))
             (aref channel2 i)))
  output)

(defun mixchannels (out buffers)
  "Maps a list of @c(buffers) (each one for each channel) into one buffer @c(out)
writing sequentially the first sample of the first channel then the first sample of
second channel and so on until final channel is reached. When process repeats for
second sample of each channel until all data is written"
  (declare (optimize (speed 3))
           (type list buffers))
  (case (length buffers)
    (2 (mixchannels-2 out
                      (first buffers)
                      (second buffers)))
    (t (mixchannels-n out buffers))))

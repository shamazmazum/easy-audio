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
    (write-sequence (integer-to-array size buf4) out-stream))
  t)

(defmacro defreader (name (&optional make-form (obj-sym (gensym))) &rest slots)
  "Generate a reader function to read data from bit-reader into
  an arbitrary object with accessor-like interface. NAME is the
  name of such function. The new function will accept two
  arguments: a bit-reader and an optional object to be
  modified. If no object is passed, it will be created with
  MAKE-FORM. You can assign a symbol OBJ-SYM to newly created
  instance. Each slot from SLOTS is a list. It has the
  following syntax:

  (ACCESSOR (:OCTETS n)|(:BITS n)|(:OCTET-VECTOR v) [:ENDIANNESS
            :BIG|:LITTLE] [:FUNCTION FUNC-NAME])

  (ACCESSOR object) must be a 'place' understandable for setf.
  One and only one of BITS, OCTETS or OCTET-VECTOR must be
  supplied. Endianness may be supplied and will be passed to
  low-level bitreader function. if FUNC-NAME is supplied,
  readed value will be passed to this function and then
  assigned to the slot."
  `(defun ,name (reader &optional (,obj-sym ,make-form))
     ,@(loop for slot-spec in slots collect
            (destructuring-bind (accessor (read-how read-how-many) . options)
                slot-spec
              `(setf (,accessor ,obj-sym)
                     ,(let ((function-name
                             (ecase read-how
                               (:octets       'bitreader:read-octets)
                               (:bits         'bitreader:read-bits)
                               (:octet-vector 'bitreader:read-octet-vector)))
                            (endianness (getf options :endianness))
                            (aux-function (getf options :function)))

                           (let ((function-call
                                  `(,function-name
                                    ,read-how-many reader
                                    ,@(if endianness (list :endianness endianness)))))
                             (if aux-function
                                 (list aux-function function-call) function-call))))))
     ,obj-sym))

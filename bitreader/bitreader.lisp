;; Thanks to Love5an, author of trivial-bit-streams
;; Do the same thing trivial-bit-streams does but without CLOS

(in-package :easy-audio.bitreader)

(defparameter *read-with-zeroing* nil
  "Affects some functions (currently only READ-OCTETS, READ-OCTET and
READ-OCTET-VECTORS) making them not only read stuff from input buffer,
but also zero read parts in the buffer. Useful for CRC calculation in
some containers")

(serapeum:-> move-forward (reader &optional bit-counter)
             (values reader &optional))
(declaim (inline move-forward))
(defun move-forward (reader &optional (bits 1))
  "Moves position in READER bit reader in range [0; 8-ibit] BITS.
Maximum value of ibit is 7. Does not check if ibit becomes out of
range."
  (with-accessors
   ((ibit reader-ibit)
    (ibyte reader-ibyte))
      reader
    (let ((new-ibit (+ ibit bits)))
    (cond
      ((< new-ibit 8)
       (setf ibit new-ibit))
      (t
       (setf ibit 0)
       (incf ibyte)))))
  reader)

(serapeum:-> read-buffer-from-stream (reader)
             (values boolean &optional))
(defun read-buffer-from-stream (reader)
  "Read internal buffer from stream"
  (setf (reader-end reader)
        (read-sequence (reader-buffer reader)
                       (reader-stream reader)))
  (not (zerop (reader-end reader))))

(serapeum:-> read-buffer-dummy (reader)
             (values boolean &optional))
(defun read-buffer-dummy (reader)
  "Read internal buffer from stream"
  (declare (ignore reader))
  nil)

(serapeum:-> make-reader-from-stream (stream &rest t)
             (values reader &optional))
(defun make-reader-from-stream (stream &rest args)
  "Make bitreader from stream"
  (apply #'make-reader
         :stream stream
         :fill-buffer-fun #'read-buffer-from-stream
         args))

(serapeum:-> make-reader-from-buffer ((sa-ub 8) &rest t)
             (values reader &optional))
(defun make-reader-from-buffer (buffer &rest args)
  "Make bitreader from buffer"
  (apply #'make-reader
         :buffer buffer
         :end (length buffer)
         :fill-buffer-fun #'read-buffer-dummy
         args))

;; If stream is 300 Mb, there are (ceiling (* 300 10^6) 4096) =
;; 73243 calls to fill-buffer. Not many, but inline it anyway
(serapeum:-> fill-buffer (reader)
             (values boolean &optional))
(declaim (inline fill-buffer))
(defun fill-buffer (reader)
  "Fills internal buffer of READER"
  #+easy-audio-check-crc
  (setf (reader-crc reader)
        (funcall (reader-crc-fun reader)
                 (reader-buffer reader)
                 (reader-crc reader)
                 :start (reader-crc-start reader)
                 :end (reader-end reader))
        (reader-crc-start reader) 0)

  (setf (reader-ibit reader) 0
        (reader-ibyte reader) 0)
  (funcall (reader-fill-buffer-fun reader) reader))

(serapeum:-> ensure-data-available (reader)
             (values &optional))
(declaim (inline ensure-data-available))
(defun ensure-data-available (reader)
  "Checks if READER can be read without calling fill-buffer"
  (if (= (reader-ibyte reader)
         (reader-end reader))
      (unless (fill-buffer reader)
        (error 'bitreader-eof :reader reader)))
  (values))

(serapeum:-> read-bit (reader)
             (values bit &optional))
(defun read-bit (reader)
  "Read a single bit from READER"
  (declare (optimize (speed 3)))
  (ensure-data-available reader)
  (prog1
      (ldb (byte 1 (- 7 (reader-ibit reader)))
	   (aref (reader-buffer reader)
		 (reader-ibyte reader)))
    (move-forward reader)))

(serapeum:-> read-bit-bw (reader)
             (values bit &optional))
(defun read-bit-bw (reader)
  "Read a single bit from READER from the end of the current IBYTE
(WavPack style)."
  (declare (optimize (speed 3)))
  (ensure-data-available reader)
  (prog1
      (ldb (byte 1 (reader-ibit reader))
	   (aref (reader-buffer reader)
		 (reader-ibyte reader)))
    (move-forward reader)))

(serapeum:-> read-octet (reader)
             (values (ub 8) &optional))
(defun read-octet (reader)
  "Reads current octet from reader Ignores ibit"
  (declare (optimize (speed 3)))
  (ensure-data-available reader)
  (prog1
      (aref (reader-buffer reader)
            (reader-ibyte reader))
    (when *read-with-zeroing*
      (setf (aref (reader-buffer reader)
                  (reader-ibyte reader))
            0))
    (incf (reader-ibyte reader))))

;; TODO: Split in two
#-sbcl
(serapeum:-> read-octets ((integer 0 7) reader &key (:endianness symbol))
             (values (unsigned-byte 56) &optional))
(defun read-octets (n reader &key (endianness :big))
  "Reads n octets in integer value"
  (declare (optimize (speed 3)))
  (let ((res 0))
    (declare (type (unsigned-byte 56) res))
    (dotimes (i n)
      (declare (type fixnum i))
      (ensure-data-available reader)
      (let ((octet (aref (reader-buffer reader) (reader-ibyte reader))))
        (setq res
              (if (eq endianness :big)
                  (logior (ash res 8) octet)
                  (logior (ash octet (ash i 3)) res))))
      (when *read-with-zeroing*
        (setf (aref (reader-buffer reader)
                    (reader-ibyte reader))
              0))
      (incf (reader-ibyte reader)))
    res))

(serapeum:-> read-octet-vector ((sa-ub 8) reader)
             (values (sa-ub 8) &optional))
(defun read-octet-vector (array reader)
  ;; Stupid and maybe slow version.
  ;; Why not? I do not use this function often
  (declare (optimize (speed 3)))
  (dotimes (i (length array))
    (ensure-data-available reader)
    (setf (aref array i)
          (aref (reader-buffer reader) (reader-ibyte reader)))
    (when *read-with-zeroing*
      (setf (aref (reader-buffer reader)
                  (reader-ibyte reader))
            0))
    (incf (reader-ibyte reader)))
  array)

(serapeum:-> read-to-byte-alignment (reader)
             (values non-negative-fixnum &optional))
(defun read-to-byte-alignment (reader)
  "Reads from READER to byte alignment. If already READER is already
byte-aligned, returns 0."
  (declare (optimize (speed 3)))
  (let ((ibit  (reader-ibit  reader))
        (ibyte (reader-ibyte reader)))
    (if (zerop ibit) 0
        (prog1
            (logand (1- (ash 1 (- 8 ibit)))
                    (aref (reader-buffer reader) ibyte))
          (setf (reader-ibit  reader) 0)
          (incf (reader-ibyte reader))))))

(serapeum:-> get-reader-position (reader)
             (values non-negative-fixnum &optional))
(declaim (inline get-reader-position))
(defun get-reader-position (reader)
  (let ((stream (reader-stream reader)))
    (if stream
        (+ (the non-negative-fixnum
                (file-position (reader-stream reader)))
           (- (reader-end reader))
           (reader-ibyte reader))
        (reader-ibyte reader))))

(serapeum:-> set-reader-position (reader non-negative-fixnum)
             (values non-negative-fixnum &optional))
(declaim (inline set-reader-position))
(defun set-reader-position (reader pos)
  (let ((stream (reader-stream reader)))
    (cond
      (stream
       (file-position stream pos)
       (fill-buffer reader))
      (t (setf (reader-ibyte reader) pos))))
  (setf (reader-ibit reader) 0)
  pos)

(serapeum:-> reader-position (reader &optional non-negative-fixnum)
             (values non-negative-fixnum &optional))
(defun reader-position (reader &optional pos)
  "Returns or sets number of readed octets.  Similar to file-position
Sets ibit to zero if val is specified"
  (declare (optimize (speed 3)))
  (if pos
      (set-reader-position reader pos)
      (get-reader-position reader)))

(serapeum:-> peek-octet (reader (ub 8))
             (values (ub 8) &optional))
(defun peek-octet (reader octet)
  "Sets input to the first octet found in stream"
  (declare (optimize (speed 3)))
  (setf (reader-ibyte reader)
	(loop for pos = (position octet (reader-buffer reader)
                                  :start (reader-ibyte reader))
              until pos do
              (unless (fill-buffer reader)
                (error 'bitreader-eof :reader reader))
              (setf (reader-ibyte reader) 0)
              finally (return pos)))
  octet)

(serapeum:-> reader-length (reader)
             (values non-negative-integer &optional))
(defun reader-length (reader)
  "Returns length of a stream in octets.

Calls #'length on a buffer reader or #'file-length on a stream reader"
  (declare (optimize (speed 3)))
  (let ((stream (reader-stream reader)))
    (if stream
        (file-length stream)
        (length (reader-buffer reader)))))

;; TODO: Split
#-sbcl
(serapeum:-> read-bits ((integer 0 56) reader &key (:endianness symbol))
             (values (unsigned-byte 56) &optional))
(defun read-bits (bits reader &key (endianness :big))
  "Read any number of bits from reader"
  (declare (optimize (speed 3)))
  (let ((result 0)
        (already-read 0))
    (declare (type (integer 0 56) already-read)
             (type (unsigned-byte 56) result))
    (with-accessors ((ibit reader-ibit)) reader
      (dotimes (i (ceiling (+ bits ibit) 8))
        (ensure-data-available reader)
        (let ((bits-to-add (min bits (- 8 ibit))))
          (declare (type bit-counter bits-to-add))
          (setq result
                (logior result
                        (ash
                         (ldb (byte bits-to-add (- 8 ibit bits-to-add))
                              (aref (reader-buffer reader)
                                    (reader-ibyte reader)))
                         (if (eq endianness :big)
                             (- bits bits-to-add)
                             already-read)))
                bits (- bits bits-to-add)
                already-read (+ already-read bits-to-add))

          (move-forward reader bits-to-add))))
    result))

(serapeum:-> count-zeros (reader)
             (values non-negative-fixnum &optional))
(defun count-zeros (reader)
  "Count number of zeros in the input. It reads the first occcured one
too to copy behaviour of removed FLAC::READ-UNARY-CODED-integer"
  (declare (optimize (speed 3)))
  (let ((res 0))
    (declare (type non-negative-fixnum res))
    (tagbody :count-cycle
       (ensure-data-available reader)
       (let* ((8-ibit (- 8 (reader-ibit reader)))
              (byte (logand (1- (ash 1 8-ibit))
                            (aref (reader-buffer reader)
                                  (reader-ibyte reader)))))

         (cond
           ((/= byte 0)
            (let ((zeros-in-octet (- 8-ibit (integer-length byte))))
              (incf res zeros-in-octet)
              (move-forward reader (1+ zeros-in-octet)))) ; Also "read" the first 1 here
           (t
            (incf res 8-ibit)
            (incf (reader-ibyte reader))
            (setf (reader-ibit reader) 0)
            (go :count-cycle)))))
    res))

#+easy-audio-check-crc
(progn
  (defun init-crc (reader &optional (start 0))
    "Initialize CRC calculation with the value start (0 by default)"
    (setf (reader-crc reader) start
          (reader-crc-start reader)
          (reader-ibyte reader)))

  (defun get-crc (reader)
    "Return calculated CRC"
    (funcall (reader-crc-fun reader)
             (reader-buffer reader)
             (reader-crc reader)
             :start (reader-crc-start reader)
             :end (reader-ibyte reader)))

  (defmacro with-skipping-crc ((reader) &body body)
    "All input operations within this macro will not affect CRC computation.
     Acts as if body forms is being computed in progn"
    (let ((crc-val (gensym))
          (result (gensym)))
      `(let ((,crc-val (get-crc ,reader))
             (,result (progn ,@body)))
         (init-crc ,reader ,crc-val)
         ,result)))

  (defmacro with-crc ((reader) &body body)
    "Execute body with enabled CRC computation and
     return CRC"
    `(progn
       (init-crc ,reader)
       ,@body
       (get-crc ,reader))))

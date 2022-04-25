;; Copyright (c) 2012-2013, Vasily Postnicov
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met: 

;; 1. Redistributions of source code must retain the above copyright notice, this
;;   list of conditions and the following disclaimer. 
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;   this list of conditions and the following disclaimer in the documentation
;;   and/or other materials provided with the distribution. 

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; Thanks to Love5an, author of trivial-bit-streams
;; Do the same thing trivial-bit-streams does but without CLOS

(in-package :easy-audio.bitreader)

(declaim (optimize #+easy-audio-unsafe-code
                   (safety 0) (speed 3)))

(defparameter *buffer-size* 4096)
(defparameter *read-with-zeroing* nil
  "Affects some functions (currently only READ-OCTETS, READ-OCTET
   and READ-OCTET-VECTORS) making them not only read stuff from
   input buffer, but also zero read parts in the buffer. Useful
   for CRC calculation in some containers")

(define-condition bitreader-eof (error)
  ((bitreader :initarg :bitreader
              :reader bitreader-eof-bitreader)))

(defstruct reader
  (ibit      0 :type bit-counter)
  (ibyte     0 :type non-negative-fixnum)
  (end       0 :type non-negative-fixnum)
  (buffer    (make-array (list *buffer-size*)
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
               0) :type function)
  stream)

(declaim (inline move-forward))
(defun move-forward (reader &optional (bits 1))
  "Moves position in READER bit reader in range [0; 8-ibit] BITS.
   Maximum value of ibit is 7. Does not check if ibit becomes
   out of range."
  (declare (type non-negative-fixnum bits))

  (with-accessors
   ((ibit reader-ibit)
    (ibyte reader-ibyte)) reader

    (let ((new-ibit (+ ibit bits)))
    (cond
      ((< new-ibit 8)
       (setf ibit new-ibit))
      (t
       (setf ibit 0)
       (incf ibyte))))))

(defun read-buffer-from-stream (reader)
  "Read internal buffer from stream"
  (setf (reader-end reader)
        (read-sequence (reader-buffer reader)
                       (reader-stream reader)))
  (not (zerop (reader-end reader))))

(defun read-buffer-dummy (reader)
  "Read internal buffer from stream"
  (declare (ignore reader))
  nil)

(defun make-reader-from-stream (stream &rest args)
  "Make bitreader from stream"
  (apply #'make-reader
         :stream stream
         :fill-buffer-fun #'read-buffer-from-stream
         args))

(defun make-reader-from-buffer (buffer &rest args)
  "Make bitreader from buffer"
  (declare (type (sa-ub 8) buffer))
  (apply #'make-reader
         :buffer buffer
         :end (length buffer)
         :fill-buffer-fun #'read-buffer-dummy
         args))

;; If stream is 300 Mb, there are (ceiling (* 300 10^6) 4096) =
;; 73243 calls to fill-buffer. Not many, but inline it anyway
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

(declaim (inline ensure-data-available))
(defun ensure-data-available (reader)
  "Checks if READER can be read without calling fill-buffer"
  (if (= (reader-ibyte reader)
         (reader-end reader))
      (if (not (fill-buffer reader))
          (error 'bitreader-eof :reader reader))))

(declaim (ftype (function (reader) bit) read-bit))
(defun read-bit (reader)
  "Read a single bit from READER"
  (ensure-data-available reader)

  (prog1
      (ldb (byte 1 (- 7 (reader-ibit reader)))
	   (aref (reader-buffer reader)
		 (reader-ibyte reader)))
    (move-forward reader)))

(declaim (ftype (function (reader) (ub 8)) read-octet))
(defun read-octet (reader)
  "Reads current octet from reader
   Ignores ibit"
  (ensure-data-available reader)

  (prog1
      (aref (reader-buffer reader) (reader-ibyte reader))
    (if *read-with-zeroing* (setf (aref (reader-buffer reader) (reader-ibyte reader)) 0))
    (incf (reader-ibyte reader))))

(declaim (ftype (function (non-negative-fixnum reader &key (:endianness symbol))
                          non-negative-fixnum)
                read-octets))
(defun read-octets (n reader &key (endianness :big))
  "Reads n octets in integer value"
  (let ((res 0))
    (declare (type non-negative-fixnum res)
             (type fixnum n)
             (type (member :big :little) endianness))
    (dotimes (i n)
      (declare (type fixnum i))
      (ensure-data-available reader)
      (let ((octet (aref (reader-buffer reader) (reader-ibyte reader))))
        (declare (type (ub 8) octet))
        (setq res
              (if (eq endianness :big)
                  (logior (the non-negative-fixnum (ash res 8)) octet)
                  (logior (the non-negative-fixnum
                               (ash octet (the non-negative-fixnum (ash i 3))))
                          res))))
      (if *read-with-zeroing* (setf (aref (reader-buffer reader) (reader-ibyte reader)) 0))
      (incf (reader-ibyte reader)))
    res))

(declaim (ftype (function ((sa-ub 8) reader) (sa-ub 8)) read-octet-vector))
(defun read-octet-vector (array reader)
  ;; Stupid and maybe slow version.
  ;; Why not? I do not use this function often
  (dotimes (i (length array))
    (ensure-data-available reader)
    (setf (aref array i)
          (aref (reader-buffer reader) (reader-ibyte reader)))
    (if *read-with-zeroing* (setf (aref (reader-buffer reader) (reader-ibyte reader)) 0))
    (incf (reader-ibyte reader)))
  array)

(declaim (ftype (function (reader) non-negative-fixnum) read-to-byte-alignment))
(defun read-to-byte-alignment (reader)
  "Reads from READER to byte alignment.
   If already READER is already byte-aligned,
   returns 0."
  (with-accessors
   ((ibyte reader-ibyte)
    (ibit reader-ibit)) reader

    (if (= ibit 0) 0
        (prog1
            (logand (1- (ash 1 (- 8 ibit)))
                    (aref (reader-buffer reader) ibyte))
          (setf ibit 0)
          (incf ibyte)))))

(declaim (ftype (function (reader &optional) non-negative-fixnum) get-reader-position)
         (inline get-reader-position))
(defun get-reader-position (reader)
  (let ((stream (reader-stream reader)))
    (if stream
        (+ (the non-negative-fixnum
                (file-position (reader-stream reader)))
           (- (reader-end reader))
           (reader-ibyte reader))
        (reader-ibyte reader))))

(declaim (ftype (function (reader t &optional) non-negative-fixnum) set-reader-position)
         (inline set-reader-position))
(defun set-reader-position (reader pos)
  (let ((stream (reader-stream reader)))
    (cond
      (stream
       (file-position stream pos)
       (fill-buffer reader))
      (t (setf (reader-ibyte reader) pos))))
  (setf (reader-ibit reader) 0)
  pos)

(declaim (ftype (function (reader &optional t) non-negative-fixnum) reader-position))
(defun reader-position (reader &optional pos)
  "Returns or sets number of readed octets.
   Similar to file-position
   Sets ibit to zero if val is specified"
  (if pos
      (set-reader-position reader pos)
      (get-reader-position reader)))

(declaim (ftype (function (reader (ub 8)) (ub 8)) peek-octet))
(defun peek-octet (reader octet)
  "Sets input to the first octet found in stream"
  (declare (type (ub 8) octet))
  (setf (reader-ibyte reader)
	(loop
           for pos = (position octet (reader-buffer reader)
                               :start (reader-ibyte reader))
           until pos
           do
             (if (not (fill-buffer reader))
                 (error 'bitreader-eof :reader reader))
             (setf (reader-ibyte reader) 0)
           finally (return pos)))
  octet)

(declaim (ftype (function (reader) non-negative-integer) reader-length))
(defun reader-length (reader)
  "Returns length of a stream in octets.

   Calls #'length on a buffer reader or #'file-length on
   a stream reader"
  (let ((stream (reader-stream reader)))
    (if stream (file-length stream) (length (reader-buffer reader)))))

(declaim (ftype (function (non-negative-integer reader &key (:endianness symbol))
                          non-negative-fixnum)
                read-bits))
(defun read-bits (bits reader &key (endianness :big))
  "Read any number of bits from reader"
  (declare
   (type non-negative-fixnum bits)
   (type (member :big :little) endianness))

  (let ((result 0)
        (already-read 0))
    (declare (type non-negative-fixnum result already-read))

    (with-accessors ((ibit reader-ibit)) reader
      (dotimes (i (ceiling (+ bits ibit) 8))
        (ensure-data-available reader)
        (let ((bits-to-add (min bits (- 8 ibit))))
          (declare (type bit-counter bits-to-add))
          (setq result (logior result
                               (the non-negative-fixnum
                                    (ash
                                     (ldb (byte bits-to-add (- 8 ibit bits-to-add))
                                          (aref (reader-buffer reader)
                                                (reader-ibyte reader)))
                                     (if (eq endianness :big)
                                         (the non-negative-fixnum
                                              (- bits bits-to-add))
                                         already-read))))
                bits (- bits bits-to-add)
                already-read (+ already-read bits-to-add))

          (move-forward reader bits-to-add))))
    result))

(declaim (ftype (function (t) non-negative-fixnum) count-zeros))
(defun count-zeros (reader)
  "Count number of zeros in the input. It reads the first
   occcured one too to copy behaviour of removed
   FLAC::READ-UNARY-CODED-integer"
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

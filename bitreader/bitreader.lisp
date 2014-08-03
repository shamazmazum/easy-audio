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

(in-package :easy-music.bitreader)

(declaim (optimize #+easy-audio-unsafe-code
                   (safety 0) (speed 3)))

(defparameter *buffer-size* 4096)
(defparameter *read-with-zeroing* nil
  "Affects some functions (currently only READ-OCTETS, READ-OCTET
   and READ-OCTET-VECTORS) making them not only read stuff from
   input buffer, but also zero read parts in the buffer. Useful
   for CRC calculation in some containers")

(define-condition bitreader-eof ()
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
  (crc-fun #'(lambda (array accum)
               (declare (ignore array accum))
               0) :type function)
  stream)

(declaim (inline move-forward))
(defun move-forward (reader &optional (bits 1))
  "Moves position in READER bit reader in range [0; 8-ibit] BITS.
   Maximum value of ibit is 7."
  (declare (type non-negative-fixnum bits))
  
  (with-accessors
   ((ibit reader-ibit)
    (ibyte reader-ibyte)) reader

    (incf ibit bits)

    (cond
     ((= ibit 8)
      (setf ibit 0)
      (incf ibyte)))))

(defun read-buffer-from-stream (reader)
  "Read internal buffer from stream"
  (setf (reader-end reader)
        (read-sequence (reader-buffer reader)
                       (reader-stream reader)))
  (if (= (reader-end reader) 0) (error 'bitreader-eof :bitreader reader)))

(defun read-buffer-dummy (reader)
  "Read internal buffer from stream"
  (error 'bitreader-eof :bitreader reader))

(defun make-reader-from-stream (stream &rest args)
  "Make bitreader from stream"
  (apply #'make-reader
         :stream stream
         :fill-buffer-fun #'read-buffer-from-stream
         args))

(defun make-reader-from-buffer (buffer &rest args)
  "Make bitreader from buffer"
  (declare (type (sa-sb 8) buffer))
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
                 (subseq (reader-buffer reader)
                         (reader-crc-start reader)
                         (reader-end reader))
                 (reader-crc reader))
        
        (reader-crc-start reader) 0)

  (setf (reader-ibit reader) 0
        (reader-ibyte reader) 0)
  (funcall (reader-fill-buffer-fun reader) reader))

(declaim (inline can-not-read))
(defun can-not-read (reader)
  "Checks if READER can be read without calling fill-buffer"
  (= (reader-ibyte reader)
     (reader-end reader)))


(declaim (ftype (function (reader) (integer 0 1)) read-bit))
(defun read-bit (reader)
  "Read a single bit from READER"
  (if (can-not-read reader) (fill-buffer reader))

  (prog1
      (ldb (byte 1 (- 7 (reader-ibit reader)))
	   (aref (reader-buffer reader)
		 (reader-ibyte reader)))
    (move-forward reader)))

(declaim (ftype (function (reader) (ub 8)) read-octet))
(defun read-octet (reader)
  "Reads current octet from reader
   Ignores ibit"
  (if (can-not-read reader) (fill-buffer reader))
  
  (prog1
      (aref (reader-buffer reader) (reader-ibyte reader))
    (if *read-with-zeroing* (setf (aref (reader-buffer reader) (reader-ibyte reader)) 0))
    (incf (reader-ibyte reader))))

#+easy-audio-use-fixnums
(declaim (ftype (function (non-negative-int reader &key (:endianness symbol)) non-negative-fixnum) read-octets))
#-easy-audio-use-fixnums
(declaim (ftype (function (non-negative-int reader &key (:endianness symbol)) non-negative-int) read-octets))
(defun read-octets (n reader &key (endianness :big))
  "Reads n octets in integer value"
  (let ((res 0)
        (j 0))
    (declare #+easy-audio-use-fixnums
             (type non-negative-fixnum res)
             (type fixnum j)
             (type (member :big :little) endianness))
    (dotimes (i n)
      (if (can-not-read reader) (fill-buffer reader))
      (let ((octet (aref (reader-buffer reader) (reader-ibyte reader))))
        (declare (type (ub 8) octet))
        (setq res
              (if (eq endianness :big)
                  (logior (ash res 8) octet)
                  (logior (ash octet j) res))))
      (if *read-with-zeroing* (setf (aref (reader-buffer reader) (reader-ibyte reader)) 0))
      (incf (reader-ibyte reader))
      (incf j 8))
    res))

(declaim (ftype (function ((sa-ub 8) reader) positive-int) read-octet-vector))
(defun read-octet-vector (array reader)
  ;; Stupid and maybe slow version.
  ;; Why not? I do not use this function often
  (dotimes (i (length array))
    (if (can-not-read reader) (fill-buffer reader))
    (setf (aref array i)
          (aref (reader-buffer reader) (reader-ibyte reader)))
    (if *read-with-zeroing* (setf (aref (reader-buffer reader) (reader-ibyte reader)) 0))
    (incf (reader-ibyte reader)))
  (length array))

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
            (ldb (byte (- 8 ibit) 0)
                 (aref (reader-buffer reader) ibyte))
          (setf ibit 0)
          (incf ibyte)))))

#+easy-audio-use-fixnums
(declaim (ftype (function (reader &optional t) non-negative-fixnum) reader-position))
#-easy-audio-use-fixnums
(declaim (ftype (function (reader &optional t) non-negative-int) reader-position))
(defun reader-position (reader &optional val)
  "Returns or sets number of readed octets.
   Similar to file-position
   Sets ibit to zero if val is specified"
  (cond
   (val
    (file-position (reader-stream reader) val)
    (fill-buffer reader)
    val)
   (t #f non-negative-fixnum
      (+ #f non-negative-fixnum
         (file-position (reader-stream reader))
         (- (reader-end reader))
         (reader-ibyte reader)))))

(declaim (ftype (function (reader (ub 8)) (ub 8)) peek-octet))
(defun peek-octet (reader octet)
  "Sets input to the first octet found in stream"
  (declare (type (ub 8) octet))
  (setf (reader-ibyte reader)
	(loop for pos = (position octet (reader-buffer reader)
				  :start (reader-ibyte reader))
	      until pos
	      do
	      (fill-buffer reader)
	      (setf (reader-ibyte reader) 0)
	      finally (return pos)))
  octet)

(declaim (ftype (function (reader) non-negative-int) reader-length))
(defun reader-length (reader)
  "Returns length of stream in octets or zero
   if the value is unknown"
  (let ((len (file-length (reader-stream reader))))
    (if len len 0)))

#+easy-audio-use-fixnums
(declaim (ftype (function (non-negative-int reader &key (:endianness symbol)) non-negative-fixnum) read-bits))
#-easy-audio-use-fixnums
(declaim (ftype (function (non-negative-int reader &key (:endianness symbol)) non-negative-int) read-bits))
(defun read-bits (bits reader &key (endianness :big))
  "Read any number of bits from reader"
  (declare
   #+easy-audio-use-fixnums
   (type non-negative-fixnum bits)
   #-easy-audio-use-fixnums
   (type non-negative-int bits)
   (type (member :big :little) endianness))

  (let ((result 0)
        (already-read 0))
    #+easy-audio-use-fixnums
    (declare (type non-negative-fixnum result already-read))
    #-easy-audio-use-fixnums
    (declare (type non-negative-int result already-read))

    (with-accessors ((ibit reader-ibit)) reader
      (dotimes (i (ceiling (+ bits ibit) 8))
        (if (can-not-read reader) (fill-buffer reader))
        (let ((bits-to-add (min bits (- 8 ibit))))
          (declare (type bit-counter bits-to-add))
          (setq result (logior result
                               #f non-negative-fixnum
                               (ash
                                (ldb (byte bits-to-add (- 8 ibit bits-to-add))
                                     (aref (reader-buffer reader)
                                           (reader-ibyte reader)))
                                #f non-negative-fixnum
                                (if (eq endianness :big)
                                    (- bits bits-to-add)
                                    already-read)))
                bits (- bits bits-to-add)
                already-read (+ already-read bits-to-add))
          
          (move-forward reader bits-to-add))))
    result))

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
             (subseq (reader-buffer reader)
                     (reader-crc-start reader)
                     (reader-ibyte reader))
             (reader-crc reader)))

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

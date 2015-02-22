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

;; Contains CRC functions used by easy-audio
;; Flac
(declaim (type (sa-ub 16) +crc-table-0-8005+))
(defparameter +crc-table-0-8005+
  (make-array 256
              :element-type '(ub 16)
              :initial-contents
              '#.(flet ((crc-for-byte (byte)
                          (declare (type (ub 8) byte))
                          (let ((crc (ash byte 8)))
                            (declare (type (ub 16) crc))
                            (loop repeat 8 do
                                 (setq crc
                                       (logand #xffff
                                               (if (/= 0 (logand #x8000 crc))
                                                   (logxor #x8005 (ash crc 1))
                                                   (ash crc 1))))
                               finally (return crc)))))
                   (loop for i below 256 collect (crc-for-byte i))))
  "Precalculated CRC-16 table, starting with 0, polynomial generator #x8005.
   Used for FLAC")

(declaim (ftype (function ((sa-ub 8) (ub 16)
                           &key (:start t) (:end t)) (ub 16)) crc-0-8005))
(defun crc-0-8005 (array accum &key (start 0) end)
  "CRC checksum used in FLAC frames"
  (declare (type (sa-ub 8) array))
  (flet ((accumulate-crc (crc x)
                         (declare (type (ub 16) crc)
                                  (type (ub 8) x))
                         (logand #xffff
                                 (logxor (ash crc 8)
                                         (aref +crc-table-0-8005+
                                               (logxor x (ash crc -8)))))))
    (reduce #'accumulate-crc array :initial-value accum :start start :end end)))

;; OGG
(declaim (type (sa-ub 32) +crc-table-0-04c11db7+))
(defparameter +crc-table-0-04c11db7+
  (make-array 256
              :element-type '(ub 32)
              :initial-contents
              '#.(flet ((crc-for-byte (byte)
                          (declare (type (ub 8) byte))
                          (let ((crc (ash byte 24)))
                            (declare (type (ub 32) crc))
                            (loop repeat 8 do
                                 (setq crc
                                       (logand #xffffffff
                                               (if (/= 0 (logand #x80000000 crc))
                                                   (logxor #x04c11db7 (ash crc 1))
                                                   (ash crc 1))))
                                 finally (return crc)))))

                   (loop for i below 256 collect (crc-for-byte i))))
  "Precalculated CRC-32 table, starting with 0, polynomial generator #x04c11db7.
   Used for OGG container")

(declaim (ftype (function ((sa-ub 8) (ub 32)
                           &key (:start t) (:end t)) (ub 32)) crc-0-04c11db7))
(defun crc-0-04c11db7 (array accum &key (start 0) end)
  "CRC checksum used in OGG container"
  (declare (type (sa-ub 8) array))
  (flet ((accumulate-crc (crc x)
                         (declare (type (ub 32) crc)
                                  (type (ub 8) x))
                         (logand #xffffffff
                                 (logxor (ash crc 8)
                                         (aref +crc-table-0-04c11db7+
                                               (logxor x (logand #xff (ash crc -24))))))))
    (reduce #'accumulate-crc array :initial-value accum :start start :end end)))

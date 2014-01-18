;; Copyright (c) 2012, Vasily Postnicov
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

(in-package :easy-audio.flac)

(declaim (optimize (speed 3)))

(defun read-bits-array (stream array size &key
			       signed
			       (len (length array))
			       (offset 0))
  ;; Will be replaced later
  (declare (type fixnum len offset size)
	   (type (simple-array (signed-byte 32)) array))
  (loop for i from offset below len do
	(setf (aref array i)
	      (if signed (unsigned-to-signed (read-bits size stream) size)
		(read-bits size stream))))
  array)

(defun read-utf8-u32 (stream)
  "for reading frame number
   copy from libFLAC"
  (let ((x (read-octet stream))
	i
	(v 0))
    (declare (type (unsigned-byte 8) x)
	     (type (unsigned-byte 32) v))
    (cond
     (( = 0 (logand x #x80))
      (setq v x i 0))
     
     ((and
       (= 0 (logand x #x20))
       (/= 0 (logand x #xC0)))
      (setq v (logand x #x1F) i 1))

     ((and
       (= 0 (logand x #x10))
       (/= 0 (logand x #xE0)))
      (setq v (logand x #x0F) i 2))

     ((and
       (= 0 (logand x #x08))
       (/= 0 (logand x #xF0)))
      (setq v (logand x #x07) i 3))

     ((and
       (= 0 (logand x #x04))
       (/= 0 (logand x #xF8)))
      (setq v (logand x #x03) i 4))

     ((and
       (= 0 (logand x #x02))
       (/= 0 (logand x #xFC)))
      (setq v (logand x #x01) i 5))
     
     (t (error 'flac-bad-frame
	       :message "Error reading utf-8 coded value")))

    (loop for j from i downto 1 do
	  (setq x (read-octet stream))
	  (if (or
	       (= 0 (logand x #x80))
	       (/= 0 (logand x #x40)))
	      (error 'flac-bad-frame
		     :message "Error reading utf-8 coded value"))
	  (setq v (ash v 6))
	  (setq v (logior v (logand x #x3F))))
    v))

(declaim (inline unsigned-to-signed)
	 (ftype (function ((unsigned-byte 32)
			   (integer 0 32))
			  (signed-byte 32))
		unsigned-to-signed))
(defun unsigned-to-signed (byte len)
  (declare (type (integer 0 32) len)
	   (type (unsigned-byte 32) byte))
  (let ((sign (ldb (byte 1 (1- len)) byte)))
    (if (= sign 0) byte (- byte (ash 1 len)))))


(declaim (ftype (function (t &optional (integer 0 1)) fixnum)
		read-unary-coded-integer)
	 (inline read-unary-coded-integer))
(defun read-unary-coded-integer (bitreader &optional (one 0))
  "Read unary coded integer from bitreader
   By default zero bit is considered as arithmetical 1,
   1 bit is terminator"
  (declare (type (integer 0 1) one))
;  (loop for bit = (tbs:read-bit bitreader)
;	while (= bit one)
;	sum 1))
  (let ((bit 0)
	(sum 0))
    (declare (type (integer 0 1) bit)
	     (type (unsigned-byte 32) sum))
  (tagbody reader-loop
	     (setq bit (read-bit bitreader))
	     (when (= one bit)
	       (incf sum)
	       (go reader-loop)))
  sum))

(declaim (ftype (function (t (integer 0 30))
			  (signed-byte 32))
		read-rice-signed)
	 (inline read-rice-signed))
(defun read-rice-signed (bitreader param)
  (declare (type (integer 0 30) param))
  (let* ((unary (the (unsigned-byte 32)
		 (read-unary-coded-integer bitreader)))
	(binary (the (unsigned-byte 32)
		  (read-bits param bitreader)))
	(val (logior (ash unary param) binary)))
      
    (if (= (ldb (byte 1 0) val) 1)
	(- -1 (ash val -1))
      (ash val -1))))

(defun restore-sync (bitreader streaminfo)
  "Restores lost sync and returns
   number of frame to be read"
  (declare (type reader bitreader)
	   (type streaminfo streaminfo))
  ;; Make sure, we are byte aligned
  ;; We must be, but anyway
  (read-to-byte-alignment bitreader)
  ;; Search first #xff octet
  (peek-octet bitreader #xff)
  (let ((pos (reader-position bitreader))
	(sync-code (read-bits 16 bitreader)))
    (declare (type non-negative-fixnum pos sync-code))
    (cond
     ((or (= sync-code #xfff8)  ;; Begin of frame header, fixed block size
	  (= sync-code #xfff9)) ;; Begin of frame header, variable block size

      (handler-case ;; Test if we really found a frame, or it is coincidence
       (prog2
	   (reader-position bitreader pos)
	   (frame-number (frame-reader bitreader streaminfo))
	 (reader-position bitreader pos))
       (flac-bad-frame ()
		       (restore-sync bitreader streaminfo))))
     
     ((= sync-code #xffff) ;; Might be first octet of frame-header
      (reader-position bitreader (the positive-fixnum (1+ pos)))
      (restore-sync bitreader streaminfo))

     (t (restore-sync bitreader streaminfo)))))

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

(in-package :easy-audio.ape)

(defconstant +ape-id+ #x2043414d)

(defconstant +min-ape-version+ 3800
  "Minimal ape version supported by this implementation")
(defconstant +max-ape-version+ 3990
  "Maximal ape version supported by this implementation")

(deftype u32 () '(unsigned-byte 32))
(deftype u16 () '(unsigned-byte 16))
(deftype u8 () '(unsigned-byte 8))

;; Format flags position

(defparameter *format-flags*
  '(:8bit
    :crc
    :has-peak-level
    :24bit
    :has-seek-elements
    :create-wav-header ))

(declaim (ftype (function (u16 symbol) boolean) flag-set-p))
(defun flag-set-p (flags flag)
  (let ((pos (position flag *format-flags*)))
    (declare (type fixnum pos))
    (= (ldb (byte 1 pos) flags) 1)))

(defstruct blocks-length
  "Length of ape metadata blocks"
  (junk           0 :type u32)
  (descriptor     0 :type u32)
  (header         0 :type u32)
  (seektable      0 :type u32)
  (wavheader      0 :type u32)
  (audiodata      0 :type u32)
  (audiodata-high 0 :type u32)
  (wavtail        0 :type u32))

(defstruct header
  "Ape header which contains metadata"
  (compression-type     0 :type u16)
  (format-flags         0 :type u16)
  (blocks-per-frame     0 :type u32)
  (final-frame-blocks   0 :type u32)
  (total-frames         0 :type u32)
  (bps                  0 :type u16)
  (channels             0 :type u16)
  (sample-rate          0 :type u32)
  (version              0 :type u16)
  (blocks-length        (make-blocks-length))
  seektable
  bittable
  md5)

(define-condition ape-error ()
  ((message :initarg :message
            :initform ""
            :reader ape-error-message))
  (:report (lambda (c s)
             (format s "Ape error: ~A"
                     (ape-error-message c)))))

(defmacro define-stub (name)
  "Defines stub function which does nothing"
  `(defun ,name (&rest args)
     (declare (ignore args))
     (warn "Stub ~A was called" ',name)))

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

(defpackage easy-audio.bitreader
  (:use #:cl #:easy-audio-core #:alexandria)
  (:nicknames #:bitreader)
  (:export
   ;; Conditions
   #:bitreader-eof

   ;; Reader structure and accessors
   #:reader              
   #:make-reader ; Obsolete
   #:make-reader-from-stream
   #:make-reader-from-buffer

   ;; "End user" functions
   #:read-bit            
   #:read-bits
   #:read-octet
   #:read-octet-vector
   #:read-octets
   #:read-to-byte-alignment
   #:count-zeros
   #:reader-position
   #:peek-octet
   #:reader-length

   #:*read-with-zeroing*
   #:with-crc
   #:with-skipping-crc

   #:defreader
   
   #+easy-audio-check-crc
   #:init-crc
   #+easy-audio-check-crc
   #:get-crc
   #+easy-audio-check-crc
   #:crc-0-8005
   #+easy-audio-check-crc
   #:crc-0-04c11db7))

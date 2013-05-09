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

(in-package :easy-audio.wav)

;; General constants
(defconstant +wav-id+ #x52494646
  "Wav format identifier (`RIFF')")

(defconstant +wav-format+ #x57415645
  "Letters (`WAVE')")

(defconstant +format-subchunk+ #x666d7420
  "Format subchunk identifier. Contains letters `fmt '")

(defconstant +data-subchunk+ #x64617461
  "Data subchunk identifier. Contains letters `data'")

(defconstant +fact-subchunk+ #x66616374
  "Fact subchunk identifier. Contains letters `fact'")

;; Audio formats
(defconstant +wave-format-unknown+     #x0000)
(defconstant +wave-format-pcm+         #x0001)
(defconstant +wave-format-float+       #x0003)
(defconstant +wave-format-alaw+        #x0006)
(defconstant +wave-format-mulaw+       #x0007)
(defconstant +wave-format-extensible+  #xfffe)

;; Subchunk structures
(defstruct (format-subchunk (:conc-name format-))
  audio-format
  channels-num
  samplerate
  bps)

(defstruct (data-subchunk (:conc-name data-))
  size)

(defstruct (fact-subchunk (:conc-name fact-))
  samples-num)

;; Condition
(define-condition wav-error ()
  ((message :initarg :message
            :initform ""
	    :type string
	    :reader wav-error-message))
  (:report (lambda (c s)
             (format s "Wav decoder error: ~A" (wav-error-message c)))))

(define-condition wav-error-subchunk (wav-error)
  ((reader      :initarg :reader
                :reader wav-error-reader)
   (rest-bytes  :initarg :rest-bytes
                :reader wav-error-rest-bytes)
   (subchunk    :initarg :subchunk
                :reader wav-error-subchunk)))

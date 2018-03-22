;; Copyright (c) 2012-2018, Vasily Postnicov
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

(declaim
 (type (ub 32)
       +wav-id+ +wav-format+
       +format-subchunk+ +data-subchunk+
       +fact-subchunk+))

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

(declaim
 (type (ub 16)
       +wave-format-unknown+ +wave-format-pcm+
       +wave-format-float+ +wave-format-alaw+
       +wave-format-mulaw+ +wave-format-extensible+))
;; Audio formats
(defconstant +wave-format-unknown+     #x0000)
(defconstant +wave-format-pcm+         #x0001
  "PCM audio format")
(defconstant +wave-format-float+       #x0003
  "Float audio format")
(defconstant +wave-format-alaw+        #x0006
  "A-law coded audio")
(defconstant +wave-format-mulaw+       #x0007
  "Mu-law coded audio")
(defconstant +wave-format-extensible+  #xfffe
  "Extensible audio format")
(defparameter +wave-format-extensible-magick+
  (make-array 14
              :element-type '(ub 8)
              :initial-contents '(#x00 #x00 #x00 #x00 #x10 #x00 #x80 #x00 #x00 #xAA #x00 #x38 #x9B #x71)))

(defclass data-chunk ()
  ((type :initarg :type
         :type (ub 32)
         :accessor riff-type)
   (size :initarg :size
         :type (ub 32)
         :accessor riff-size))
  (:documentation "Chunk of data with size DATA-SIZE"))

(defclass riff-chunk (data-chunk)
  ((subtype   :initarg :subtype
              :type (ub 32)
              :accessor riff-subtype)
   (subchunks :initform nil
              :type list
              :accessor riff-subchunks))
  (:documentation "RIFF chunk, such as WAVE or LIST chunks"))

(defclass wave-chunk (riff-chunk) ()
  (:documentation "Main chunk in the .wav file"))
(defclass list-chunk (riff-chunk) ()
  (:documentation "Auxiliary container chunk"))

(defclass subchunk (data-chunk) ()
  (:documentation "Subchunk of data"))

(defclass format-subchunk (subchunk)
  ((audio-format :type (ub 16)
                 :accessor format-audio-format
                 :documentation "Audio format")
   (channels-num :type (ub 16)
                 :accessor format-channels-num
                 :documentation "Number of channels in the stream")
   (samplerate   :type (ub 32)
                 :accessor format-samplerate
                 :documentation "Samplerate in Hertz")
   (byte-rate    :type (ub 32)
                 :accessor format-byte-rate)
   (block-align  :type (ub 16)
                 :accessor format-block-align)
   (bps          :type (ub 16)
                 :accessor format-bps
                 :documentation "Bits per sample")
   ;; Extended format
   (valid-bps    :type (ub 16)
                 :accessor format-valid-bps
                 :documentation "Valid bits per sample")
   (channel-mask :type (ub 32)
                 :accessor format-channel-mask
                 :documentation "Channel mask of used channels")
   (subformat    :type (sa-ub 8)
                 :accessor format-subformat
                 :documentation "Extended audio format"))
  (:documentation "Audio format subchunk"))

(defclass data-subchunk (subchunk)
  ((audio-position :type unsigned-byte
                   :accessor data-audio-position
                   :initarg :audio-position))
  (:documentation "Audio data subchunk"))

(defclass fact-subchunk (subchunk)
  ((samples-num    :type (ub 32)
                   :accessor fact-samples-num
                   :documentation "Number of interchannel samples"))
  (:documentation "Subchunk with actual number of samples"))

(defgeneric read-chunk-body (reader chunk)
  (:documentation "Read the chunk's body from the stream"))
(defgeneric chunk-sanity-checks (chunk)
  (:documentation "Sanity checks for a chunk"))

;; Condition
(define-condition wav-error (error simple-condition)
  ()
  (:report (lambda (c s)
             (apply #'format s
                    (concatenate 'string
                                 "Wav decoder error: "
                                 (simple-condition-format-control c))
                    (simple-condition-format-arguments c))))
  (:documentation "General Wav error"))

(define-condition wav-error-chunk (wav-error)
  ((reader     :initarg :reader
               :reader wav-error-reader)
   (rest-bytes :initarg :rest-bytes
               :reader wav-error-rest-bytes)
   (chunk      :initarg :chunk
               :reader wav-error-chunk))
  (:documentation "Error while reading a chunk"))

(define-condition wav-warning (warning simple-condition)
  ()
  (:report (lambda (c s)
             (apply #'format s
                    (concatenate 'string
                                 "Wav decoder warning: "
                                 (simple-condition-format-control c))
                    (simple-condition-format-arguments c))))
  (:documentation "General Wav warning"))

(define-condition wav-unknown-chunk (wav-warning)
  ((chunk      :initarg :chunk
               :reader wav-warning-chunk))
  (:documentation "Unknown chunk warning"))

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

(in-package :easy-audio.wav-examples)

(defun decode-wav (name-in name-out)
  "Decode coded (with a-law, mu-law) wav files"
  (with-open-file (in name-in :element-type '(unsigned-byte 8))
    (let* ((reader (open-wav in))
           (subchunks (read-wav-header reader))
           (format (car subchunks))
           (audio-format (format-audio-format format)))

      (if (= audio-format +wave-format-pcm+)
          (error "Already contains decoded pcm data"))
      (if (and (/= audio-format +wave-format-alaw+)
               (/= audio-format +wave-format-mulaw+))
           (error "Wav is not coded with g.711"))

      ;; Write pcm wav header
      (with-open-file
          (out name-out
               :direction :output
               :if-exists :supersede
               :if-does-not-exist :create
               :element-type '(unsigned-byte 8))
        (write-pcm-wav-header out
                              :samplerate (format-samplerate format)
                              :channels (format-channels-num format)
                              :bps 16
                              :totalsamples (samples-num subchunks)))

      ;; Write decoded data
      (with-open-file
          (out name-out
               :direction :output
               :if-exists :append
               :element-type '(signed-byte 16))
        (file-position out (file-length out))
        (write-sequence
         (decode-wav-data
          format
          (read-wav-data reader format (samples-num subchunks)))
         out))))
  t)

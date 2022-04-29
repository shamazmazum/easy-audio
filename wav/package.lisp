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

(defpackage easy-audio.wav
  (:use #:cl
        #:easy-audio.bitreader
        #:easy-audio.core)
  (:local-nicknames (:ns :nibbles-streams))
  (:export #:+wav-id+   ; Useful constants which can be used in examples
           #:+wav-format+
           #:+format-subchunk+
           #:+data-subchunk+

           #:+wave-format-pcm+
           #:+wave-format-float+
           #:+wave-format-alaw+
           #:+wave-format-mulaw+
           #:+wave-format-extensible+

           #:data-chunk ; A typed container
           #:riff-type
           #:riff-size

           #:riff-chunk ; Riff chunk (a container)
           #:riff-subchunks
           #:riff-subtype

           #:format-audio-format ; Format subchunk and accessors
           #:format-channels-num
           #:format-samplerate
           #:format-bps
           #:format-valid-bps
           #:format-channel-mask
           #:format-subchunk

           #:data-subchunk  ; Data subchunk and accessors
           #:data-size
           #:data-audio-position

           #:fact-subchunk ; Fact subchunk and accessors
           #:fact-samples-num

           #:info-subchunk ; INFO subchunk and accessors
           #:info-key
           #:info-value

           #:wav-error  ; Conditions
           #:wav-error-chunk
           #:wav-warning
           #:wav-unknown-chunk

           #:skip-subchunk ; Restarts

           #:open-wav
           #:read-wav-header
           #:read-wav-data
           #:decode-wav-data
           #:reader-position-to-audio-data

           #:samples-num ; Helper functions
           #:get-info-metadata

           #:write-pcm-wav-header ;; Simple writing
           #:with-output-to-wav))

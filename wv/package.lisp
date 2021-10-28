;; Copyright (c) 2012-2014, Vasily Postnicov
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

(defpackage easy-audio.wv
  (:use #:cl
        #:alexandria
        #:bitreader
        #:easy-audio-core
        #:utils)
  (:nicknames #:wv)
  (:export #:wavpack-error ; Conditions
           #:wavpack-warning
           #:block-error
           #:lost-sync
           #:unknown-metadata

           #:read-new-block-single    ; Restarts and recovery from errors
           #:read-new-block-multichannel
           #:read-new-block

           #:metadata-riff-header ; Metadata types
           #:metadata-riff-trailer
           
           #:block-samplerate ; Block parameters
           #:block-bps
           #:block-channels
           #:block-track-number ; Unused in current format specification
           #:block-index-number ; Unused in current format specification
           #:block-total-samples
           #:block-block-index
           #:block-block-samples
           #:block-metadata
           #:metadata-data
           #:flag-set-p

           #:read-wv-block ; Functions
           #:read-wv-block-multichannel
           #:decode-wv-block
           #:restore-sync
           #:restore-sync-multichannel
           #:seek-sample
           #:open-wv
           #:make-output-buffers

           #:with-output-buffers ; Macros
           #:with-open-wv

           #:*residual-buffers*)) ; Variables

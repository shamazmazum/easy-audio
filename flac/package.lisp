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

(defpackage easy-audio.flac
  (:use #:cl
        #:bitreader
        #:easy-audio.core
        #:alexandria)
  (:nicknames #:flac)
  (:local-nicknames (:sera :serapeum))
  (:export #:streaminfo ; Metadata
	   #:streaminfo-minblocksize
	   #:streaminfo-maxblocksize
	   #:streaminfo-minframesize
	   #:streaminfo-maxframesize
	   #:streaminfo-samplerate
	   #:streaminfo-channels
	   #:streaminfo-bitspersample
	   #:streaminfo-totalsamples
	   #:streaminfo-md5

	   #:seekpoint
	   #:seekpoint-samplenum
	   #:seekpoint-offset
	   #:seekpoint-samples-in-frame
           #:seektable
	   #:seektable-seekpoints

	   #:vorbis-comment
	   #:vorbis-vendor-comment
	   #:vorbis-user-comments

	   #:cuesheet
	   #:cuesheet-catalog-id
	   #:cuesheet-lead-in
	   #:cuesheet-cdp
	   #:cuesheet-tracks

	   #:cuesheet-track
	   #:cuesheet-track-offset
	   #:cuesheet-track-number
	   #:cuesheet-track-isrc
	   #:cuesheet-track-type
	   #:cuesheet-track-pre-emphasis
	   #:cuesheet-track-indices

	   #:cuesheet-index
	   #:cuesheet-index-offset
	   #:cuesheet-index-number

           #:picture
           #:picture-type
           #:picture-mime-type
           #:picture-description
           #:picture-width
           #:picture-height
           #:picture-depth
           #:picture-color-num
           #:picture-picture

	   ;; And so on with metadata classes
	   ;; Frame and its slots
	   
	   #:frame
	   #:frame-streaminfo
	   #:frame-blocking-strategy
	   #:frame-block-size
	   #:frame-sample-rate
	   #:frame-channel-assignment
	   #:frame-sample-size
	   #:frame-number
	   #:frame-crc-8
	   #:frame-subframes
	   #:frame-crc-16

	   ;; Functions
	   #:open-flac
           #:read-metadata
	   #:read-frame
           #:open-ogg-flac
           #:read-ogg-metadata
	   #:read-ogg-frame
	   #:frame-decode
	   #:seek-sample
	   #:metadata-find-seektable
           #:make-output-buffers

           ;; Macros
           #:with-output-buffers
           #:with-open-flac
           #:with-open-ogg-flac

	   ;; Conditions
	   #:flac-error
	   #:flac-bad-metadata
	   #:flac-bad-frame

	   ;; Restarts
	   #:skip-malformed-metadata
           #:skip-malformed-frame
           #:stop-reading-frame
           #:read-raw-block

           ;;Variables
           #:*out-buffers*))

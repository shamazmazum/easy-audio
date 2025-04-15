(defpackage easy-audio.wv
  (:use #:cl
        #:alexandria
        #:easy-audio.bitreader
        #:easy-audio.core)
  (:local-nicknames (:sera :serapeum))
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

           #:with-open-wv)) ;; Macros

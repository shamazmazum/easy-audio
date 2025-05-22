(defpackage easy-audio.ape
  (:use #:cl
        #:alexandria
        #:easy-audio.bitreader
        #:easy-audio.core)
  (:local-nicknames (#:sera #:serapeum))
  (:export
   ;; Conditions
   #:ape-error
   #:apev2-tag-error
   ;; Macros
   #:with-open-ape
   ;; APE tags
   #:read-tag
   #:read-tag-from-end
   #:*apev2-external-format*
   ;; APE audio format
   #:open-ape
   #:read-metadata
   #:read-frame
   #:decode-frame
   #:seconds=>frame-number
   ;; Metadata accessors
   #:metadata
   #:metadata-version
   #:metadata-compression-type
   #:metadata-blocks-per-frame
   #:metadata-final-frame-blocks
   #:metadata-total-frames
   #:metadata-bps
   #:metadata-channels
   #:metadata-samplerate
   #:metadata-total-samples
   #:frame-samples))

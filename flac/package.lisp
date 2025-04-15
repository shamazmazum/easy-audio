(defpackage easy-audio.flac
  (:use #:cl
        #:easy-audio.bitreader
        #:easy-audio.core
        #:alexandria)
  (:local-nicknames (:sera :serapeum)
                    (:ogg  :easy-audio.ogg))
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
           #:decode-frame
           #:seek-sample
           #:metadata-find-seektable

           ;; Macros
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
           #:read-raw-block))

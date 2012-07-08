(defpackage cl-flac
  (:use #:cl)
  (:nicknames #:flac)
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

	   #:seekpoint-samplenum
	   #:seekpoint-offset
	   #:seekpoint-samples-in-frame
	   #:seektable-seekpoints
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
	   #:frame-reader
	   #:subframe-decode ; Not needed to end user, actually
	   #:frame-decode

	   #:close-reader

	   ;; Conditions
	   #:flac-error
	   #:flac-eof
	   #:flac-bad-metadata

	   ;; Macro
	   #:with-open-flac))

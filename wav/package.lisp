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

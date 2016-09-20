(defpackage easy-audio.utils
  (:nicknames #:utils)
  (:use #:cl #:easy-audio-early)
  (:export #:mixchannels
           #:write-pcm-wav-header
           #:defreader))

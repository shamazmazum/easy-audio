(in-package :easy-audio.flac-examples)

;; Works only for 8 or 16 bps
(defun ogg2wav (ogg-name wav-name)
  "Decodes flac to wav. Works only for 8 or 16 bps,
   fixed block size and if total samples in stream is known"
  (with-open-ogg-flac (in-reader ogg-name)
    (let* ((blocks (read-ogg-metadata in-reader))
           (streaminfo (the streaminfo (first blocks)))
           (minblocksize (streaminfo-minblocksize streaminfo))
           (maxblocksize (streaminfo-maxblocksize streaminfo))
           (totalsamples (streaminfo-totalsamples streaminfo))
           (blocksize minblocksize)
           (bps (streaminfo-bitspersample streaminfo))
           (channels (streaminfo-channels streaminfo))
           (samplerate (streaminfo-samplerate streaminfo)))

      (when (zerop totalsamples)
        (error "Number of total samples is unknown"))
      (when (/= minblocksize maxblocksize)
        (error "Block size must be fixed"))

      (unless (or (= 8 bps)
                  (= 16 bps))
        (error "Bps must be 16 or 8"))

      (with-output-to-wav (out-stream wav-name
                           :supersede t
                           :samplerate samplerate
                           :channels channels
                           :bps bps
                           :totalsamples totalsamples)
        (with-output-buffers (streaminfo)
          (loop for i below totalsamples by blocksize
                for bufsize = (min (- totalsamples i) blocksize)
                for buf = (make-array (* bufsize channels) :element-type '(signed-byte 32)) do
                  (write-sequence (mixchannels buf (frame-decode (read-ogg-frame in-reader streaminfo)))
                                  out-stream)))))))

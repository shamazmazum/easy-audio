(in-package :easy-audio.flac-examples)

;; Works only for 8 or 16 bps
(defun flac2wav (flac-name wav-name)
  "Decodes flac to wav. Works only for 8 or 16 bps,
   fixed block size and if total samples in stream is known"
  (with-open-flac (in-reader flac-name)
    (let* ((blocks (read-metadata in-reader))
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
                  (= 16 bps)
                  (= 24 bps))
        (error "Bps must be 8, 16 or 24"))

      (with-output-to-wav (out-stream    wav-name
                           :supersede    t
                           :samplerate   samplerate
                           :channels     channels
                           :bps          bps
                           :totalsamples totalsamples)
        (loop for i below totalsamples by blocksize
              for bufsize = (min (- totalsamples i) blocksize) do
              (write-sequence
               (interleave-channels (decode-frame (read-frame in-reader streaminfo)))
               out-stream))))))

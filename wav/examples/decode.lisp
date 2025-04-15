(in-package :easy-audio.wav-examples)

(defun decode-wav (name-in name-out)
  "Decode coded (with a-law, mu-law) wav files"
  (with-open-file (in name-in :element-type '(unsigned-byte 8))
    (let* ((reader (open-wav in))
           (subchunks (read-wav-header reader))
           (format (car subchunks))
           (audio-format (format-audio-format format)))

      (when (= audio-format +wave-format-pcm+)
        (error "Already contains decoded pcm data"))
      (when (and (/= audio-format +wave-format-alaw+)
                 (/= audio-format +wave-format-mulaw+))
        (error "Wav is not coded with g.711"))
      (reader-position-to-audio-data reader subchunks)

      (with-output-to-wav (out name-out
                               :supersede    t
                               :samplerate   (format-samplerate format)
                               :channels     (format-channels-num format)
                               :bps          16
                               :totalsamples (samples-num subchunks))
        (write-sequence
         (decode-wav-data
          format
          (read-wav-data reader format (samples-num subchunks)))
         out))))
  t)

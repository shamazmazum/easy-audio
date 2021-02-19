(in-package :easy-audio.ape-examples)

(defun ape2wav (ape-name wav-name)
  "Decodes ape file to wav."
  (with-open-ape (in-reader ape-name)
    (let* ((meta (read-metadata in-reader))
           (total-samples (metadata-total-samples meta))
           (total-frames (metadata-total-frames meta))
           (bps (metadata-bps meta))
           (channels (metadata-channels meta))
           (samplerate (metadata-samplerate meta)))
      
      (with-output-to-wav (out-stream wav-name
                           :supersede t
                           :samplerate samplerate
                           :channels channels
                           :bps bps
                           :totalsamples total-samples)
        (loop for i below total-frames
              for frame = (read-frame in-reader meta i)
              for buf = (make-array (* channels (frame-samples frame))
                                    :element-type '(signed-byte 32))
              do
                 (write-sequence (mixchannels buf (decode-frame frame)) out-stream))))))

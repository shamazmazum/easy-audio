(in-package :easy-audio.wv-examples)

(defun wv2wav (wv-name wav-name)
  "Decode wavpack to wav."
  (with-open-wv (reader wv-name)
    (restore-sync reader)
    (let* ((first-block (read-wv-block reader))
           (channels (block-channels first-block))
           (bps (block-bps first-block))
           (total-samples (block-total-samples first-block))
           (samplerate (block-samplerate first-block)))

      (reader-position reader 0)
      (restore-sync reader)

      (with-output-to-wav (out-stream wav-name
                           :supersede t
                           :samplerate samplerate
                           :channels channels
                           :bps bps
                           :totalsamples total-samples)
        (handler-case
            (loop with samples-written = 0
                  while (< samples-written total-samples)
                  for block = (read-wv-block reader)
                  for samples = (block-block-samples block)
                  for buf = (make-array (* samples channels) :element-type '(signed-byte 32))
                  do
                     (incf samples-written samples)
                     (write-sequence (mixchannels buf (decode-wv-block block))
                                     out-stream)))))))

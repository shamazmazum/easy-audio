(in-package :easy-audio.wav)

(defun write-pcm-wav-header (out-stream &key samplerate channels bps totalsamples)
  "Writes header of uncompressed wav into stream"
  (let ((size (/ (* bps channels totalsamples) 8)))
    (nibbles:write-ub32/be +wav-id+ out-stream)
    (nibbles:write-ub32/le (+ 36 size) out-stream)
    (nibbles:write-ub32/be +wav-format+ out-stream)

    ;; Subchunk 1
    (nibbles:write-ub32/be +format-subchunk+ out-stream)
    (nibbles:write-ub32/le 16 out-stream)
    (nibbles:write-ub16/le +wave-format-pcm+ out-stream)
    (nibbles:write-ub16/le channels out-stream)
    (nibbles:write-ub32/le samplerate out-stream)

    (nibbles:write-ub32/le (/ (* samplerate channels bps) 8) out-stream)
    (nibbles:write-ub16/le (/ (* channels bps) 8) out-stream)
    (nibbles:write-ub16/le bps out-stream)

    ;; Subchunk 2
    (nibbles:write-ub32/be +data-subchunk+ out-stream)
    (nibbles:write-ub32/le size out-stream))
  (values))

(defmacro with-output-to-wav ((stream filename
                                      &key supersede samplerate channels bps totalsamples)
                              &body body)
  "Opens a STREAM and writes PCM-coded (uncompressed) WAV header to a file with filename FILENAME"
  (let ((file-stream (gensym)))
    `(with-open-file (,file-stream ,filename
                                   :direction         :output
                                   :element-type      '(unsigned-byte 8)
                                   ,@(if supersede    '(:if-exists :supersede))
                                   :if-does-not-exist :create)
       (write-pcm-wav-header ,file-stream
                             :samplerate   ,samplerate
                             :channels     ,channels
                             :bps          ,bps
                             :totalsamples ,totalsamples)
       (let ((,stream (make-instance 'ns:nibbles-output-stream
                                     :stream       ,file-stream
                                     :element-type (list 'signed-byte ,bps))))
         ,@body))))

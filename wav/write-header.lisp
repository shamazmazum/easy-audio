(in-package :easy-audio.wav)

(defun write-pcm-wav-header (out-stream &key samplerate channels bps totalsamples)
  "Writes header of uncompressed wav into stream"
  (let ((size (ash (* bps channels totalsamples) -3))
        (buf2 (make-array 2 :element-type '(unsigned-byte 8)))
        (buf4 (make-array 4 :element-type '(unsigned-byte 8))))

    (write-sequence (integer-to-array-be wav:+wav-id+ buf4) out-stream)
    (write-sequence (integer-to-array (+ 36 size) buf4) out-stream)
    (write-sequence (integer-to-array-be wav:+wav-format+ buf4) out-stream)

    ;; Subchunk 1
    (write-sequence (integer-to-array-be wav:+format-subchunk+ buf4) out-stream)
    (write-sequence #(16 0 0 0) out-stream)
    (write-sequence (integer-to-array wav:+wave-format-pcm+ buf2) out-stream)
    (write-sequence (integer-to-array channels buf2) out-stream)
    (write-sequence (integer-to-array samplerate buf4) out-stream)

    (write-sequence (integer-to-array
                     (ash
                      (* samplerate channels bps) -3)
                     buf4)
                    out-stream)

    (write-sequence (integer-to-array
                     (ash
                      (* channels bps) -3)
                     buf2)
                    out-stream)

    (write-sequence (integer-to-array
                     bps buf2)
                    out-stream)

    ;; Subchunk 2
    (write-sequence (integer-to-array-be wav:+data-subchunk+ buf4) out-stream)
    (write-sequence (integer-to-array size buf4) out-stream))
  t)

(defmacro with-output-to-wav ((stream filename
                                      &key supersede samplerate channels bps totalsamples)
                              &body body)
  "Opens a STREAM and writes PCM-coded (uncompressed) WAV header to a file with filename FILENAME"
  `(progn
     (with-open-file (,stream ,filename
                              :direction :output
                              :element-type '(unsigned-byte 8)
                              ,@(if supersede '(:if-exists :supersede))
                              :if-does-not-exist :create)
       (write-pcm-wav-header ,stream
                             :samplerate ,samplerate
                             :channels ,channels
                             :bps ,bps
                             :totalsamples ,totalsamples))
     (with-open-file (,stream ,filename
                              :direction :output
                              :element-type (list 'signed-byte ,bps)
                              :if-exists :append)
       ,@body)
     t))

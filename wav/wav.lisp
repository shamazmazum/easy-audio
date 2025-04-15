(in-package :easy-audio.wav)

(defun skip-subchunk (c)
  "Invoke @c(skip-subchank) restart"
  (invoke-restart 'skip-subchunk c))

(defmacro number-case (keyform &body cases)
  (let ((sym (gensym)))
    `(let ((,sym ,keyform))
       (cond
         ,@(loop for case in cases collect
                (let ((test (car case)))
                  (if (eq test t)
                      `(t ,@(cdr case))
                      `((= ,sym ,test) ,@(cdr case)))))))))

(defun chunk-class (chunk)
  (let ((type (riff-type chunk)))
    (number-case type
      (+wav-id+ 'wave-chunk)
      (+format-subchunk+ 'format-subchunk)
      (+data-subchunk+   'data-subchunk)
      (+fact-subchunk+   'fact-subchunk)
      (+list-chunk+      'list-chunk)
      (t (warn 'wav-unknown-chunk
               :format-control "Unknown chunk type ~x (~s)"
               :format-arguments (list type
                                       (code=>string type))
               :chunk chunk)
         'data-chunk))))

(defun info-subchunk-key (type)
  (number-case type
    (+info-name+       :name)
    (+info-subject+    :subject)
    (+info-artist+     :artist)
    (+info-comment+    :comment)
    (+info-keywords+   :keywords)
    (+info-software+   :software)
    (+info-engineer+   :engineer)
    (+info-technician+ :technician)
    (+info-creation+   :creation-time)
    (+info-genre+      :genre)
    (+info-copyright+  :copyright)
    (t (intern (code=>string type)
               (find-package :keyword)))))

(defmethod read-chunk-header (reader (parent-chunk t))
  (declare (ignore parent-chunk))
  (let* ((type (read-octets 4 reader))
         (size (read-octets 4 reader :endianness :little))
         (chunk (make-instance 'data-chunk :type type :size size)))
    (change-class chunk (chunk-class chunk))))

;; FIXME: INFO subchunk size is rounded to closest even bigger than (or equal to)
;; the value in a file.
(defmethod read-chunk-header (reader (parent-chunk list-chunk))
  (if (= (riff-subtype parent-chunk)
         +list-info+)
      (let ((type (read-octets 4 reader))
            (size (read-octets 4 reader :endianness :little)))
        (make-instance 'info-subchunk
                       :type type
                       :size (+ size (logand 1 size))))
      (call-next-method)))

(defmethod chunk-sanity-checks ((chunk wave-chunk))
  (when (/= (riff-subtype chunk) +wav-format+)
    (error 'wav-error :format-control "Not a WAV stream"))
  chunk)

(defmethod chunk-sanity-checks ((chunk list-chunk))
  (let ((subtype (riff-subtype chunk)))
    (when (/= subtype +list-info+)
      (warn 'wav-unknown-chunk
            :format-control "LIST chunk of unusual subtype ~x (~s)"
            :format-arguments (list subtype (code=>string subtype))
            :chunk chunk))))

(defmethod chunk-sanity-checks ((chunk data-chunk))
  chunk)

;; FIXME: For debugging
#+nil
(defmethod read-body :before (reader (chunk data-chunk))
  (declare (ignore chunk))
  (format t "Reader pos: ~x~%" (reader-position reader))
  (force-output t))

(defmethod read-body (reader (chunk data-chunk))
  (reader-position reader (+ (riff-size chunk)
                             (reader-position reader)))
  chunk)

(defmethod read-body :before (reader (chunk data-subchunk))
  (setf (data-audio-position chunk)
        (reader-position reader)))

(defmethod read-body (reader (chunk info-subchunk))
  (setf (info-key chunk) (info-subchunk-key (riff-type chunk)))
  (let ((string-buffer (read-octet-vector (make-array (riff-size chunk)
                                                      :element-type '(ub 8))
                                          reader)))
    (when (or (every #'zerop string-buffer)
              (not (zerop (aref string-buffer (1- (riff-size chunk))))))
      (error 'wav-error-chunk
             :format-control "Value in INFO subchunk is not a null-terminated string"
             :rest-bytes 0
             :chunk chunk))
    (setf (info-value chunk)
          (flexi-streams:octets-to-string
           (subseq string-buffer 0 (1+ (position 0 string-buffer :from-end t :test #'/=))))))
  chunk)

(defreader (read-format-subchunk) (t)
  (format-audio-format (:octets 2) :endianness :little)
  (format-channels-num (:octets 2) :endianness :little)
  (format-samplerate (:octets 4) :endianness :little)
  (format-byte-rate (:octets 4) :endianness :little)
  (format-block-align (:octets 2) :endianness :little)
  (format-bps (:octets 2) :endianness :little))

(defreader (read-extended-format) (t)
  (format-valid-bps (:octets 2) :endianness :little)
  (format-channel-mask (:octets 4) :endianness :little)
  (format-subformat (:octet-vector (make-array 16 :element-type '(unsigned-byte 8)))))

(defun check-extensible-audio-format (format)
  "Check extensible audio format magick"
  (if (= (format-audio-format format)
         +wave-format-extensible+)
      (let ((subformat (format-subformat format)))
        (when (not (equalp (subseq subformat 2) +wave-format-extensible-magick+))
          (error 'wav-error-chunk
                 :format-control "Invalid extensible format magick"
                 :rest-bytes 0
                 :chunk format))
        (setf (format-audio-format format)
              (logior (aref subformat 0)
                      (ash (aref subformat 1) 8)))))
  format)

(defmethod read-body (reader (chunk format-subchunk))
  (read-format-subchunk reader chunk)
  (let ((size (riff-size chunk)))
    (if (= size 16) chunk
        (let ((extended-size (read-octets 2 reader :endianness :little)))
          ;; Sanity checks
          (unless (or (and (zerop extended-size) (= size 18))
                      (and (= extended-size 22) (= size 40)))
            (error 'wav-error-chunk
                   :format-control "Malformed format subchunk"
                   :rest-bytes (- size 18)
                   :reader reader
                   :chunk chunk))

          (when (not (zerop extended-size))
            (read-extended-format reader chunk)
            (check-extensible-audio-format chunk))))
    chunk))

(defmethod read-body (reader (chunk fact-subchunk))
  (setf (fact-samples-num chunk) (read-octets 4 reader :endianness :little))
  (when (/= (riff-size chunk) 4)
    (error 'wav-error-chunk
           :format-control "Fact subchunk size is not 4. Do not know what to do"
           :rest-bytes (- (riff-size chunk) 4)
           :chunk chunk))
  chunk)

(defmethod read-body (reader (chunk riff-chunk))
  (setf (riff-subtype chunk) (read-octets 4 reader))
  (chunk-sanity-checks chunk)

  (setf (riff-subchunks chunk)
        (with-interactive-debug
          (loop
             with data-read = 8
             with subchunks = nil
             while (< data-read (riff-size chunk))
             do
               (restart-case
                   (let ((subchunk (read-body reader (read-chunk-header reader chunk))))
                     (incf data-read (+ 8 (riff-size subchunk)))
                     (push subchunk subchunks))
                 (skip-subchunk (c)
                   :interactive (lambda () (list *current-condition*))
                   :report "Skip reading subchunk"
                   (unless (zerop (wav-error-rest-bytes c))
                     (read-octets (wav-error-rest-bytes c)
                                  (wav-error-reader c)))
                   (incf data-read (+ 8 (riff-size (wav-error-chunk c))))))
             finally (return (reverse subchunks)))))
  chunk)

(defun open-wav (stream)
  "Opens a wav stream and returns a bit reader object"
  (make-reader :stream stream))

(defun read-wav-header (reader)
  "Read RIFF chunks from an audio stream"
  (let ((riff-chunk (read-chunk-header reader nil)))
    (unless (typep riff-chunk 'wave-chunk)
      (error 'wav-error :format-control "Not a WAV stream"))
    (read-body reader riff-chunk)

    ;; Sanity checks
    (let* ((subchunks (riff-subchunks riff-chunk))
           (format-subchunk (car subchunks)))
      (unless (typep format-subchunk 'format-subchunk)
        (error 'wav-error :format-control "First subchunk is not a format subchunk"))
      (unless (or (= (format-audio-format format-subchunk) +wave-format-pcm+)
                  (find-if #'(lambda (x) (typep x 'fact-subchunk)) subchunks))
        (error 'wav-error :format-control "No fact subchunk in compressed wav"))
      subchunks)))

;; Helper function(s)
(defun samples-num (subchunks)
  "Returns a number of interchannel samples in the stream."
  (let ((fact (find 'fact-subchunk subchunks :key #'type-of))
        (data (find 'data-subchunk subchunks :key #'type-of))
        (format (find 'format-subchunk subchunks :key #'type-of)))
    (if fact
        (fact-samples-num fact)
        (/ (riff-size data) (format-channels-num format)
           (ash (format-bps format) -3)))))

(defun get-info-metadata (subchunks)
  "Return metadata in the LIST INFO subchunks as an association list"
  (let* ((list-chunk (find 'list-chunk subchunks :key #'type-of))
         (info-subchunks (remove 'info-subchunk (riff-subchunks list-chunk)
                                 :test-not #'eql
                                 :key #'type-of)))
    (mapcar (lambda (info-subchunk)
              (cons (info-key info-subchunk)
                    (info-value info-subchunk)))
            info-subchunks)))

(defun reader-position-to-audio-data (reader subchunks)
  "Set the reader's position to beginning of audio data"
  (let ((data (find 'data-subchunk subchunks :key #'type-of)))
    (reader-position reader (data-audio-position data))))

(defun decompose (buffer channel-buffers)
  (let ((nsamples (length (car channel-buffers)))
        (channels (length channel-buffers)))
    (loop for i below nsamples
          for idx from 0 by channels do
          (loop for channel in channel-buffers
                for offset from 0 by 1 do
                (setf (aref channel i)
                      (aref buffer (+ idx offset))))))
  channel-buffers)

(declaim
 (ftype
  (function ((unsigned-byte 32) (integer 0 32)) (signed-byte 32))
  unsigned->signed))
(defun unsigned->signed (x bps)
  "Unsigned to signed converter"
  (declare (optimize (speed 3))
           (type (integer 0 32) bps)
           (type (unsigned-byte 32) x))
  (if (zerop (ldb (byte 1 (1- bps)) x)) x
      (- (1+ (logxor (1- (ash 1 bps)) x)))))

(defun read-wav-data (reader format nsamples &key decompose)
  "Read a portion of audio data in the wav stream. Requires a @c(bitreader) and
@c(format) subchunk. Reads exactly @c(nsamples) interchannel
samples. Optionally, decomposes them into different by-channel arrays if
@c(decompose) is @c(T)."
  (let* ((channels (format-channels-num format))
         (bps (format-bps format))
         (audio-format (format-audio-format format))
         (buffer (make-array (* nsamples channels) :element-type '(signed-byte 32))))
    (loop for i below (length buffer) do
         (setf (aref buffer i)
               (let ((sample (read-bits bps reader :endianness :little)))
                 (if (and (= audio-format +wave-format-pcm+)
                          (/= bps 8))
                     (unsigned->signed sample bps) sample))))
    (if decompose
        (decompose buffer
                   (loop repeat channels collect
                        (make-array nsamples :element-type '(signed-byte 32))))
        buffer)))

(defun decode-wav-data (format buffer)
  "Decodes wav audio data in the @c(buffer). Often, in the case of uncompressed
  data, it simply returns the @c(buffer) unmodified."
  (let ((audio-format (format-audio-format format)))
    (cond
      ((= audio-format +wave-format-pcm+) buffer)
      ((= audio-format +wave-format-alaw+)
       (map-into buffer #'easy-audio.general:g.711-alaw-decode buffer))
      ((= audio-format +wave-format-mulaw+)
       (map-into buffer #'easy-audio.general:g.711-ulaw-decode buffer))
      (t (error 'wav-error
                :format-control "Unknown audio encoding: ~d"
                :format-arguments (list audio-format))))))

(in-package :easy-audio-tests)

(def-suite bitreader :description "Bitreader tests")
(def-suite flac      :description "Flac decoder tests")
(def-suite ogg       :description "OGG container tests")
(def-suite decoders  :description "General decoders tests")
(def-suite wavpack   :description "Wavpack tests")
(def-suite ape       :description "Ape tests")
(def-suite core      :description "Core tests")

(defun prepare-input (&rest args)
  (apply #'concatenate
         'vector
         (mapcar (lambda (elem)
                   (if (atom elem) (list elem) elem))
                 args)))

;; Can it be done with FiveAM itself?
;; Maybe it's good idea to create suite registry here?
(defun run-tests ()
  "Run all tests and return T if all tests have passed"
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(bitreader flac ogg decoders wavpack ape core))))

(in-suite bitreader)
(test bitreader-tests
  "Test low-level bitreader functions"
  (with-input-from-sequence (input #(1 2 3 128 129 4 1 2 #xc5 #x00 #x0c))
    ;; Set internal buffer size low to check buffer refill
    (let ((bitreader::*buffer-size* 3)
          (reader (bitreader:make-reader :stream input)))
      ;; "Not associated with file" blah-blah
      ;; (is (= (bitreader:reader-length reader) 6))
      (is (= (bitreader:read-octet reader) 1))
      (is (= (bitreader:read-bits 16 reader) 515))
      (is (= (bitreader:reader-position reader) 3))
      (is (= (bitreader:read-bit reader) 1))
      (is (= (bitreader:read-bit reader) 0))
      (is (= (bitreader:read-to-byte-alignment reader) 0))
      (is (= (bitreader:read-bit reader) 1))
      (is (= (bitreader:read-to-byte-alignment reader) 1))
      (is (= (bitreader:read-bits 8 reader) 4))
      (is (= (bitreader:read-octets 2 reader) 258))
      ;; Test zero counter
      (is (= (bitreader:read-bits 2 reader) 3))
      (is (= (bitreader:count-zeros reader) 3))
      (is (= (bitreader:read-bits 2 reader) 1))
      ;; And with multiple octets
      (is (= (bitreader:count-zeros reader) 12))
      (is (= (bitreader:read-to-byte-alignment reader) 4)))))

(test reader-position
  "Test READER-POSITION function"
  (with-input-from-sequence (input #(1 2 3 4 5 6 7 8 9 10))
    ;; Set internal buffer size low to check buffer refill
    (let ((bitreader::*buffer-size* 4)
          (reader (bitreader:make-reader :stream input)))
      (is (= (bitreader:read-octet reader) 1))
      (is (= (bitreader:reader-position reader) 1))
      (is (= (bitreader:read-octet reader) 2))
      (is (= (bitreader:reader-position reader) 2))
      ;; Short jump backwards
      (bitreader:reader-position reader 1)
      (is (= (bitreader:read-octet reader) 2))
      (is (= (bitreader:reader-position reader) 2))
      ;; Short jump forwards
      (bitreader:reader-position reader 3)
      (is (= (bitreader:read-octet reader) 4))
      (is (= (bitreader:reader-position reader) 4))
      ;; Long jump forwards
      (bitreader:reader-position reader 9)
      (is (= (bitreader:read-octet reader) 10))
      (is (= (bitreader:reader-position reader) 10))
      ;; Long jump backwards
      (bitreader:reader-position reader 2)
      (is (= (bitreader:read-octet reader) 3))
      (is (= (bitreader:reader-position reader) 3)))))

(test bitreader-little-endian
  "Test low-level bitreader functions"
  (with-input-from-sequence (input #(2 1 3 128))
    ;; Set internal buffer size low to check buffer refill
    (let ((bitreader::*buffer-size* 3)
          (reader (bitreader:make-reader :stream input)))
      ;; "Not associated with file" blah-blah
      ;; (is (= (bitreader:reader-length reader) 6))
      (is (= (bitreader:read-octets 2 reader :endianness :little) 258))
      (is (= (bitreader:read-bits 16 reader :endianness :little) 32771)))))

#+easy-audio-check-crc
(test bitreader-check-crc
  "Check CRC functions"
  (let ((funcs-and-crcs (list (cons #'bitreader:crc-0-8005 #x0c1e)
                              (cons #'bitreader:crc-0-04c11db7 #xac691451))))
    (loop for (func . crc) in funcs-and-crcs do
         (with-input-from-sequence (input #(1 2 3))
           ;; Set internal buffer size low to check buffer refill
           (let ((reader (bitreader:make-reader :stream input
                                                :crc-fun func)))
             (bitreader:init-crc reader)
             (bitreader:read-octet reader)
             (bitreader:read-octet reader)
             (bitreader:read-octet reader)
             (is (= crc (bitreader:get-crc reader))))))))

(in-suite flac)
(test flac-decode
  "Decode flac audio"
  (flet ((check-file (wav flac)
           (let ((tmp-name  (asdf:system-relative-pathname
                             :easy-audio/tests "tests/tmp.wav"))
                 (wav-name  (asdf:system-relative-pathname
                             :easy-audio/tests wav))
                 (flac-name (asdf:system-relative-pathname
                             :easy-audio/tests flac)))
             (flac-examples:flac2wav flac-name tmp-name)
             (is (equalp (md5:md5sum-file wav-name)
                         (md5:md5sum-file tmp-name))))))
    (check-file "tests/sample-mono.wav" "tests/sample-mono.flac")
    (check-file "tests/sample-stereo.wav" "tests/sample-stereo.flac")
    (check-file "tests/sample-stereo.wav" "tests/sample-stereo-low.flac")))

(test flac-seek
  "Test frame seek"
  (with-open-file (in (asdf:system-relative-pathname
                       :easy-audio/tests "tests/sample-stereo.flac")
                      :element-type '(unsigned-byte 8))
    (let ((reader (flac:open-flac in)))
      (map nil
           (lambda (n) (finishes (flac:seek-sample reader n)))
           '(10000 20000 30000 40000 50000)))))

(in-suite ogg)
(test ogg-restore-sync
  "Test restore sync ability"
  (with-input-from-sequence (input (prepare-input
                                    1 #x4f 3            ; Junk
                                    #x4f #x67 #x67 #x53 ; OggS
                                    #x00 #x02           ; First page of logical bitstream
                                    #x00 #x00 #x00 #x00
                                    #x00 #x00 #x00 #x00 ; 0 absolute granule position
                                    #xbe #xba #xfe #xca ; Stream serial number
                                    #x00 #x00 #x00 #x00 ; Page number
                                    #x1d #xc7 #x2d #x0a ; CRC
                                    #x01                ; 1 segment
                                    #x01                ; with length of 1 byte
                                    #x03))              ; Content
    (let ((reader (ogg:open-ogg input)))
      (is (= (ogg:restore-sync reader) 3))
      (is (equalp #(#x03) (ogg:read-packet reader))))))

(test ogg-decode
  "Decode ogg audio file"
  (flet ((check-file (wav flac)
           (let ((tmp-name  (asdf:system-relative-pathname
                             :easy-audio/tests "tests/tmp.wav"))
                 (wav-name  (asdf:system-relative-pathname
                             :easy-audio/tests wav))
                 (flac-name (asdf:system-relative-pathname
                             :easy-audio/tests flac)))
             (flac-examples:ogg2wav flac-name tmp-name)
             (is (equalp (md5:md5sum-file wav-name)
                         (md5:md5sum-file tmp-name))))))
    (check-file "tests/sample-mono.wav" "tests/sample-mono.oga")
    (check-file "tests/sample-stereo.wav" "tests/sample-stereo.oga")))

(in-suite decoders)
(test g.711-ulaw
  "Test g.711 uLaw decoder"
  (is (= (general:g.711-ulaw-decode #xff) 0))
  (is (= (general:g.711-ulaw-decode #xea) #xd4))
  (is (= (general:g.711-ulaw-decode #xda) #x022c))
  (is (= (general:g.711-ulaw-decode #xca) #x04dc))) ; And so on...

(test g.711-alaw
  "Test g.711 A-Law decoder"
  (is (= (general:g.711-alaw-decode #x55) -8))
  (is (= (general:g.711-alaw-decode #x54) -24))
  (is (= (general:g.711-alaw-decode #x40) #x-158))
  (is (= (general:g.711-alaw-decode #x70) #x-2b0)))

(in-suite wavpack)
(test wv-decode
  "Decode audio file"
  (flet ((check-file (wav wv)
           (let ((tmp-name (asdf:system-relative-pathname
                            :easy-audio/tests "tests/tmp.wav"))
                 (wav-name (asdf:system-relative-pathname
                            :easy-audio/tests wav))
                 (wv-name  (asdf:system-relative-pathname
                            :easy-audio/tests wv)))
             (handler-bind
                 ((warning #'muffle-warning))
               (wv-examples:wv2wav wv-name tmp-name))
             (is (equalp (md5:md5sum-file wav-name)
                         (md5:md5sum-file tmp-name))))))
    (check-file "tests/sample-mono.wav" "tests/sample-mono.wv")
    (check-file "tests/sample32-mono.wav" "tests/sample32-mono.wv")
    (check-file "tests/sample-stereo.wav" "tests/sample-stereo.wv")
    (check-file "tests/sample32-stereo.wav" "tests/sample32-stereo.wv")
    (check-file "tests/sample32-upsample.wav" "tests/sample32-upsample.wv")))

(test wv-seek
  "Test frame seek"
  (with-open-file (in (asdf:system-relative-pathname
                       :easy-audio/tests "tests/sample-stereo.wv")
                      :element-type '(unsigned-byte 8))
    (let ((reader (wv:open-wv in)))
      (loop for n in '(10000 20000 30000 40000 50000) do
            (finishes
              (handler-bind
                  ((warning #'muffle-warning))
                (wv:seek-sample reader n)))))))

(in-suite ape)
(test ape-decode
  "Decode ape sample file"
  (flet ((check-file (wav ape)
           (let ((tmp-name  (asdf:system-relative-pathname
                             :easy-audio/tests "tests/tmp.wav"))
                 (wav-name  (asdf:system-relative-pathname
                             :easy-audio/tests wav))
                 (ape-name (asdf:system-relative-pathname
                            :easy-audio/tests ape)))
             (ape-examples:ape2wav ape-name tmp-name)
             (is (equalp (md5:md5sum-file wav-name)
                         (md5:md5sum-file tmp-name))))))
    (check-file "tests/sample-stereo.wav" "tests/sample-stereo.ape")
    (check-file "tests/sample-mono.wav" "tests/sample-mono.ape")))

(test apev2-tags
  "Test apev2 tags reader"
  (with-open-file (input (asdf:system-relative-pathname
                          :easy-audio/tests "tests/sample-stereo.wv")
                         :element-type '(unsigned-byte 8))
    (let* ((reader (wv:open-wv input))
           (items (ape:read-apev2-tag-from-end reader))
           (item1 (find "Key1" items :key #'ape:apev2-tag-item-key :test #'string=))
           (item2 (find "Key2" items :key #'ape:apev2-tag-item-key :test #'string=)))
      (is-true (and item1 (string= (ape:apev2-tag-item-value item1) "Value1")))
      (is-true (and item2 (string= (ape:apev2-tag-item-value item2) "Value2"))))))

(in-suite core)
(defun mixed-correctly-p (output a1 a2)
  (every #'identity
         (loop for i below (length a1)
            for j from 0 by 2
            collect
              (and (= (aref output j) (aref a1 i))
                   (= (aref output (1+ j))
                      (aref a2 i))))))

(test mixchannels-2
  "Test MIXCHANNELS-2 special case"
  (let ((data1 (make-array 522 :element-type '(signed-byte 32)
                           :initial-contents
                           (loop repeat 522 collect (- (random 1000) 2000))))
        (data2 (make-array 522 :element-type '(signed-byte 32)
                           :initial-contents
                           (loop repeat 522 collect (- (random 1000) 2000)))))
    (is-true (mixed-correctly-p (core:interleave-channels (list data1 data2)) data1 data2))))

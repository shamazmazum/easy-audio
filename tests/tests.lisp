(in-package :easy-audio-tests)

(def-suite bitreader :description "Bitreader tests")
(def-suite flac      :description "Flac decoder tests")
(def-suite decoders  :description "General decoders tests")

;; Can it be done with FiveAM itself?
;; Maybe it's good idea to create suite registry here?
(defun run-tests ()
  "Run all tests"
  (explain! (run 'bitreader))
  (explain! (run 'flac))
  (explain! (run 'decoders)))
(export 'run-tests)

(in-suite bitreader)
(test bitreader-tests
  "Test low-level bitreader functions"
  (with-input-from-sequence (input #(1 2 3 128 129 4))
    ;; Set internal buffer size low to check buffer refill
    (let* ((bitreader::*buffer-size* 3)
           (reader (bitreader:make-reader :stream input)))
      #+easy-audio-check-crc
      (bitreader:init-crc reader)
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
      #+easy-audio-check-crc
      (is (= (bitreader:get-crc reader) #x48e2)))))

(in-suite flac)
(test flac-bitreader-tests
  "Test advanced bitreader for flac decoder"
  (with-input-from-sequence (input #(224 170 150   #xc3 #x88   #x7f))
    (let ((reader (bitreader:make-reader :stream input)))
      (is (= (flac::read-utf8-u32 reader) 2710))
      (is (= (flac::read-utf8-u32 reader) 200))
      (is (= (flac::read-utf8-u32 reader) 127))))

  (is (= (flac::unsigned-to-signed 13 4) -3))
  (is (= (flac::unsigned-to-signed 13 5) 13))

  (with-input-from-sequence (input #(#x00 #x8a))
    (let ((reader (bitreader:make-reader :stream input)))
      (is (= (flac::read-unary-coded-integer reader) 8))
      (is (= (flac::read-rice-signed reader 3) 13))
      ;; Check is reading operations consumed right amount of input
      (is (= (bitreader::read-to-byte-alignment reader) 0)))))
      
;; TODO in this suite: Flac bitreader is already well-covered. Cover Flac reader
;; and decoder.

(test read-simple-frame
  "Read simple stereo frame with silence.
   Covers only subframe-verbatim and subframe-constant readers."
  ;; Note that CRC8 (last byte in next line) is actually 0 (it's not checked)
  ;; 192 samples
  (with-input-from-sequence (input `#(#xff #xf8 #x19 #x12 #x00 #x00           ; Header
                                      #x00 #x0a                               ; Constant subframe with 8-bit value 10
                                      #x02 ,@(loop repeat 192 collect #x0b) ; Verbatim subframe with 8-bit values
                                      #x4d #x6b))                             ; CRC-16
    (let* ((reader (bitreader:make-reader :stream input))
           (decoded-buf
            (handler-case
                (flac:frame-decode
                 (flac:frame-reader reader nil))
              (flac:flac-error () ()))))
      (is (equalp decoded-buf
                  (list (make-array 192 :initial-element 10)
                        (make-array 192 :initial-element 11)))))))

(in-suite decoders)
(test g.711-ulaw
  "Test g.711 uLaw decoder"
  (is (= (general-decoders:g.711-ulaw-decode #xff) 0))
  (is (= (general-decoders:g.711-ulaw-decode #xea) #xd4))
  (is (= (general-decoders:g.711-ulaw-decode #xda) #x022c))
  (is (= (general-decoders:g.711-ulaw-decode #xca) #x04dc))) ; And so on...

(test g.711-alaw
  "Test g.711 A-Law decoder"
  (is (= (general-decoders:g.711-alaw-decode #x55) -8))
  (is (= (general-decoders:g.711-alaw-decode #x54) -24))
  (is (= (general-decoders:g.711-alaw-decode #x40) #x-158))
  (is (= (general-decoders:g.711-alaw-decode #x70) #x-2b0)))

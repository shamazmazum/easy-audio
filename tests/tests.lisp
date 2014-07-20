;; Copyright (c) 2012-2014, Vasily Postnicov
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met: 

;; 1. Redistributions of source code must retain the above copyright notice, this
;;   list of conditions and the following disclaimer. 
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;   this list of conditions and the following disclaimer in the documentation
;;   and/or other materials provided with the distribution. 

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; Comment it out if you do not want restrictions

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
      (is (= (bitreader:read-bits 8 reader) 4)))))

#+easy-audio-check-crc
(test bitreader-check-crc
  "Check CRC functions"
  (let ((funcs-and-crcs (list (cons #'bitreader:crc-0-8005 #x0c1e))))
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
    (let ((reader (bitreader:make-reader :stream input
                                         :crc-fun #'bitreader:crc-0-8005)))
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
    (let* ((reader (bitreader:make-reader :stream input
                                          :crc-fun #'bitreader:crc-0-8005))
           (frame  (flac:frame-reader reader nil)))
      (is (equalp (flac:frame-decode frame)
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

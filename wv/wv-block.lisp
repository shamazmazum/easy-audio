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

(in-package :easy-audio.wv)

(define-condition wv-condition ()
  ((message :reader wv-condition-message
            :initarg :message))
  (:report (lambda (c s)
	     (format s "WavPack: ~A"
		     (wv-condition-message c))))
  (:documentation "General (unspecified) WavPack condition"))
(define-condition wv-block-error (wv-condition) ())

(defvar-unbound *current-block*
    "Bound to block currently being readed by block reader")

(defclass wv-metadata ()
  ((id   :accessor wv-metadata-id
         :type (ub 8))
   (size :accessor wv-metadata-size
         :type (ub 24)) ; I do not like 'word-size' name in the
                        ; specification
   (data :accessor wv-metadata-data))) ; Usually it is unbound

(define-condition wv-unknown-metadata (wv-condition warning)
  ((metadata :reader unknown-metadata
             :initarg :metadata))
  (:report (lambda (c s)
	     (format s "WavPack: cannot understand metadata (id=~d)"
                     (wv-metadata-id (unknown-metadata c))))))

(defgeneric read-wv-metadata-body (metadata reader)
  (:method ((metadata wv-metadata) reader)
    (let* ((data-size (ash (wv-metadata-size metadata) 1))
           (data (make-array (list data-size)
                            :element-type '(ub 8))))
      (read-octet-vector data reader)
      (setf (wv-metadata-data metadata) data))

    (warn 'wv-unknown-metadata :metadata metadata)
    metadata))

;; Metadata id masks
(defconstant +meta-id-function+            #x1f)
(defconstant +meta-id-useless-for-decoder+ #x20)
(defconstant +meta-id-data-length--1+      #x40)
(defconstant +meta-id-large-block+         #x80)

;; Assigned metadata ids
(defconstant +meta-id-dummy+               #x0)
(defconstant +meta-id-decorr-terms+        #x2)
(defconstant +meta-id-decorr-weights+      #x3)
(defconstant +meta-id-decorr-samples+      #x4)
(defconstant +meta-id-entropy-vars+        #x5)
(defconstant +meta-id-hybrid-profile+      #x6)
(defconstant +meta-id-shaping-weights+     #x7)
(defconstant +meta-id-float-info+          #x8)
(defconstant +meta-id-int32-info+          #x9)
(defconstant +meta-id-wv-bitstream+        #xa)
(defconstant +meta-id-wvc-bitstream+       #xb)
(defconstant +meta-id-wvx-bitstream+       #xc)
(defconstant +meta-id-channel-info+        #xd)

;; Metadata ids of metadata needless for decoder
(defconstant +meta-id-riff-header+         #x21)
(defconstant +meta-id-riff-trailer+        #x22)
(defconstant +meta-id-config-block+        #x25)
(defconstant +meta-id-md5-checksum+        #x26)
(defconstant +meta-id-samplerate+          #x27)

(defun large-meta-p (metadata)
  (= (logand (wv-metadata-id metadata)
             +meta-id-large-block+)
     +meta-id-large-block+))

(defreader read-wv-metadata% ((make-instance 'wv-metadata) metadata)
  (wv-metadata-id (:octets 1))
  (wv-metadata-size (:octets (if (large-meta-p metadata) 3 1)) :endianness :little))

(defun read-wv-metadata (reader)
  (let ((metadata (read-wv-metadata% reader)))
    ;; FIXME: promote to needed type
    (read-wv-metadata-body metadata reader)))

;; WavPack format specs:
#|Here is the 32-byte little-endian header at the front of every WavPack block:

typedef struct {
    char ckID [4];              // "wvpk"
    uint32_t ckSize;            // size of entire block (minus 8, of course)
    uint16_t version;           // 0x402 to 0x410 are currently valid for decode
    uchar track_no;             // track number (0 if not used, like now)
    uchar index_no;             // track sub-index (0 if not used, like now)
    uint32_t total_samples;     // total samples for entire file, but this is
                                // only valid if block_index == 0 and a value of
                                // -1 indicates unknown length
    uint32_t block_index;       // index of first sample in block relative to
                                // beginning of file (normally this would start
                                // at 0 for the first block)
    uint32_t block_samples;     // number of samples in this block (0 = no audio)
    uint32_t flags;             // various flags for id and decoding
    uint32_t crc;               // crc for actual decoded data
} WavpackHeader;
|#

(defconstant +wv-id+ #x7776706b)
(defconstant +wv-id/first-octet+ #x77)

(defstruct wv-block
  (id            0 :type (ub 32))
  (size          0 :type (ub 32))

  (version       0 :type (ub 16))
  (track-number  0 :type (ub 8))
  (index-number  0 :type (ub 8))
  (total-samples 0 :type (ub 32))
  (block-index   0 :type (ub 32))
  (block-samples 0 :type (ub 32))
  (flags         0 :type (ub 32))
  (crc           0 :type (ub 32))
  ;; FIXME: "Following the 32-byte header to the end of the block are
  ;; a series of "metadata" sub-blocks". Does it mean that audio data
  ;; is in there?
  metadata)

(defmacro define-get-value/shift+mask (name-spec)
  "Define value-getting function. This function will accept an integer
   number and extract a value using defined mask and shift values like
   so: (ash (logand number mask) shift).

   NAME-SPEC can be a list (NAME SYM) or just a symbol NAME. NAME is the
   name of the function to be defined. Mask and shift values used must
   have names +FLAGS-NAME-MASK+ and +FLAGS-NAME-SHIFT+ or
   +FLAGS-SYM-MASK+ and +FLAGS-SYM-SHIFT+ if SYM is supplied."
  (let* ((name (if (atom name-spec) name-spec (first name-spec)))
         (sym  (if (atom name-spec) name-spec (second name-spec)))
         (mask  (intern (concatenate 'string "+FLAGS-" (symbol-name sym) "-MASK+")))
         (shift (intern (concatenate 'string "+FLAGS-" (symbol-name sym) "-SHIFT+"))))
    `(defun ,name (flags)
       (ash (logand flags ,mask) ,shift))))

(defconstant +flags-1-byte/sample+     #x00000000)
(defconstant +flags-2-byte/sample+     #x00000001)
(defconstant +flags-3-byte/sample+     #x00000002)
(defconstant +flags-4-byte/sample+     #x00000003)
(defconstant +flags-byte/sample-mask+  #x00000003)

(defconstant +flags-stereo-output+     #x00000000)
(defconstant +flags-mono-output+       #x00000004)
(defconstant +flags-output-mask+       #x00000004)

(defconstant +flags-lossless-mode+     #x00000000)
(defconstant +flags-hybrid-mode+       #x00000008)
(defconstant +flags-mode-mask+         #x00000008)

(defconstant +flags-stereo-true+       #x00000000)
(defconstant +flags-stereo-joint+      #x00000010)
(defconstant +flags-stereo-mask+       #x00000010)

(defconstant +flags-channels-indep+    #x00000000)
(defconstant +flags-channels-decor+    #x00000020)
(defconstant +flags-channels-mask+     #x00000020)

(defconstant +flags-flat-noise+        #x00000000)
(defconstant +flags-noise-shaping+     #x00000040)
(defconstant +flags-noise-mask+        #x00000040)

(defconstant +flags-data-integer+      #x00000000)
(defconstant +flags-data-float+        #x00000080)
(defconstant +flags-data-mask+         #x00000080)

(defconstant +flags-extended-int+      #x00000000)
(defconstant +flags-shifted-int+       #x00000100)
(defconstant +flags-int-mask+          #x00000100)

(defconstant +flags-hybrid-param/noise+    #x00000000)
(defconstant +flags-hybrid-param/bitrate+  #x00000200)
(defconstant +flags-hybrid-param-mask+     #x00000200)

(defconstant +flags-hybrid-noise-balanced+ #x00000400)

(defconstant +flags-initial-block+     #x00000800)

(defconstant +flags-final-block+       #x00001000)

(defconstant +flags-left-shift-amount-mask+ #x0003e000)
(defconstant +flags-left-shift-amount-shift+ -13)

(defconstant +flags-max-magnitude-mask+ #x007c0000)
(defconstant +flags-max-magnitude-shift+ -18)

(defconstant +flags-samplerate-mask+ #x07800000)
(defconstant +flags-samplerate-shift+ -23)

;; Bits 27-28 are ignored

(defconstant +flags-use-iir+       #x20000000)
(defconstant +flags-false-stereo+  #x40000000)
(defconstant +flags-reserved-zero+ #x80000000)

(define-get-value/shift+mask left-shift-amount)
(define-get-value/shift+mask max-magnitude)
(define-get-value/shift+mask (samplerate% samplerate))
(defun samplerate (flags)
  ;; Maybe I should use (cond ...) instead generating a whole list
  (let ((samplerate% (samplerate% flags))
        (samplerate-list (list 6000  8000  9600
                               11025 12000 16000
                               22050 24000 32000
                               44100 48000 64000
                               88200 96000 192000)))
    (declare (dynamic-extent samplerate-list))
    (nth samplerate% samplerate-list)))

;; Coding guide to myself:

;; 1) When I need to check if flag (bit) is set, use (logand x flag)
;; 2) When I need to choose which flag in the set is set, use case
;;    macro with (logand x mask)
;; 3) When I need to get a value from flags using masks and shifts, use
;;    automatically generated special functions

(defreader read-wv-block% ((make-wv-block))
    (wv-block-id            (:octets 4) :endianness :big)
    (wv-block-size          (:octets 4) :endianness :little)
    (wv-block-version       (:octets 2) :endianness :little)
    (wv-block-track-number  (:octets 1))
    (wv-block-index-number  (:octets 1))
    (wv-block-total-samples (:octets 4) :endianness :little)
    (wv-block-block-index   (:octets 4) :endianness :little)
    (wv-block-block-samples (:octets 4) :endianness :little)
    (wv-block-flags         (:octets 4) :endianness :little)
    (wv-block-crc           (:octets 4) :endianness :little))

(defun read-wv-block (reader)
  (let ((wv-block (read-wv-block% reader)))
    (if (/= (wv-block-id wv-block) +wv-id+)
        (error 'wv-block-error :message "WavPack ckID /= 'wvpk'"))

    (let ((version (wv-block-version wv-block)))
      (if (or (< version #x402) ; FIXME: is this range inclusive?
              (> version #x410))
          (error 'wv-block-error :message "Unsupported WavPack block version")))

    (if (/= (logand (wv-block-flags wv-block) +flags-reserved-zero+) 0)
        ;; Specification says we should "refuse to decode if set"
        (error 'wv-block-error :message "Reserved flag is set to 1"))

    (let ((sub-blocks-size (- (wv-block-size wv-block) 24))
          (*current-block* wv-block))
      (if (< sub-blocks-size 0)
          (error 'wv-block-error :message "Sub-blocks size is less than 0"))
      (setf (wv-block-metadata wv-block)
            (loop with bytes-read = 0
               while (< bytes-read sub-blocks-size)
               for metadata = (read-wv-metadata reader)
               do (incf bytes-read (+ 1 (if (large-meta-p metadata) 3 1)
                                      (ash (wv-metadata-size metadata) 1)))
               collect metadata
               finally (if (> bytes-read sub-blocks-size)
                           (error 'wv-block-error "Read more sub-block bytes than needed")))))

    wv-block))

(defun restore-sync (reader)
  "Go to the first block in the stream"
  (peek-octet reader +wv-id/first-octet+)
  (let ((position (reader-position reader)))
    (handler-case
        (prog1
            (wv-block-block-index (read-wv-block reader))
          (reader-position reader position))
      (wv-block-error ()
        (reader-position reader (1+ position))
        (restore-sync reader)))))

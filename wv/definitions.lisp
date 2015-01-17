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

;; Conditions
(define-condition wv-condition ()
  ((message :reader wv-condition-message
            :initarg :message))
  (:report (lambda (c s)
	     (format s "WavPack: ~A"
		     (wv-condition-message c))))
  (:documentation "General (unspecified) WavPack condition"))

(define-condition block-error (wv-condition) ())

(define-condition unknown-metadata (wv-condition warning)
  ((metadata :reader unknown-metadata
             :initarg :metadata))
  (:report (lambda (c s)
	     (format s "WavPack: cannot understand metadata (id=~d)"
                     (metadata-id (unknown-metadata c))))))
;; --------
;; Metadata
;; --------
(defclass metadata ()
  ((id          :accessor metadata-id
                :type (ub 8))
   (size        :accessor metadata-size
                :type (ub 24)) ; NB: in bytes, not in words
   (actual-size :accessor metadata-actual-size
                :type (ub 24)) ; Size or size-1
   (data        :accessor metadata-data))) ; Usually it is unbound

(defclass metadata-decorr (metadata)
  ((decorr-passes :accessor metadata-decorr-passes)))

(defclass metadata-decorr-terms (metadata-decorr) ())
(defclass metadata-decorr-weights (metadata-decorr) ())
(defclass metadata-decorr-samples (metadata-decorr)
  ((decorr-samples :accessor metadata-decorr-samples)))

(defclass metadata-entropy (metadata)
  ((entropy-median :accessor metadata-entropy-median)))

(defclass metadata-residual (metadata)
  ((reader :type reader
           :accessor metadata-residual-reader)))
(defclass metadata-wv-residual (metadata-residual) ())

(defgeneric read-metadata-body (metadata reader))

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

;; --------------
;; WavPack blocks
;; --------------
(defconstant +wv-id+ #x7776706b)
(defconstant +wv-id/first-octet+ #x77)

(defstruct decorr-pass
  (term      0 :type (sb 32)) ; FIXME: these fields are signed
  (delta     0 :type (sb 32))
  weight aweight sum)

(defstruct (wv-block (:conc-name block-)
                     (:print-function
                      (lambda (struct stream k)
                        (declare (ignore k))
                        (print-unreadable-object (struct stream
                                                  :type t :identity t)
                          (format stream "samples ~d..~d"
                                  (block-block-index struct)
                                  (+ (block-block-index struct)
                                     (block-block-samples struct)))))))
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
  metadata
  decorr-passes
  decorr-samples
  entropy-median
  residual)

(defvar-unbound *current-block*
    "Bound to block currently being readed by block reader")

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

(define-get-value/shift+mask left-shift-amount)
(define-get-value/shift+mask max-magnitude)
(define-get-value/shift+mask (samplerate% samplerate))

(defun bit-set-p (value mask)
  (/= (logand value mask) 0))

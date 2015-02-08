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

(defvar *residual-buffers* nil)

(defun samplerate (flags)
  (let ((samplerate% (samplerate% flags))
        (samplerate-list (list 6000  8000  9600
                               11025 12000 16000
                               22050 24000 32000
                               44100 48000 64000
                               88200 96000 192000)))
    (declare (dynamic-extent samplerate-list))
    (nth samplerate% samplerate-list)))

(defun get-med (median i)
  (declare (optimize (speed 3))
           (type (sa-ub 32) median))
  (1+ (ash (aref median i) -4)))

(macrolet ((define-median-inc/dec (divs)
             `(progn
                ,@(loop for div in divs
                        for i from 0 by 1 collect
                       (let ((inc-name (intern (format nil "INC-MED~d" i)))
                             (dec-name (intern (format nil "DEC-MED~d" i))))
                         `(progn
                            (defun ,inc-name (median)
                              (declare (optimize (speed 3))
                                       (type (sa-ub 32) median))
                              (incf (aref median ,i)
                                    (* 5 (floor (+ ,div (aref median ,i)) ,div)))
                              median)

                            (defun ,dec-name (median)
                              (declare (optimize (speed 3))
                                       (type (sa-ub 32) median))
                              (decf (aref median ,i)
                                    (* 2 (floor (+ ,div (aref median ,i) -2) ,div)))
                              median)))))))
  (define-median-inc/dec (128 64 32)))

(defun decode-residual (wv-block)
  (declare (optimize (speed 3)))
  (if (bit-set-p (block-flags wv-block) +flags-hybrid-mode+)
      (error 'block-error :message "Cannot work with hybrid mode"))
  (let ((metadata-residual (find 'metadata-wv-residual (the list (block-metadata wv-block))
                                 :key #'type-of))
        (channels (if (bit-set-p (block-flags wv-block) +flags-mono-output+) 1 2))
        (samples (block-block-samples wv-block)))

    (if metadata-residual
        (let (#|(residual (loop for i below channels collect
                             (let ((residual-buffer (nth i *residual-buffers*)))
                               (if (and residual-buffer
                                        (= (length (the (sa-sb 32) residual-buffer)) samples))
                                   (map-into residual-buffer (lambda (x) (declare (ignore x)) 0) residual-buffer)
                                   (make-array samples
                                               :element-type '(sb 32)
                                               :initial-element 0)))))|#
              (residual (if *residual-buffers*
                            (mapc (lambda (buffer)
                                    (declare (type (sa-sb 32) buffer))
                                    (map-into buffer (lambda (x) (declare (ignore x)) 0) buffer))
                                  *residual-buffers*)
                            (loop repeat channels collect (make-array samples
                                                                      :element-type '(sb 32)
                                                                      :initial-element 0))))
              (coded-residual-reader (metadata-residual-reader metadata-residual))
              (medians (block-entropy-median wv-block))
              holding-one holding-zero zero-run-met)

          (do* ((i       0)
                (sample  0 (ash i (- 1 channels)))
                (channel 0 (logand i (1- channels))))
               ((= sample samples) sample)
            (declare (type (ub 32) i sample channel))

            (if (> sample samples)
                (error 'block-error :message "Accidentally read too much samples"))
            (cond
              ((and (< (aref (the (sa-ub 32) (first medians)) 0) 2)
                    (or (null (second medians))
                        (< (aref (the (sa-ub 32) (second medians)) 0) 2))
                    (not holding-one)
                    (not holding-zero)
                    (not zero-run-met))
               ;; Run of zeros - do nothing
               (let ((zero-length (read-elias-code coded-residual-reader)))
                 (when (/= zero-length 0)
                   (incf i zero-length)
                   (setq medians (loop repeat channels collect
                                      (make-array 3 :element-type '(ub 32) :initial-element 0)))))
               (setq zero-run-met t))

              (t
               (setq zero-run-met nil)
               (let ((ones-count 0))
                 (declare (type non-negative-fixnum ones-count))
                 (cond
                   (holding-zero (setq holding-zero nil))
                   (t
                    (setq ones-count (read-unary-coded-integer coded-residual-reader #.(1+ 16)))
                    (when (>= ones-count 16)
                      (if (= ones-count 17) (error 'block-error :message "Invalid residual code"))
                      (incf ones-count (read-elias-code coded-residual-reader)))
                    (psetq
                     holding-one (/= (logand ones-count 1) 0)
                     ones-count (+ (ash ones-count -1) (if holding-one 1 0)))
                    (setq holding-zero (not holding-one))))

                 (let ((median (nth channel medians))
                       (low 0)
                       (high 0))
                   (declare (type (sb 32) low high)
                            (type (sa-ub 32) median))
                   (cond
                     ((= ones-count 0)
                      (setq high (1- (get-med median 0)))
                      (dec-med0 median))
                     (t
                      (setq low (get-med median 0))
                      (inc-med0 median)
                      (cond
                        ((= ones-count 1)
                         (setq high (+ low (get-med median 1) -1))
                         (dec-med1 median))
                        (t
                         (setq low (+ low (get-med median 1)))
                         (inc-med1 median)
                         (cond
                           ((= ones-count 2)
                            (setq high (+ low (get-med median 2) -1))
                            (dec-med2 median))
                           (t
                            (setq low (+ low (the (sb 32)
                                                  (* (get-med median 2)
                                                     (- ones-count 2))))
                                  high (+ low (get-med median 2) -1))
                            (inc-med2 median)))))))
                   (incf low (read-code coded-residual-reader (- high low)))
                   (setf (aref (the (sa-sb 32) (nth channel residual)) sample)
                         (if (= (residual-read-bit coded-residual-reader) 1)
                             (lognot low) low))))
               (incf i))))
          (read-to-byte-alignment coded-residual-reader)
          ;; For some reason residual reader looses some useful ("actual") data at the end
          ;; and it seems to be OK. But check if we loose too much
          (if (> (- (the (ub 24) (reader-length coded-residual-reader))
                    (the (ub 24) (reader-position coded-residual-reader))) 1)
              (error 'block-error :message "Too much useful data is lost in residual reader"))
          (setf (block-residual wv-block) residual))))
  wv-block)

;; Coding guide to myself:

;; 1) When I need to check if flag (bit) is set, use bit-set-p function
;; 2) When I need to choose which flag in the set is set, use cond
;;    macro with (logand x mask)
;; 3) When I need to get a value from flags using masks and shifts, use
;;    automatically generated special functions

(defreader read-wv-block% ((make-wv-block))
    (block-id            (:octets 4) :endianness :big)
    (block-size          (:octets 4) :endianness :little)
    (block-version       (:octets 2) :endianness :little)
    (block-track-number  (:octets 1))
    (block-index-number  (:octets 1))
    (block-total-samples (:octets 4) :endianness :little)
    (block-block-index   (:octets 4) :endianness :little)
    (block-block-samples (:octets 4) :endianness :little)
    (block-flags         (:octets 4) :endianness :little)
    (block-crc           (:octets 4) :endianness :little))

(defun read-wv-block (reader)
  (declare (optimize (speed 3)))
  (let ((wv-block (read-wv-block% reader)))
    (if (/= (block-id wv-block) +wv-id+)
        (error 'block-error :message "WavPack ckID /= 'wvpk'"))

    (let ((version (block-version wv-block)))
      (if (or (< version #x402) ; FIXME: is this range inclusive?
              (> version #x410))
          (error 'block-error :message "Unsupported WavPack block version")))

    (if (bit-set-p (block-flags wv-block) +flags-reserved-zero+)
        ;; Specification says we should "refuse to decode if set"
        (error 'block-error :message "Reserved flag is set to 1"))

    (let ((sub-blocks-size (- (block-size wv-block) 24))
          (*current-block* wv-block))
      (if (< sub-blocks-size 0)
          (error 'block-error :message "Sub-blocks size is less than 0"))
      (setf (block-metadata wv-block)
            (loop with bytes-read fixnum = 0
               while (< bytes-read sub-blocks-size)
               for metadata = (read-metadata reader)
               do (incf bytes-read (+ 1 (if (bit-set-p (metadata-id metadata)
                                                       +meta-id-large-block+) 3 1)
                                      (the (ub 24) (metadata-size metadata))))
               collect metadata
               finally (if (> bytes-read sub-blocks-size)
                           (error 'block-error :message "Read more sub-block bytes than needed")))))

    (decode-residual wv-block)))

(defun restore-sync (reader)
  "Go to the first block in the stream"
  (peek-octet reader +wv-id/first-octet+)
  (let ((position (reader-position reader)))
    (handler-case
        (prog1
            (block-block-index (read-wv-block reader))
          (reader-position reader position))
      (block-error ()
        (reader-position reader (1+ position))
        (restore-sync reader)))))

(defun make-wv-block-reader (reader)
  (let* ((first-block
          (let ((position (reader-position reader)))
            (prog1
                (read-wv-block reader)
              (reader-position reader position))))
         (buffers (loop repeat (if (bit-set-p (block-flags first-block) +flags-mono-output+) 1 2) collect
                       (make-array (block-block-samples first-block) :element-type '(signed-byte 32)))))
    (flet ((read-wv-block% (reader)
             (let ((*residual-buffers* buffers))
               (read-wv-block reader))))
      #'read-wv-block%)))

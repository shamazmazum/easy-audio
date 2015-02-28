;; Copyright (c) 2012-2015, Vasily Postnicov
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

(defun read-wv-block-multichannel% (reader)
  (let ((first-block (read-wv-block reader)))
    (if (not (flag-set-p first-block +flags-initial-block+))
        (error 'lost-sync :message "Lost sync: the first block in multichannel configuration is not initial"))
    (if (flag-set-p first-block +flags-final-block+) (list first-block)
        (cons first-block
              (loop for wv-block = (read-wv-block reader)
                    collect wv-block
                    until (flag-set-p wv-block +flags-final-block+))))))

(defun read-wv-block-multichannel (reader)
  "Read a list of Wavpack blocks in an multichannel configuration
   different from 1.0 or 2.0. Each block in the list can itself
   be mono or stereo. Read the format specification for the
   details."
  (restart-case
      (read-wv-block-multichannel% reader)
    (read-new-block-multichannel ()
      :report "Restore sync and read a new multichannel blocks list"
      (restore-sync-multichannel reader)
      (read-wv-block-multichannel reader))))

(defun restore-sync-multichannel (reader)
  (restore-sync reader)
  (labels ((restore-sync-multichannel% (reader)
             (let ((reader-position (reader-position reader)))
               (handler-case
                   (prog1
                       (block-block-index (first
                                           (read-wv-block-multichannel reader)))
                     (reader-position reader reader-position))
                 (lost-sync ()
                   (reader-position reader (1+ reader-position))
                   (restore-sync-multichannel% reader))))))
    (restore-sync-multichannel% reader)))

(defun read-new-block (c)
  "Function to be supplied to HANDLER-BIND in order to deal with LOST-SYNC condition.
   It transfers control to RESTORE-SYNC-AND-RETRY-BLOCK or to RESTORE-SYNC-AND-RETRY-BLOCK-MULTICH
   depending on the situation. A newly read block or a list of blocks is always returned from
   HANDLER-BIND if BITREADER-EOF is not signalled"
  (declare (ignore c))
  (if (find-restart 'read-new-block-multichannel)
      (invoke-restart 'read-new-block-multichannel)
      (invoke-restart 'read-new-block-single)))

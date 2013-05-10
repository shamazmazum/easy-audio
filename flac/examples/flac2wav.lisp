;; Copyright (c) 2012, Vasily Postnicov
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

(in-package :easy-audio.flac-examples)

;; Works only for 8 or 16 bps
(defun flac2wav (flac-name wav-name)
  "Decodes flac to wav. Works only for 8 or 16 bps,
   fixed block size and if total samples in stream is known"
  (with-open-file (in flac-name :element-type '(unsigned-byte 8))
    (multiple-value-bind (blocks in-reader) (open-flac in)
      (let* ((streaminfo (the streaminfo (first blocks)))
             
             (minblocksize (streaminfo-minblocksize streaminfo))
             (maxblocksize (streaminfo-maxblocksize streaminfo))
             (totalsamples (streaminfo-totalsamples streaminfo))
             (blocksize minblocksize)
             
             (bps (streaminfo-bitspersample streaminfo))
             (channels (streaminfo-channels streaminfo))
             (samplerate (streaminfo-samplerate streaminfo)))
      
        (if (= 0 totalsamples)
            (error "Number of total samples is unknown"))
        (if (/= minblocksize maxblocksize)
            (error "Block size must be fixed"))
      
        (if (not (or (= 8 bps)
                     (= 16 bps)))
            (error "Bps must be 16 or 8"))
      
        (with-open-file
         (out-stream wav-name
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create
                     :element-type '(unsigned-byte 8))

         ;; Fill headers
         (write-pcm-wav-header out-stream
                               :samplerate samplerate
                               :channels channels
                               :bps bps
                               :totalsamples totalsamples))
                        
      
        (with-open-file
         (out-stream wav-name
                     :direction :output
                     :if-exists :append
                     :element-type (list 'signed-byte bps))
         
         (file-position out-stream
                        (/ (ash 44 3) bps))
		      
         (let ((buf (make-array (* blocksize channels)
                                :element-type '(signed-byte 32))))
			
           (loop for i below totalsamples
                 by blocksize do
                 (write-sequence (mixchannels buf (frame-decode (frame-reader in-reader streaminfo)))
                                 out-stream))))))))

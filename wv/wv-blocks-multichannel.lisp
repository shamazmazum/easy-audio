(in-package :easy-audio.wv)

(defun read-wv-block-multichannel% (reader)
  (let ((first-block (read-wv-block reader)))
    (when (not (flag-set-p first-block +flags-initial-block+))
      (error 'lost-sync
             :format-control
             "Lost sync: the first block in multichannel configuration is not initial"))
    (if (flag-set-p first-block +flags-final-block+)
        (list first-block)
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
  "Restore sync in multichannel configuration.
The reader position is set to the beginning of the first channel block."
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
  "Function to be supplied to @c(handler-bind) in order to deal with @c(lost-sync)
condition. It transfers control to @c(read-new-block-single) or to
@c(read-new-block-multichannel) depending on the situation. A newly read block or a
list of blocks is always returned from @c(handler-bind) if @c(bitreader-eof) is not
signalled."
  (declare (ignore c))
  (if (find-restart 'read-new-block-multichannel)
      (invoke-restart 'read-new-block-multichannel)
      (invoke-restart 'read-new-block-single)))

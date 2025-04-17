(in-package :easy-audio.flac)

(sera:-> decode-subframe-postprocess (subframe-header (sa-sb 32))
         (values (sa-sb 32) &optional))
(defun decode-subframe-postprocess (header output)
  (declare (optimize (speed 3)))
  (let ((wasted-bits (subframe-header-wasted-bps header)))
    (if (not (zerop wasted-bits))
        (map-into output
                  (lambda (sample)
                    (ash sample wasted-bits))
                  output)
        output)))

(sera:-> decode-subframe-constant (subframe-constant)
         (values (sa-sb 32) &optional))
(defun decode-subframe-constant (subframe)
  (declare (optimize (speed 3)))
  (let ((header (subframe-constant-header subframe)))
    (decode-subframe-postprocess
     header (make-array (subframe-header-block-size header)
                        :element-type '(sb 32)
                        :initial-element (subframe-constant-value subframe)))))

(sera:-> decode-subframe-verbatim (subframe-verbatim)
         (values (sa-sb 32) &optional))
(defun decode-subframe-verbatim (subframe)
  (declare (optimize (speed 3)))
  (decode-subframe-postprocess
   (subframe-verbatim-header subframe)
   (subframe-verbatim-data   subframe)))

(sera:-> decode-subframe-fixed (subframe-fixed)
         (values (sa-sb 32) &optional))
(defun decode-subframe-fixed (subframe)
  ;; Decodes subframe destructively modifiying it
  (declare (optimize (speed 3)))
  (let* ((header   (subframe-fixed-header   subframe))
         (residual (subframe-fixed-residual subframe))
         (order    (subframe-fixed-order    subframe))
         (blocksize (subframe-header-block-size header)))
    (decode-subframe-postprocess
     header
     (if (zerop order)
         (copy-seq residual)
         (let ((data (make-array blocksize :element-type '(sb 32))))
           (replace data residual :end1 order)
           (cond
             ;; 0 - out-buf contains decoded data
             ((= order 1)
              (loop for i from 1 below blocksize do
                    (setf (aref data i)
                          (+ (aref residual i)
                             (aref data (1- i))))))
             ((= order 2)
              (loop for i from 2 below blocksize do
                    (setf (aref data i)
                          (+
                           (aref residual i)
                           (+ (* (aref data (- i 1)) 2))
                           (- (* (aref data (- i 2))))))))
             ((= order 3)
              (loop for i from 3 below blocksize do
                    (setf (aref data i)
                          (+
                           (aref residual i)
                           (+ (* (aref data (- i 1)) 3))
                           (- (* (aref data (- i 2)) 3))
                           (+ (* (aref data (- i 3))))))))
             ((= order 4)
              (loop for i from 4 below blocksize do
                    (setf (aref data i)
                          (+
                           (aref residual i)
                           (+ (* (aref data (- i 1)) 4))
                           (- (* (aref data (- i 2)) 6))
                           (+ (* (aref data (- i 3)) 4))
                           (- (* (aref data (- i 4)))))))))
           data)))))

(sera:-> decode-subframe-lpc (subframe-lpc)
         (values (sa-sb 32) &optional))
(defun decode-subframe-lpc (subframe)
  (declare (optimize (speed 3)))
  (let* ((header   (subframe-lpc-header          subframe))
         (residual (subframe-lpc-residual        subframe))
         (shift    (subframe-lpc-coeff-shift     subframe))
         (order    (subframe-lpc-order           subframe))
         (coeff    (subframe-lpc-predictor-coeff subframe))
         (blocksize (subframe-header-block-size header))
         (data (make-array blocksize :element-type '(sb 32))))
    (replace data residual :end1 order)
    (loop for i from order below blocksize do
          (setf (aref data i)
                (+ (aref residual i)
                   (the fixnum
                        (ash
                         (loop for j below order sum
                               (* (aref coeff j)
                                  (aref data (- i j 1)))
                               fixnum)
                         (- shift))))))
    (decode-subframe-postprocess header data)))

(sera:-> decode-subframe (subframe)
         (values (sa-sb 32) &optional))
(declaim (inline decode-subframe))
(defun decode-subframe (subframe)
  (etypecase subframe
    (subframe-verbatim (decode-subframe-verbatim subframe))
    (subframe-constant (decode-subframe-constant subframe))
    (subframe-fixed    (decode-subframe-fixed    subframe))
    (subframe-lpc      (decode-subframe-lpc      subframe))))

(defun decode-frame (frame)
  "Decode a frame. Returns list of decoded audio buffers (one buffer for each channel)."
  (declare (optimize (speed 3)))
  (let ((decoded-subframes
         (mapcar #'decode-subframe (frame-subframes frame)))
        (assignment (frame-channel-assignment frame)))
    (declare (type non-negative-fixnum assignment))

    (when (<= assignment +max-channels+)
      (return-from decode-frame decoded-subframes))
    (when (/= 2 (length decoded-subframes))
      (error 'flac-error
             :format-control "Bad channel assignment/number of subframes"))

    (let ((left  (first  decoded-subframes))
          (right (second decoded-subframes)))
      (declare (type (sa-sb 32) left right))
      (cond
       ((= +left-side+ assignment)
        (map-into right #'- left right))
       ((= +right-side+ assignment)
        (map-into left #'+ left right))
       ((= +mid-side+ assignment)
        (loop for i below (frame-block-size frame)
              for side = (aref right i)
              for mid = (logior
                         (ash (aref left i) 1)
                         (logand side 1))
              do
              (setf (aref left i)
                    (ash (+ mid side) -1)
                    (aref right i)
                    (ash (- mid side) -1))))))
    decoded-subframes))

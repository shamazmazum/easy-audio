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
  (let* ((header (subframe-constant-header subframe))
         (out-buf (subframe-header-out-buf header))
         (constant (subframe-constant-value subframe)))
    (decode-subframe-postprocess header (fill out-buf constant))))

(sera:-> decode-subframe-verbatim (subframe-verbatim)
         (values (sa-sb 32) &optional))
(defun decode-subframe-verbatim (subframe)
  (declare (optimize (speed 3)))
  (let ((header (subframe-verbatim-header subframe)))
    (decode-subframe-postprocess header (subframe-header-out-buf header))))

(sera:-> decode-subframe-fixed (subframe-fixed)
         (values (sa-sb 32) &optional))
(defun decode-subframe-fixed (subframe)
  ;; Decodes subframe destructively modifiying it
  (declare (optimize (speed 3)))
  (let* ((header (subframe-fixed-header subframe))
         (out-buf (subframe-header-out-buf header))
         (order (subframe-fixed-order subframe))
         (len (length out-buf)))
    (cond
     ;; 0 - out-buf contains decoded data
     ((= order 1)
      (loop for i from 1 below len do
            (incf (aref out-buf i)
                  (aref out-buf (1- i)))))
     ((= order 2)
      (loop for i from 2 below len do
            (incf (aref out-buf i)
                  (- (ash (aref out-buf (1- i)) 1)
                     (aref out-buf (- i 2))))))
     ((= order 3)
      (loop for i from 3 below len do
            (incf (aref out-buf i)
                  (+ (ash (- (aref out-buf (1- i))
                             (aref out-buf (- i 2))) 1)
                     (- (aref out-buf (1- i))
                             (aref out-buf (- i 2)))
                     (aref out-buf (- i 3))))))
     ((= order 4)
      (loop for i from 4 below len do
            (incf (aref out-buf i)
                  (- (ash (+ (aref out-buf (1- i))
                             (aref out-buf (- i 3))) 2)
                     (+ (ash (aref out-buf (- i 2)) 2)
                        (ash (aref out-buf (- i 2)) 1))
                     (aref out-buf (- i 4)))))))
    (decode-subframe-postprocess header out-buf)))

(sera:-> decode-subframe-lpc (subframe-lpc)
         (values (sa-sb 32) &optional))
(defun decode-subframe-lpc (subframe)
  (declare (optimize (speed 3)))
  (let* ((header (subframe-lpc-header subframe))
         (out-buf (subframe-header-out-buf header))
         (shift (subframe-lpc-coeff-shift subframe))
         (order (subframe-lpc-order subframe))
         (coeff (subframe-lpc-predictor-coeff subframe)))
    (loop for i from order below (length out-buf) do
          (incf (aref out-buf i)
                (the fixnum
                     (ash
                      (loop for j below order sum
                            (* (aref coeff j)
                               (aref out-buf (- i j 1)))
                            fixnum)
                      (- shift)))))
    (decode-subframe-postprocess header out-buf)))

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
  "Decode a frame destructively modifying (and garbaging) all subframes within.
Returns list of decoded audio buffers (one buffer for each channel)."
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

    (destructuring-bind (left right) decoded-subframes
      (declare (type (sa-sb 32) left right))
      (cond
       ((= +left-side+ assignment)
        ;; Maybe just a loop?
        (map-into right #'-
                  left right))

       ((= +right-side+ assignment)
        (map-into left #'+
                  left right))

       ((= +mid-side+ assignment)
        (let ((block-size (frame-block-size frame)))
          (declare (type fixnum block-size))
          (dotimes (i block-size)
            (let* ((side (aref right i))
                   (mid (logior
                         (ash (aref left i) 1)
                         (logand side 1))))

              (setf (aref left i)
                    (ash (+ mid side) -1)
                    (aref right i)
                    (ash (- mid side) -1))))))))
    decoded-subframes))

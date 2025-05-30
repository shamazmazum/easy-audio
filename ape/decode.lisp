(in-package :easy-audio.ape)

(defconstant +history-size+ 512)
(defconstant +predictor-order+ 8)
(defconstant +predictor-size+ 50)

(defconstant +ydelaya+ (+ 18 (* +predictor-order+ 4)))
(defconstant +ydelayb+ (+ 18 (* +predictor-order+ 3)))
(defconstant +xdelaya+ (+ 18 (* +predictor-order+ 2)))
(defconstant +xdelayb+ (+ 18 (* +predictor-order+ 1)))

(defconstant +yadaptcoeffsa+ 18)
(defconstant +xadaptcoeffsa+ 14)
(defconstant +yadaptcoeffsb+ 10)
(defconstant +xadaptcoeffsb+  5)

(define-constant +predictor-versions+
    '(3950 3930 0)
  :test #'equalp)

(define-constant +filter-orders+
    '(()
      (16)
      (64)
      (32 256)
      (16 256 1280))
  :test #'equalp)

(define-constant +fracbits+
    '(()
      (11)
      (11)
      (10 13)
      (11 13 15))
  :test #'equalp)

(define-constant +coeffs-3930+
    (make-array
     4
     :element-type '(sb 32)
     :initial-contents '(98 -109 317 360))
  :test #'equalp)

(declaim (inline dot-product))
(defun dot-product (x y &key (start1 0) (start2 0))
  (loop
    for i from start1 below (length x)
    for j from start2 below (length y)
    sum (* (aref x i) (aref y j)) fixnum))

(declaim (inline zeros))
(defun zeros (n &key (type '(sb 32)))
  (make-array n
              :element-type type
              :initial-element 0))

(defun apply-filter (entropy order fracbits)
  (declare (type (sa-sb 32) entropy)
           (type (integer 1 15) fracbits)
           (type non-negative-fixnum order)
           (optimize (speed 3)))
  (let ((coeffs (zeros order :type '(sb 32)))
        (buffer (zeros (+ +history-size+ (ash order 1))
                       :type '(sb 32)))
        (avg 0))
    (declare (type (sb 32) avg))
    (dotimes (i (length entropy))
      (let ((buffer-idx (rem i +history-size+))
            (current-data (aref entropy i)))

        ;; Copy data from back to start when... we need to
        (when (zerop buffer-idx)
          (loop for i below (ash order 1) do
            (setf (aref buffer i)
                  (aref buffer (+ i +history-size+)))))

        ;; Decoded output
        (let ((new-data
                (+ current-data
                   (ash (+ (ash 1 (1- fracbits))
                           (dot-product
                            coeffs buffer
                            :start2 (+ buffer-idx order)))
                        (- fracbits)))))

          ;; Update filter coeffs
          (loop for i below order do
            (setf (aref coeffs i)
                  (- (aref coeffs i)
                     (* (aref buffer (+ buffer-idx i))
                        (signum current-data)))))

          (setf (aref entropy i)
                new-data
                (aref buffer (+ buffer-idx (ash order 1)))
                (clamp new-data -32768 32767)
                (aref buffer (+ buffer-idx order))
                (cond
                  ((<= (abs new-data)
                       (truncate (* avg 4) 3))
                   (- (ash (signum new-data) 3)))
                  ((<= (abs new-data)
                       (* avg 3))
                   (- (ash (signum new-data) 4)))
                  (t
                   (- (ash (signum new-data) 5)))))
          (incf avg (truncate (- (abs new-data) avg) 16))

        (symbol-macrolet
            ((x-1 (aref buffer (+ buffer-idx order -1)))
             (x-2 (aref buffer (+ buffer-idx order -2)))
             (x-8 (aref buffer (+ buffer-idx order -8))))
          (setf x-1 (ash x-1 -1)
                x-2 (ash x-2 -1)
                x-8 (ash x-8 -1)))))))
  entropy)

;; TODO: Maybe refactor this shit
(defun make-predictor-updater-stereo (history delay-a delay-b adapt-a adapt-b)
  (declare (type (sa-sb 32) history)
           (type (sb 32) delay-a delay-b adapt-a adapt-b)
           (optimize (speed 3)))
  (let ((last-a   0)
        (filter-a 0)
        (filter-b 0)
        (coeffs-a (copy-seq +coeffs-3930+))
        (coeffs-b (zeros 5)))
    (declare (type (sb 32) last-a filter-a filter-b)
             (type (sa-sb 32) coeffs-a coeffs-b))
    (flet ((%go (x other-x idx)
             (declare (type (sb 32) x other-x)
                      (type non-negative-fixnum idx))
             (let ((history-idx (rem idx +history-size+)))
               (symbol-macrolet
                   ;; Oh shit, this is almost C stuff translated as is
                   ;; Can this be rewritten in more lispy way?
                   ((hist-delay-a   (aref history (+ history-idx delay-a)))
                    (hist-delay-a-1 (aref history (+ history-idx delay-a -1)))

                    (hist-delay-b   (aref history (+ history-idx delay-b)))
                    (hist-delay-b-1 (aref history (+ history-idx delay-b -1)))

                    (hist-adapt-a   (aref history (+ history-idx adapt-a)))
                    (hist-adapt-a-1 (aref history (+ history-idx adapt-a -1)))

                    (hist-adapt-b   (aref history (+ history-idx adapt-b)))
                    (hist-adapt-b-1 (aref history (+ history-idx adapt-b -1))))
                 (let ((shit (- other-x (ash (* 31 filter-b) -5))))
                   (declare (type (sb 32) shit))
                   (setf hist-delay-a last-a
                         hist-adapt-a (* -1 (signum last-a))
                         hist-delay-a-1 (- last-a hist-delay-a-1)
                         hist-adapt-a-1 (* -1 (signum hist-delay-a-1))
                         hist-delay-b shit
                         hist-adapt-b (* -1 (signum shit))
                         hist-delay-b-1 (- shit hist-delay-b-1)
                         hist-adapt-b-1 (* -1 (signum hist-delay-b-1))
                         filter-b other-x)
                   (let ((prediction-a (dot-product history coeffs-a
                                                    :start1 (+ history-idx delay-a -3)))
                         (prediction-b (dot-product history coeffs-b
                                                    :start1 (+ history-idx delay-b -4))))
                     (declare (type (sb 32) prediction-a prediction-b))
                     (setf last-a
                           (+ x (ash (+ prediction-a (ash prediction-b -1)) -10))
                           filter-a
                           (+ last-a (ash (* 31 filter-a) -5)))))

                 (let ((sign (signum x)))
                   (dotimes (i 4)
                     (declare (type fixnum i))
                     (decf (aref coeffs-a (- 3 i))
                           (* sign (aref history (+ history-idx adapt-a (- i))))))
                   (dotimes (i 5)
                     (declare (type fixnum i))
                     (decf (aref coeffs-b (- 4 i))
                           (* sign (aref history (+ history-idx adapt-b (- i)))))))))
             filter-a))
      #'%go)))

(defun make-predictor-updater-mono (history delay-a adapt-a)
  (declare (type (sa-sb 32) history)
           (type (sb 32) delay-a adapt-a)
           (optimize (speed 3)))
  (let ((last-a   0)
        (filter-a 0)
        (coeffs-a (copy-seq +coeffs-3930+)))
    (declare (type (sb 32) last-a filter-a)
             (type (sa-sb 32) coeffs-a))
    (flet ((%go (x idx)
             (declare (type (sb 32) x)
                      (type non-negative-fixnum idx))
             (let ((history-idx (rem idx +history-size+)))
               (symbol-macrolet
                   ;; Oh shit, this is almost C stuff translated as is
                   ;; Can this be rewritten in more lispy way?
                   ((hist-delay-a   (aref history (+ history-idx delay-a)))
                    (hist-delay-a-1 (aref history (+ history-idx delay-a -1)))

                    (hist-adapt-a   (aref history (+ history-idx adapt-a)))
                    (hist-adapt-a-1 (aref history (+ history-idx adapt-a -1))))
                 (setf hist-delay-a last-a
                       hist-delay-a-1 (- last-a hist-delay-a-1)
                       hist-adapt-a (* -1 (signum last-a))
                       hist-adapt-a-1 (* -1 (signum hist-delay-a-1)))
                 (let ((prediction-a (dot-product history coeffs-a
                                                  :start1 (+ history-idx delay-a -3))))
                   (declare (type (sb 32) prediction-a))
                   (setf last-a
                         (+ x (ash prediction-a -10))
                         filter-a
                         (+ last-a (ash (* 31 filter-a) -5)))))

               (let ((sign (signum x)))
                 (dotimes (i 4)
                   (declare (type fixnum i))
                   (decf (aref coeffs-a (- 3 i))
                         (* sign (aref history (+ history-idx adapt-a (- i))))))))
             filter-a))
      #'%go)))

(sera:-> predictor-update/3950-stereo (frame)
         (values frame &optional))
(defun predictor-update/3950-stereo (frame)
  (declare (optimize (speed 3)))
  ;; Seems like this function cannot be applyed to all channels
  ;; independently (like APPLY-FILTER)
  (let* ((history (zeros (+ +history-size+ +predictor-size+)))
         (update-y (make-predictor-updater-stereo
                    history +ydelaya+ +ydelayb+ +yadaptcoeffsa+ +yadaptcoeffsb+))
         (update-x (make-predictor-updater-stereo
                    history +xdelaya+ +xdelayb+ +xadaptcoeffsa+ +xadaptcoeffsb+))
         (y (first  (frame-output frame)))
         (x (second (frame-output frame))))
    (declare (type (sa-sb 32) history x y)
             (type function update-x update-y))
    (loop for i below (frame-samples frame) do
          (when (zerop (rem i +history-size+))
            (loop for j below +predictor-size+ do
                  (setf (aref history j)
                        (aref history (+ j +history-size+)))))
          (setf (aref y i)
                (funcall update-y (aref y i) (if (zerop i) 0 (aref x (1- i))) i)
                (aref x i)
                (funcall update-x (aref x i) (aref y i) i))))
  frame)

(sera:-> predictor-update/3950-mono (frame)
         (values frame &optional))
(defun predictor-update/3950-mono (frame)
  (declare (optimize (speed 3)))
  (let* ((history (zeros (+ +history-size+ +predictor-size+)))
         (update-y (make-predictor-updater-mono
                    history +ydelaya+ +yadaptcoeffsa+))
         (y (first  (frame-output frame))))
    (declare (type (sa-sb 32) history y)
             (type function update-y))
    (loop for i below (frame-samples frame) do
          (when (zerop (rem i +history-size+))
            (loop for j below +predictor-size+ do
                  (setf (aref history j)
                        (aref history (+ j +history-size+)))))
          (setf (aref y i)
                (funcall update-y (aref y i) i))))
  frame)

(sera:-> predictor-update/3950 (frame (member :mono :stereo))
         (values frame &optional))
(declaim (inline predictor-update/3950))
(defun predictor-update/3950 (frame channels)
  (ecase channels
    (:mono   (predictor-update/3950-mono   frame))
    (:stereo (predictor-update/3950-stereo frame))))

(sera:-> predictor-decode/3950 (frame (member :stereo :mono))
         (values frame &optional))
(defun predictor-decode/3950 (frame channels)
  (declare (optimize (speed 3)))
  (let ((orders   (nth (frame-fset frame) +filter-orders+))
        (fracbits (nth (frame-fset frame) +fracbits+)))
    (flet ((apply-filter-channels (order fracbits)
             (mapc (lambda (channel)
                     (apply-filter channel order fracbits))
                   (frame-output frame))))
      (mapc #'apply-filter-channels orders fracbits)))
  (predictor-update/3950 frame channels))

(sera:-> predictor-decode (frame (member :mono :stereo))
         (values frame &optional))
(declaim (inline predictor-decode))
(defun predictor-decode (frame channels)
  (let ((version (find (frame-version frame) +predictor-versions+
                       :test #'>=)))
    (case version
      (3950 (predictor-decode/3950 frame channels))
      (t (error 'ape-error
                :format-control "Cannot decode frame, unsupported version ~d"
                :format-arguments (list (frame-version frame)))))))

(defun decode-frame (frame)
  "Decode an audio frame. Return a list of decoded channels. Each
channel is a simple array with elements of type @c((signed-byte 32))."
  (declare (optimize (speed 3)))
  (let ((mode (if (= (length (frame-output frame)) 2)
                  :stereo :mono)))
    ;; Apply predictor filters
    (predictor-decode frame mode)
    ;; Decorrelate channels
    (when (eq mode :stereo)
      (let ((left  (first  (frame-output frame)))
            (right (second (frame-output frame))))
        (declare (type (sa-sb 32) left right))
        (loop for i below (frame-samples frame) do
          (symbol-macrolet ((x (aref right i))
                            (y (aref left  i)))
            ;; Can I use psetf here without getting rounding problems?
            (let* ((%y (- x (truncate y 2)))
                   (%x (+ %y y)))
              (setf x %x y %y))))))
    ;; Scale output
    (let ((output (mapcar
                   (lambda (channel)
                     (declare (type (sa-sb 32) channel))
                     (map-into
                      channel
                      (case (frame-bps frame)
                        (8 (lambda (x)
                             (declare (type (sb 32) x))
                             (logand #xff (+ #x80 x))))
                        (24 (lambda (x)
                              (declare (type (sb 32) x))
                              (ash x 8)))
                        (t #'identity))
                      channel))
                   (frame-output frame))))
      (if (some-bits-set-p (frame-flags frame) +pseudo-stereo+)
          (let ((channel (first output)))
            (list channel channel))
          output))))

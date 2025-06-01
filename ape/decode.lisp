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

;; FIXME: Why order is NON-NEGATIVE-FIXNUM?
(sera:-> apply-filter ((sa-sb 32) non-negative-fixnum (integer 1 15))
         (values (sa-sb 32) &optional))
(defun apply-filter (entropy order fracbits)
  (declare (optimize (speed 3)))
  (let ((coeffs (zeros order :type '(sb 32)))
        (buffer (zeros (+ +history-size+ (ash order 1))
                       :type '(sb 32)))
        (result (make-array (length entropy) :element-type '(signed-byte 32)))
        (avg 0))
    (declare (type (sb 32) avg))
    (dotimes (i (length entropy))
      (let ((buffer-idx (rem i +history-size+))
            (current-data (aref entropy i)))
        ;; Copy data from back to start when... we need to
        (when (zerop buffer-idx)
          (replace buffer buffer :end1 (ash order 1) :start2 +history-size+))
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
                (decf (aref coeffs i)
                      (* (aref buffer (+ buffer-idx i))
                         (signum current-data))))
          (setf (aref result i)
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
                x-8 (ash x-8 -1))))))
    result))

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

(sera:-> predictor-update/3950-stereo (list (ub 32))
         (values &optional))
(defun predictor-update/3950-stereo (channels samples)
  (declare (optimize (speed 3)))
  ;; Seems like this function cannot be applyed to all channels
  ;; independently (like APPLY-FILTER)
  (let* ((history (zeros (+ +history-size+ +predictor-size+)))
         (update-y (make-predictor-updater-stereo
                    history +ydelaya+ +ydelayb+ +yadaptcoeffsa+ +yadaptcoeffsb+))
         (update-x (make-predictor-updater-stereo
                    history +xdelaya+ +xdelayb+ +xadaptcoeffsa+ +xadaptcoeffsb+))
         (y (first  channels))
         (x (second channels)))
    (declare (type (sa-sb 32) history x y)
             (type function update-x update-y))
    (loop for i below samples do
          (when (zerop (rem i +history-size+))
            (loop for j below +predictor-size+ do
                  (setf (aref history j)
                        (aref history (+ j +history-size+)))))
          (setf (aref y i)
                (funcall update-y (aref y i) (if (zerop i) 0 (aref x (1- i))) i)
                (aref x i)
                (funcall update-x (aref x i) (aref y i) i))))
  (values))

(sera:-> predictor-update/3950-mono (list (ub 32))
         (values &optional))
(defun predictor-update/3950-mono (channels samples)
  (declare (optimize (speed 3)))
  (let* ((history (zeros (+ +history-size+ +predictor-size+)))
         (update-y (make-predictor-updater-mono
                    history +ydelaya+ +yadaptcoeffsa+))
         (y (first channels)))
    (declare (type (sa-sb 32) history y)
             (type function update-y))
    (loop for i below samples do
          (when (zerop (rem i +history-size+))
            (loop for j below +predictor-size+ do
                  (setf (aref history j)
                        (aref history (+ j +history-size+)))))
          (setf (aref y i)
                (funcall update-y (aref y i) i))))
  (values))

(sera:-> predictor-update/3950 (list (ub 32) (member :mono :stereo))
         (values &optional))
(declaim (inline predictor-update/3950))
(defun predictor-update/3950 (channels samples mode)
  (ecase mode
    (:mono   (predictor-update/3950-mono   channels samples))
    (:stereo (predictor-update/3950-stereo channels samples))))

(sera:-> predictor-decode/3950 (frame (member :stereo :mono))
         (values list &optional))
(defun predictor-decode/3950 (frame mode)
  (declare (optimize (speed 3)))
  (let ((orders   (nth (frame-fset frame) +filter-orders+))
        (fracbits (nth (frame-fset frame) +fracbits+)))
    (flet ((apply-filter-channels (channels order fracbits)
             (mapcar
              (lambda (channel)
                (apply-filter channel order fracbits))
              channels)))
      (let ((output
             (si:foldl
              (lambda (channels flt)
                (apply-filter-channels channels (car flt) (cdr flt)))
              (frame-entropy frame)
              (si:zip
               (si:list->iterator orders)
               (si:list->iterator fracbits)))))
        (predictor-update/3950 output (frame-samples frame) mode)
        output))))

(sera:-> predictor-decode (frame (member :mono :stereo))
         (values list &optional))
(declaim (inline predictor-decode))
(defun predictor-decode (frame mode)
  (let ((version (find (frame-version frame) +predictor-versions+
                       :test #'>=)))
    (case version
      (3950 (predictor-decode/3950 frame mode))
      (t (error 'ape-error
                :format-control "Cannot decode frame, unsupported version ~d"
                :format-arguments (list (frame-version frame)))))))

(sera:-> scale-channel ((sa-sb 32) (ub 16))
         (values (sa-sb 32) &optional))
(defun scale-channel (channel bps)
  (declare (optimize (speed 3)))
  (cond
    ((= bps 8)
     (map-into
      channel
      (lambda (x) (logand #xff (+ #x80 x)))
      channel))
    ((= bps 24)
     (map-into
      channel
      (lambda (x) (ash x 8))
      channel))
    (t channel)))

(defun decode-frame (frame)
  "Decode an audio frame. Return a list of decoded channels. Each
channel is a simple array with elements of type @c((signed-byte 32))."
  (declare (optimize (speed 3)))
  (let* ((mode (if (= (length (frame-entropy frame)) 2)
                   :stereo :mono))
         ;; Apply predictor filters
         (output (predictor-decode frame mode)))
    ;; Decorrelate channels
    (when (eq mode :stereo)
      (let ((left  (first  output))
            (right (second output)))
        (declare (type (sa-sb 32) left right))
        (loop for i below (frame-samples frame) do
          (symbol-macrolet ((x (aref right i))
                            (y (aref left  i)))
            ;; Can I use psetf here without getting rounding problems?
            (let* ((%y (- x (truncate y 2)))
                   (%x (+ %y y)))
              (setf x %x y %y))))))
    ;; Scale output
    (loop with bps = (frame-bps frame)
          for channel in output do
          (scale-channel channel bps))
    ;; Restore pseudo stereo if needed
    (if (some-bits-set-p (frame-flags frame) +pseudo-stereo+)
        (let ((channel (first output)))
          (list channel channel))
        output)))

(in-package :easy-audio.utils-sbcl)

(eval-when (:compile-toplevel)
  (defknown make-simd-pack-sb32
      ((sb 32)
       (sb 32)
       (sb 32)
       (sb 32))
      (simd-pack integer)
      (flushable foldable))

  (define-vop (make-simd-pack-sb32)
    (:translate make-simd-pack-sb32)
    (:policy :fast-safe)
    (:args (w :scs (signed-reg))
           (x :scs (signed-reg))
           (y :scs (signed-reg))
           (z :scs (signed-reg)))
    (:arg-types signed-num
                signed-num
                signed-num
                signed-num)
    (:temporary (:sc int-sse-reg) tmp1)
    (:temporary (:sc int-sse-reg) tmp2)
    (:results (res :scs (int-sse-reg)))
    (:result-types simd-pack-int)
    (:generator 0
                (inst movd res z)
                (inst movd tmp1 x)
                (inst punpckldq tmp1 res)
                (inst movd tmp2 y)
                (inst movd res w)
                (inst punpckldq res tmp2)
                (inst punpckldq res tmp1)))

  (defknown store-simd-pack-to-sa-sb32
      ((sa-sb 32)
       non-negative-fixnum
       (simd-pack integer))
      (simd-pack integer))

  (define-vop (store-simd-pack-to-sa-sb32)
    (:translate store-simd-pack-to-sa-sb32)
    (:policy :fast-safe)
    (:args (array :scs (descriptor-reg))
           (index :scs (any-reg))
           (pack  :scs (int-sse-reg) :target res))
    (:arg-types simple-array-signed-byte-32
                sb-vm::positive-fixnum
                simd-pack-int)
    (:results (res :scs (int-sse-reg)))
    (:result-types simd-pack-int)
    (:generator 0
                (move res pack)
                (inst movntdq (sb-vm::make-ea
                               :qword
                               :base array
                               :index index
                               :scale 2
                               :disp (- (* sb-vm::vector-data-offset
                                           sb-vm::n-word-bytes)
                                        sb-vm::other-pointer-lowtag)) res))))

(defun make-simd-pack-sb32 (w x y z)
  (make-simd-pack-sb32 w x y z))

(defun store-simd-pack-to-sa-sb32 (array index pack)
  (store-simd-pack-to-sa-sb32 array index pack))

(in-package :easy-audio.utils)

(defun mixchannels-2 (output channel1 channel2)
  "Special case of MIXCHANNELS when number of channels = 2"
  (declare (type (sa-sb 32)
                 output channel1 channel2)
           (optimize (speed 3)))

  (let* ((len (length output))
         (len-ch (length channel1))
         (len-ch-shift (ash len-ch 1)))
    (if (or
         (> len-ch-shift len)
         (/= len-ch (length channel2)))
        (error "bad arrays: out len ~d, ch1 len ~d, ch2 len ~d"
               len len-ch (length channel2)))

    (if (/= len-ch
            (loop for i below (logand len-ch (lognot 1)) by 2
               for j from 0 by 4
               do
                 (utils-sbcl:store-simd-pack-to-sa-sb32
                  output
                  j
                  (utils-sbcl:make-simd-pack-sb32
                   (aref channel1 i)
                   (aref channel2 i)
                   (aref channel1 (1+ i))
                   (aref channel2 (1+ i))))
               finally (return (+ i 2))))

        (setf (aref output (- len-ch-shift 2))
              (aref channel1 (1- len-ch))
              (aref output (- len-ch-shift 1))
              (aref channel2 (1- len-ch)))))
  output)

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

  (let ((len (length output))
        (len-ch (length channel1)))
    (if (or
         (/= len (ash (length channel2) 1))
         (/= len-ch (length channel2)))
        (error "bad arrays"))

    (if (/= len
            (loop for i below (logand len (lognot 3)) by 4
               for j = (ash i -1)
               do
                 (utils-sbcl:store-simd-pack-to-sa-sb32
                  output
                  i
                  (utils-sbcl:make-simd-pack-sb32
                   (aref channel1 j)
                   (aref channel2 j)
                   (aref channel1 (1+ j))
                   (aref channel2 (1+ j))))
                 finally (return (+ i 4))))

        (setf (aref output (- len 2))
              (aref channel1 (1- len-ch))
              (aref output (- len 1))
              (aref channel2 (1- len-ch)))))
  output)

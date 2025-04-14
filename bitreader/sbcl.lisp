(in-package :easy-audio.bitreader)

(sb-c:defknown read-bits ((integer 0 56) reader &key (:endianness symbol))
    (unsigned-byte 56)
    ()
  :overwrite-fndb-silently t)

(sb-c:defknown read-octets ((integer 0 7) reader &key (:endianness symbol))
    (unsigned-byte 56)
    ()
  :overwrite-fndb-silently t)

(defun make-ub-type (bits)
  (sb-kernel:make-numeric-type
   :class    'integer
   :complexp :real
   :low      0
   :high     (1- (expt 2 bits))))

(sb-c:defoptimizer (read-bits sb-c:derive-type) ((n reader &key endianness))
  (declare (ignore reader endianness))
  (if (sb-c:constant-lvar-p n)
      (make-ub-type (sb-c:lvar-value n))
      (make-ub-type 56)))

(sb-c:defoptimizer (read-octets sb-c:derive-type) ((n reader &key endianness))
  (declare (ignore reader endianness))
  (if (sb-c:constant-lvar-p n)
      (make-ub-type (* 8 (sb-c:lvar-value n)))
      (make-ub-type 56)))

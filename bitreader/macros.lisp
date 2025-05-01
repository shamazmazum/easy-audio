(in-package :easy-audio.bitreader)

(defun reader-function-amount (symbol amount)
  (let ((read-bit-p (eq symbol :bit)))
    (if (and (or (not amount) read-bit-p)
             (or amount (not read-bit-p)))
        (error "You must specify amount if you do not read :BIT"))
    (if (not read-bit-p) (list amount))))

(defmacro defreader ((name &optional docstring) (&optional make-form (obj-sym (gensym) obj-sym-given))
                     &rest slots)
  "Generate a reader function to read data from bit-reader into an
arbitrary object with accessor-like interface. NAME is the name of
such function. The new function will accept two arguments: a
bit-reader and an optional object to be modified. If no object is
passed, it will be created with MAKE-FORM. You can assign a symbol
OBJ-SYM to newly created instance. Each slot from SLOTS is a list. It
has the following syntax:

  (ACCESSOR (:BIT)|(:OCTETS n)|(:BITS n)|(:OCTET-VECTOR v)
            [:ENDIANNESS :BIG|:LITTLE] [:FUNCTION FUNC-NAME] [:COND
            FORM])

(ACCESSOR object) must be a 'place' understandable for setf.  One and
only one of BITS, OCTETS or OCTET-VECTOR must be supplied. Endianness
may be supplied and will be passed to low-level bitreader function. if
FUNC-NAME is supplied, readed value will be passed to this function
and then assigned to the slot. If COND is supplied, data will be read
only if FORM evaluates to T.

UPD: If ACCESSOR is NIL, no data will be stored to anywhere, but it
will be read accordingly to specifications and then lost for good.

If both OBJ-SYM is not given and MAKE-FORM is NIL, the bitreader
itself will be returned from reader function."
  (let ((reader (gensym))
        (only-reading (not (or obj-sym-given make-form))))
    `(defun ,name ,(cond
                     (only-reading (list reader))
                     (make-form `(,reader &optional (,obj-sym ,make-form)))
                     (t (list reader obj-sym)))
       ,@(if docstring (list docstring) nil)
       ,@(loop for slot-spec in slots collect
              (destructuring-bind (accessor (read-how &optional read-how-many) . options)
                  slot-spec
                (let* ((function-name
                        (ecase read-how
                          (:octets       'read-octets)
                          (:bits         'read-bits)
                          (:bit          'read-bit)
                          (:octet-vector 'read-octet-vector)))
                       (endianness (getf options :endianness))
                       (aux-function (getf options :function))
                       (conditional-form (getf options :cond))
                       (function-call
                        `(,function-name
                          ,@(reader-function-amount read-how read-how-many)
                          ,reader
                          ,@(if endianness (list :endianness endianness))))
                       (read-value
                        (if aux-function
                            (list aux-function function-call) function-call))
                       (read-form
                         (if accessor
                             (progn
                               (when only-reading
                                 (error "There cannot be any accessors in this reader"))
                               `(setf (,accessor ,obj-sym) ,read-value))
                             read-value)))
                  (if conditional-form
                      `(if ,conditional-form ,read-form)
                      read-form))))
       ,(if (or obj-sym-given make-form) obj-sym reader))))

(defmacro defreader* ((name ctor (&rest args)) &rest entries)
  "Like a DEFREADER but does not expand into SETFs. Can be used to
read into read-only structures."
  (let ((reader (gensym)) ignored)
    `(defun ,name (,reader ,@args)
       (let ,(loop for entry in entries collect
                   (destructuring-bind (variable (read-how &optional read-how-many)
                                                 &key endianness function cond-form ignore)
                       entry
                     (when ignore (push variable ignored))
                     (let* ((function-name (ecase read-how
                                             (:octets       'read-octets)
                                             (:bits         'read-bits)
                                             (:bit          'read-bit)
                                             (:octet-vector 'read-octet-vector)))
                            (function-call `(,function-name
                                             ,@(reader-function-amount read-how read-how-many)
                                             ,reader
                                             ,@(if endianness
                                                   (list :endianness endianness))))
                            (binding-form (if function
                                              `(,function ,function-call) function-call))
                            (final-form (if cond-form
                                            `(if ,cond-form ,binding-form) binding-form)))
                       `(,variable ,final-form))))
         ,@(if ignored `((declare (ignore ,ignored))))
         (,ctor ,@args ,@(mapcar
                          #'first (remove-if
                                   (lambda (var) (member var ignored))
                                   entries :key #'first)))))))

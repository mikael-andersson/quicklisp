;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cffi-abcl.lisp --- CFFI-SYS implementation for ABCL/JNA.
;;;
;;; Copyright (C) 2009, Luis Oliveira  <loliveira@common-lisp.net>
;;; Copyright (C) 2012, Mark Evenson  <evenson.not.org@gmail.com>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;

;;; This implementation requires the Java Native Access (JNA) library.
;;; <http://jna.dev.java.net/>
;;;
;;; JNA may be automatically loaded into the current JVM process from
;;; abcl-1.1.0-dev via
;;;
;;;   (require 'abcl-contrib)
;;;   (require 'jna)

(require 'abcl-contrib)
(require 'jna)

;;; This is a preliminary version that will have to be cleaned up,
;;; optimized, etc. Nevertheless, it passes all of the relevant CFFI
;;; tests except MAKE-POINTER.HIGH. Shareable Vectors are not
;;; implemented yet.

;;;# Administrivia

(defpackage #:cffi-sys
  (:use #:cl #:java)
  (:import-from #:alexandria #:hash-table-values #:length=)
  (:export
   #:canonicalize-symbol-name-case
   #:foreign-pointer
   #:pointerp
   #:pointer-eq
   #:null-pointer
   #:null-pointer-p
   #:inc-pointer
   #:make-pointer
   #:pointer-address
   #:%foreign-alloc
   #:foreign-free
   #:with-foreign-pointer
   #:%foreign-funcall
   #:%foreign-funcall-pointer
   #:%foreign-type-alignment
   #:%foreign-type-size
   #:%load-foreign-library
   #:%close-foreign-library
   #:native-namestring
   #:%mem-ref
   #:%mem-set
   ;; #:make-shareable-byte-vector
   ;; #:with-pointer-to-vector-data
   #:%foreign-symbol-pointer
   #:%defcallback
   #:%callback))

(in-package #:cffi-sys)

(defun private-jfield (class-name field-name instance)
  (let ((field (find field-name
                     (jcall (jmethod "java.lang.Class" "getDeclaredFields")
                            (jclass class-name))
                     :key #'jfield-name
                     :test #'string=)))
    (jcall (jmethod "java.lang.reflect.Field" "setAccessible" "boolean")
           field (make-immediate-object t :boolean))
    (jcall (jmethod "java.lang.reflect.Field" "get" "java.lang.Object")
           field instance)))

;;; XXX: doesn't match jmethod-arguments.
(defun private-jmethod (class-name method-name)
  (let ((method (find method-name
                      (jcall (jmethod "java.lang.Class" "getDeclaredMethods")
                             (jclass class-name))
                      :key #'jmethod-name
                      :test #'string=)))
    (jcall (jmethod "java.lang.reflect.Method" "setAccessible" "boolean")
           method (make-immediate-object t :boolean))
    method))

(defun private-jconstructor (class-name &rest params)
  (let* ((param-classes (mapcar #'jclass params))
         (cons (find-if (lambda (x &aux (cons-params (jconstructor-params x)))
                          (and (length= param-classes cons-params)
                               (loop for param in param-classes
                                     and param-x across cons-params
                                     always (string= (jclass-name param)
                                                     (jclass-name param-x)))))
                        (jcall (jmethod "java.lang.Class"
                                        "getDeclaredConstructors")
                               (jclass class-name)))))
    (jcall (jmethod "java.lang.reflect.Constructor" "setAccessible" "boolean")
           cons (make-immediate-object t :boolean))
    cons))

;;;# Symbol Case

(defun canonicalize-symbol-name-case (name)
  (string-upcase name))

;;;# Pointers

(deftype foreign-pointer ()
  '(satisfies pointerp))

(defun pointerp (ptr)
  "Return true if PTR is a foreign pointer."
  (jclass-superclass-p (jclass "com.sun.jna.Pointer") (jclass-of ptr)))

(defun make-pointer (address)
  "Return a pointer pointing to ADDRESS."
  (jnew (private-jconstructor "com.sun.jna.Pointer" "long") address))

(defun pointer-address (pointer)
  "Return the address pointed to by PTR."
  (private-jfield "com.sun.jna.Pointer" "peer" pointer))

(defun pointer-eq (ptr1 ptr2)
  "Return true if PTR1 and PTR2 point to the same address."
  (= (pointer-address ptr1) (pointer-address ptr2)))

(defun null-pointer ()
  "Construct and return a null pointer."
  (make-pointer 0))

(defun null-pointer-p (ptr)
  "Return true if PTR is a null pointer."
  (zerop (pointer-address ptr)))

(defun inc-pointer (ptr offset)
  "Return a fresh pointer pointing OFFSET bytes past PTR."
  (make-pointer (+ (pointer-address ptr) offset)))

;;;# Allocation

(defun %foreign-alloc (size)
  "Allocate SIZE bytes on the heap and return a pointer."
  (make-pointer
   (jcall (private-jmethod "com.sun.jna.Memory" "malloc")
          nil size)))

(defun foreign-free (ptr)
  "Free a PTR allocated by FOREIGN-ALLOC."
  (jcall (private-jmethod "com.sun.jna.Memory" "free")
         nil (pointer-address ptr)))

;;; TODO: stack allocation.
(defmacro with-foreign-pointer ((var size &optional size-var) &body body)
  "Bind VAR to SIZE bytes of foreign memory during BODY.  The pointer
in VAR is invalid beyond the dynamic extent of BODY, and may be
stack-allocated if supported by the implementation.  If SIZE-VAR is
supplied, it will be bound to SIZE during BODY."
  (unless size-var
    (setf size-var (gensym "SIZE")))
  `(let* ((,size-var ,size)
          (,var (%foreign-alloc ,size-var)))
     (unwind-protect
          (progn ,@body)
       (foreign-free ,var))))

;;;# Shareable Vectors
;;;
;;; This interface is very experimental.  WITH-POINTER-TO-VECTOR-DATA
;;; should be defined to perform a copy-in/copy-out if the Lisp
;;; implementation can't do this.

;;; TODO.

(defun make-shareable-byte-vector (size)
  "Create a Lisp vector of SIZE bytes can passed to
WITH-POINTER-TO-VECTOR-DATA."
  (error "unimplemented"))

(defmacro with-pointer-to-vector-data ((ptr-var vector) &body body)
  "Bind PTR-VAR to a foreign pointer to the data in VECTOR."
  (warn "unimplemented"))

;;;# Dereferencing

(defun foreign-type-to-java-class (type)
  (jclass
   (ecase type
     ((:int :unsigned-int) "java.lang.Integer")
     ((:long :unsigned-long) "com.sun.jna.NativeLong")
     ((:long-long :unsigned-long-long) "java.lang.Long")
     (:pointer "com.sun.jna.Pointer")
     (:float "java.lang.Float")
     (:double "java.lang.Double")
     ((:char :unsigned-char) "java.lang.Byte")
     ((:short :unsigned-short) "java.lang.Short"))))

(defun %foreign-type-size (type)
  "Return the size in bytes of a foreign type."
  (jstatic "getNativeSize" "com.sun.jna.Native"
           (foreign-type-to-java-class type)))

;;; FIXME.
(defun %foreign-type-alignment (type)
  "Return the alignment in bytes of a foreign type."
  (%foreign-type-size type))

(defun unsigned-type-p (type)
  (case type
    ((:unsigned-char
      :unsigned-int
      :unsigned-short
      :unsigned-long
      :unsigned-long-long) t)
    (t nil)))

(defun jna-getter (type)
  (ecase type
    ((:char :unsigned-char) "getByte")
    (:double "getDouble")
    (:float "getFloat")
    ((:int :unsigned-int) "getInt")
    ((:long :unsigned-long) "getNativeLong")
    ((:long-long :unsigned-long-long) "getLong")
    (:pointer "getPointer")
    ((:short :unsigned-short) "getShort")))

(defun lispify-value (value type)
  (when (and (eq type :pointer) (null value))
    (return-from lispify-value (null-pointer)))
  (when (or (eq type :long) (eq type :unsigned-long))
    (setq value (jcall (jmethod "com.sun.jna.NativeLong" "longValue") value)))
  (let ((bit-size (* 8 (%foreign-type-size type))))
    (if (and (unsigned-type-p type) (logbitp (1- bit-size) value))
        (lognot (logxor value (1- (expt 2 bit-size))))
        value)))

(defun %mem-ref (ptr type &optional (offset 0))
  (lispify-value
   (jcall (jmethod "com.sun.jna.Pointer" (jna-getter type) "long")
          ptr offset)
   type))

(defun jna-setter (type)
  (ecase type
    ((:char :unsigned-char) "setByte")
    (:double "setDouble")
    (:float "setFloat")
    ((:int :unsigned-int) "setInt")
    ((:long :unsigned-long) "setNativeLong")
    ((:long-long :unsigned-long-long) "setLong")
    (:pointer "setPointer")
    ((:short :unsigned-short) "setShort")))

(defun jna-setter-arg-type (type)
  (ecase type
    ((:char :unsigned-char) "byte")
    (:double "double")
    (:float "float")
    ((:int :unsigned-int) "int")
    ((:long :unsigned-long) "com.sun.jna.NativeLong")
    ((:long-long :unsigned-long-long) "long")
    (:pointer "com.sun.jna.Pointer")
    ((:short :unsigned-short) "short")))

(defun %mem-set (value ptr type &optional (offset 0))
  (let* ((bit-size (* 8 (%foreign-type-size type)))
         (val (if (and (unsigned-type-p type) (logbitp (1- bit-size) value))
                  (lognot (logxor value (1- (expt 2 bit-size))))
                  value)))
    (jcall (jmethod "com.sun.jna.Pointer"
                    (jna-setter type) "long" (jna-setter-arg-type type))
           ptr
           offset
           (if (or (eq type :long) (eq type :unsigned-long))
               (jnew (jconstructor "com.sun.jna.NativeLong" "long") val)
               val)))
  value)

;;;# Calling Foreign Functions

(defun find-foreign-function (name library)
  (flet ((find-it (name library)
           (ignore-errors
             (jcall (jmethod "com.sun.jna.NativeLibrary" "getFunction"
                             "java.lang.String")
                    library name))))
    (if (eq library :default)
        (loop for lib in (hash-table-values *loaded-libraries*)
              for fn = (find-it name lib)
              when fn do (return fn))
        (find-it name (gethash library *loaded-libraries*)))))

(defun convert-calling-convention (convention)
  (ecase convention
    (:stdcall "ALT_CONVENTION")
    (:cdecl "C_CONVENTION")))

(defun make-function-pointer (pointer cconv)
  (jnew (private-jconstructor "com.sun.jna.Function"
                              "com.sun.jna.Pointer" "int")
        pointer
        (jfield "com.sun.jna.Function"
                (convert-calling-convention convention))))

(defun lisp-value-to-java (value foreign-type)
  (if (eq foreign-type :pointer)
      value
      (jnew (ecase foreign-type
              ((:int :unsigned-int) (jconstructor "java.lang.Integer" "int"))
              ((:long-long :unsigned-long-long)
                 (jconstructor "java.lang.Long" "long"))
              ((:long :unsigned-long)
                 (jconstructor "com.sun.jna.NativeLong" "long"))
              ((:short :unsigned-short) (jconstructor "java.lang.Short" "short"))
              ((:char :unsigned-char) (jconstructor "java.lang.Byte" "byte"))
              (:float (jconstructor "java.lang.Float" "float"))
              (:double (jconstructor "java.lang.Double" "double")))
            value)))

(defun %%foreign-funcall (function args arg-types return-type)
  (let ((jargs (jnew-array "java.lang.Object" (length args))))
    (loop for arg in args and type in arg-types and i from 0
          do (setf (jarray-ref jargs i)
                   (lisp-value-to-java arg type)))
    (if (eq return-type :void)
        (progn
          (jcall (jmethod "com.sun.jna.Function" "invoke" "[Ljava.lang.Object;")
                 function jargs)
          (values))
        (lispify-value
         (jcall (jmethod "com.sun.jna.Function" "invoke"
                         "java.lang.Class" "[Ljava.lang.Object;")
                function
                (foreign-type-to-java-class return-type)
                jargs)
         return-type))))

(defun foreign-funcall-type-and-args (args)
  (let ((return-type :void))
    (loop for (type arg) on args by #'cddr
          if arg collect type into types
          and collect arg into fargs
          else do (setf return-type type)
          finally (return (values types fargs return-type)))))

(defmacro %foreign-funcall (name args &key library convention)
  (declare (ignore convention))
  (multiple-value-bind (types fargs rettype)
      (foreign-funcall-type-and-args args)
    `(%%foreign-funcall (find-foreign-function ',name ',library)
                        (list ,@fargs) ',types ',rettype)))

(defmacro %foreign-funcall-pointer (ptr args &key convention)
  (multiple-value-bind (types fargs rettype)
      (foreign-funcall-type-and-args args)
    `(%%foreign-funcall (make-function-pointer ,ptr ',convention)
                        (list ,@fargs) ',types ',rettype)))

;;;# Callbacks

(defun foreign-to-callback-type (type)
  (ecase type
    ((:int :unsigned-int)
     :int)
    ((:long :unsigned-long)
     (jvm::make-jvm-class-name "com.sun.jna.NativeLong"))
    ((:long-long :unsigned-long-long)
     (jvm::make-jvm-class-name "java.lang.Long"))
    (:pointer
     (jvm::make-jvm-class-name "com.sun.jna.Pointer"))
    (:float
     :float)
    (:double
     :double)
    ((:char :unsigned-char)
     :byte)
    ((:short :unsigned-short)
     :short)
    (:void
     :void)))

(defvar *callbacks* (make-hash-table))

(defmacro %defcallback (name return-type arg-names arg-types body
                        &key convention)
  (declare (ignore convention))
  `(let ((interface-name ,(define-jna-callback-interface return-type arg-types)))
     (setf (gethash ',name *callbacks*)
           (jinterface-implementation interface-name "callback"
                                      (lambda (,@arg-names)
                                        ,body)))))

(jvm::define-class-name +callback-object+ "com.sun.jna.Callback")
(defconstant +dynamic-callback-package+
  "org/armedbear/jna/dynamic/callbacks")

(defun define-jna-callback-interface (returns args)
  (multiple-value-bind (interface interface-name)
      (%define-jna-callback-interface
       (foreign-to-callback-type returns)
       (mapcar (lambda (type) (foreign-to-callback-type type)) args))
    (load-class interface)
    interface-name))

(defun %define-jna-callback-interface (returns args)
  "Returns the Java byte[] array of a class representing a Java interface.

The fully qualified dotted name of the generated class is returned as
the second value."
  (let ((name (symbol-name (gensym))))
    (values
     (define-java-interface name +dynamic-callback-package+
       `(("callback" ,returns ,args))
       `(,+callback-object+))
     (format nil "~A.~A"
             (substitute #\. #\/ +dynamic-callback-package+) name))))

(defun define-java-interface (name package methods
                              &optional (superinterfaces nil))
"Define a class for a Java interface called NAME in PACKAGE with METHODS.

METHODS is a list of (NAME RETURN-TYPE (ARG-TYPES)) entries.  NAME is
a string.  The values of RETURN-TYPE and the list of ARG-TYPES for the
defined method follow the are either references to Java objects as
created by JVM::MAKE-JVM-CLASS-NAME, or keywords representing Java
primtive types as contained in JVM::MAP-PRIMITIVE-TYPE.

SUPERINTERFACES optionally contains a list of interfaces that this
interface extends specified as fully qualifed dotted Java names."
  (let* ((class-name-string (format nil "~A/~A" package name))
         (class-name (jvm::make-jvm-class-name class-name-string))
         (class (jvm::make-class-interface-file class-name)))
    (dolist (superinterface superinterfaces)
      (jvm::class-add-superinterface
       class
       (if (typep superinterface 'jvm::jvm-class-name)
           superinterface
           (jvm::make-jvm-class-name superinterface))))
    (dolist (method methods)
      (let ((name (first method))
            (returns (second method))
            (args (third method)))
      (jvm::class-add-method
       class
       (jvm::make-jvm-method name returns args
                             :flags '(:public :abstract)))))
    (jvm::finalize-class-file class)
    (let ((s (sys::%make-byte-array-output-stream)))
      (jvm::write-class-file class s)
      (sys::%get-output-stream-bytes s))))

(defun load-class (class-bytes)
  "Load the Java byte[] array CLASS-BYTES as a Java class."
  (let ((load-class-method
         (jmethod "org.armedbear.lisp.JavaClassLoader"
                  "loadClassFromByteArray" "[B")))
    (jcall load-class-method java::*classloader* class-bytes)))

;;; Test function: unused in CFFI
(defun write-class (class-bytes pathname)
  "Write the Java byte[] array CLASS-BYTES to PATHNAME."
  (with-open-file (stream pathname
                          :direction :output
                          :element-type '(signed-byte 8))
    (dotimes (i (jarray-length class-bytes))
      (write-byte (jarray-ref class-bytes i) stream))))

(defun %callback (name)
  (or (gethash name *callbacks*)
      (error "Undefined callback: ~S" name)))

;;;# Loading and Closing Foreign Libraries

(defparameter *loaded-libraries* (make-hash-table))

(defun %load-foreign-library (name path)
  "Load a foreign library, signals a simple error on failure."
  (handler-case
      (let ((lib (jstatic "getInstance" "com.sun.jna.NativeLibrary" path)))
        (setf (gethash name *loaded-libraries*) lib)
        lib)
    (java-exception (e)
      (error (jcall (jmethod "java.lang.Exception" "getMessage")
                    (java-exception-cause e))))))

;;; FIXME. Should remove libraries from the hash table.
(defun %close-foreign-library (handle)
  "Closes a foreign library."
  #+#:ignore (setf *loaded-libraries* (remove handle *loaded-libraries*))
  (jcall (jmethod "com.sun.jna.NativeLibrary" "dispose") handle))

(defun native-namestring (pathname)
  (namestring pathname))

;;;# Foreign Globals

(defun %foreign-symbol-pointer (name library)
  "Returns a pointer to a foreign symbol NAME."
  (flet ((find-it (name library)
           (let ((p (ignore-errors
                      (jcall (private-jmethod "com.sun.jna.NativeLibrary"
                                              "getSymbolAddress")
                             library name))))
             (unless (null p)
               (make-pointer p)))))
    (if (eq library :default)
        (loop for lib in (hash-table-values *loaded-libraries*)
              for fn = (find-it name lib)
              when fn do (return fn))
        (find-it name library))))

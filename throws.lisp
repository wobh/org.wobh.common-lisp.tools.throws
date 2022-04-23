;; -*- mode: lisp -*-

(defpackage #:org.wobh.common-lisp.tools.throws
  (:use #:common-lisp)
  (:nicknames #:throws)
  (:export #:inject
           #:biject
           #:reject)
  (:documentation "ORG.WOBH.COMMON-LISP.TOOLS.THROWS

Package of iteratives."))

(in-package #:org.wobh.common-lisp.tools.throws)

(defgeneric inject (a-function a-collection &key &allow-other-keys)
  (:method ((a-function function) (a-collection sequence)
            &key key from-end start end initial-value &allow-other-keys)
    (apply #'reduce
           a-function
           a-collection
           (reduce (lambda (kwargs kwpair)
                     (if (null (second kwpair))
                         kwargs
                         (append kwargs kwpair)))
                    (list (list :key key)
                          (list :from-end from-end)
                          (list :start start)
                          (list :end end)
                          (list :initial-value initial-value)))))
  (:method ((a-function function) (a-succession succession) &key accumulator)
    (loop
      for item = (nextf a-succession)
      for accumulator = (funcall a-function accumulator item)
      until (zerop (sizef a-succession))
      finally
	 (return accumulator))))

(defgeneric biject (a-function a-collection &key &allow-other-keys)
  (:method ((a-function function) (a-collection sequence) &key &allow-other-keys)
    (map (type-of a-collection)
         a-function
         a-collection))
  (:method ((a-function function) (a-succession succession))
    (let ((accumulator (make-instance (type-of a-succession))))
      (inject (lambda (acc elt)
		(pushf acc (funcall a-function elt))
		acc)
	      a-succession
	      :accumulator accumulator)))
  (:method ((a-function function) (a-collection t))
    (inject (lambda (acc elt)
	      (store acc (funcall a-function elt)))
	    a-collection
	    :initial-value (make-instance (class-of a-collection)))))

(defgeneric reject (a-function a-collection &key &allow-other-keys)
  (:method ((a-function function) (a-collection sequence)
            &key from-end start end count key &allow-other-keys)
    (apply #'remove-if
           a-function
           a-collection
           (reduce (lambda (kwargs kwpair)
                     (if (null (second kwpair))
                         kwargs
                         (append kwargs kwpair)))
                    (list (list :from-end from-end)
                          (list :start start)
                          (list :end end)
                          (list :count count)
                          (list :key key)))))
  (:method ((a-function function) (a-succession succession))
    (let ((accumulator (make-instance (type-of a-succession))))
      (inject (lambda (acc elt)
		(unless (null (funcall a-function elt))
		  (pushf acc elt))
		acc)
	      a-succession
	      :accumulator accumulator)))
  (:method ((a-function function) (a-collection t))
    (inject (lambda (acc elt)
	      (if (null (funcall a-function elt))
		  acc
		  (storef acc elt))))))


;; Accessors

(defgeneric fetchf (collection index)
  (:method ((collection hash-table) (index t))
    (let ((out (gethash index collection)))
      (values out collection)))
  (:method ((collection string) (index integer))
    (let ((out (char collection index)))
      (values out collection)))
  (:method ((collection simple-vector) (index integer))
    (let ((out (svref collection index)))
      (values out collection))))

(defgeneric pluckf (collection index)
  (:method ((collection hash-table) (index t))
    (let ((out (gethash index collection)))
      (values (and (remhash index collection) out)
	      collection
	      index))))

(defgeneric storef (collection index item)
  (:method ((collection hash-table) (index t) (item t))
    (setf (gethash index collection) item)
    collection)
  (:method ((collection string) (index integer) (item character))
    (setf (char collection index) item)
    collection)
  (:method ((collection simple-vector) (index integer) (item t))
    (setf (svref collection index) item)
    collection))

(defgeneric nmergef (a-collection b-collection)
  (:method ((a-collection hash-table) (b-collection hash-table))
    (with-hash-table-iterator (next-itemf b-collection)
      (loop
	 (multiple-value-bind (morep key value) (next-itemf)
	   (unless morep (return a-collection))
	   (setf (gethash key a-collection)
		 value))))))

(defgeneric naffixf (a-collection b-collection)
  (:method ((a-collection hash-table) (b-collection hash-table))
    (mergef a-collection b-collection))
  (:method ((a-collection sequence) (b-collection sequence))
    (map-into a-collection #'identity a-collection b-collection)))

(defgeneric slicef (a-collection &rest keys)
  (:method ((a-collection hash-table) &rest keys)
    (loop
      with out = (make-hash-table
		  :test (hash-table-test a-collection)
		  :rehash-size (hash-table-rehash-size a-collection)
		  :rehash-threshold (hash-table-rehash-threshold a-collection))
      for key in keys
      do (setf (gethash key out)
	       (gethash key a-collection))
      finally (return out))))

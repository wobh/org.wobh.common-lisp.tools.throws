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
  (:method ((a-function function) (a-collection hash-table)
	    &key key initial-value &allow-other-keys)
    (loop
      with acc = initial-value
      with fetch = (or key #'identity)
      for a-value being each hash-value
	of a-collection
      do (setf acc
	       (funcall a-function
			acc
			(funcall fetch a-value)))
      finally (return acc))))

(defgeneric biject (a-function a-collection &key &allow-other-keys)
  (:method ((a-function function) (a-collection sequence) &key &allow-other-keys)
    (map (type-of a-collection)
         a-function
         a-collection))
  (:method ((a-function function) (a-collection hash-table) &key &allow-other-keys)
    (maphash a-function a-collection)))

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
  (:method ((a-function function) (a-collection hash-table)
	    &key key &allow-other-keys)
    (loop
      with out = (make-hash-table :test (hash-table-test a-collection)
				  :size (hash-table-size a-collection)
				  :rehash-size (hash-table-rehash-size a-collection)
				  :rehash-threshold (hash-table-rehash-threshold a-collection))
      with fetch = (or key #'identity)
      for a-value being each hash-value
	of a-collection
	  using (hash-key a-key)
      when (funcall a-function
		    (funcall fetch a-value))
	do (setf (gethash a-key out)
		 a-value)
      finally (return out))))


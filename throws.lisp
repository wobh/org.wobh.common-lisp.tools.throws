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
                          (list :initial-value initial-value))))))

(defgeneric biject (a-function a-collection &key &allow-other-keys)
  (:method ((a-function function) (a-collection sequence) &key &allow-other-keys)
    (map (type-of a-collection)
         a-function
         a-collection)))

(defgeneric reject (a-function a-collection &key &allow-other-keys)
  (:method ((a-function function) (a-collection sequence)
            &key from-end start end count key &allow-other-keys)
    (apply #'remove-if-not
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
                          (list :key key))))))

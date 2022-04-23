;; -*- mode: lisp -*-

(defpackage #:org.wobh.common-lisp.tools.throws-test
  (:use #:common-lisp)
  (:local-nicknames (#:throws #:org.wobh.common-lisp.tools.throws))
  (:documentation "ORG.WOBH.COMMON-LISP.TOOLS.THROWS-TEST

Tests for the throws package"))

(in-package #:org.wobh.common-lisp.tools.throws-test)

;;; Test throws:inject

(let ((subject '(0 1 2 3 4 5))
      (expect 15))
  (assert (= expect (throws:inject #'+ subject))))

(let ((subject #(0 1 2 3 4 5))
      (expect 15))
  (assert (= expect (throws:inject #'+ subject))))

(let ((subject '(#\b #\e #\d #\c #\a))
      (expect "bedca"))
  (assert (string= expect
                   (with-output-to-string (a-stream)
                     (throws:inject (lambda (a-strm a-char) (princ a-char a-strm) a-strm)
                                    subject
                                    :initial-value a-stream)))))

(let ((subject (let ((a-hash-table (make-hash-table)))
                 (setf (gethash :foo a-hash-table) 1)
                 (setf (gethash :bar a-hash-table) 2)
                 (setf (gethash :baz a-hash-table) 3)
                 a-hash-table))
      (expect 6))
  (assert (= expect
             (throws:inject (lambda (acc elt)
                              (destructuring-bind (a-key a-value) elt
                                (declare (ignorable a-key))
                                (+ acc a-value)))
                            subject
                            :initial-value 0))))


;;; Test throws:biject

(let ((subject '(0 1 2 3 4 5))
      (expect '(1 2 3 4 5 6)))
  (assert (equal expect
                 (throws:biject #'1+ subject))))

(let ((subject #(0 1 2 3 4 5))
      (expect #(1 2 3 4 5 6)))
  (assert (equalp expect
                  (throws:biject #'1+ subject))))

(let ((subject (let ((a-hash-table (make-hash-table)))
                 (setf (gethash :foo a-hash-table) 1)
                 (setf (gethash :bar a-hash-table) 2)
                 (setf (gethash :baz a-hash-table) 3)
                 a-hash-table))
      (expect 6))
  (assert (= expect
             (throws:inject (lambda (acc elt)
                              (destructuring-bind (morep key value) elt
                                (declare (ignorable morep key))
                                (+ acc value)))
                            subject))))


;;; Test throws:reject

(let ((subject '(0 1 2 3 4 5))
      (expect '(1 3 5)))
  (assert (equal expect
                 (throws:reject #'evenp subject))))

(let ((subject #(0 1 2 3 4 5))
      (expect #(1 3 5)))
  (assert (equalp expect
                 (throws:reject #'evenp subject))))

(let ((subject (let ((a-hash-table (make-hash-table)))
                 (setf (gethash :foo a-hash-table) 0)
                 (setf (gethash :bar a-hash-table) 1)
                 (setf (gethash :baz a-hash-table) 2)
                 (setf (gethash :qux a-hash-table) 3)
                 (setf (gethash :zot a-hash-table) 4)
                 (setf (gethash :wat a-hash-table) 5)
                 a-hash-table))
      (expect '(0 2 4)))
  (assert (equal expect
                 (loop
                   for a-value being each hash-value
                     of (throws:reject #'evenp subject)
                   collect a-value into out
                   finally (return (sort out #'<))))))

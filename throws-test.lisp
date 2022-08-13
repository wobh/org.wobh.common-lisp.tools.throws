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


;;; Test throws:biject

(let ((subject '(0 1 2 3 4 5))
      (expect '(1 2 3 4 5 6)))
  (assert (equal expect
                 (throws:biject #'1+ subject))))

(let ((subject #(0 1 2 3 4 5))
      (expect #(1 2 3 4 5 6)))
  (assert (equalp expect
                  (throws:biject #'1+ subject))))


;;; Test throws:reject

(let ((subject '(0 1 2 3 4 5))
      (expect '(1 3 5)))
  (assert (equal expect
                 (throws:reject #'evenp subject))))

(let ((subject #(0 1 2 3 4 5))
      (expect #(1 3 5)))
  (assert (equalp expect
                 (throws:reject #'evenp subject))))


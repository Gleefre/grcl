;;;; This is scratch for GRCL [ implementation of CL ].
;;;; Copyright 2023 Gleefre

;;;; This code is evaluated on the host implementation.

(in-package #:cl-user)

(defpackage #:grcl (:use))

(defpackage #:grcl/host
  (:use #:cl))

(in-package #:grcl/host)

;; @foo is used to read symbols in #:grcl package
(defun |@-reader| (stream char)
  (declare (ignore char))
  (let ((symbol (read stream t nil t)))
    (check-type symbol symbol)
    (intern (symbol-name symbol) "GRCL")))

(set-macro-character #\@ #'|@-reader|)

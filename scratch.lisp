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
  (let ((*package* (find-package '#:grcl)))
    (read stream t nil t)))

(set-macro-character #\@ #'|@-reader|)

;; def/class/ automatically defines :initarg and :accessor for each slot
(defmacro def/class/ (name slots)
  `(defclass ,name ()
     (,@(mapcar (lambda (slot)
                  `(,slot :initarg ,(intern (symbol-name slot) "KEYWORD")
                          :accessor ,(intern (format nil "/~A-~A/" name slot))))
                slots))))

;;; The following section implements GRCL symbols and packages.

(def/class/ @symbol (name function package value plist))
(def/class/ @package (name nicknames use-list internal-symbols external-symbols shadowing-symbols))

(defconstant /+keyword+/ (make-instance '@package :name "KEYWORD"))
(defconstant /+cl+/ (make-instance '@package :name "CL"))

(defun @symbolp (thing)
  (typep thing '@symbol))

(defun @keywordp (symbol)
  (and (@symbolp symbol)
       (eq (/symbol-package/ symbol) /+keyword+/)))

(deftype @keyword () '(and @symbol (satisfies @keywordp)))

(defun @make-symbol (name)
  (make-instance '@symbol :name name :package nil :plist ()))

;; Symbol value

(defun @boundp (symbol)
  (slot-boundp symbol 'value))

(defun @makeunbound (symbol)
  (slot-makunbound symbol 'value))

(defun @symbol-value (symbol)
  (/symbol-value/ symbol))

(defun @set (symbol value)
  (setf (/symbol-value/ symbol) value))

(defun (setf @symbol-value) (value symbol)
  (set symbol value))

;; Symbol function

(defun @fboundp (symbol)
  (slot-boundp symbol 'function))

(defun @fmakeunbound (symbol)
  (slot-makunbound symbol 'value))

(defun @symbol-function (symbol)
  (/symbol-function/ symbol))

(defun (setf @symbol-function) (value symbol)
  (setf (/symbol-function/ symbol) value))

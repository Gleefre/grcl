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

;;; The following section implements GRCL symbols.

(def/class/ @symbol (name function package value plist))

(defun @symbolp (thing)
  (typep thing '@symbol))

(defun @keywordp (symbol)
  (and (@symbolp symbol)
       (eq (/symbol-package/ symbol) /+keyword+/)))

(deftype @keyword '(and symbol (satisfies keywordp)))

;; FIXME: should use @nil and @()
(defun @make-symbol (name)
  (make-instance '@symbol :name name :package nil :plist ()))

;;; The following section implements GRCL packages.

(def/class/ @package (name nicknames use-list internal-symbols external-symbols shadowing-symbols))

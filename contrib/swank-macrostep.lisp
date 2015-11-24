;;; swank-macrostep.lisp -- fancy macro-expansion via macrostep.el
;;
;; Authors: Luís Oliveira <luismbo@gmail.com>
;;          Jon Oddie <j.j.oddie@gmail.com>
;;
;; License: Public Domain

(defpackage swank-macrostep
  (:use cl swank)
  (:import-from swank
		#:*macroexpand-printer-bindings*
                #:with-buffer-syntax
		#:with-bindings
                #:to-string
                #:macroexpand-all
                #:compiler-macroexpand-1
                #:defslimefun
                #:collect-macro-forms)
  (:export #:macrostep-expand-1
           #:macro-form-p))

(in-package #:swank-macrostep)

(defslimefun macrostep-expand-1 (string &optional compiler-macros?)
  (with-buffer-syntax ()
    (let ((form (read-from-string string)))
      (multiple-value-bind (expansion error-message)
	  (expand-form-once form compiler-macros?)
	(if error-message
            `(:error ,error-message)
	    (multiple-value-bind (macros compiler-macros)
		(collect-macro-forms expansion)
	      (let* ((all-macros (append macros compiler-macros))
		     (pretty-expansion (pprint-to-string expansion))
		     (positions (collect-form-positions expansion
							pretty-expansion
							all-macros))
                     (macro-operators
                      (loop
                         for form in all-macros
                         for (start end) in positions
                         when (and start end)
                         collect (let ((op-name (to-string (first form)))
                                       (op-type
                                        (if (member form macros)
                                            :macro
                                            :compiler-macro))
                                       (line-number
                                        (position-line start pretty-expansion)))
                                   (list op-name
                                         op-type
                                         start
                                         line-number
                                         (length op-name))))))
		`(:ok ,pretty-expansion ,macro-operators))))))))

(defun expand-form-once (form compiler-macros?)
  (multiple-value-bind (expansion expanded?)
      (macroexpand-1 form)
    (if expanded?
	(values expansion nil)
	(if (not compiler-macros?)
	    (values nil "Not a macro form")
	    (multiple-value-bind (expansion expanded?)
		(compiler-macroexpand-1 form)
	      (if expanded?
		  (values expansion nil)
		  (values nil "Not a macro or compiler-macro form")))))))

(defun pprint-to-string (object &optional pprint-dispatch)
  (let ((*print-pprint-dispatch* (or pprint-dispatch *print-pprint-dispatch*)))
    (with-bindings *macroexpand-printer-bindings*
      (to-string object))))

(defun position-line (position string)
  (let ((line 0))
    (loop for i upto position
          for char across string
          when (eql char #\Newline)
            do (incf line))
    line))

(defslimefun macro-form-p
    (string &optional compiler-macros?)
  (with-buffer-syntax ()
    (let ((form
           (handler-case
               (read-from-string string)
             (error (condition)
               (unless (debug-on-swank-error)
                 (return-from macro-form-p
                   `(:error ,(format "Read error: ~A" condition))))))))
      `(:ok ,(macro-form-type form nil compiler-macros?)))))

(defun macro-form-type (form env compiler-macros?)
  (cond
    ((or (not (consp form))
         (not (symbolp (car form))))
     nil)
    ((macro-function (car form) env)
     :macro)
    ((and compiler-macros?
          (compiler-macro-function (car form) env))
     (multiple-value-bind (expansion expanded?)
         (compiler-macroexpand-1 form)
       (declare (ignore expansion))
       (if expanded?
           :compiler-macro
         nil)))
    (t
     nil)))

;;;; Tracking Pretty Printer
(defun marker-char-p (char)
  (<= #xe000 (char-code char) #xe8ff))

(defun make-marker-char (id)
  ;; using the private-use characters U+E000..U+F8FF as markers, so
  ;; that's our upper limit for how many we can use.
  (assert (<= 0 id #x8ff))
  (code-char (+ #xe000 id)))

(defun marker-char-id (char)
  (assert (marker-char-p char))
  (- (char-code char) #xe000))

(defparameter +whitespace+ (mapcar #'code-char '(9 13 10 32)))

(defun whitespacep (char)
  (member char +whitespace+))

(defun collect-form-positions (expansion printed-expansion forms)
  (loop for (start end)
          in (collect-marker-positions
              (pprint-to-string expansion (make-tracking-pprint-dispatch forms))
              (length forms))
        collect (when (and start end)
                  (list (find-non-whitespace-position printed-expansion start)
                        (find-non-whitespace-position printed-expansion end)))))

(defun make-tracking-pprint-dispatch (forms)
  (let ((original-table *print-pprint-dispatch*)
        (table (copy-pprint-dispatch)))
    (flet ((maybe-write-marker (position stream)
             (when position
               (write-char (make-marker-char position) stream))))
      (set-pprint-dispatch 'cons
                           (lambda (stream cons)
                             (let ((pos (position cons forms)))
                               (maybe-write-marker pos stream)
                               ;; delegate printing to the original table.
                               (funcall (pprint-dispatch cons original-table)
                                        stream
                                        cons)
                               (maybe-write-marker pos stream)))
                           most-positive-fixnum
                           table))
    table))

(defun collect-marker-positions (string position-count)
  (let ((positions (make-array position-count :initial-element nil)))
    (loop with p = 0
          for char across string
          unless (whitespacep char)
            do (if (marker-char-p char)
                   (push p (aref positions (marker-char-id char)))
                   (incf p)))
    (map 'list #'reverse positions)))

(defun find-non-whitespace-position (string position)
  (loop with non-whitespace-position = -1
        for i from 0 and char across string
        unless (whitespacep char)
          do (incf non-whitespace-position)
        until (eql non-whitespace-position position)
        finally (return i)))

(provide :swank-macrostep)

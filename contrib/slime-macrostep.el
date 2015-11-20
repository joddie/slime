;;; slime-macrostep.el -- fancy macro-expansion via macrostep.el

;; Authors: Luís Oliveira <luismbo@gmail.com>
;;          Jon Oddie <j.j.oddie@gmail.com
;;
;; License: GNU GPL (same license as Emacs)

;;; Description:

;; Fancier in-place macro-expansion using macrostep.el (originally
;; written for Emacs Lisp).  To use, position point before the
;; open-paren of the macro call in a SLIME source or REPL buffer, and
;; type `C-c M-e' or `M-x macrostep-expand'.  The pretty-printed
;; result of `macroexpand-1' will be inserted inline in the current
;; buffer, which is temporarily read-only while macro expansions are
;; visible.  If the expansion is itself a macro call, expansion can be
;; continued by typing `e'.  Expansions are collapsed to their
;; original macro forms by typing `c' or `q'.  Other macro- and
;; compiler-macro calls in the expansion will be font-locked
;; differently, and point can be moved there quickly by typing `n' or
;; `p'.  For more details, see the documentation of
;; `macrostep-expand'.

;;; Code:

(require 'slime)
(eval-and-compile
  (require 'macrostep nil t)
  ;; Use bundled version if not separately installed
  (require 'macrostep "../lib/macrostep"))
(eval-when-compile (require 'cl-lib))

(defvar slime-repl-mode-hook)
(defvar slime-repl-mode-map)

(define-slime-contrib slime-macrostep
  "Interactive macro expansion via macrostep.el."
  (:authors "Luís Oliveira       <luismbo@gmail.com>"
            "Jon Oddie           <j.j.oddie@gmail.com>")
  (:license "GPL")
  (:swank-dependencies swank-macrostep)
  (:on-load
   (add-hook 'slime-mode-hook #'macrostep-slime-mode-hook)
   (define-key slime-mode-map (kbd "C-c M-e") #'macrostep-expand)
   (with-eval-after-load 'slime-repl
     (add-hook 'slime-repl-mode-hook #'macrostep-slime-mode-hook)
     (define-key slime-repl-mode-map (kbd "C-c M-e") #'macrostep-expand))))

(defun macrostep-slime-mode-hook ()
  (setq macrostep-sexp-at-point-function #'slime-sexp-at-point)
  (setq macrostep-environment-at-point-function (lambda () nil))
  (setq macrostep-expand-1-function #'macrostep-slime-expand-1)
  (setq macrostep-print-function #'macrostep-slime-insert)
  (setq macrostep-macro-form-p-function #'macrostep-slime-macro-form-p))

(defun macrostep-slime-expand-1 (string _ignore)
  (cl-destructuring-bind (expansion positions error-message)
      (slime-eval
       `(swank-macrostep:macrostep-expand-1
         ,string ,macrostep-expand-compiler-macros))
    (if error-message
        (error "%s" error-message)
      (list expansion positions))))

(defun macrostep-slime-insert (result _ignore)
  "Insert RESULT at point, indenting to match the current column."
  (cl-destructuring-bind (expansion positions) result
    (let* ((indent-string (concat "\n" (make-string (current-column) ? )))
           (expansion (replace-regexp-in-string "\n" indent-string expansion))
           (start (point))
           (column-offset (current-column)))
      (insert expansion)
      (macrostep-slime--propertize-macros start column-offset positions))))

(defun macrostep-slime--propertize-macros (start-offset column-offset positions)
  "Put text properties on macro forms."
  (dolist (position positions)
    (cl-destructuring-bind (_ type start start-line op-length)
        position
      (let ((opening-parenthesis-position
              (+ start-offset start (* column-offset start-line))))
        (put-text-property opening-parenthesis-position
                           (1+ opening-parenthesis-position)
                           'macrostep-macro-start
                           t)
        ;; this assumes that the operator starts right next to the
        ;; opening parenthesis. We could probably be more robust.
        (let ((op-start (1+ opening-parenthesis-position)))
          (put-text-property op-start
                             (+ op-start op-length)
                             'font-lock-face
                             (if (eq type :macro)
                                 'macrostep-macro-face
                                 'macrostep-compiler-macro-face)))))))

(defun macrostep-slime-macro-form-p (string _ignore)
  (when string
    (slime-eval
     `(swank-macrostep:macro-form-p
       ,string ,macrostep-expand-compiler-macros))))



(provide 'slime-macrostep)

;; Tests for slime-macrostep.  The following are expected failures:
;;
;; - highlighting of (compiler-)macro sub-forms fails on CLISP: see
;;   comment in SWANK-MACROSTEP::MAKE-TRACKING-PPRINT-DISPATCH.
;;
;; - SWANK::COLLECT-MACRO-FORMS currently fails to detect compiler
;;   macros on CLISP, ABCL and CCL.

(require 'slime-macrostep)
(require 'slime-tests)
(require 'cl-lib)

(defun slime-macrostep-eval-definitions (definitions)
  (slime-check-top-level)
  (slime-compile-string definitions 0)
  (slime-sync-to-top-level 5))

(defmacro slime-macrostep-with-text (buffer-text &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (lisp-mode)
     (save-excursion
       (insert ,buffer-text))
     ,@body))

(defun slime-macrostep-search (form)
  "Search forward for FORM, leaving point at its first character."
  (let ((case-fold-search t))
    (search-forward-lax-whitespace form))
  (goto-char (match-beginning 0)))



(def-slime-test (slime-macrostep-expand-defmacro)
    (definition buffer-text original expansion)
  "Test that simple macrostep expansion works."
  '(("(defmacro macrostep-dummy-macro (&rest args)
        `(expansion of ,@args))"

     "(progn
        (first body form)
        (second body form)
        (macrostep-dummy-macro (first (argument)) second (third argument))
        (remaining body forms))"

     "(macrostep-dummy-macro (first (argument)) second (third argument))"

     "(expansion of (first (argument)) second (third argument))"))
  (slime-macrostep-eval-definitions definition)
  (slime-macrostep-with-text buffer-text
    (slime-macrostep-search original)
    (macrostep-expand)
    (slime-test-expect "Macroexpansion is correct"
                       expansion
                       (downcase (slime-sexp-at-point))
                       #'slime-test-macroexpansion=)))

(def-slime-test (slime-macrostep-fontify-macros
                 (:fails-for "clisp"))
    (definition buffer-text original subform)
  "Test that macro forms in expansions are font-locked"
  '(("(defmacro macrostep-dummy-1 (&rest args)
        `(expansion including (macrostep-dummy-2 ,@args)))
      (defmacro macrostep-dummy-2 (&rest args)
        `(final expansion of ,@args))"

     "(progn
        (first body form)
        (second body form)
        (macrostep-dummy-1 (first (argument)) second (third argument))
        (remaining body forms))"

     "(macrostep-dummy-1 (first (argument)) second (third argument))"

     "(macrostep-dummy-2 (first (argument)) second (third argument))"))
  (slime-macrostep-eval-definitions definition)
  (slime-macrostep-with-text buffer-text
    (slime-macrostep-search original)
    (macrostep-expand)
    (slime-macrostep-search subform)
    (forward-char)                      ; move over open paren
    (slime-check "Head of macro form in expansion is fontified correctly"
        (eq (get-char-property (point) 'font-lock-face)
         'macrostep-macro-face))))

(def-slime-test (slime-macrostep-fontify-compiler-macros
                 (:fails-for "armedbear" "clisp" "ccl"))
    (definition buffer-text original subform)
  "Test that compiler-macro forms in expansions are font-locked"
  '(("(defmacro macrostep-dummy-3 (&rest args)
        `(expansion including (macrostep-dummy-4 ,@args)))
      (defun macrostep-dummy-4 (&rest args)
        args)
      (define-compiler-macro macrostep-dummy-4 (&rest args)
        `(compile-time expansion of ,@args))"

     "(progn
        (first body form)
        (second body form)
        (macrostep-dummy-3 first second third)
        (remaining body forms))"

     "(macrostep-dummy-3 first second third)"

     "(macrostep-dummy-4 first second third)"))
  (slime-macrostep-eval-definitions definition)
  (slime-macrostep-with-text buffer-text
    (slime-macrostep-search original)
    (let ((macrostep-expand-compiler-macros t))
      (macrostep-expand))
    (slime-macrostep-search subform)
    (forward-char)                      ; move over open paren
    (slime-check "Head of compiler-macro in expansion is fontified correctly"
        (eq (get-char-property (point) 'font-lock-face)
         'macrostep-compiler-macro-face))))

(provide 'slime-macrostep-tests)

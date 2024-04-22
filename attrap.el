;;; attrap.el --- ATtempt To Repair At Point  -*- lexical-binding: t -*-

;; Copyright (c) 2018 Jean-Philippe Bernardy


;; Author: Jean-Philippe Bernardy <jeanphilippe.bernardy@gmail.com>
;; Maintainer: Jean-Philippe Bernardy <jeanphilippe.bernardy@gmail.com>
;; URL: https://github.com/jyp/attrap
;; Created: February 2018
;; Keywords: programming, tools
;; Package-Requires: ((dash "2.12.0") (emacs "25.1") (f "0.19.0") (s "1.11.0"))
;; Version: 0.2

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Attrap! provides a command to attempt to fix the flycheck error at point.
;;
;; Users: Invoke the command `attrap-attrap' when point is on a
;; flycheck or flymake error, and check the results.  (If several
;; fixes apply you will be asked which one to apply.) Attrap!
;; currently comes with builtin fixers for haskell (GHC messages) and
;; elisp.
;;
;; Configuration: `attrap-flymake-backends-alist' is an alist from
;; flymake backend to attrap fixer.  `attrap-flycheck-checkers-alist'
;; is an alist from flycheck checker symbol to attrap fixer.  All the
;; See below for the definition of a fixer.
;;
;; A fixer is a element is a side-effect-free function mapping an
;; error message MSG to a list of options.  An option is a cons of a
;; description and a repair.  (Thus a list of options is an alist.)
;; The repair is a function of no argument which is meant to apply one
;; fix suggested by MSG in the current buffer, at point.  The
;; description is meant to be a summarized user-facing s-expr which
;; describes the repair.  This description can be used for example for
;; selecting the best repair.  An option can be conveniently defined
;; using `attrap-option'.  A singleton option list can be conveniently
;; defined using `attrap-one-option'.


;;; Code:
(require 'dash)
(require 's)

(declare-function flycheck-error-message "ext:flycheck" (cl-x))
(declare-function flycheck-overlays-at "ext:flycheck" (pos))
(declare-function flycheck-get-checker-for-buffer "ext:flycheck" ())
(declare-function flymake-diagnostics "ext:flycheck" (&optional beg end))
(declare-function flymake-diagnostic-backend "flymake" (diag))
(declare-function flymake-diagnostic-beg "flymake" (diag))
(declare-function flymake-diagnostic-end "flymake" (diag))
(declare-function flymake-diagnostic-text "flymake" (diag))

(defcustom attrap-flycheck-checkers-alist '((haskell-dante . attrap-ghc-fixer)
                                            (emacs-lisp . attrap-elisp-fixer))
  "An alist from flycheck checker symbol to attrap fixer."
  :type '(alist :key-type symbol :value-type function)
  :group 'attrap)

(declare-function flymake-flycheck-diagnostic-function-for 'flymake-flycheck (checker))
(with-eval-after-load 'flymake-flycheck
  (defalias 'attrap-flymake-hlint
    (flymake-flycheck-diagnostic-function-for 'haskell-hlint)))

(defcustom attrap-flymake-backends-alist
  '((dante-flymake . attrap-ghc-fixer)
    (LaTeX-flymake . attrap-LaTeX-fixer)
    (attrap-flymake-hlint . attrap-hlint-fixer)
    (elisp-flymake-byte-compile . attrap-elisp-fixer)
    (elisp-flymake-checkdoc . attrap-elisp-fixer))
  "An alist from flymake backend to attrap fixer."
  :type '(alist :key-type symbol :value-type function)
  :group 'attrap)

(defun attrap-select-and-apply-option (options)
  "Ask the user which of OPTIONS is best, then apply it."
  (when (not options) (error "No fixer applies to the issue at point"))
  (let* ((named-options (--map (cons (format "%s" (car it)) (cdr it)) options))
         (selected-fix (if (eq 1 (length options))
                           (car options)
                         (assoc (completing-read "repair using: "
                                                 named-options
                                                 nil
                                                 t)
                                named-options))))
    ;; (message "SELECTED-FIX: %s" selected-fix)
    ;; (message "Applied %s" (car selected-fix))
    (save-excursion
      (funcall (cdr selected-fix)))))

;;;###autoload
(defun attrap-flymake (pos)
  "Attempt to repair the flymake error at POS."
  (interactive "d")
  (let ((diags (flymake-diagnostics pos)))
    (when (not diags) (error "No flymake diagnostic at point"))
    (attrap-select-and-apply-option
     (-non-nil (--mapcat (let ((fixer (alist-get (flymake-diagnostic-backend it)
                                                 attrap-flymake-backends-alist)))
                           (when fixer (funcall fixer
                                                (flymake-diagnostic-text it)
                                                (flymake-diagnostic-beg it)
                                                (flymake-diagnostic-end it))))
                         diags)))))


;;;###autoload
(defun attrap-flycheck (pos)
  "Attempt to repair the flycheck error at POS."
  (interactive "d")
  (let ((messages (-filter
                   #'car
                   (--map (list (flycheck-error-message
                                 (overlay-get it 'flycheck-error))
                                (overlay-start it)
                                (overlay-end it))
                          (flycheck-overlays-at pos))))
        (checker (flycheck-get-checker-for-buffer)))
    (when (not messages) (error "No flycheck message at point"))
    (when (not checker) (error "No flycheck-checker for current buffer"))
    (let ((fixers (-map #'cdr (--filter (eq (car it) checker) attrap-flycheck-checkers-alist))))
      (when (not fixers) (error "No fixers for flycheck-checker %s" checker))
      (attrap-select-and-apply-option
       (-non-nil (-mapcat
                  (lambda (msg) (-mapcat
                                 (lambda (fixer) (apply fixer msg))
                                 fixers))
                  messages))))))

;;;###autoload
(defun attrap-attrap (pos)
  "Attempt to repair the error at POS."
  (interactive "d")
  (cond
   ((and (bound-and-true-p flyspell-mode)
         (fboundp 'flyspell-overlay-p)
         (-any #'flyspell-overlay-p (overlays-at (point))))
    (if (fboundp 'flyspell-correct-at-point)
        (flyspell-correct-at-point)))
   ((bound-and-true-p flymake-mode) (attrap-flymake pos))
   ((bound-and-true-p flycheck-mode) (attrap-flycheck pos))
   (t (error "Expecting flymake or flycheck to be active"))))

(defcustom attrap-haskell-extensions
  '("AllowAmbiguousTypes"
    "BangPatterns"
    "ConstraintKinds"
    "ConstrainedClassMethods"
    "DataKinds"
    "DefaultSignatures"
    "DeriveAnyClass"
    "DeriveDataTypeable"
    "DeriveFoldable"
    "DeriveFunctor"
    "DeriveGeneric"
    "DeriveTraversable"
    "DerivingStrategies"
    "DerivingVia"
    "EmptyCase"
    "EmptyDataDecls"
    "EmptyDataDeriving"
    "ExistentialQuantification"
    "ExplicitNamespaces"
    "FlexibleContexts"
    "FlexibleInstances"
    "FunctionalDependencies"
    "GADTs"
    "GeneralizedNewtypeDeriving"
    "ImportQualifiedPost"
    "InstanceSigs"
    "KindSignatures"
    "LambdaCase"
    "LinearTypes"
    "MonoLocalBinds"
    "MultiParamTypeClasses"
    "NamedFieldPuns"
    "ParallelListComp"
    "PartialTypeSignatures"
    "PatternGuards"
    "PatternSynonyms"
    "PolyKinds"
    "QuantifiedConstraints"
    "RankNTypes"
    "RecordWildCards"
    "ScopedTypeVariables"
    "StandaloneDeriving"
    "StandaloneKindSignatures"
    ; "StarIsType" ; not a 'cool' extension
    "TemplateHaskell"
    "TransformListComp"
    "TupleSections"
    "TypeApplications"
    "TypeFamilies"
    "TypeFamilyDependencies"
    "TypeInType"
    "TypeOperators"
    "TypeSynonymInstances"
    "UndecidableSuperClasses"
    "UndecidableInstances"
    "UnliftedNewtypes"
    "ViewPatterns")
  "Language extensions that Attrap can use to fix errors."
  :type '(repeat string)
  :group 'attrap)

(defmacro attrap-option (description &rest body)
  "Create an attrap option with DESCRIPTION and BODY.
The body is code that performs the fix."
  (declare (indent 1))
  `(let ((saved-match-data (match-data)))
     (cons ,description
           (lambda ()
             (set-match-data saved-match-data 'evaporate)
             ,@body))))

(defmacro attrap-one-option (description &rest body)
  "Create an attrap option list with a single element of DESCRIPTION and BODY."
  (declare (indent 1))
  `(list (attrap-option ,description ,@body)))

(defmacro attrap-alternatives (&rest clauses)
  "Append all succeeding clauses.
Each clause looks like (CONDITION BODY...).  CONDITION is
evaluated and, if the value is non-nil, this clause succeeds:
then the expressions in BODY are evaluated and the last one's
value is a list which is appended to the result of
`attrap-alternatives'.  Usage: (attrap-alternatives CLAUSES...)"
  `(append ,@(mapcar (lambda (c) `(when ,(car c) ,@(cdr c))) clauses)))

(defun attrap-elisp-fixer (msg _beg _end)
  "An `attrap' fixer for any elisp warning given as MSG."
  (append
   (when-let ((match (s-match "You should have a section marked \"\\(.*\\)\"" msg)))
     (attrap-one-option 'insert-section-header
       (beginning-of-line)
       (insert (nth 1 match) "\n")))
   (when (string-match "Lisp symbol ‘\\(.*\\)’ should appear in quotes" msg)
     (attrap-one-option 'kill-message-period
       (let ((sym (match-string 1 msg)))
         (re-search-forward sym)
         (replace-match (concat "`" sym "'") nil t nil 0))))
   (when (string-match "Error messages should \\*not\\* end with a period" msg)
     (attrap-one-option 'kill-message-period
       (let ((case-fold-search nil))
         (re-search-forward "\\.\"" (line-end-position))
         (replace-match "\"" nil t nil 0))))
   (when (string-match "Name emacs should appear capitalized as Emacs" msg)
     (attrap-one-option 'capitalize-emacs
       (let ((case-fold-search nil))
         (re-search-forward "emacs" (line-end-position))
         (replace-match "Emacs" nil t nil 0))))
   (when (string-match "should be capitalized" msg)
     (attrap-one-option 'capitalize
       (capitalize-word 1)))
   (when (string-match "You should have a section marked \"" msg)
     (attrap-one-option 'capitalize
       (capitalize-word 1)))
   (when (string-match "White space found at end of line" msg)
     (attrap-one-option 'delete-trailing-space
       (end-of-line)
       (delete-region (point) (progn (skip-chars-backward "\t ") (point)))))
   (when (string-match "There should be two spaces after a period" msg)
     (attrap-one-option 'add-space
       (beginning-of-line)
       (re-search-forward "\\(\\.\\) [^ ]" (line-end-position))
       (replace-match ". " nil t nil 1)))
   (when (string-match "might as well have a documentation" msg)
     (attrap-one-option 'add-empty-doc
       (beginning-of-line)
       (insert "  \"\"\n")))
   (when (string-match "should have documentation" msg)
     (attrap-one-option 'add-empty-doc
       (beginning-of-line)
       (insert "  \"\"\n")))
   (when (string-match "The footer should be: " msg)
     (let ((footer (s-replace "\\n" "\n" (substring msg (match-end 0)))))
       (attrap-one-option 'add-footer
         (end-of-line)
         (insert (concat "\n" footer)))))
   (when (string-match "First line is not a complete sentence" msg)
     (attrap-one-option 'merge-lines
       (end-of-line)
       (delete-char 1)))
   (when (string-match "First sentence should end with punctuation" msg)
     (attrap-one-option 'add-punctuation
       (insert ".")))))

(defmacro attrap-insert-language-pragma (extension)
  "Action: Insert language language EXTENSION pragma at beginning of file."
  `(attrap-option (list 'use-extension ,extension)
    (goto-char 1)
    (insert (concat "{-# LANGUAGE " ,extension " #-}\n"))))

(defmacro attrap-add-to-import (missing module line col)
  "Action: insert MISSING to the import of MODULE.
The import ends at LINE and COL in the file."
  `(attrap-option (list 'add-to-import-list ,module)
     (let ((end-line (string-to-number ,line))
           (end-col (string-to-number ,col)))
       (goto-char (point-min))
       (forward-line (1- end-line))
       (move-to-column (1- end-col))
       (skip-chars-backward " \t")
       (unless (looking-back "(" (- (point) 2)) (insert ","))
       (insert (attrap-add-operator-parens ,missing)))))

(defun attrap-ghc-fixer (msg pos _end)
  "An `attrap' fixer for any GHC error or warning.
Error is given as MSG and reported between POS and END."
  (let ((normalized-msg (s-collapse-whitespace msg)))
  (rx-let ((parens (body) (seq "(" body ")"))
           (lin-col (l c) (seq "(" (group-n l (* num)) "," (group-n c (* num))")"))
           (multiline-span (l1 c1 l2 c2) (seq (lin-col l1 c1) "-" (lin-col l2 c2)))
           (monoline-span (l1 c1 l2 c2) (seq (group-n l2 (group-n l1 (* num))) ":" (group-n c1 (* num)) "-" (group-n c2 (* num))))
           (any-span (l1 c1 l2 c2) (or (monoline-span l1 c1 l2 c2) (multiline-span l1 c1 l2 c2)))
           (src-loc (l1 c1 l2 c2) (seq (* (not ":"))":" (any-span l1 c1 l2 c2)))
           (module-name (+ (any "_." alphanumeric)))
           (identifier (n) (seq "‘" (group-n n (* (not "’"))) "’")))
  (append
   (when (string-match "Parse error in pattern: pattern" msg)
     (list (attrap-insert-language-pragma "PatternSynonyms")))
   (when (string-match "No explicit implementation for" msg)
    (attrap-one-option 'insert-method
      (let ((missings (s-match-strings-all "‘\\([^’]*\\)’"
                                           (car (s-split-up-to "In the instance declaration" msg 1)))))
        (end-of-line)
        (dolist (missing missings)
          (insert (format "\n  %s = _" (nth 1 missing)))))))
   (when (string-match "No explicit associated type or default declaration for ‘\\(.*\\)’" msg)
    (attrap-one-option 'insert-type
      (let ((type (match-string 1 msg)))
        (end-of-line)
        (insert (format "\n  type %s = _" type)))))
   (when (s-matches? (rx "Using ‘*’ (or its Unicode variant) to mean ‘Data.Kind.Type’") msg)
    (attrap-one-option 'replace-star-by-Type
      (goto-char pos)
      (delete-char 1)
      (insert "Type")
      (unless (search-backward-regexp "import.*Data\.Kind" nil t)
        (search-backward-regexp "^module")
        (end-of-line)
        (insert "\nimport Data.Kind (Type)"))))
   (when (string-match "Valid hole fits include" msg)
    (let* ((options (-map 'cadr (-non-nil (--map (s-match "[ ]*\\([^ ]*\\) ::" it) (s-split "\n" (substring msg (match-end 0))))))))
      (--map (attrap-option (list 'plug-hole it)
               (goto-char pos)
               (delete-char 1)
               (insert it))
             options)))
   (when (string-match "Redundant constraints?: (?\\([^,)\n]*\\)" msg)
    (attrap-one-option 'delete-redundant-constraint
      (let ((constraint (match-string 1 msg)))
        (search-forward constraint) ; find type sig
        (delete-region (match-beginning 0) (match-end 0))
        (when (looking-at "[ \t]*,")
          (delete-region (point) (search-forward ",")))
        (when (looking-at "[ \t]*=>")
          (delete-region (point) (search-forward "=>"))))))
   (when (string-match "The type signature for ‘\\(.*\\)’[ \t\n]*lacks an accompanying binding" msg)
    (attrap-one-option 'add-binding
      (beginning-of-line)
      (forward-line)
      (insert (concat (match-string 1 msg) " = _\n"))))
   (when (string-match "add (\\(.*\\)) to the context of[\n ]*the type signature for:[ \n]*\\([^ ]*\\) ::" msg)
    (attrap-one-option 'add-constraint-to-context
      (let ((missing-constraint (match-string 1 msg))
            (function-name (match-string 2 msg)))
        (search-backward-regexp (concat (regexp-quote function-name) "[ \t]*::[ \t]*" )) ; find type sig
        (goto-char (match-end 0))
        (when (looking-at "forall\\|∀") ; skip quantifiers
          (search-forward "."))
        (skip-chars-forward "\n\t ") ; skip spaces
        (insert (concat missing-constraint " => ")))))
   (when (string-match "Unticked promoted constructor: ‘\\(.*\\)’" msg)
    (let ((constructor (match-string 1 msg)))
      (attrap-one-option 'tick-promoted-constructor
        (goto-char pos)
        ;; when the constructor is infix, flycheck reports the wrong position.
        (search-forward constructor)
        (backward-char (length constructor))
        (insert "'"))))
   (when (string-match "Patterns not matched:" msg)
    (attrap-one-option 'add-missing-patterns
      (let ((patterns (mapcar #'string-trim (split-string (substring msg (match-end 0)) "\n" t " ")))) ;; patterns to match
        (if (string-match "In an equation for ‘\\(.*\\)’:" msg)
            (let ((function-name (match-string 1 msg)))
              (end-of-line)
              (dolist (pattern patterns)
                (insert (concat "\n" function-name " " pattern " = _"))))
          (end-of-line) ;; assuming that the case expression is on multiple lines and that "of" is at the end of the line
          (dolist (pattern patterns)
            (insert "\n     ") ;; fixme: guess how much indent is needed.
            (insert (concat pattern " -> _")))))))
   (when (string-match "A do-notation statement discarded a result of type" msg)
    (attrap-one-option 'explicitly-discard-result
      (goto-char pos)
      (insert "_ <- ")))
   (when (string-match "\\(Failed to load interface for\\|Could not find module\\) ‘\\(.*\\)’\n[ ]*Perhaps you meant[ \n]*\\([^ ]*\\)" msg)
    (attrap-one-option 'rename-module-import
      (let ((replacement (match-string 3 msg)))
        ;; ^^ delete-region may garble the matches
        (search-forward (match-string 2 msg))
        (delete-region (match-beginning 0) (point))
        (insert replacement))))
   (when (string-match "Unsupported extension: \\(.*\\)\n[ ]*Perhaps you meant ‘\\([^‘]*\\)’" msg)
    (attrap-one-option 'rename-extension
      (let ((replacement (match-string 2 msg)))
        ;; ^^ delete-region may garble the matches
        (goto-char pos)
        (search-forward (match-string 1 msg))
        (delete-region (match-beginning 0) (point))
        (insert replacement))))
   (when-let ((match (s-match (rx "Perhaps you want to add " (identifier 1)
                                  " to the import list in the import of " (identifier 2)
                                  " " (parens (src-loc 3 4 5 6)))
                              normalized-msg)))
     (list (attrap-add-to-import (nth 1 match) (nth 2 match) (nth 5 match) (nth 6 match))))
   (when-let ((match (s-match (rx "Perhaps you want to add " (identifier 1)
                                  " to one of these import lists:")
                              normalized-msg)))
     (--map (attrap-add-to-import (nth 1 match) (nth 2 it) (nth 5 it) (nth 6 it))
            (s-match-strings-all (rx (identifier 2) " " (parens (src-loc 3 4 5 6))) msg)))
    ;; Not in scope: data constructor ‘SimpleBroadcast’
    ;; Perhaps you meant ‘SimpleBroadCast’ (imported from TypedFlow.Types)
    ;; Not in scope: ‘BackCore.argmax’
    ;;     Perhaps you meant one of these:
    ;;       ‘BackCore.argMax’ (imported from TensorFlow.GenOps.Core),
    ;;       ‘BackCore.argMax'’ (imported from TensorFlow.GenOps.Core),
    ;;       ‘BackCore.max’ (imported from TensorFlow.GenOps.Core)
    ;;       ‘BackCore.maxx’ (line 523)
   (when-let ((match
               (s-match (rx (or (seq (or "Data constructor" "Variable") " not in scope:"
                                     (* (any " \n\t")) (group-n 1 (+ (not (any " \n")))))
                                (seq "Not in scope: "
                                     (or "" "data constructor " "type constructor or class ") (identifier 1))))
                        msg)))
    (let* ((delete (nth 1 match))
           (delete-has-paren (eq ?\( (elt delete 0)))
           (delete-no-paren (if delete-has-paren (substring delete 1 (1- (length delete))) delete))
           (rest (nth 1 (s-match (rx "Perhaps you meant" (? " one of these:") (group (+ anychar))) normalized-msg)))
           (replacements (s-match-strings-all
                          (rx (identifier 1) " "
                              (parens (or (seq "imported from " (group-n 2 module-name))
                                          (group-n 2 (seq "line "  (* num))))))
                          rest)))
      (--map (attrap-option (list 'replace delete-no-paren 'by (nth 1 it) 'from (nth 2 it))
               (goto-char pos)
               (let ((case-fold-search nil))
                 (search-forward delete-no-paren (+ (length delete) pos))
                 (replace-match (nth 1 it) t)))
             replacements)))
    (when (string-match "It could refer to" msg) ;; ambiguous identifier
     (let ((replacements (--map (nth 1 it) (s-match-strings-all  (rx (identifier 1) ",") msg))))
       (--map (attrap-option (list 'rename it)
                (apply #'delete-region (dante-ident-pos-at-point))
                (insert it))
              replacements)))
   (when (string-match "\\(Top-level binding\\|Pattern synonym\\) with no type signature:[\n ]*" msg)
    (attrap-one-option 'add-signature
      (beginning-of-line)
      (insert (concat (substring msg (match-end 0)) "\n"))))
   (when (string-match "Defined but not used" msg)
    (attrap-one-option 'add-underscore
      (goto-char pos)
      (insert "_")))
   (when (string-match "Unused quantified type variable ‘\\(.*\\)’" msg)
    (attrap-one-option 'delete-type-variable
      ;; note there can be a kind annotation, not just a variable.
      (delete-region (point) (+ (point) (- (match-end 1) (match-beginning 1))))))
   ;;     Module ‘TensorFlow.GenOps.Core’ does not export ‘argmax’.
   (when-let ((m (s-match (rx "No module named " (identifier 1) " is imported.") msg)))
     (attrap-one-option (list 'add-import (nth 1 m))
       (goto-char 1)
       (search-forward-regexp (rx "module" (*? anychar) "where"))
       (insert "\n" "import " (nth 1 m) "\n")))
   (when-let ((match (s-match (rx (or (seq "The " (? "qualified ") "import of " (identifier 1)
                                           " from module " (identifier 2) " is redundant")
                                      (seq "Module " (identifier 2) " does not export " (identifier 1))))
                        normalized-msg)))
    (attrap-one-option 'delete-import
      (let ((redundant (nth 1 match)))
        (save-excursion
          (when (looking-at (rx "import"))
            ; if there are several things redundant, the message starts at 'import'
              (search-forward "(")) ; the imported things are after the parenthesis
          (dolist (r (s-split ", " redundant t))
          (save-excursion
            (re-search-forward (rx-to-string (if (s-matches? (rx bol alphanumeric) r)
                                                 `(seq word-start ,r word-end) ; regular ident
                                               `(seq "(" ,r ")")))) ; operator
            (replace-match "")
            (when (looking-at "(..)") (delete-char 4))
            (when (or (looking-at (rx (* space) ","))
                      (looking-back (rx "," (* space)) (line-beginning-position)))
              (replace-match ""))))))))
   (when (string-match (rx "The " (? "qualified ") "import of " (identifier 1) " is redundant") msg)
    (attrap-one-option 'delete-module-import
      (delete-region
       (point)
       (progn
         (unless (looking-at
                  (rx "import" (+ space) module-name (? (+ space) "hiding") (* space)))
           (error "Import statement not found"))
         (goto-char (match-end 0))
         (when (looking-at "(") ; skip the import list if any
           (forward-sexp))
         (skip-chars-forward "\t\n ")
         (point)))))
   (when (string-match "Found type wildcard ‘\\(.*\\)’[ \t\n]*standing for ‘\\([^’]*\\)’" msg)
    (attrap-one-option 'explicit-type-wildcard
      (let ((wildcard (match-string 1 msg))
            (type-expr (match-string 2 msg)))
        (goto-char pos)
        (search-forward wildcard)
        (replace-match (concat "(" type-expr ")") t))))
   (when (and (string-match-p "parse error on input ‘case’" msg) ; Obsolete with GHC 9, which appears to recognize Lambda case specially.
              (save-excursion
                (goto-char pos)
                (string-match-p (rx "\\case\\_>") (buffer-substring-no-properties pos (line-end-position)))))
     (list (attrap-insert-language-pragma "LambdaCase")))
   (when (s-matches? (rx (or "Illegal symbol ‘forall’ in type"
                             (seq "Perhaps you intended to use"
                                  (* anything)
                                  "language extension to enable explicit-forall syntax")))
                     normalized-msg)
     (list (attrap-insert-language-pragma "ScopedTypeVariables")))
   (when-let (match (s-match (rx "Fields of " (identifier 1) " not initialised: "
                                 (group-n 2 (+ (not (any "•")))) "•")
                             msg))
     (attrap-one-option 'initialize-fields
       (let ((fields (s-split "," (nth 2 match) t)))
         (search-forward "{")
         (dolist (f fields)
           (insert (format ",%s = _\n" (s-trim f)))))))
   (--map (attrap-insert-language-pragma it)
          (--filter (s-matches? it normalized-msg) attrap-haskell-extensions))))))

(defun attrap-add-operator-parens (name)
  "Add parens around a NAME if it refers to a Haskell operator."
  (if (string-match-p "^[[:upper:][:lower:]_']" name)
      name
    (concat "(" name ")")))

(defun attrap-hlint-fixer (msg pos end)
  "Fixer for any hlint hint given as MSG and reported between POS and END."
  (rx-let ((indented-line (seq space (* not-newline) "\n"))
           (snippet (+ indented-line)))
  (cond
   ((s-matches? (rx (or "Perhaps you should remove it."
                        "Use fewer LANGUAGE pragmas"))
                msg)
    (attrap-one-option 'kill-unused
      (delete-region pos (+ 2 end))))
   ((s-matches? (rx "Redundant $") msg)
    (attrap-one-option 'kill-dollar
      (delete-region pos (+ 1 end))))
   ((s-matches? (rx "Redundant bracket") msg)
    (attrap-one-option 'kill-brackets
      (delete-region pos (1+ pos))
      (delete-region (1- end) end)))
   ((string-match
     (rx "Found:\n"
         (group snippet)
         "Perhaps:\n"
         (group snippet)
         (? (seq "Note: " (+ not-newline) "\n"))
         (* space) "[haskell-hlint]")
     msg)
    (let ((replacement (match-string 2 msg)))
      (attrap-one-option 'replace-as-hinted
        (delete-region pos (+ 1 end))
        (insert (s-trim (s-collapse-whitespace replacement)))))))))

(defun attrap-LaTeX-fixer (msg pos _end)

  (cond
   ((s-matches? (rx "Use either `` or '' as an alternative to `\"'.")msg) 
    (list (attrap-option 'fix-open-dquote
            (delete-region pos (1+ pos))
            (insert "``"))
          (attrap-option 'fix-close-dquote
            (delete-region pos (1+ pos))
            (insert "''"))))
   ((s-matches? (rx "Non-breaking space (`~') should have been used.") msg)
    (attrap-one-option 'non-breaking-space
      (if (looking-at (rx space))
          (delete-region pos (1+ pos))
          (delete-region (save-excursion (skip-chars-backward "\n\t ") (point)) (point)))
      (insert "~")))
   ((s-matches? (rx "Interword spacing (`\\ ') should perhaps be used.") msg)
    (attrap-one-option 'use-interword-spacing
      (delete-region pos (1+ pos))
      (insert "\\ ")))
   ((s-matches? (rx "Delete this space to maintain correct pagereferences.") msg)
    (attrap-one-option 'fix-space-pageref
      (if (looking-back (rx bol (* space)))
          (progn (skip-chars-backward "\n\t ")
                 (insert "%"))
        (delete-region (point) (save-excursion (skip-chars-forward " \t") (point))))))))

(provide 'attrap)
;;; attrap.el ends here


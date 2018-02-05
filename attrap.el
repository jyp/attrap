;;; attrap.el --- ATtempt To Repair At Point  -*- lexical-binding: t -*-

;; Copyright (c) 2018 Jean-Philippe Bernardy


;; Author: Jean-Philippe Bernardy <jeanphilippe.bernardy@gmail.com>
;; Maintainer: Jean-Philippe Bernardy <jeanphilippe.bernardy@gmail.com>
;; URL: https://github.com/jyp/attrap
;; Created: February 2018
;; Keywords: programming, tools
;; Package-Requires: ((dash "2.12.0") (emacs "25.1") (f "0.19.0") (flycheck "0.30") (s "1.11.0"))
;; Version: 0.1

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
;; flycheck error, and check the results.  Attrap! currently comes
;; with builtin fixers for `haskell-dante' and `emacs-lisp'.
;;
;; Emacs mode providers: mode-dependent fixers can be declared by
;; setting the relevant flycheck checker symbol `attrap-fixers'
;; property.  This property is a list of fixers.  A fixer is a element
;; is a side-effect-free function mapping an error message MSG to a
;; list of options.  An option is a cons of a description and a
;; repair.  (Thus a list of options is an alist.) The repair is a
;; function of no argument which is meant to apply one fix suggested
;; by MSG in the current buffer, at point.  The description is meant
;; to be a summarized user-facing description of the repair, for
;; example for selecting the best repair.  An option can be
;; conveniently defined using `attrap-option'.  A singleton option
;; list can be conveniently defined using `attrap-one-option'.


;;; Code:
(require 'dash)
(require 's)
(require 'flycheck)

;;;###autoload
(defun attrap-attrap (pos)
  "Attempt to repair the flycheck error at POS."
  (interactive "d")
  (let ((messages (-filter
                   #'car
                   (--map (list (flycheck-error-message
                                 (overlay-get it 'flycheck-error))
                                (overlay-start it))
                          (flycheck-overlays-at pos))))
        (checker (flycheck-get-checker-for-buffer)))
    (when (not messages) (error "No flycheck message at point"))
    (message "awutn:%s" messages)
    (when (not checker) (error "No flycheck-checker for current buffer"))
    (let ((fixers (get checker 'attrap-fixers)))
      (when (not fixers) (error "No fixers for flycheck-checker %s" checker))
      (let* ((options (-non-nil (-mapcat
                                 (lambda (msg) (-mapcat
                                                (lambda (fixer) (apply fixer msg))
                                                fixers))
                                 messages)))
             (named-options (--map (cons (format "%s" (car it)) (cdr it)) options)))
        (when (not options) (error "No fixer applies to the issue at point"))
        (let ((selected-fix (if (eq 1 (length options))
                                (car options)
                              (assoc (completing-read "repair using: "
                                                      named-options
                                                      nil
                                                      t)
                                     named-options))))
          (message "Applied %s" (car selected-fix))
          (save-excursion
            (funcall (cdr selected-fix))))))))

(defcustom attrap-haskell-extensions
  '("AllowAmbiguousTypes" "BangPatterns" "ConstraintKinds" "DataKinds" "DeriveFoldable" "DeriveFunctor" "DeriveGeneric" "DeriveTraversable" "EmptyCase" "FlexibleContexts" "FlexibleInstances" "FunctionalDependencies" "GADTs" "GeneralizedNewtypeDeriving" "InstanceSigs" "KindSignatures" "MultiParamTypeClasses" "PartialTypeSignatures" "PatternSynonyms" "PolyKinds" "RankNTypes" "RecordWildCards" "ScopedTypeVariables" "StandaloneDeriving" "TransformListComp" "TupleSections" "TypeApplications" "TypeFamilies" "TypeInType" "TypeOperators" "TypeSynonymInstances" "UndecidableSuperClasses" "UndecidableInstances" "ViewPatterns" )
  "Language extensions that Attrap can use to fix errors."
  :group 'dante
  :type '(repeat string))

(defmacro attrap-option (description &rest body)
  "Create an attrap option with DESCRIPTION and BODY."
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
Each clause looks like (CONDITION BODY...).  CONDITION is evaluated
and, if the value is non-nil, this clause succeeds:
then the expressions in BODY are evaluated and the last one's
value is a list which is appended to the result of attrap-alternatives.
usage: (attrap-alternatives CLAUSES...)"
  `(append ,@(mapcar (lambda (c) `(when ,(car c) ,@(cdr c))) clauses)))

(defun attrap-elisp-fixer (msg _)
  "An `attrap' fixer for any elisp warning given as MSG."
  (attrap-alternatives
   ((string-match "White space found at end of line" msg)
    (attrap-one-option 'delete-trailing-space
      (end-of-line)
      (delete-region (point) (progn (skip-chars-backward "\t ") (point)))))
   ((string-match "There should be two spaces after a period" msg)
    (attrap-one-option 'add-space
      (beginning-of-line)
      (re-search-forward "\\(\\.\\) [^ ]" (line-end-position))
      (replace-match ". " nil t nil 1)))
   ((string-match "might as well have a documentation" msg)
    (attrap-one-option 'add-empty-doc
      (beginning-of-line)
      (insert "  \"\"\n")))
   ((string-match "The footer should be: " msg)
    (let ((footer (s-replace "\\n" "\n" (substring msg (match-end 0)))))
      (attrap-one-option 'add-footer
        (end-of-line)
        (insert (concat "\n" footer)))))
   ((string-match "First line is not a complete sentence" msg)
    (attrap-one-option 'merge-lines
      (end-of-line)
      (delete-char 1)))
   ((string-match "First sentence should end with punctuation" msg)
    (attrap-one-option 'add-punctuation
      (end-of-line)
      (backward-char)
      (insert ".")))))

(put 'emacs-lisp 'attrap-fixers (list 'attrap-elisp-fixer))


(defun attrap--search-here (string)
  "Search for STRING if the point if within it."
  (let ((p (point)))
    (forward-char (length string))
    (message "ASH: %s" string)
    (search-backward string (- p (length string)) nil)))

(defun attrap-ghc-fixer (msg pos)
  "An `attrap' fixer for any GHC error or warning given as MSG and reported at POS."
  (cond
   ((string-match "Redundant constraints?: (?\\([^,)\n]*\\)" msg)
    (attrap-one-option 'delete-reduntant-constraint
      (let ((constraint (match-string 1 msg)))
        (search-forward constraint) ; find type sig
        (delete-region (match-beginning 0) (match-end 0))
        (when (looking-at "[ \t]*,")
          (delete-region (point) (search-forward ",")))
        (when (looking-at "[ \t]*=>")
          (delete-region (point) (search-forward "=>"))))))
   ((string-match "The type signature for ‘\\(.*\\)’[ \t\n]*lacks an accompanying binding" msg)
    (attrap-one-option 'add-binding
      (beginning-of-line)
      (forward-line)
      (insert (concat (match-string 1 msg) " = _\n"))))
   ((string-match "add (\\(.*\\)) to the context of[\n ]*the type signature for:[ \n]*\\([^ ]*\\) ::" msg)
    (attrap-one-option 'add-constraint-to-context
      (let ((missing-constraint (match-string 1 msg))
            (function-name (match-string 2 msg)))
        (search-backward-regexp (concat (regexp-quote function-name) "[ \t]*::[ \t]*" )) ; find type sig
        (goto-char (match-end 0))
        (when (looking-at "forall\\|∀") ; skip quantifiers
          (search-forward "."))
        (skip-chars-forward "\n\t ") ; skip spaces
        (insert (concat missing-constraint " => ")))))
   ((string-match "Unticked promoted constructor: ‘\\(.*\\)’" msg)
    (let ((constructor (match-string 1 msg)))
      (attrap-one-option 'tick-promoted-constructor
        (goto-char pos)
        ;; when the constructor is infix, flycheck reports the wrong position.
        (search-forward constructor)
        (backward-char (length constructor))
        (insert "'"))))
   ((string-match "Patterns not matched:" msg)
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
   ((string-match "A do-notation statement discarded a result of type" msg)
    (attrap-one-option 'explicitly-discard-result
      (goto-char pos)
      (insert "_ <- ")))
   ((string-match "Failed to load interface for ‘\\(.*\\)’\n[ ]*Perhaps you meant[ \n]*\\([^ ]*\\)" msg)
    (attrap-one-option 'rename-module-import
      (let ((replacement (match-string 2 msg)))
        ;; ^^ delete-region may garble the matches
        (search-forward (match-string 1 msg))
        (delete-region (match-beginning 0) (point))
        (insert replacement))))
   ((string-match "Perhaps you want to add ‘\\(.*\\)’ to the import list[\n\t ]+in the import of[ \n\t]*‘.*’[\n\t ]+([^:]*:\\([0-9]*\\):[0-9]*-\\([0-9]*\\))" msg)
    (attrap-one-option 'add-to-import-list
      (let ((missing (match-string 1 msg))
            (line (string-to-number (match-string 2 msg)))
            (end-col (string-to-number (match-string 3 msg))))
        (goto-char (point-min))
        (forward-line (1- line))
        (move-to-column (1- end-col))
        (skip-chars-backward " \t")
        (unless (looking-back "(" (- (point) 2)) (insert ","))
        (insert missing))))
   ((string-match "not in scope: \\([^ ]*\\)" msg)
    (let* ((delete (match-string 1 msg))
           (delete-has-paren (eq ?\( (elt delete 0)))
           (delete-no-paren (if delete-has-paren (substring delete 1 (1- (length delete))) delete))
           (replacements (s-match-strings-all "‘\\([^‘]*\\)’ (\\([^)]*\\))" msg)))
      (--map (attrap-option (list 'replace delete-no-paren (nth 1 it) (nth 2 it))
               ;; ^^ delete-region may garble the matches
               (goto-char pos)
               (search-forward delete-no-paren (+ (length delete) pos))
               (replace-match (nth 1 it)))
             replacements)))
   ((string-match "\\(Top-level binding\\|Pattern synonym\\) with no type signature:[\n ]*" msg)
    (attrap-one-option 'add-signature
      (beginning-of-line)
      (insert (concat (substring msg (match-end 0)) "\n"))))
   ((string-match "Defined but not used" msg)
    (attrap-one-option 'add-underscore
      (goto-char pos)
      (insert "_")))
   ((string-match "Unused quantified type variable ‘\\(.*\\)’" msg)
    (attrap-one-option 'delete-type-variable
      ;; note there can be a kind annotation, not just a variable.
      (delete-region (point) (+ (point) (- (match-end 1) (match-beginning 1))))))
   ((string-match "The import of ‘\\(.*\\)’ from module ‘[^’]*’ is redundant" msg)
    (attrap-one-option 'delete-import
      (let ((redundant (match-string 1 msg)))
        (search-forward redundant)
        (replace-match "")
        (when (looking-at ",") (delete-char 1)))))
   ((string-match "The import of ‘[^’]*’ is redundant" msg)
    (attrap-one-option 'delete-module-import
      (beginning-of-line)
      (delete-region (point) (progn (next-logical-line) (point)))))
   ((string-match "Found type wildcard ‘\\(.*\\)’[ \t\n]*standing for ‘\\(.*\\)’" msg)
    (attrap-one-option 'explicit-type-wildcard
      (let ((wildcard (match-string 1 msg))
            (type-expr (match-string 2 msg)))
        (goto-char pos)
        (search-forward wildcard)
        (replace-match (concat "(" type-expr ")")))))
   ((--any? (s-matches? it msg) attrap-haskell-extensions)
    (--map (attrap-option (list 'use-extension it)
             (goto-char 1)
             (insert (concat "{-# LANGUAGE " it " #-}\n")))
           (--filter (s-matches? it msg) attrap-haskell-extensions)))))

(put 'haskell-dante 'attrap-fixers (list 'attrap-ghc-fixer))
(provide 'attrap)
;;; attrap.el ends here


;;; tla+-mode.el --- Major mode for TLA+  -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2024 Raven Hallsby

;; Author: Raven Hallsby <karl@hallsby.com>
;; Maintainer: Raven Hallsby <karl@hallsby>

;; Homepage: https://github.com/KarlJoad/tla+-mode
;; Keywords: TLA+ languages tree-sitter

;; Package-Version: 0.0.1-git
;; Package-Requires: (
;;     (emacs "29.1"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; tla+-mode is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; tla+-mode is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with tla+-mode.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; tla+-mode is a major-mode for TLA+ that leverages the excellent
;; tree-sitter-tlaplus tre-sitter parser for unmatched syntax highlighting
;; accuracy and REPL tools to provide an interactive development environment
;; for both TLC- and Apalache-checked models and TLAPS-proved theorems.

;; tla+-mode inherits code from two major sources:
;;   1. https://github.com/carlthuringer/tla-mode
;;   2. https://git.sdf.org/bch/tlamode
;; The first variant used tree-sitter to provide excellent syntax highlighting.
;; The second included many helpful tools, and keybindings, such as being able
;; to access tlc2-repl from within Emacs.

;;; Code:

(require 'treesit)
(eval-when-compile (require 'rx))

(defgroup tla+ nil
  "Support for the TLA+ model specification language."
  :tag "TLA+"
  :group 'languages
  :version "29.1"
  :link '(custom-manual "(tla+) Top")
  :link '(info-link "(tla+) Customization")
  :link '(url-link "https://github.com/KarlJoad/tla+-mode")
  :link '(emacs-commentary-link :tag "Commentary" "tla+-mode.el")
  :link '(emacs-library-link :tag "Lisp File" "tla+-mode.el"))

(defun tla+-mode-comment-setup ()
  "Set up local variables for TLA+ comments.

Set up:
 - `comment-start'
 - `comment-end'
 - `comment-start-skip'
 - `comment-end-skip'
 - `adaptive-fill-mode'
 - `adaptive-fill-first-line-regexp'
 - `paragraph-start'
 - `paragraph-separate'
 - `fill-paragraph-function'"

  ;; \* is a line-comment, (* *) is a block comment, which can cover multiple
  ;; lines.
  (setq-local comment-start "\\*")
  (setq-local comment-end "")

  (setq-local comment-start-skip (rx (or (seq "\\" (+ "*"))
                                         (seq "(" (+ "*")))
                                     (* (syntax whitespace))))
  (setq-local comment-end-skip
              (rx (* (syntax whitespace))
                  (group (or (syntax comment-end)
                             (seq (+ "*") ")")))))

  (setq-local adaptive-fill-mode t)

  ;; This matches (1) empty spaces (the default), (2) "\*", (3) "(*",
  ;; but do not match "*)", because we don't want to use "*)" as
  ;; prefix when filling.  (Actually, it doesn't matter, because
  ;; `comment-start-skip' matches "(*" which will cause
  ;; `fill-context-prefix' to use "(*" as a prefix for filling, that's
  ;; why we mask the "(*" in `tla+-mode--fill-paragraph'.)
  (setq-local adaptive-fill-regexp
              (concat (rx (* (syntax whitespace))
                          (group (seq (or "\\" "(") (+ "*") (* "*"))))
                      adaptive-fill-regexp))
  ;; Note the missing * comparing to `adaptive-fill-regexp'.  The
  ;; reason for its absence is a bit convoluted to explain.  Suffice
  ;; to say that without it, filling a single line paragraph that
  ;; starts with /* doesn't insert * at the beginning of each
  ;; following line, and filling a multi-line paragraph whose first
  ;; two lines start with * does insert * at the beginning of each
  ;; following line.  If you know how does adaptive filling works, you
  ;; know what I mean.
  (setq-local adaptive-fill-first-line-regexp
              (rx bos
                  (seq (* (syntax whitespace))
                       (group (seq "/" (+ "/")))
                       (* (syntax whitespace)))
                  eos))
  ;; Same as `adaptive-fill-regexp'.
  (setq-local paragraph-start
              (rx (or (seq (* (syntax whitespace))
                           (group (or (seq "/" (+ "/")) (* "*")))
                           (* (syntax whitespace))
                           ;; Add this eol so that in
                           ;; `fill-context-prefix', `paragraph-start'
                           ;; doesn't match the prefix.
                           eol)
                      "\f")))
  (setq-local paragraph-separate paragraph-start)
  (setq-local fill-paragraph-function #'tla+-mode--fill-paragraph))

(defvar treesit-load-name-override-list)

(defun tla+-mode--set-modeline ()
  "Set Emacs modeline with this major-mode's name."
  (setq mode-name "TLA+")
  (force-mode-line-update))

(defun tla+-mode--font-lock-settings ()
  "Tree-sitter font-lock settings."
  (treesit-font-lock-rules
   ;; FIXME: This is taken directly from c-ts-mode! This needs to be customized
   ;; to the concrete parse tree that tree-sitter returns!
   :language 'tlaplus
   :feature 'comment
   `((comment) @font-lock-comment-face
     (comment) @contextual)

   :language 'tlaplus
   :feature 'constant
   `((true) @font-lock-constant-face
     (false) @font-lock-constant-face
     (null) @font-lock-constant-face)

   :language 'tlaplus
   :feature 'keyword
   `([,@(c-ts-mode--keywords mode)] @font-lock-keyword-face)

   :language 'tlaplus
   :feature 'operator
   `([,@c-ts-mode--operators] @font-lock-operator-face
     "!" @font-lock-negation-char-face)

   :language 'tlaplus
   :feature 'string
   `((string_literal) @font-lock-string-face
     (system_lib_string) @font-lock-string-face)

   :language 'tlaplus
   :feature 'literal
   `((number_literal) @font-lock-number-face
     (char_literal) @font-lock-constant-face)

   :language 'tlaplus
   :feature 'type
   `((primitive_type) @font-lock-type-face
     (type_identifier) @font-lock-type-face
     (sized_type_specifier) @font-lock-type-face
     [,@c-ts-mode--type-keywords] @font-lock-type-face)

   :language 'tlaplus
   :feature 'definition
   ;; Highlights identifiers in declarations.
   `((declaration
      declarator: (_) @c-ts-mode--fontify-declarator)

     (field_declaration
      declarator: (_) @c-ts-mode--fontify-declarator)

     (function_definition
      declarator: (_) @c-ts-mode--fontify-declarator)

     (parameter_declaration
      declarator: (_) @c-ts-mode--fontify-declarator)

     (enumerator
      name: (identifier) @font-lock-property-name-face))

   :language 'tlaplus
   :feature 'assignment
   ;; TODO: Recursively highlight identifiers in parenthesized
   ;; expressions, see `c-ts-mode--fontify-declarator' for
   ;; inspiration.
   '((assignment_expression
      left: (identifier) @font-lock-variable-name-face)
     (assignment_expression
      left: (field_expression field: (_) @font-lock-property-use-face))
     (assignment_expression
      left: (pointer_expression
             (identifier) @font-lock-variable-name-face))
     (assignment_expression
      left: (subscript_expression
             (identifier) @font-lock-variable-name-face))
     (init_declarator declarator: (_) @c-ts-mode--fontify-declarator))

   :language 'tlaplus
   :feature 'function
   '((call_expression
      function:
      [(identifier) @font-lock-function-call-face
       (field_expression field: (field_identifier) @font-lock-function-call-face)]))

   :language 'tlaplus
   :feature 'variable
   '((identifier) @c-ts-mode--fontify-variable)

   :language 'tlaplus
   :feature 'error
   '((ERROR) @c-ts-mode--fontify-error)

   :feature 'escape-sequence
   :language 'tlaplus
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language 'tlaplus
   :feature 'property
   '((field_identifier) @font-lock-property-use-face)

   :language 'tlaplus
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'tlaplus
   :feature 'delimiter
   '((["," ":" ";"]) @font-lock-delimiter-face)

   :language 'tlaplus
   :feature 'emacs-devel
   :override t
   `(((call_expression
       (call_expression function: (identifier) @fn)
       @c-ts-mode--fontify-DEFUN)
      (:match "\\`DEFUN\\'" @fn))

     ((function_definition type: (_) @for-each-tail)
      @c-ts-mode--fontify-for-each-tail
      (:match ,c-ts-mode--for-each-tail-regexp @for-each-tail)))))

;;;###autoload
(define-derived-mode tla+-mode prog-mode "TLA+"
  "Major mode for TLA+ specifications, powered by tree-sitter.

Key bindings:
\\{tla+-mode-map}

Configuration:
	   You must at least set the variable to the TLA2 Toolbox.  This
	   can be done by setting the variable in the Emacs configuration
	   file (i.e. ~/.emacs or ~/.emacs.d/init.el)
	      (setq tla+-tlatools-path </path/to/tla2tools.jar>)
	   or with:
	      \\[execute-extended-command] \"customize-group\" <RET> tla+

	   You may also set the following paths:
	      tla+-java-path
	      tla+-dvipdf-path
	      tla+-dvips-path
	      tla+-tla+-tlatex-arguments
	   TLC options can be set globally or in the TLC configuration
	   GUI dialogue:
	      tla+-tlc-deadlock
	      tla+-tlc-simulate
	      tla+-tlc-depth
	      tla+-tlc-coverage
	      tla+-tlc-workers
	   To get help on one of the variables:
	      \\[describe-variable] <variablename>
	      \\[execute-extended-command] describe-variable <variablename>"
  :group 'tla+
  :after-hook (tla+-mode--set-modeline)

  ;; Comments
  ;; Teach Emacs what TLA+'s comments use as delimiters
  (tla+-mode-comment-setup)

  ;; Electric
  (setq-local electric-indent-chars
              (append "{}()<>" electric-indent-chars))

  ;; Configuration that is only possible if tree-sitter is present and ready
  ;; for TLA+ code.
  (when (treesit-ready-p 'tlaplus)
    (treesit-parser-create 'tlaplus)

    ;; Font-locking (Syntax Highlighting)
    (setq-local treesit-font-lock-feature-list
                '((comment definition)
                  (keyword preprocessor string type)
                  (assignment constant escape-sequence label literal)
                  (bracket delimiter error function operator property variable)))
    (setq-local treesit-font-lock-settings (tla+-mode--font-lock-settings))

    ;; Finally, set up Emacs' treesit manager & tree-sitter for use.
    (treesit-major-mode-setup))

  ;; Indentation should use spaces not tabs
  (setq-local indent-tabs-mode 'nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tla" . tla+-mode))

;;;
;;; Top-bar menu for TLA+-mode
(easy-menu-define tla+-mode-menu (list tla+-mode-map)
  "Menu for `tla+-mode'."
  '("TLA+"
    ["Do something (PLACEHOLDER)" (lambda () ())
     :help "Do something (PLACEHOLDER)"]))

(provide 'tla+-mode)
;;; tla+-mode.el ends here

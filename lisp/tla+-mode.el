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

  ;; Comments
  (setq-local comment-start "//")
  (setq-local comment-start-skip "//+[\[ ]*"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tla" . tla+-mode))

(provide 'tla+-mode)
;;; tla+-mode.el ends here

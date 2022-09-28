;;; noq-mode.el --- Major Mode for editing Noq source code -*- lexical-binding: t -*-

;; Copyright (C) 2021 Alexey Kutepov <reximkut@gmail.com>

;; Author: Alexey Kutepov <reximkut@gmail.com>
;; URL: https://github.com/tsoding/noq

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;
;; Major Mode for editing Noq source code

(defconst noq-mode-syntax-table
  (with-syntax-table (copy-syntax-table)
    ;; Python-style comments
    (modify-syntax-entry ?# "<")
    (modify-syntax-entry ?\n ">")
    ;; C/C++ style comments
	(modify-syntax-entry ?/ ". 124b")
	(modify-syntax-entry ?* ". 23")
	(modify-syntax-entry ?\n "> b")
	;; (modify-syntax-entry ?/ ". 124b")
	;; (modify-syntax-entry ?* ". 23")
	;; (modify-syntax-entry ?\n "> b")
    ;; Chars are the same as strings
    (modify-syntax-entry ?' "\"")
    (syntax-table))
  "Syntax table for `noq-mode'.")

(eval-and-compile
  (defconst noq-apply-strategies
    '("all" "deep")))

(eval-and-compile
  (defconst noq-keywords
    '("undo" "quit" "delete" "load" "save")))

(defconst noq-highlights
  `((
    ;; Keywords
    ,(regexp-opt noq-keywords 'words) . 'font-lock-keyword-face)

    ;; `Apply` strategies
    (,(format "\\(%s\\)[\t ]*|" (mapconcat 'regexp-quote noq-apply-strategies "\\|"))
     1 'font-lock-type-face)
    ("\\([0-9]+\\)[\t ]*|" 1 'font-lock-type-face)

    ;; Variables
    ("\\(^\\|[^a-zA-Z0-9_]\\)\\([_A-Z][_a-zA-Z0-9]*\\)" 2 'font-lock-variable-name-face)

    ;; Functor names
    ("\\([^\n\| ]*\\)[\t ]*::" 1 'font-lock-function-name-face)
    ))

;;;###autoload
(define-derived-mode noq-mode prog-mode "noq"
  "Major Mode for editing Noq source code."
  :syntax-table noq-mode-syntax-table
  (setq font-lock-defaults '(noq-highlights))
  (setq-local comment-start "// "))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.noq\\'" . noq-mode))

(provide 'noq-mode)

;;; noq-mode.el ends here

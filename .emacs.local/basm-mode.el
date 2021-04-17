;;; basm-mode.el --- Major Mode for editing BASM Assembly Code -*- lexical-binding: t -*-

;; Copyright (C) 2021 Alexey Kutepov <reximkut@gmail.com>

;; Author: Alexey Kutepov <reximkut@gmail.com>
;; URL: http://github.com/tsoding/bm

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
;; Major Mode for editing BASM Assembly Code. The language for a
;; simple Virtual Machine.

(defconst basm-mode-syntax-table
  (with-syntax-table (copy-syntax-table)
    (modify-syntax-entry ?\; "<")
    (modify-syntax-entry ?\n ">")
    (modify-syntax-entry ?\" "\"")
    (modify-syntax-entry ?\' "\"")
    (syntax-table))
  "Syntax table for `basm-mode'.")

(eval-and-compile
  (defconst basm-instructions
    '("nop" "push" "drop" "dup"
      "plusi" "minusi" "multi" "divi" "modi"
      "multu" "divu" "modu"
      "plusf" "minusf" "multf" "divf"
      "jmp" "jmp_if" "halt" "swap" "not"
      "eqi" "gei" "gti" "lei" "lti" "nei"
      "equ" "geu" "gtu" "leu" "ltu" "neu"
      "eqf" "gef" "gtf" "lef" "ltf" "nef"
      "ret" "call" "native"
      "andb" "orb" "xor" "shr" "shl" "notb"
      "read8u" "read16u" "read32u" "read64u"
      "read8i" "read16i" "read32i" "read64i"
      "write8" "write16" "write32" "write64"
      "i2f" "u2f" "f2i" "f2u")))

(defconst basm-highlights
  `(("%[[:word:]_]+" . font-lock-preprocessor-face)
    ("[[:word:]_]+\\:" . font-lock-constant-face)
    (,(regexp-opt basm-instructions 'symbols) . font-lock-keyword-face)))

;;;###autoload
(define-derived-mode basm-mode fundamental-mode "basm"
  "Major Mode for editing BASM Assembly Code."
  (setq font-lock-defaults '(basm-highlights))
  (set-syntax-table basm-mode-syntax-table))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(b\\|h\\)asm\\'" . basm-mode))

(provide 'basm-mode)

;;; basm-mode.el ends here

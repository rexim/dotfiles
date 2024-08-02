(require 'subr-x)

(defvar c3-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; C/C++ style comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    ;; Chars are the same as strings
    (modify-syntax-entry ?' "\"" table)
    ;; Treat <> as punctuation (needed to highlight C++ keywords
    ;; properly in template syntax)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?% "." table)
    table))

(defun c3-types ()
  '("void" "bool"
    "ichar" "char"
    ;; Integer types
    "short" "ushort" "int" "uint" "long" "ulong" "int128" "uint128"
    "iptr" "uptr"
    "isz" "usz"
    ;; Floating point types
    "float16" "float" "double" "float128"
    ;; Other types
    "any" "anyfault" "typeid"        
    ;; C compatibility types
    "CChar" "CShort" "CUShort" "CInt" "CUInt" "CLong" "CULong" "CLongLong" "CULongLong" "CFloat" "CDouble" "CLongDouble"
    ;; CT types
    "$typefrom" "$tyypeof" "$vatype"       
    ))

(defun c3-keywords ()
  '("asm"         "assert"      "bitstruct"   
    "break"       "case"        "catch"
    "const"       "continue"    "def"
    "default"     "defer"       "distinct"
    "do"          "else"        "enum"        
    "extern"      "false"       "fault"
    "for"         "foreach"     "foreach_r"
    "fn"          "tlocal"      "if"
    "inline"      "import"      "macro"
    "module"      "nextcase"    "null"
    "return"      "static"      "struct"
    "switch"      "true"        "try"
    "union"       "var"         "while"
    "$alignof"    "$assert"     "$case"
    "$checks"     "$default"    "$defined"
    "$echo"       "$else"       "$endfor"
    "$endforeach" "$endif"      "$endswitch"
    "$for"        "$foreach"    "$if"
    "$include"    "$nameof"     "$offsetof"
    "$qnameof"    "$sizeof"     "$stringify"
    "$vacount"    "$vaconst"    "$varef"
    "$vaarg"      "$vaexpr"     "$vasplat" 
))

(defun c3-font-lock-keywords ()
  (list
   `("#.*include \\(\\(<\\|\"\\).*\\(>\\|\"\\)\\)" . (1 font-lock-string-face))
   `(,(regexp-opt (c3-keywords) 'symbols) . font-lock-keyword-face)
   `(,(regexp-opt (c3-types) 'symbols) . font-lock-type-face)))

(defun c3--space-prefix-len (line)
  (- (length line)
     (length (string-trim-left line))))

(defun c3--previous-non-empty-line ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (thing-at-point 'line t)))

(defun c3--desired-indentation ()
  (let ((cur-line (string-trim-right (thing-at-point 'line t)))
        (prev-line (string-trim-right (c3--previous-non-empty-line)))
        (indent-len 4))
    (cond
     ((and (string-suffix-p "{" prev-line)
           (string-prefix-p "}" (string-trim-left cur-line)))
      (c3--space-prefix-len prev-line))
     ((string-suffix-p "{" prev-line)
      (+ (c3--space-prefix-len prev-line) indent-len))
     ((string-prefix-p "}" (string-trim-left cur-line))
      (max (- (c3--space-prefix-len prev-line) indent-len) 0))
     (t (c3--space-prefix-len prev-line)))))

(defun c3-indent-line ()
  (interactive)
  (when (not (bobp))
    (let* ((current-indentation
            (c3--space-prefix-len (thing-at-point 'line t)))
           (desired-indentation
            (c3--desired-indentation))
           (n (max (- (current-column) current-indentation) 0)))
      (indent-line-to desired-indentation)
      (forward-char n))))

;;;###autoload
(define-derived-mode c3-mode prog-mode "Simple C3"
  "Simple major mode for C3."
  :syntax-table c3-mode-syntax-table
  (setq-local font-lock-defaults '(c3-font-lock-keywords))
  (setq-local indent-line-function 'c3-indent-line)
  (setq-local comment-start "// "))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.c3\\'" . c3-mode))
(add-to-list 'auto-mode-alist '("\\.c3i\\'" . c3-mode))

(provide 'c3-mode)

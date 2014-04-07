;;; fan-mode.el --- Fan mode derived mode

;; Author: Kevin McIntire
;; Created: 09 Dec 2008
;; Keywords: C languages oop fan 

;; Copyright (C) 2008 Kevin McIntire

;; This program is free software; you can redistribute it and/or modify it under the terms of the GNU
;; General Public License as published by the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
;; the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License along with this program; if not, write
;; to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

;;; Commentary:

;;  This mode was developed using Russel Winder's <russel@russel.org.uk> excellent Groovy mode.
;;  Unlike that mode, it is a half baked sorry excuse.  Elispers please help.

;;; Tasks:

;; TODO need to support  ** comments
;; TODO need to support facets
;; TODO need to support closures
;; TODO indentation on adding some methods is utterly horrific

(require 'cc-mode)

(eval-when-compile
  (let ((load-path
         (if (and (boundp 'byte-compile-dest-file)
                  (stringp byte-compile-dest-file))
             (cons (file-name-directory byte-compile-dest-file) load-path)
           load-path)))
    (load "cc-mode" nil t) ; C# mode has this
    (load "cc-fonts" nil t) ; C# mode has this
    (load "cc-langs" nil t) ; C# mode has this
    (load "cc-bytecomp" nil t) ; Awk mode has this
))

(eval-and-compile
  (c-add-language 'fan-mode 'java-mode))

(c-lang-defconst c-assignment-operators
  fan '("=" ":=" "*=" "/=" "%=" "+=" "-=" ">>=" "<<=" "&=" "^=" "|="))

;;  Fan allows `?.' as well as `.'
(c-lang-defconst c-identifier-ops
                 fan '((left-assoc "." "?." "->" "?->")))

(c-lang-defconst c-ref-list-kwds
  fan '("using" "as" "is" "isnot"))

;; Fan allows operators such as `*.', `?.', `.&' and `.@'.  Java mode puts `*' 
;;(c-lang-defconst c-after-id-concat-ops
;;  fan '( "*" "&" "@" ))

;;;;  Should really do something with `c-string-escaped-newlines' and `c-multiline-string-start-char' to
;;;;  handle the triple delimeter multiline strings.

;; Because of the above we have to redefine `c_operators' because no other language has `.&' and
;; `.@' operators.

(c-lang-defconst c-operators
  "List describing all operators, along with their precedence and
associativity.  The order in the list corresponds to the precedence of
the operators: The operators in each element is a group with the same
precedence, and the group has higher precedence than the groups in all
following elements.  The car of each element describes the type of of
the operator group, and the cdr is a list of the operator tokens in
it.  The operator group types are:

'prefix         Unary prefix operators.
'postfix        Unary postfix operators.
'postfix-if-paren
                Unary postfix operators if and only if the chars have
                parenthesis syntax.
'left-assoc     Binary left associative operators (i.e. a+b+c means (a+b)+c).
'right-assoc    Binary right associative operators (i.e. a=b=c means a=(b=c)).
'right-assoc-sequence
                Right associative operator that constitutes of a
                sequence of tokens that separate expressions.  All the
                tokens in the group are in this case taken as
                describing the sequence in one such operator, and the
                order between them is therefore significant.

Operators containing a character with paren syntax are taken to match
with a corresponding open/close paren somewhere else.  A postfix
operator with close paren syntax is taken to end a postfix expression
started somewhere earlier, rather than start a new one at point.  Vice
versa for prefix operators with open paren syntax.

Note that operators like \".\" and \"->\" which in language references
often are described as postfix operators are considered binary here,
since CC Mode treats every identifier as an expression."

  fan `(
           ;; Primary.
           ,@(c-lang-const c-identifier-ops)
             
             (postfix-if-paren "<" ">") ; Templates.
             
             (prefix "super")
             
             ;; Postfix.
             (left-assoc "." "*." "?." ".&" ".@")
             
             (postfix "++" "--" "[" "]" "(" ")" "<:" ":>")
             
             ;; Unary.
             (prefix "++" "--" "+" "-" "!" "~" "new" "(" ")")
             
             ;; Multiplicative.
             (left-assoc "*" "/" "%")
             
             ;; Additive.
             (left-assoc "+" "-")
             
             ;; Shift.
             (left-assoc "<<" ">>" ">>>")
             
             ;; Relational.
             (left-assoc "<" ">" "<=" ">=" "instanceof" "<=>")
             
             ;; Matching.
             (left-assoc "=~" "==~" )

             ;; Equality.
             (left-assoc "==" "!=" )
             
             ;; Bitwise and.
             (left-assoc "&")
             
             ;; Bitwise exclusive or.
             (left-assoc "^")
             
             ;; Bitwise or.
             (left-assoc "|")
             
             ;; Logical and.
             (left-assoc "&&")
             
             ;; Logical or.
             (left-assoc "||")
             
             ;; Conditional.
             (right-assoc-sequence "?" ":")
             
             ;; Assignment.
             (right-assoc ,@(c-lang-const c-assignment-operators))
             
             ;; Exception.
             ;(prefix "throw") ; Java mode didn't have this but c++ mode does.  Humm...
             
             ;; Sequence.
             (left-assoc ",")

             ;; Separator for parameter list and code in a closure.
             (left-assoc "->")
             ))

;; Fan allows newline to terminate a statement unlike Java and like Awk.  We draw on the Awk
;; Mode `Virtual semicolon material.  The idea is to say when an EOL is a `virtual semicolon,
;; i.e. a statement terminator.
(c-lang-defconst c-stmt-delim-chars
                 fan "^;{}\n\r?:")

(c-lang-defconst c-stmt-delim-chars-with-comma
                 fan "^;,{}\n\r?:")

;;  Is there a virtual semicolon at POS or point?
;;
;;  A virtual semicolon is considered to lie just after the last non-syntactic-whitespace
;; character on a line where the EOL is the statement terminator.  A real semicolon never
;; counts as a virtual one.
(defun fan-at-vsemi-p ( &optional pos )
  (save-excursion
    (let ((pos-or-point (if pos (goto-char pos) (point))))
      (if (eq pos-or-point (point-min))
          nil
        (and
         (not (char-equal (char-before) ?\;))
         (fan-ws-or-comment-to-eol-p pos-or-point)
         (fan-not-in-statement-p pos-or-point))))))

(c-lang-defconst c-at-vsemi-p-fn
                 fan 'fan-at-vsemi-p)

(defun fan-ws-or-comment-to-eol-p ( pos )
  (save-excursion
    (goto-char pos)
    (skip-chars-forward " \t")
    (char-equal (char-after) ?\n)))

(defun fan-not-in-statement-p ( pos )
  (save-excursion
    (goto-char pos)
    (if (equal (point) (point-min))
        nil
      (backward-char 1)
      (or
       (not (looking-at "[=+*/%<]"))
       (if (char-equal (char-after) ?>)
           (if (equal (point) (point-min))
               nil
             (char-equal (char-before) ?-)))))))

(defun fan-vsemi-status-unknown-p () nil)

(c-lang-defconst c-vsemi-status-unknown-p-fn
                 fan 'c-fan-vsemi-status-unknown-p)


;;  Java does not do this but perhaps it should?
(c-lang-defconst c-type-modifier-kwds
                 fan '("volatile" "transient"))

;;;;  Should we be tinkering with `c-block-stmt-1-key' or `c-block-stmt-2-key' to deal with closures
;;;;  following what appears to be function calls or even field names?
(c-lang-defconst c-block-stmt-2-kwds
  fan '("for" "if" "switch" "while" "catch" "foreach"))


;; Fan allows use of `<%' and `%>' in template expressions.
;(c-lang-defconst c-other-op-syntax-tokens
;  fan '( "<%" "%>" ))

(c-lang-defconst c-modifier-kwds
                 fan '( "abstract" "final" "const" "private" "protected" "internal" "public" 
                           "static" "synchronized" "virtual" "override" "once" ))

(c-lang-defconst c-constant-kwds
  fan '( "true" "false" "null" ))

(c-lang-defconst c-primary-expr-kwds
  fan '( "this" "super" ))

(c-lang-defconst c-type-prefix-kwds
  fan '("class" "enum" "mixin"))

;; Allow classes after the : in the class declartion to be fontified. 
(c-lang-defconst c-typeless-decl-kwds
  fan '(":"))

;; Sets up the enum to handle the list properly
(c-lang-defconst c-brace-list-decl-kwds
  fan '("enum"))

;;;;  Should we be changing `c-opt-inexpr-brace-list-key' to deal with closures after function calls and
;;;;  field expressions?

;; We need to include the "as" for the cast and "in" for for.
(c-lang-defconst c-other-kwds
                 fan '( "in" "as" ))

;; TODO What, exactly, is a "primitive" in fan??
;; This list is odd at best...
(c-lang-defconst c-primitive-type-kwds
  fan '("Obj" "Str" "Int" "Decimal" "Uri" "Bool" "Void" "Weekday" 
        "DateTime" "Duration" "Enum" "Field" "Float" "List" "Map" 
        "Method" "Month" "Range" "Slot" ))

;; comment starter definitions for various languages.  language specific
(defconst c-fan-comment-start-regexp "\\(\\(//+\\|/\\*+\\)\\|\\*\\*\\)\\s *")

;; TODO can't seem to get ** comments working...
(c-lang-defconst c-comment-start  
  fan "\\(\\(//+\\|/\\*+\\)\\|\\*\\*\\)\\s *")
(c-lang-defconst c-comment-start-skip
  fan "\\(\\(//+\\|/\\*+\\)\\|\\*\\*\\)\\s *")
(c-lang-defconst c-comment-start-regexp  
  fan "\\(\\(//+\\|/\\*+\\)\\|\\*\\*\\)\\s *")
(c-lang-defconst c-doc-comment-start-regexp
  fan "\\(\\(//+\\|/\\*+\\)\\|\\*\\*\\)\\s *")

(defconst fan-font-lock-keywords-1 (c-lang-const c-matchers-1 fan)
  "Minimal highlighting for Fan mode.
Fontifies nothing except the syntactic fontification of strings and
comments.")

(defconst fan-font-lock-keywords-2 (c-lang-const c-matchers-2 fan)
  "Fast normal highlighting for Fan mode.
In addition to `java-font-lock-keywords-1', this adds fontification of
keywords, simple types, declarations that are easy to recognize, the
user defined types on `java-font-lock-extra-types', and the doc
comment styles specified by `c-doc-comment-style'.")

(defconst fan-font-lock-keywords-3 (c-lang-const c-matchers-3 fan)
  "Accurate normal highlighting for Fan mode.
Like `java-font-lock-keywords-2' but detects declarations in a more
accurate way that works in most cases for arbitrary types without the
need for `java-font-lock-extra-types'.")

(defvar fan-font-lock-keywords fan-font-lock-keywords-3
  "Default expressions to highlight in Fan mode.")

(defun fan-font-lock-keywords-2 ()
  (c-compose-keywords-list fan-font-lock-keywords-2))
(defun fan-font-lock-keywords-3 ()
  (c-compose-keywords-list fan-font-lock-keywords-3))
(defun fan-font-lock-keywords ()
  (c-compose-keywords-list fan-font-lock-keywords))

(defvar fan-mode-syntax-table nil
  "Syntax table used in Fan mode buffers.")
(or fan-mode-syntax-table
    (setq fan-mode-syntax-table
          (funcall (c-lang-const c-make-mode-syntax-table fan))))

(defvar fan-mode-abbrev-table nil
  "Abbreviation table used in fan-mode buffers.")
(c-define-abbrev-table 'fan-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the syntactic context, and which
  ;; therefore should trigger reindentation when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)
    ("finally" "finally" c-electric-continued-statement 0)))

(defvar fan-mode-map ()
  "Keymap used in fan-mode buffers.")
(if fan-mode-map
    nil
  (setq fan-mode-map (c-make-inherited-keymap))
  )


;; Custom variables
(defcustom fan-mode-hook nil
  "*Hook called by `fan-mode'."
  :type 'hook
  :group 'c)

;;; The entry point into the mode
(defun fan-mode ()
  "Major mode for editing Fan code.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `fan-mode-hook'.

Key bindings:
\\{fan-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table fan-mode-syntax-table)
  (setq major-mode 'fan-mode
        mode-name "Fan"
        local-abbrev-table fan-mode-abbrev-table
        abbrev-mode t)
  (use-local-map fan-mode-map)
  (c-init-language-vars fan-mode)
  (c-common-init 'fan-mode)
  (setq comment-start "\\(\\(//+\\|/\\*+\\)\\|\\*\\*\\)\\s *"
        comment-end ""
        comment-start-skip "\\(\\(//+\\|/\\*+\\)\\|\\*\\*\\)\\s *"
        comment-start-regexp "\\(\\(//+\\|/\\*+\\)\\|\\*\\*\\)\\s *"
        c-comment-prefix-regexp "\\(\\(//+\\|/\\*+\\)\\|\\*\\*\\)\\s *"
        )
  ;;(easy-menu-add fan-menu)
  ;;(cc-imenu-init cc-imenu-fan-generic-expression)
  (c-run-mode-hooks 'c-mode-common-hook 'fan-mode-hook)
  (c-update-modeline))

(provide 'fan-mode)

;;; fan-mode.el ends here

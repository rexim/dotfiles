;;; teal-mode.el --- a major-mode for editing Teal scripts  -*- lexical-binding: t -*-

;; Author: 2020 Ruin0x11 <ipickering2@gmail.com>
;;         2011-2013 immerrr <immerrr+teal@gmail.com>
;;         2010-2011 Reuben Thomas <rrt@sc3d.org>
;;         2006 Juergen Hoetzel <juergen@hoetzel.info>
;;         2004 various (support for Teal 5 and byte compilation)
;;         2001 Christian Vogler <cvogler@gradient.cis.upenn.edu>
;;         1997 Bret Mogilefsky <mogul-teal@gelatinous.com> starting from
;;              tcl-mode by Gregor Schmid <schmid@fb3-s7.math.tu-berlin.de>
;;              with tons of assistance from
;;              Paul Du Bois <pld-teal@gelatinous.com> and
;;              Aaron Smith <aaron-teal@gelatinous.com>.
;;
;; URL:         https://github.com/Ruin0x11/teal-mode
;; Version:     20200808-dev
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;; Keywords: languages, processes, tools

;; This field is expanded to commit SHA and commit date during the
;; archive creation.
;; Revision: $Format:%h (%cD)$
;;

;;; Commentary:

;; teal-mode provides support for editing Teal files. It is derived from
;; teal-mode. The following is copied from teal-mode's commentary:

;; The following variables are available for customization (see more via
;; `M-x customize-group teal`):

;; - Var `teal-indent-level':
;;   indentation offset in spaces
;; - Var `teal-indent-string-contents':
;;   set to `t` if you like to have contents of multiline strings to be
;;   indented like comments
;; - Var `teal-indent-nested-block-content-align':
;;   set to `nil' to stop aligning the content of nested blocks with the
;;   open parenthesis
;; - Var `teal-indent-close-paren-align':
;;   set to `t' to align close parenthesis with the open parenthesis,
;;   rather than with the beginning of the line
;; - Var `teal-mode-hook':
;;   list of functions to execute when teal-mode is initialized
;; - Var `teal-documentation-url':
;;   base URL for documentation lookup
;; - Var `teal-documentation-function': function used to
;;   show documentation (`eww` is a viable alternative for Emacs 25)

;; These are variables/commands that operate on the Teal process:

;; - Var `teal-default-application':
;;   command to start the Teal process (REPL)
;; - Var `teal-default-command-switches':
;;   arguments to pass to the Teal process on startup (make sure `-i` is there
;;   if you expect working with Teal shell interactively)
;; - Cmd `teal-start-process': start new REPL process, usually happens automatically
;; - Cmd `teal-kill-process': kill current REPL process

;; These are variables/commands for interaction with the Teal process:

;; - Cmd `teal-show-process-buffer': switch to REPL buffer
;; - Cmd `teal-hide-process-buffer': hide window showing REPL buffer
;; - Var `teal-always-show': show REPL buffer after sending something
;; - Cmd `teal-send-buffer': send whole buffer
;; - Cmd `teal-send-current-line': send current line
;; - Cmd `teal-send-defun': send current top-level function
;; - Cmd `teal-send-region': send active region
;; - Cmd `teal-restart-with-whole-file': restart REPL and send whole buffer

;; See "M-x apropos-command ^teal-" for a list of commands.
;; See "M-x customize-group teal" for a list of customizable variables.


;;; Code:
(eval-when-compile
  (require 'cl-lib))

(require 'lua-mode)
(require 'comint)
(require 'newcomment)
(require 'rx)


;; rx-wrappers for Teal

(eval-when-compile
  ;; Silence compilation warning about `compilation-error-regexp-alist' defined
  ;; in compile.el.
  (require 'compile))

(eval-and-compile
  (if (fboundp #'rx-let)
      (progn
        ;; Emacs 27+ way of customizing rx
        (defvar teal--rx-bindings)

        (setq
         teal--rx-bindings
         '((symbol (&rest x) (seq symbol-start (or x) symbol-end))
           (ws (* (any " \t")))
           (ws+ (+ (any " \t")))

           (teal-name (symbol (seq (+ (any alpha "_")) (* (any alnum "_")))))
           (teal-typeargs (seq "<" (+ teal-name (opt ",")) ">"))
           (teal-typename (or teal-name (seq teal-name teal-typeargs)))
           (teal-type (or (seq "{" (group-n 1 teal-typename (opt ":" teal-typename)) "}")
                          (seq "(" (* teal-typename (opt ",")) ")")
                          teal-typename))
           (teal-funcname (seq teal-name (* ws "." ws teal-name)
                           (opt ws ":" ws teal-name)))
           (teal-funcheader
            ;; Outer (seq ...) is here to shy-group the definition
            (seq (or (seq (symbol "function") ws (group-n 1 teal-funcname))
                     (seq (group-n 1 teal-funcname) ws "=" ws
                          (symbol "function")))))
           (teal-number
            (seq (or (seq (+ digit) (opt ".") (* digit))
                         (seq (* digit) (opt ".") (+ digit)))
                     (opt (regexp "[eE][+-]?[0-9]+"))))
           (teal-assignment-op (seq "=" (or buffer-end (not (any "=")))))
           (teal-token (or "+" "-" "*" "/" "%" "^" "#" "==" "~=" "<=" ">=" "<"
                       ">" "=" ";" ":" "," "." ".." "..."))
           (teal-keyword
            (symbol "and" "break" "do" "else" "elseif" "end"  "for" "function"
                    "goto" "if" "in" "local" "not" "or" "repeat" "return"
                    "then" "until" "while"
                    "record" "enum" "functiontype" "as" "is" "global"))))

        (defmacro teal-rx (&rest regexps)
          (eval `(rx-let ,teal--rx-bindings
                   (rx ,@regexps))))

        (defun teal-rx-to-string (form &optional no-group)
          (rx-let-eval teal--rx-bindings
            (rx-to-string form no-group))))
    (progn
      ;; Pre-Emacs 27 way of customizing rx
      (defvar teal-rx-constituents)
      (defvar rx-parent)

      (defun teal-rx-to-string (form &optional no-group)
        "Teal-specific replacement for `rx-to-string'.

See `rx-to-string' documentation for more information FORM and
NO-GROUP arguments."
        (let ((rx-constituents teal-rx-constituents))
          (rx-to-string form no-group)))

      (defmacro teal-rx (&rest regexps)
        "Teal-specific replacement for `rx'.

See `rx' documentation for more information about REGEXPS param."
        (cond ((null regexps)
               (error "No regexp"))
              ((cdr regexps)
               (teal-rx-to-string `(and ,@regexps) t))
              (t
               (teal-rx-to-string (car regexps) t))))

      (defun teal--new-rx-form (form)
        "Add FORM definition to `teal-rx' macro.

FORM is a cons (NAME . DEFN), see more in `rx-constituents' doc.
This function enables specifying new definitions using old ones:
if DEFN is a list that starts with `:rx' symbol its second
element is itself expanded with `teal-rx-to-string'. "
        (let ((form-definition (cdr form)))
          (when (and (listp form-definition) (eq ':rx (car form-definition)))
            (setcdr form (teal-rx-to-string (cadr form-definition) 'nogroup)))
          (push form teal-rx-constituents)))

      (defun teal--rx-symbol (form)
        ;; form is a list (symbol XXX ...)
        ;; Skip initial 'symbol
        (setq form (cdr form))
        ;; If there's only one element, take it from the list, otherwise wrap the
        ;; whole list into `(or XXX ...)' form.
        (setq form (if (eq 1 (length form))
                       (car form)
                     (append '(or) form)))
	(and (fboundp 'rx-form) ; Silence Emacs 27's byte-compiler.
             (rx-form `(seq symbol-start ,form symbol-end) rx-parent)))

      (setq teal-rx-constituents (copy-sequence rx-constituents))

      (mapc 'teal--new-rx-form
            `((symbol teal--rx-symbol 1 nil)
              (ws . "[ \t]*") (ws+ . "[ \t]+")
              (teal-name :rx (symbol (regexp "[[:alpha:]_]+[[:alnum:]_]*")))
              (teal-typeargs :rx (seq "<" (+ teal-name (opt ",")) ">"))
              (teal-typename :rx (or teal-name (seq teal-name teal-typeargs)))
              (teal-type :rx (or (seq "{" (group-n 1 teal-typename (opt ":" teal-typename)) "}")
                                 (seq "(" (* teal-typename (opt ",")) ")")
                                 teal-typename))
              (teal-funcname
               :rx (seq teal-name (* ws "." ws teal-name)))
              (teal-funcheader
               ;; Outer (seq ...) is here to shy-group the definition
               :rx (seq (or (seq (symbol "function") ws (group-n 1 teal-funcname))
                            (seq (group-n 1 teal-funcname) ws "=" ws
                                 (symbol "function")))))
              (teal-funcargs :rx (seq "(" (* (seq teal-name (opt ":" ws teal-type) (opt ","))) ")"))
              (teal-number
               :rx (seq (or (seq (+ digit) (opt ".") (* digit))
                            (seq (* digit) (opt ".") (+ digit)))
                        (opt (regexp "[eE][+-]?[0-9]+"))))
              (teal-assignment-op
               :rx (seq "=" (or buffer-end (not (any "=")))))
              (teal-token
               :rx (or "+" "-" "*" "/" "%" "^" "#" "==" "~=" "<=" ">=" "<"
                       ">" "=" ";" ":" "," "." ".." "..."))
              (teal-keyword
               :rx (symbol "and" "break" "do" "else" "elseif" "end"  "for" "function"
                           "goto" "if" "in" "local" "not" "or" "repeat" "return"
                           "then" "until" "while"
                           "record" "enum" "functiontype" "as" "is" "global"))))
      )))


;; Local variables
(defgroup teal nil
  "Major mode for editing Teal code."
  :prefix "teal-"
  :group 'languages)

(defcustom teal-indent-level 3
  "Amount by which Teal subexpressions are indented."
  :type 'integer
  :group 'teal
  :safe #'integerp)

(defcustom teal-indent-start-of-block t
  "If non-nil, indent blocks at the start of the line containing
the block, instead of the position of the block start keyword in
the middle of the line."
  :type 'boolean
  :group 'teal)

(defcustom teal-comment-start "-- "
  "Default value of `comment-start'."
  :type 'string
  :group 'teal)

(defcustom teal-comment-start-skip "---*[ \t]*"
  "Default value of `comment-start-skip'."
  :type 'string
  :group 'teal)

(defcustom teal-default-application "tl"
  "Default application to run in Teal process."
  :type '(choice (string)
                 (cons string integer))
  :group 'teal)

(defcustom teal-default-command-switches (list "-i")
  "Command switches for `teal-default-application'.
Should be a list of strings."
  :type '(repeat string)
  :group 'teal)
(make-variable-buffer-local 'teal-default-command-switches)

(defcustom teal-always-show t
  "*Non-nil means display teal-process-buffer after sending a command."
  :type 'boolean
  :group 'teal)

(defcustom teal-documentation-function 'browse-url
  "Function used to fetch the Teal reference manual."
  :type `(radio (function-item browse-url)
                ,@(when (fboundp 'eww) '((function-item eww)))
                ,@(when (fboundp 'w3m-browse-url) '((function-item w3m-browse-url)))
                (function :tag "Other function"))
  :group 'teal)

(defcustom teal-documentation-url
  (or (and (file-readable-p "/usr/share/doc/teal/manual.html")
           "file:///usr/share/doc/teal/manual.html")
      "http://www.teal.org/manual/5.1/manual.html")
  "URL pointing to the Teal reference manual."
  :type 'string
  :group 'teal)


(defvar teal-process nil
  "The active Teal process")

(defvar teal-process-buffer nil
  "Buffer used for communication with the Teal process")

(defun teal--customize-set-prefix-key (prefix-key-sym prefix-key-val)
  (cl-assert (eq prefix-key-sym 'teal-prefix-key))
  (set prefix-key-sym (if (and prefix-key-val (> (length prefix-key-val) 0))
                          ;; read-kbd-macro returns a string or a vector
                          ;; in both cases (elt x 0) is ok
                          (elt (read-kbd-macro prefix-key-val) 0)))
  (if (fboundp 'teal-prefix-key-update-bindings)
      (teal-prefix-key-update-bindings)))

(defcustom teal-prefix-key "\C-c"
  "Prefix for all teal-mode commands."
  :type 'string
  :group 'teal
  :set 'teal--customize-set-prefix-key
  :get '(lambda (sym)
          (let ((val (eval sym))) (if val (single-key-description (eval sym)) ""))))

(defvar teal-mode-menu (make-sparse-keymap "Teal")
  "Keymap for teal-mode's menu.")

(defvar teal-prefix-mode-map
  (eval-when-compile
    (let ((result-map (make-sparse-keymap)))
      (mapc (lambda (key_defn)
              (define-key result-map (read-kbd-macro (car key_defn)) (cdr key_defn)))
            '(("C-l" . teal-send-buffer)
              ("C-f" . teal-search-documentation)))
      result-map))
  "Keymap that is used to define keys accessible by `teal-prefix-key'.

If the latter is nil, the keymap translates into `teal-mode-map' verbatim.")

(defvar teal--electric-indent-chars
  (mapcar #'string-to-char '("}" "]" ")")))


(defvar teal-mode-map
  (let ((result-map (make-sparse-keymap)))
    (unless (boundp 'electric-indent-chars)
      (mapc (lambda (electric-char)
              (define-key result-map
                (read-kbd-macro
                 (char-to-string electric-char))
                #'teal-electric-match))
            teal--electric-indent-chars))
    (define-key result-map [menu-bar teal-mode] (cons "Teal" teal-mode-menu))

    ;; FIXME: see if the declared logic actually works
    ;; handle prefix-keyed bindings:
    ;; * if no prefix, set prefix-map as parent, i.e.
    ;;      if key is not defined look it up in prefix-map
    ;; * if prefix is set, bind the prefix-map to that key
    (if (boundp 'teal-prefix-key)
        (define-key result-map (vector teal-prefix-key) teal-prefix-mode-map)
      (set-keymap-parent result-map teal-prefix-mode-map))
    result-map)
  "Keymap used in teal-mode buffers.")

(defvar teal-electric-flag t
  "If t, electric actions (like automatic reindentation) will happen when an electric
 key like `{' is pressed")
(make-variable-buffer-local 'teal-electric-flag)

(defcustom teal-prompt-regexp "[^\n]*\\(>[\t ]+\\)+$"
  "Regexp which matches the Teal program's prompt."
  :type  'regexp
  :group 'teal)

(defcustom teal-traceback-line-re
  ;; This regexp skips prompt and meaningless "stdin:N:" prefix when looking
  ;; for actual file-line locations.
  "^\\(?:[\t ]*\\|.*>[\t ]+\\)\\(?:[^\n\t ]+:[0-9]+:[\t ]*\\)*\\(?:\\([^\n\t ]+\\):\\([0-9]+\\):\\)"
  "Regular expression that describes tracebacks and errors."
  :type 'regexp
  :group 'teal)

(defvar teal--repl-buffer-p nil
  "Buffer-local flag saying if this is a Teal REPL buffer.")
(make-variable-buffer-local 'teal--repl-buffer-p)


(defadvice compilation-find-file (around teal--repl-find-file
                                         (marker filename directory &rest formats)
                                         activate)
  "Return Teal REPL buffer when looking for \"stdin\" file in it."
  (if (and
       teal--repl-buffer-p
       (string-equal filename "stdin")
       ;; NOTE: this doesn't traverse `compilation-search-path' when
       ;; looking for filename.
       (not (file-exists-p (expand-file-name
                        filename
                        (when directory (expand-file-name directory))))))
      (setq ad-return-value (current-buffer))
    ad-do-it))


(defadvice compilation-goto-locus (around teal--repl-goto-locus
                                          (msg mk end-mk)
                                          activate)
  "When message points to Teal REPL buffer, go to the message itself.
Usually, stdin:XX line number points to nowhere."
  (let ((errmsg-buf (marker-buffer msg))
        (error-buf (marker-buffer mk)))
    (if (and (with-current-buffer errmsg-buf teal--repl-buffer-p)
             (eq error-buf errmsg-buf))
        (progn
          (compilation-set-window (display-buffer (marker-buffer msg)) msg)
          (goto-char msg))
      ad-do-it)))


(defcustom teal-indent-string-contents nil
  "If non-nil, contents of multiline string will be indented.
Otherwise leading amount of whitespace on each line is preserved."
  :group 'teal
  :type 'boolean)

(defcustom teal-indent-nested-block-content-align t
  "If non-nil, the contents of nested blocks are indented to
align with the column of the opening parenthesis, rather than
just forward by `teal-indent-level'."
  :group 'teal
  :type 'boolean)

(defcustom teal-indent-close-paren-align t
  "If non-nil, close parenthesis are aligned with their open
parenthesis.  If nil, close parenthesis are aligned to the
beginning of the line."
  :group 'teal
  :type 'boolean)

(defcustom teal-jump-on-traceback t
  "*Jump to innermost traceback location in *teal* buffer.  When this
variable is non-nil and a traceback occurs when running Teal code in a
process, jump immediately to the source code of the innermost
traceback location."
  :type 'boolean
  :group 'teal)

(defcustom teal-mode-hook nil
  "Hooks called when Teal mode fires up."
  :type 'hook
  :group 'teal)

(defvar teal-region-start (make-marker)
  "Start of special region for Teal communication.")

(defvar teal-region-end (make-marker)
  "End of special region for Teal communication.")

(defvar teal-emacs-menu
  '(["Restart With Whole File" teal-restart-with-whole-file t]
    ["Kill Process" teal-kill-process t]
    ["Hide Process Buffer" teal-hide-process-buffer t]
    ["Show Process Buffer" teal-show-process-buffer t]
    ["Beginning Of Proc" teal-beginning-of-proc t]
    ["End Of Proc" teal-end-of-proc t]
    ["Set Teal-Region Start" teal-set-teal-region-start t]
    ["Set Teal-Region End" teal-set-teal-region-end t]
    ["Send Teal-Region" teal-send-teal-region t]
    ["Send Current Line" teal-send-current-line t]
    ["Send Region" teal-send-region t]
    ["Send Proc" teal-send-proc t]
    ["Send Buffer" teal-send-buffer t]
    ["Search Documentation" teal-search-documentation t])
  "Emacs menu for Teal mode.")

;; the whole defconst is inside eval-when-compile, because it's later referenced
;; inside another eval-and-compile block
(eval-and-compile
  (defconst
    teal--builtins
    (let*
        ((modules
          '("_G" "_VERSION" "assert" "collectgarbage" "dofile" "error" "getfenv"
            "getmetatable" "ipairs" "load" "loadfile" "loadstring" "module"
            "next" "pairs" "pcall" "print" "rawequal" "rawget" "rawlen" "rawset"
            "require" "select" "setfenv" "setmetatable" "tonumber" "tostring"
            "type" "unpack" "xpcall" "self"
            ("bit32" . ("arshift" "band" "bnot" "bor" "btest" "bxor" "extract"
                        "lrotate" "lshift" "replace" "rrotate" "rshift"))
            ("coroutine" . ("create" "isyieldable" "resume" "running" "status"
                            "wrap" "yield"))
            ("debug" . ("debug" "getfenv" "gethook" "getinfo" "getlocal"
                        "getmetatable" "getregistry" "getupvalue" "getuservalue"
                        "setfenv" "sethook" "setlocal" "setmetatable"
                        "setupvalue" "setuservalue" "traceback" "upvalueid"
                        "upvaluejoin"))
            ("io" . ("close" "flush" "input" "lines" "open" "output" "popen"
                     "read" "stderr" "stdin" "stdout" "tmpfile" "type" "write"))
            ("math" . ("abs" "acos" "asin" "atan" "atan2" "ceil" "cos" "cosh"
                       "deg" "exp" "floor" "fmod" "frexp" "huge" "ldexp" "log"
                       "log10" "max" "maxinteger" "min" "mininteger" "modf" "pi"
                       "pow" "rad" "random" "randomseed" "sin" "sinh" "sqrt"
                       "tan" "tanh" "tointeger" "type" "ult"))
            ("os" . ("clock" "date" "difftime" "execute" "exit" "getenv"
                     "remove"  "rename" "setlocale" "time" "tmpname"))
            ("package" . ("config" "cpath" "loaded" "loaders" "loadlib" "path"
                          "preload" "searchers" "searchpath" "seeall"))
            ("string" . ("byte" "char" "dump" "find" "format" "gmatch" "gsub"
                         "len" "lower" "match" "pack" "packsize" "rep" "reverse"
                         "sub" "unpack" "upper"))
            ("table" . ("concat" "insert" "maxn" "move" "pack" "remove" "sort"
                        "unpack"))
            ("utf8" . ("char" "charpattern" "codepoint" "codes" "len"
                       "offset")))))

      (cl-labels
       ((module-name-re (x)
                        (concat "\\(?1:\\_<"
                                (if (listp x) (car x) x)
                                "\\_>\\)"))
        (module-members-re (x) (if (listp x)
                                   (concat "\\(?:[ \t]*\\.[ \t]*"
                                           "\\_<\\(?2:"
                                           (regexp-opt (cdr x))
                                           "\\)\\_>\\)?")
                                 "")))

       (concat
        ;; common prefix:
        ;; - beginning-of-line
        ;; - or neither of [ '.', ':' ] to exclude "foo.string.rep"
        ;; - or concatenation operator ".."
        "\\(?:^\\|[^:. \t]\\|[.][.]\\)"
        ;; optional whitespace
        "[ \t]*"
        "\\(?:"
        ;; any of modules/functions
        (mapconcat (lambda (x) (concat (module-name-re x)
                                       (module-members-re x)))
                   modules
                   "\\|")
        "\\)"))))

  "A regexp that matches Teal builtin functions & variables.

This is a compilation of 5.1, 5.2 and 5.3 builtins taken from the
index of respective Teal reference manuals.")

(eval-and-compile
  (defun teal-make-delimited-matcher (elt-regexp sep-regexp end-regexp)
    "Construct matcher function for `font-lock-keywords' to match a sequence.

It's supposed to match sequences with following EBNF:

ELT-REGEXP { SEP-REGEXP ELT-REGEXP } END-REGEXP

The sequence is parsed one token at a time.  If non-nil is
returned, `match-data' will have one or more of the following
groups set according to next matched token:

1. matched element token
2. unmatched garbage characters
3. misplaced token (i.e. SEP-REGEXP when ELT-REGEXP is expected)
4. matched separator token
5. matched end token

Blanks & comments between tokens are silently skipped.
Groups 6-9 can be used in any of argument regexps."
    (let*
        ((delimited-matcher-re-template
          "\\=\\(?2:.*?\\)\\(?:\\(?%s:\\(?4:%s\\)\\|\\(?5:%s\\)\\)\\|\\(?%s:\\(?1:%s\\)\\)\\)")
         ;; There's some magic to this regexp. It works as follows:
         ;;
         ;; A. start at (point)
         ;; B. non-greedy match of garbage-characters (?2:)
         ;; C. try matching separator (?4:) or end-token (?5:)
         ;; D. try matching element (?1:)
         ;;
         ;; Simple, but there's a trick: pt.C and pt.D are embraced by one more
         ;; group whose purpose is determined only after the template is
         ;; formatted (?%s:):
         ;;
         ;; - if element is expected, then D's parent group becomes "shy" and C's
         ;;   parent becomes group 3 (aka misplaced token), so if D matches when
         ;;   an element is expected, it'll be marked with warning face.
         ;;
         ;; - if separator-or-end-token is expected, then it's the opposite:
         ;;   C's parent becomes shy and D's will be matched as misplaced token.
         (elt-expected-re (format delimited-matcher-re-template
                                  3 sep-regexp end-regexp "" elt-regexp))
         (sep-or-end-expected-re (format delimited-matcher-re-template
                                         "" sep-regexp end-regexp 3 elt-regexp)))

      (lambda (end)
        (let* ((prev-elt-p (match-beginning 1))
               (prev-end-p (match-beginning 5))

               (regexp (if prev-elt-p sep-or-end-expected-re elt-expected-re))
               (comment-start (teal-comment-start-pos (syntax-ppss)))
               (parse-stop end))

          ;; If token starts inside comment, or end-token was encountered, stop.
          (when (and (not comment-start)
                     (not prev-end-p))
            ;; Skip all comments & whitespace. forward-comment doesn't have boundary
            ;; argument, so make sure point isn't beyond parse-stop afterwards.
            (while (and (< (point) end)
                        (forward-comment 1)))
            (goto-char (min (point) parse-stop))

            ;; Reuse comment-start variable to store beginning of comment that is
            ;; placed before line-end-position so as to make sure token search doesn't
            ;; enter that comment.
            (setq comment-start
                  (teal-comment-start-pos
                   (save-excursion
                     (parse-partial-sexp (point) parse-stop
                                         nil nil nil 'stop-inside-comment)))
                  parse-stop (or comment-start parse-stop))

            ;; Now, let's match stuff.  If regular matcher fails, declare a span of
            ;; non-blanks 'garbage', and the next iteration will start from where the
            ;; garbage ends.  If couldn't match any garbage, move point to the end
            ;; and return nil.
            (or (re-search-forward regexp parse-stop t)
                (re-search-forward "\\(?1:\\(?2:[^ \t]+\\)\\)" parse-stop 'skip)
                (prog1 nil (goto-char end)))))))))

(defun teal-end-of-function-args-position (&rest args)
  (save-excursion
    (condition-case nil
        (progn
          ;; assume looking at "("
          (forward-sexp)
          (backward-char 1)
          (point))
      (error (point)))))

(defun teal-match-function-args (end)
  (if (looking-at-p ")")
      nil
    (or (re-search-forward (teal-rx (group-n 1 teal-name) ":" ws (group-n 2 teal-type)) end t)
        (re-search-forward (teal-rx (group-n 2 teal-type)) end t)
        (prog1 nil (goto-char end)))))

(defun teal-match-type-annotation (end)
  (if (looking-at-p ",")
      (progn
        (re-search-forward (teal-rx teal-type) end t)
        (teal-match-type-annotation end))
    (progn
      (backward-char 1)
      (if (looking-at-p ":")
          ;; TODO handle array/tuple types (" [{(]")
          (re-search-forward (teal-rx teal-type) end t)
        (forward-char 1)))))

(defvar teal-font-lock-keywords
  `(;; highlight the hash-bang line "#!/foo/bar/teal" as comment
    ("^#!.*$" . font-lock-comment-face)

    ;; Builtin constants
    (,(teal-rx (symbol "true" "false" "nil"))
     . font-lock-constant-face)

    ;; Keywords
    (,(teal-rx teal-keyword)
     . font-lock-keyword-face)

    ;; Labels used by the "goto" statement
    ;; Highlights the following syntax:  ::label::
    (,(teal-rx "::" ws teal-name ws "::")
      . font-lock-constant-face)

    ;; Highlights the name of the label in the "goto" statement like
    ;; "goto label"
    (,(teal-rx (symbol (seq "goto" ws+ (group-n 1 teal-name))))
      (1 font-lock-constant-face))

    (,(teal-rx ws (group-n 1 (symbol "as" "is")) ws (group-n 2 teal-type))
     (1 font-lock-keyword-face nil noerror)
     (2 font-lock-type-face nil noerror))

    ("^[ \t]*\\_<for\\_>"
     (,(teal-make-delimited-matcher (teal-rx teal-name) ","
                                   (teal-rx (or (symbol "in") teal-assignment-op)))
      nil nil
      (1 font-lock-variable-name-face nil noerror)
      (2 compilation-error-face t noerror)
      (3 compilation-error-face t noerror)))

    (,(teal-rx (or bol ";") ws "{" (group-n 1 teal-name) "}" eol)
     (1 font-lock-type-face nil noerror))

    ;; Handle local variable/function names
    ;;  local blalba, xyzzy =
    ;;        ^^^^^^  ^^^^^
    ;;
    ;;  local function foobar(x,y,z)
    ;;                 ^^^^^^
    ;;  local foobar = function(x,y,z)
    ;;        ^^^^^^
    (,(teal-rx (or bol ";") ws (symbol "local" "global"))
     (0 font-lock-keyword-face)

     (,(teal-rx (group-n 1 teal-name) ws teal-assignment-op ws (symbol "enum" "record" "functiontype"))
      nil nil
      (1 font-lock-type-face nil noerror))

     ;; (* nonl) at the end is to consume trailing characters or otherwise they
     ;; delimited matcher would attempt to parse them afterwards and wrongly
     ;; highlight parentheses as incorrect variable name characters.
     (,(teal-rx point ws teal-funcheader (* nonl))
      nil nil
      (1 font-lock-function-name-face nil noerror))

     (,(teal-make-delimited-matcher (teal-rx teal-name) ","
                                    (teal-rx (or teal-assignment-op ":")))
      nil nil
      (1 font-lock-variable-name-face nil noerror))

     (teal-match-type-annotation
      nil nil
      (0 font-lock-type-face nil noerror)))

    (,(teal-rx (or bol ";") ws teal-funcheader)
     (1 font-lock-function-name-face))

    (,(teal-rx teal-funcheader (opt (group-n 1 teal-typeargs)))
     (1 font-lock-type-face nil noerror)

     (teal-match-function-args
      (teal-end-of-function-args-position)
      nil
      (1 font-lock-variable-name-face nil noerror)
      (2 font-lock-type-face nil noerror))

     ;; TODO separated matcher
     (,(teal-rx ":" ws (group-n 1 (* nonl)))
      nil nil
      (1 font-lock-type-face nil noerror)))

    (,(teal-rx (or bol ";") ws (group-n 1 teal-name) ":" ws)
     (1 font-lock-variable-name-face nil noerror)

     (,(teal-rx teal-type)
      nil nil
      (0 font-lock-type-face nil noerror)))

    ;; Highlight Teal builtin functions and variables
    (,teal--builtins
     (1 font-lock-builtin-face) (2 font-lock-builtin-face nil noerror))

    (,(teal-rx (or (group-n 1
                           "@" (symbol "author" "copyright" "field" "release"
                                       "return" "see" "usage" "description"))
                  (seq (group-n 1 "@" (symbol "param" "class" "name")) ws+
                       (group-n 2 teal-name))))
     (1 font-lock-keyword-face t)
     (2 font-lock-variable-name-face t noerror)))

  "Default expressions to highlight in Teal mode.")

(defvar teal-imenu-generic-expression
  `(("Requires" ,(teal-rx (or bol ";") ws (opt (seq (symbol "local" "global") ws)) (group-n 1 teal-name) ws "=" ws (symbol "require")) 1)
    ("Records" ,(teal-rx (or bol ";") ws (opt (seq (symbol "local" "global") ws)) (group-n 1 teal-name) ws "=" ws (symbol "record")) 1)
    ("Enums" ,(teal-rx (or bol ";") ws (opt (seq (symbol "local" "global") ws)) (group-n 1 teal-name) ws "=" ws (symbol "enum")) 1)
    (nil ,(teal-rx (or bol ";") ws (opt (seq (symbol "local" "global") ws)) teal-funcheader) 1))
  "Imenu generic expression for teal-mode.  See `imenu-generic-expression'.")

(defvar teal-sexp-alist '(("then" . "end")
                          ("function" . "end")
                          ("do" . "end")
                          ("repeat" . "until")
                          ("record" . "end")
                          ("enum" . "end")))

(defvar teal-mode-abbrev-table nil
  "Abbreviation table used in teal-mode buffers.")

(define-abbrev-table 'teal-mode-abbrev-table
  '(("end"    "end"    teal-indent-line :system t)
    ("else"   "else"   teal-indent-line :system t)
    ("elseif" "elseif" teal-indent-line :system t)))

(defvar teal-mode-syntax-table
  (with-syntax-table (copy-syntax-table)
    ;; main comment syntax: begins with "--", ends with "\n"
    (modify-syntax-entry ?- ". 12")
    (modify-syntax-entry ?\n ">")

    ;; main string syntax: bounded by ' or "
    (modify-syntax-entry ?\' "\"")
    (modify-syntax-entry ?\" "\"")

    ;; single-character binary operators: punctuation
    (modify-syntax-entry ?+ ".")
    (modify-syntax-entry ?* ".")
    (modify-syntax-entry ?/ ".")
    (modify-syntax-entry ?^ ".")
    (modify-syntax-entry ?% ".")
    (modify-syntax-entry ?> ".")
    (modify-syntax-entry ?< ".")
    (modify-syntax-entry ?= ".")
    (modify-syntax-entry ?~ ".")

    (syntax-table))
  "`teal-mode' syntax table.")

;;;###autoload
(define-derived-mode teal-mode prog-mode "Teal"
  "Major mode for editing Teal code."
  :abbrev-table teal-mode-abbrev-table
  :syntax-table teal-mode-syntax-table
  :group 'teal
  (setq-local font-lock-defaults '(teal-font-lock-keywords ;; keywords
                                        nil                    ;; keywords-only
                                        nil                    ;; case-fold
                                        nil                    ;; syntax-alist
                                        nil                    ;; syntax-begin
                                        ))
  (setq-local font-lock-multiline t)

  (setq-local syntax-propertize-function
              'teal--propertize-multiline-bounds)

  (setq-local parse-sexp-lookup-properties   t)
  (setq-local indent-line-function           'teal-indent-line)
  (setq-local beginning-of-defun-function    'teal-beginning-of-proc)
  (setq-local end-of-defun-function          'teal-end-of-proc)
  (setq-local comment-start                  teal-comment-start)
  (setq-local comment-start-skip             teal-comment-start-skip)
  (setq-local comment-use-syntax             t)
  (setq-local fill-paragraph-function        #'teal--fill-paragraph)
  (with-no-warnings
    (setq-local comment-use-global-state     t))
  (setq-local imenu-generic-expression       teal-imenu-generic-expression)
  (when (boundp 'electric-indent-chars)
    ;; If electric-indent-chars is not defined, electric indentation is done
    ;; via `teal-mode-map'.
    (setq-local electric-indent-chars
                  (append electric-indent-chars teal--electric-indent-chars)))


  ;; setup menu bar entry (XEmacs style)
  (if (and (featurep 'menubar)
           (boundp 'current-menubar)
           (fboundp 'set-buffer-menubar)
           (fboundp 'add-menu)
           (not (assoc "Teal" current-menubar)))
      (progn
        (set-buffer-menubar (copy-sequence current-menubar))
        (add-menu nil "Teal" teal-emacs-menu)))
  ;; Append Teal menu to popup menu for Emacs.
  (if (boundp 'mode-popup-menu)
      (setq mode-popup-menu
            (cons (concat mode-name " Mode Commands") teal-emacs-menu)))

  ;; hideshow setup
  (unless (assq 'teal-mode hs-special-modes-alist)
    (add-to-list 'hs-special-modes-alist
                 `(teal-mode
                   ,(regexp-opt (mapcar 'car teal-sexp-alist) 'words) ;start
                   ,(regexp-opt (mapcar 'cdr teal-sexp-alist) 'words) ;end
                   nil teal-forward-sexp))))



;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tl\\'" . teal-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("teal" . teal-mode))

(defun teal-electric-match (arg)
  "Insert character and adjust indentation."
  (interactive "P")
  (let (blink-paren-function)
   (self-insert-command (prefix-numeric-value arg)))
  (if teal-electric-flag
      (teal-indent-line))
  (blink-matching-open))

;; private functions

(defun teal--fill-paragraph (&optional justify region)
  ;; Implementation of forward-paragraph for filling.
  ;;
  ;; This function works around a corner case in the following situations:
  ;;
  ;;     <>
  ;;     -- some very long comment ....
  ;;     some_code_right_after_the_comment
  ;;
  ;; If point is at the beginning of the comment line, fill paragraph code
  ;; would have gone for comment-based filling and done the right thing, but it
  ;; does not find a comment at the beginning of the empty line before the
  ;; comment and falls back to text-based filling ignoring comment-start and
  ;; spilling the comment into the code.
  (save-excursion
    (while (and (not (eobp))
                (progn (move-to-left-margin)
                       (looking-at paragraph-separate)))
      (forward-line 1))
    (let ((fill-paragraph-handle-comment t))
      (fill-paragraph justify region))))


(defun teal-prefix-key-update-bindings ()
  (let (old-cons)
    (if (eq teal-prefix-mode-map (keymap-parent teal-mode-map))
        ;; if prefix-map is a parent, delete the parent
        (set-keymap-parent teal-mode-map nil)
      ;; otherwise, look for it among children
      (if (setq old-cons (rassoc teal-prefix-mode-map teal-mode-map))
          (delq old-cons teal-mode-map)))

    (if (null teal-prefix-key)
        (set-keymap-parent teal-mode-map teal-prefix-mode-map)
      (define-key teal-mode-map (vector teal-prefix-key) teal-prefix-mode-map))))

(defun teal-set-prefix-key (new-key-str)
  "Changes `teal-prefix-key' properly and updates keymaps

This function replaces previous prefix-key binding with a new one."
  (interactive "sNew prefix key (empty string means no key): ")
  (teal--customize-set-prefix-key 'teal-prefix-key new-key-str)
  (message "Prefix key set to %S"  (single-key-description teal-prefix-key))
  (teal-prefix-key-update-bindings))

(defun teal-string-p (&optional pos)
  "Returns true if the point is in a string."
  (save-excursion (elt (syntax-ppss pos) 3)))

(defun teal-comment-start-pos (parsing-state)
  "Return position of comment containing current point.

If point is not inside a comment, return nil."
  (and parsing-state (nth 4 parsing-state) (nth 8 parsing-state)))

(defun teal-comment-or-string-p (&optional pos)
  "Returns true if the point is in a comment or string."
  (save-excursion (let ((parse-result (syntax-ppss pos)))
                    (or (elt parse-result 3) (elt parse-result 4)))))

(defun teal-comment-or-string-start-pos (&optional pos)
  "Returns start position of string or comment which contains point.

If point is not inside string or comment, return nil."
  (save-excursion (elt (syntax-ppss pos) 8)))

;; They're propertized as follows:
;; 1. generic-comment
;; 2. generic-string
;; 3. equals signs
(defconst teal-ml-begin-regexp
  "\\(?:\\(?1:-\\)-\\[\\|\\(?2:\\[\\)\\)\\(?3:=*\\)\\[")


(defun teal-try-match-multiline-end (end)
  "Try to match close-bracket for multiline literal around point.

Basically, detect form of close bracket from syntactic
information provided at point and re-search-forward to it."
  (let ((comment-or-string-start-pos (teal-comment-or-string-start-pos)))
    ;; Is there a literal around point?
    (and comment-or-string-start-pos
         ;; It is, check if the literal is a multiline open-bracket
         (save-excursion
           (goto-char comment-or-string-start-pos)
           (looking-at teal-ml-begin-regexp))

         ;; Yes it is, look for it matching close-bracket.  Close-bracket's
         ;; match group is determined by match-group of open-bracket.
         (re-search-forward
          (format "]%s\\(?%s:]\\)"
                  (match-string-no-properties 3)
                  (if (match-beginning 1) 1 2))
          end 'noerror))))


(defun teal-try-match-multiline-begin (limit)
  "Try to match multiline open-brackets.

Find next opening long bracket outside of any string/comment.
If none can be found before reaching LIMIT, return nil."

  (let (last-search-matched)
    (while
        ;; This loop will iterate skipping all multiline-begin tokens that are
        ;; inside strings or comments ending either at EOL or at valid token.
        (and (setq last-search-matched
                   (re-search-forward teal-ml-begin-regexp limit 'noerror))

             ;; Handle triple-hyphen '---[[' situation in which the multiline
             ;; opener should be skipped.
             ;;
             ;; In HYPHEN1-HYPHEN2-BRACKET1-BRACKET2 situation (match-beginning
             ;; 0) points to HYPHEN1, but if there's another hyphen before
             ;; HYPHEN1, standard syntax table will only detect comment-start
             ;; at HYPHEN2.
             ;;
             ;; We could check for comment-start at HYPHEN2, but then we'd have
             ;; to flush syntax-ppss cache to remove the result saying that at
             ;; HYPHEN2 there's no comment or string, because under some
             ;; circumstances that would hide the fact that we put a
             ;; comment-start property at HYPHEN1.
             (or (teal-comment-or-string-start-pos (match-beginning 0))
                 (and (eq ?- (char-after (match-beginning 0)))
                      (eq ?- (char-before (match-beginning 0)))))))

    last-search-matched))

(defun teal-match-multiline-literal-bounds (limit)
  ;; First, close any multiline literal spanning from previous block. This will
  ;; move the point accordingly so as to avoid double traversal.
  (or (teal-try-match-multiline-end limit)
      (teal-try-match-multiline-begin limit)))

(defun teal--propertize-multiline-bounds (start end)
  "Put text properties on beginnings and ends of multiline literals.

Intended to be used as a `syntax-propertize-function'."
  (save-excursion
    (goto-char start)
    (while (teal-match-multiline-literal-bounds end)
      (when (match-beginning 1)
        (put-text-property (match-beginning 1) (match-end 1)
                           'syntax-table (string-to-syntax "!")))
      (when (match-beginning 2)
        (put-text-property (match-beginning 2) (match-end 2)
                           'syntax-table (string-to-syntax "|"))))))


(defun teal-indent-line ()
  "Indent current line for Teal mode.
Return the amount the indentation changed by."
  (let (indent
        (case-fold-search nil)
        ;; save point as a distance to eob - it's invariant w.r.t indentation
        (pos (- (point-max) (point))))
    (back-to-indentation)
    (if (teal-comment-or-string-p)
        (setq indent (teal-calculate-string-or-comment-indentation)) ;; just restore point position
      (setq indent (max 0 (teal-calculate-indentation))))

    (when (not (equal indent (current-column)))
      (delete-region (line-beginning-position) (point))
      (indent-to indent))

    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))

    indent))

(defun teal-calculate-string-or-comment-indentation ()
  "This function should be run when point at (current-indentation) is inside string"
  (if (and (teal-string-p) (not teal-indent-string-contents))
      ;; if inside string and strings aren't to be indented, return current indentation
      (current-indentation)

    ;; At this point, we know that we're inside comment, so make sure
    ;; close-bracket is unindented like a block that starts after
    ;; left-shifter.
    (let ((left-shifter-p (looking-at "\\s *\\(?:--\\)?\\]\\(?1:=*\\)\\]")))
      (save-excursion
        (goto-char (teal-comment-or-string-start-pos))
        (+ (current-indentation)
           (if (and left-shifter-p
                    (looking-at (format "--\\[%s\\["
                                        (match-string-no-properties 1))))
               0
             teal-indent-level))))))

(defun teal-find-regexp (direction regexp &optional limit ignore-p)
  "Searches for a regular expression in the direction specified.
Direction is one of 'forward and 'backward.
By default, matches in comments and strings are ignored, but what to ignore is
configurable by specifying ignore-p. If the regexp is found, returns point
position, nil otherwise.
ignore-p returns true if the match at the current point position should be
ignored, nil otherwise."
  (let ((ignore-func (or ignore-p 'teal-comment-or-string-p))
        (search-func (if (eq direction 'forward)
                         're-search-forward 're-search-backward))
        (case-fold-search nil))
    (catch 'found
      (while (funcall search-func regexp limit t)
        (if (and (not (funcall ignore-func (match-beginning 0)))
                 (not (funcall ignore-func (match-end 0))))
            (throw 'found (point)))))))

(defconst teal-block-regexp
  (eval-when-compile
    (concat
     "\\(\\_<"
     (regexp-opt '("do" "function" "repeat" "then"
                   "else" "elseif" "end" "until"
                   "record" "enum") t)
     "\\_>\\)\\|"
     (regexp-opt '("{" "(" "[" "]" ")" "}") t))))

(defconst teal-block-token-alist
  '(("do"       "\\_<end\\_>"                            "\\_<for\\|while\\_>"    middle-or-open)
    ("function" "\\_<end\\_>"                            nil                      open)
    ("repeat"   "\\_<until\\_>"                          nil                      open)
    ("then"     "\\_<\\(e\\(lse\\(if\\)?\\|nd\\)\\)\\_>" "\\_<\\(else\\)?if\\_>"  middle)
    ("{"        "}"                                      nil                      open)
    ("["        "]"                                      nil                      open)
    ("("        ")"                                      nil                      open)
    ("if"       "\\_<then\\_>"                           nil                      open)
    ("for"      "\\_<do\\_>"                             nil                      open)
    ("while"    "\\_<do\\_>"                             nil                      open)
    ("else"     "\\_<end\\_>"                            "\\_<then\\_>"           middle)
    ("elseif"   "\\_<then\\_>"                           "\\_<then\\_>"           middle)
    ("end"      nil                                      "\\_<\\(do\\|function\\|then\\|else\\|record\\|enum\\)\\_>"  close)
    ("local"    "\\_<end\\_>"                            nil                      open)
    ("global"   "\\_<end\\_>"                            nil                      open)
    ("until"    nil                                      "\\_<repeat\\_>"         close)
    ("record"   "\\_<end\\_>"                            nil                      open)
    ("enum"     "\\_<end\\_>"                            nil                      open)
    ("}"        nil                                      "{"                      close)
    ("]"        nil                                      "\\["                    close)
    (")"        nil                                      "("                      close))
  "This is a list of block token information blocks.
Each token information entry is of the form:
  KEYWORD FORWARD-MATCH-REGEXP BACKWARDS-MATCH-REGEXP TOKEN-TYPE
KEYWORD is the token.
FORWARD-MATCH-REGEXP is a regexp that matches all possible tokens when going forward.
BACKWARDS-MATCH-REGEXP is a regexp that matches all possible tokens when going backwards.
TOKEN-TYPE determines where the token occurs on a statement. open indicates that the token appears at start, close indicates that it appears at end, middle indicates that it is a middle type token, and middle-or-open indicates that it can appear both as a middle or an open type.")

(defconst teal-indentation-modifier-regexp
  ;; The absence of else is deliberate, since it does not modify the
  ;; indentation level per se. It only may cause the line, in which the
  ;; else is, to be shifted to the left.
  (concat
   "\\(\\_<"
   ;; TODO: In Teal, "function" might not start a block if it's part of a type
   ;; annotation, so this code will not get indented correctly:
   ;;
   ;; local func_a: function(): number
   ;; local func_b: function(): number
   ;;
   (regexp-opt '("do" "function" "repeat" "then" "if" "else" "elseif" "for" "while" "record" "enum") t)
   "\\_>\\|"
   (regexp-opt '("{" "(" "["))
   "\\)\\|\\(\\_<"
   (regexp-opt '("end" "until") t)
   "\\_>\\|"
   (regexp-opt '("]" ")" "}"))
   "\\)")
  )

(defun teal-get-block-token-info (token)
  "Returns the block token info entry for TOKEN from teal-block-token-alist"
  (assoc token teal-block-token-alist))

(defun teal-get-token-match-re (token-info direction)
  "Returns the relevant match regexp from token info"
  (cond
   ((eq direction 'forward) (cadr token-info))
   ((eq direction 'backward) (nth 2 token-info))
   (t nil)))

(defun teal-get-token-type (token-info)
  "Returns the relevant match regexp from token info"
   (nth 3 token-info))

(defun teal-backwards-to-block-begin-or-end ()
  "Move backwards to nearest block begin or end.  Returns nil if not successful."
  (interactive)
  (teal-find-regexp 'backward teal-block-regexp))

(defun teal-find-matching-token-word (token &optional direction)
  "Find matching open- or close-token for TOKEN in DIRECTION.
Point has to be exactly at the beginning of TOKEN, e.g. with | being point

  {{ }|}  -- (teal-find-matching-token-word \"}\" 'backward) will return
          -- the first {
  {{ |}}  -- (teal-find-matching-token-word \"}\" 'backward) will find
          -- the second {.

DIRECTION has to be either 'forward or 'backward."
  (let* ((token-info (teal-get-block-token-info token))
         (match-type (teal-get-token-type token-info))
         ;; If we are on a middle token, go backwards. If it is a middle or open,
         ;; go forwards
         (search-direction (or direction
                               (if (or (eq match-type 'open)
                                       (eq match-type 'middle-or-open))
                                   'forward
                                 'backward)
                               'backward))
         (match (teal-get-token-match-re token-info search-direction))
         maybe-found-pos)
    ;; if we are searching forward from the token at the current point
    ;; (i.e. for a closing token), need to step one character forward
    ;; first, or the regexp will match the opening token.
    (if (eq search-direction 'forward) (forward-char 1))
    (catch 'found
      ;; If we are attempting to find a matching token for a terminating token
      ;; (i.e. a token that starts a statement when searching back, or a token
      ;; that ends a statement when searching forward), then we don't need to look
      ;; any further.
      (if (or (and (eq search-direction 'forward)
                   (eq match-type 'close))
              (and (eq search-direction 'backward)
                   (eq match-type 'open)))
          (throw 'found nil))
      (while (teal-find-regexp search-direction teal-indentation-modifier-regexp)
        ;; have we found a valid matching token?
        (let ((found-token (match-string 0))
              (found-pos (match-beginning 0)))
          (let ((found-type (teal-get-token-type
                             (teal-get-block-token-info found-token))))
            (if (not (and match (string-match match found-token)))
                ;; no - then there is a nested block. If we were looking for
                ;; a block begin token, found-token must be a block end
                ;; token; likewise, if we were looking for a block end token,
                ;; found-token must be a block begin token, otherwise there
                ;; is a grammatical error in the code.
                (if (not (and
                          (or (eq match-type 'middle)
                              (eq found-type 'middle)
                              (eq match-type 'middle-or-open)
                              (eq found-type 'middle-or-open)
                              (eq match-type found-type))
                          (goto-char found-pos)
                          (teal-find-matching-token-word found-token
                                                        search-direction)))
                    (when maybe-found-pos
                      (goto-char maybe-found-pos)
                      (throw 'found maybe-found-pos)))
              ;; yes.
              ;; if it is a not a middle kind, report the location
              (when (not (or (eq found-type 'middle)
                             (eq found-type 'middle-or-open)))
                (let ((pos (if teal-indent-start-of-block
                               (save-excursion
                                 (goto-char found-pos)
                                 (back-to-indentation)
                                 (point))
                             found-pos)))
                  (throw 'found pos)))
              ;; if it is a middle-or-open type, record location, but keep searching.
              ;; If we fail to complete the search, we'll report the location
              (when (eq found-type 'middle-or-open)
                (setq maybe-found-pos found-pos))
              ;; Cannot use tail recursion. too much nesting on long chains of
              ;; if/elseif. Will reset variables instead.
              (setq token found-token)
              (setq token-info (teal-get-block-token-info token))
              (setq match (teal-get-token-match-re token-info search-direction))
              (setq match-type (teal-get-token-type token-info))))))
      maybe-found-pos)))

(defun teal-goto-matching-block-token (&optional parse-start direction)
  "Find block begion/end token matching the one at the point.
This function moves the point to the token that matches the one
at the current point.  Returns the point position of the first character of
the matching token if successful, nil otherwise.

Optional PARSE-START is a position to which the point should be moved first.
DIRECTION has to be 'forward or 'backward ('forward by default)."
  (if parse-start (goto-char parse-start))
  (let ((case-fold-search nil))
    (if (looking-at teal-indentation-modifier-regexp)
        (let ((position (teal-find-matching-token-word (match-string 0)
                                                      direction)))
          (and position
               (goto-char position))))))

(defun teal-goto-matching-block (&optional noreport)
  "Go to the keyword balancing the one under the point.
If the point is on a keyword/brace that starts a block, go to the
matching keyword that ends the block, and vice versa.

If optional NOREPORT is non-nil, it won't flag an error if there
is no block open/close open."
  (interactive)
  ;; search backward to the beginning of the keyword if necessary
  (if (eq (char-syntax (following-char)) ?w)
      (re-search-backward "\\_<" nil t))
  (let ((position (teal-goto-matching-block-token)))
    (if (and (not position)
             (not noreport))
        (error "Not on a block control keyword or brace")
      position)))

(defun teal-forward-line-skip-blanks (&optional back)
  "Move 1 line forward (back if BACK is non-nil) skipping blank lines.

Moves point 1 line forward (or backward) skipping lines that contain
no Teal code besides comments.  The point is put to the beginning of
the line.

Returns final value of point as integer or nil if operation failed."
  (catch 'found
    (while t
      (unless (eql (forward-line (if back -1 1)) 0)    ;; 0 means success
        (throw 'found nil))
      (unless (or (looking-at "\\s *\\(--.*\\)?$")
                  (teal-comment-or-string-p))
        (throw 'found (point))))))

(eval-when-compile
  (defconst teal-operator-class
    "-+*/^.=<>~:&|"))

(defconst teal-cont-eol-regexp
  (eval-when-compile
    (concat
     "\\(\\_<"
     (regexp-opt '("and" "or" "not" "in" "for" "while"
                   "local" "global" "function" "if" "until" "elseif" "return")
                 t)
     "\\_>\\|"
     "\\(^\\|[^" teal-operator-class "]\\)"
     (regexp-opt '("+" "-" "*" "/" "%" "^" ".." "=="
                   "=" "<" ">" "<=" ">=" "~=" "." ":"
                   "&" "|" "~" ">>" "<<" "~")
                 t)
     "\\)"
     "\\s *\\="))
  "Regexp that matches the ending of a line that needs continuation.

This regexp starts from eol and looks for a binary operator or an unclosed
block intro (i.e. 'for' without 'do' or 'if' without 'then') followed by
an optional whitespace till the end of the line.")

(defconst teal-cont-bol-regexp
  (eval-when-compile
    (concat
     "\\=\\s *"
     "\\(\\_<"
     (regexp-opt '("and" "or" "not") t)
     "\\_>\\|"
     (regexp-opt '("+" "-" "*" "/" "%" "^" ".." "=="
                   "=" "<" ">" "<=" ">=" "~=" "." ":"
                   "&" "|" "~" ">>" "<<" "~")
                 t)
     "\\($\\|[^" teal-operator-class "]\\)"
     "\\)"))
  "Regexp that matches a line that continues previous one.

This regexp means, starting from point there is an optional whitespace followed
by Teal binary operator.  Teal is very liberal when it comes to continuation line,
so we're safe to assume that every line that starts with a binop continues
previous one even though it looked like an end-of-statement.")

(defun teal-last-token-continues-p ()
  "Return non-nil if the last token on this line is a continuation token."
  (let ((line-begin (line-beginning-position))
        (line-end (line-end-position)))
    (save-excursion
      (end-of-line)
      ;; we need to check whether the line ends in a comment and
      ;; skip that one.
      (while (teal-find-regexp 'backward "-" line-begin 'teal-string-p)
        (if (looking-at "--")
            (setq line-end (point))))
      (goto-char line-end)
      (re-search-backward teal-cont-eol-regexp line-begin t))))

(defun teal-first-token-continues-p ()
  "Return non-nil if the first token on this line is a continuation token."
  (let ((line-end (line-end-position)))
    (save-excursion
      (beginning-of-line)
      ;; if first character of the line is inside string, it's a continuation
      ;; if strings aren't supposed to be indented, `teal-calculate-indentation' won't even let
      ;; the control inside this function
      (re-search-forward teal-cont-bol-regexp line-end t))))

(defconst teal-block-starter-regexp
  (eval-when-compile
    (concat
     "\\(\\_<"
     (regexp-opt '("do" "while" "repeat" "until" "if" "then"
                   "else" "elseif" "end" "for" "local" "global" "record" "enum") t)
     "\\_>\\)")))

(defun teal-first-token-starts-block-p ()
  "Return non-nil if the first token on this line is a block starter token."
  (let ((line-end (line-end-position)))
    (save-excursion
      (beginning-of-line)
      (re-search-forward (concat "\\s *" teal-block-starter-regexp) line-end t))))

(defun teal-is-continuing-statement-p (&optional parse-start)
  "Return non-nil if the line at PARSE-START continues a statement.

More specifically, return the point in the line that is continued.
The criteria for a continuing statement are:

* the last token of the previous line is a continuing op,
  OR the first token of the current line is a continuing op

"
  (let ((prev-line nil))
    (save-excursion
      (if parse-start (goto-char parse-start))
      (save-excursion (setq prev-line (teal-forward-line-skip-blanks 'back)))
      (and prev-line
           (not (teal-first-token-starts-block-p))
           (or (teal-first-token-continues-p)
               (and (goto-char prev-line)
                    ;; check last token of previous nonblank line
                    (teal-last-token-continues-p)))))))

(defun teal-make-indentation-info-pair (found-token found-pos)
  "Create a pair from FOUND-TOKEN and FOUND-POS for indentation calculation.

This is a helper function to teal-calculate-indentation-info.
Don't use standalone."
  (cond
   ;; function is a bit tricky to indent right. They can appear in a lot ot
   ;; different contexts. Until I find a shortcut, I'll leave it with a simple
   ;; relative indentation.
   ;; The special cases are for indenting according to the location of the
   ;; function. i.e.:
   ;;       (cons 'absolute (+ (current-column) teal-indent-level))
   ;; TODO: Fix this. It causes really ugly indentations for in-line functions.
   ((string-equal found-token "function")
    (cons 'relative teal-indent-level))

   ;; block openers
   ((and teal-indent-nested-block-content-align
	 (member found-token (list "{" "(" "[")))
    (save-excursion
      (let ((found-bol (line-beginning-position)))
        (forward-comment (point-max))
        ;; If the next token is on this line and it's not a block opener,
        ;; the next line should align to that token.
        (if (and (zerop (count-lines found-bol (line-beginning-position)))
                 (not (looking-at teal-indentation-modifier-regexp)))
            (cons 'absolute (current-column))
          (cons 'relative teal-indent-level)))))

   ;; These are not really block starters. They should not add to indentation.
   ;; The corresponding "then" and "do" handle the indentation.
   ((member found-token (list "if" "for" "while"))
    (cons 'relative 0))
   ;; closing tokens follow: These are usually taken care of by
   ;; teal-calculate-indentation-override.
   ;; elseif is a bit of a hack. It is not handled separately, but it needs to
   ;; nullify a previous then if on the same line.
   ((member found-token (list "until" "elseif"))
    (save-excursion
      (let ((line (line-number-at-pos)))
        (if (and (teal-goto-matching-block-token found-pos 'backward)
                 (= line (line-number-at-pos)))
            (cons 'remove-matching 0)
          (cons 'relative 0)))))

   ;; else is a special case; if its matching block token is on the same line,
   ;; instead of removing the matching token, it has to replace it, so that
   ;; either the next line will be indented correctly, or the end on the same
   ;; line will remove the effect of the else.
   ((string-equal found-token "else")
     (save-excursion
       (let ((line (line-number-at-pos)))
         (if (and (teal-goto-matching-block-token found-pos 'backward)
                  (= line (line-number-at-pos)))
             (cons 'replace-matching (cons 'relative teal-indent-level))
                   (cons 'relative teal-indent-level)))))

   ;; Block closers. If they are on the same line as their openers, they simply
   ;; eat up the matching indentation modifier. Otherwise, they pull
   ;; indentation back to the matching block opener.
   ((member found-token (list ")" "}" "]" "end"))
    (save-excursion
      (let ((line (line-number-at-pos)))
        (teal-goto-matching-block-token found-pos 'backward)
        (if (/= line (line-number-at-pos))
            (teal-calculate-indentation-info (point))
          (cons 'remove-matching 0)))))

   ;; Everything else. This is from the original code: If opening a block
   ;; (match-data 1 exists), then push indentation one level up, if it is
   ;; closing a block, pull it one level down.
   ('other-indentation-modifier
    (cons 'relative (if (nth 2 (match-data))
                        ;; beginning of a block matched
                        teal-indent-level
                      ;; end of a block matched
                      (- teal-indent-level))))))

(defun  teal-add-indentation-info-pair (pair info)
  "Add the given indentation info PAIR to the list of indentation INFO.
This function has special case handling for two tokens: remove-matching,
and replace-matching.  These two tokens are cleanup tokens that remove or
alter the effect of a previously recorded indentation info.

When a remove-matching token is encountered, the last recorded info, i.e.
the car of the list is removed.  This is used to roll-back an indentation of a
block opening statement when it is closed.

When a replace-matching token is seen, the last recorded info is removed,
and the cdr of the replace-matching info is added in its place.  This is used
when a middle-of the block (the only case is 'else') is seen on the same line
the block is opened."
  (cond
   ( (eq 'remove-matching (car pair))
     ; Remove head of list
     (cdr info))
   ( (eq 'replace-matching (car pair))
     ; remove head of list, and add the cdr of pair instead
     (cons (cdr pair) (cdr info)))
   ( (listp (cdr-safe pair))
     (nconc pair info))
   ( t
     ; Just add the pair
     (cons pair info))))

(defun teal-calculate-indentation-info-1 (indentation-info bound)
  "Helper function for `teal-calculate-indentation-info'.

Return list of indentation modifiers from point to BOUND."
  (while (teal-find-regexp 'forward teal-indentation-modifier-regexp
                          bound)
    (let ((found-token (match-string 0))
          (found-pos (match-beginning 0)))
      (setq indentation-info
            (teal-add-indentation-info-pair
             (teal-make-indentation-info-pair found-token found-pos)
             indentation-info))))
  indentation-info)


(defun teal-calculate-indentation-info (&optional parse-end)
  "For each block token on the line, computes how it affects the indentation.
The effect of each token can be either a shift relative to the current
indentation level, or indentation to some absolute column. This information
is collected in a list of indentation info pairs, which denote absolute
and relative each, and the shift/column to indent to."
  (let (indentation-info)

    (while (teal-is-continuing-statement-p)
      (teal-forward-line-skip-blanks 'back))

    ;; calculate indentation modifiers for the line itself
    (setq indentation-info (list (cons 'absolute (current-indentation))))

    (back-to-indentation)
    (setq indentation-info
          (teal-calculate-indentation-info-1
           indentation-info (min parse-end (line-end-position))))

    ;; and do the following for each continuation line before PARSE-END
    (while (and (eql (forward-line 1) 0)
                (<= (point) parse-end))

      ;; handle continuation lines:
      (if (teal-is-continuing-statement-p)
          ;; if it's the first continuation line, add one level
          (unless (eq (car (car indentation-info)) 'continued-line)
            (push (cons 'continued-line teal-indent-level) indentation-info))

        ;; if it's the first non-continued line, subtract one level
        (when (eq (car (car indentation-info)) 'continued-line)
          (pop indentation-info)))

      ;; add modifiers found in this continuation line
      (setq indentation-info
            (teal-calculate-indentation-info-1
             indentation-info (min parse-end (line-end-position)))))

    indentation-info))


(defun teal-accumulate-indentation-info (info)
  "Accumulates the indentation information previously calculated by
teal-calculate-indentation-info. Returns either the relative indentation
shift, or the absolute column to indent to."
  (let ((info-list (reverse info))
        (type 'relative)
        (accu 0))
    (mapc (lambda (x)
            (setq accu (if (eq 'absolute (car x))
                           (progn (setq type 'absolute)
                                  (cdr x))
                         (+ accu (cdr x)))))
          info-list)
    (cons type accu)))

(defun teal-calculate-indentation-block-modifier (&optional parse-end)
  "Return amount by which this line modifies the indentation.
Beginnings of blocks add teal-indent-level once each, and endings
of blocks subtract teal-indent-level once each. This function is used
to determine how the indentation of the following line relates to this
one."
  (let (indentation-info)
    (save-excursion
      ;; First go back to the line that starts it all
      ;; teal-calculate-indentation-info will scan through the whole thing
      (let ((case-fold-search nil))
        (setq indentation-info
              (teal-accumulate-indentation-info
               (teal-calculate-indentation-info parse-end)))))

    (if (eq (car indentation-info) 'absolute)
        (- (cdr indentation-info) (current-indentation))
      (cdr indentation-info))))


(eval-when-compile
  (defconst teal--function-name-rx
    '(seq symbol-start
          (+ (any alnum "_"))
          (* "." (+ (any alnum "_")))
          (? ":" (+ (any alnum "_")))
          symbol-end)
    "Teal function name regexp in `rx'-SEXP format."))


(defconst teal--left-shifter-regexp
  (eval-when-compile
    (rx
     ;; This regexp should answer the following questions:
     ;; 1. is there a left shifter regexp on that line?
     ;; 2. where does block-open token of that left shifter reside?
     (or (seq (group-n 1 symbol-start "local" (+ blank)) "function" symbol-end)
         (seq (group-n 1 symbol-start "global" (+ blank)) "function" symbol-end)

         (seq (group-n 1 (eval teal--function-name-rx) (* blank)) (any "{("))
         (seq (group-n 1 (or
                          ;; assignment statement prefix
                          (seq (* nonl) (not (any "<=>~")) "=" (* blank))
                          ;; return statement prefix
                          (seq word-start "return" word-end (* blank))))
              ;; right hand side
              (or "{"
                  "function"
                  (seq (group-n 1 (eval teal--function-name-rx) (* blank))
                       (any "({")))))))

  "Regular expression that matches left-shifter expression.

Left-shifter expression is defined as follows.  If a block
follows a left-shifter expression, its contents & block-close
token should be indented relative to left-shifter expression
indentation rather then to block-open token.

For example:
   -- 'local a = ' is a left-shifter expression
   -- 'function' is a block-open token
   local a = function()
      -- block contents is indented relative to left-shifter
      foobarbaz()
   -- block-end token is unindented to left-shifter indentation
   end

The following left-shifter expressions are currently handled:
1. local function definition with function block, begin-end
2. function call with arguments block, () or {}
3. assignment/return statement with
   - table constructor block, {}
   - function call arguments block, () or {} block
   - function expression a.k.a. lambda, begin-end block.")


(defun teal-point-is-after-left-shifter-p ()
  "Check if point is right after a left-shifter expression.

See `teal--left-shifter-regexp' for description & example of
left-shifter expression. "
  (save-excursion
    (let ((old-point (point)))
      (back-to-indentation)
      (and
       (/= (point) old-point)
       (looking-at teal--left-shifter-regexp)
       (= old-point (match-end 1))))))



(defun teal-calculate-indentation-override (&optional parse-start)
  "Return overriding indentation amount for special cases.

If there's a sequence of block-close tokens starting at the
beginning of the line, calculate indentation according to the
line containing block-open token for the last block-close token
in the sequence.

If not, return nil."
  (let (case-fold-search token-info block-token-pos)
    (save-excursion
      (if parse-start (goto-char parse-start))

      (back-to-indentation)
      (unless (teal-comment-or-string-p)
        (while
            (and (looking-at teal-indentation-modifier-regexp)
                 (setq token-info (teal-get-block-token-info (match-string 0)))
                 (not (eq 'open (teal-get-token-type token-info))))
          (setq block-token-pos (match-beginning 0))
          (goto-char (match-end 0))
          (skip-syntax-forward " " (line-end-position)))

        (when (teal-goto-matching-block-token block-token-pos 'backward)
          ;; Exception cases: when the start of the line is an assignment,
          ;; go to the start of the assignment instead of the matching item
          (if (or (not teal-indent-close-paren-align)
                  (teal-point-is-after-left-shifter-p))
              (current-indentation)
            (current-column)))))))

(defun teal-calculate-indentation ()
  "Return appropriate indentation for current line as Teal code."
  (save-excursion
    (let ((cur-line-begin-pos (line-beginning-position)))
      (or
       ;; when calculating indentation, do the following:
       ;; 1. check, if the line starts with indentation-modifier (open/close brace)
       ;;    and if it should be indented/unindented in special way
       (teal-calculate-indentation-override)

       (when (teal-forward-line-skip-blanks 'back)
         ;; the order of function calls here is important. block modifier
         ;; call may change the point to another line
         (let* ((modifier
                 (teal-calculate-indentation-block-modifier cur-line-begin-pos)))
           (+ (current-indentation) modifier)))

       ;; 4. if there's no previous line, indentation is 0
       0))))

(defvar teal--beginning-of-defun-re
  (teal-rx-to-string '(: bol (? (symbol "local" "global") ws+) teal-funcheader))
  "Teal top level (matches only at the beginning of line) function header regex.")


(defun teal-beginning-of-proc (&optional arg)
  "Move backward to the beginning of a Teal proc (or similar).

With argument, do it that many times.  Negative arg -N
means move forward to Nth following beginning of proc.

Returns t unless search stops due to beginning or end of buffer."
  (interactive "P")
  (or arg (setq arg 1))

  (while (and (> arg 0)
              (re-search-backward teal--beginning-of-defun-re nil t))
    (setq arg (1- arg)))

  (while (and (< arg 0)
              (re-search-forward teal--beginning-of-defun-re nil t))
    (beginning-of-line)
    (setq arg (1+ arg)))

  (zerop arg))

(defun teal-end-of-proc (&optional arg)
  "Move forward to next end of Teal proc (or similar).
With argument, do it that many times.  Negative argument -N means move
back to Nth preceding end of proc.

This function just searches for a `end' at the beginning of a line."
  (interactive "P")
  (or arg
      (setq arg 1))
  (let ((found nil)
        (ret t))
    (if (and (< arg 0)
             (not (bolp))
             (save-excursion
               (beginning-of-line)
               (eq (following-char) ?})))
        (forward-char -1))
    (while (> arg 0)
      (if (re-search-forward "^end" nil t)
          (setq arg (1- arg)
                found t)
        (setq ret nil
              arg 0)))
    (while (< arg 0)
      (if (re-search-backward "^end" nil t)
          (setq arg (1+ arg)
                found t)
        (setq ret nil
              arg 0)))
    (if found
        (progn
          (beginning-of-line)
          (forward-line)))
    ret))

(defvar teal-process-init-code
  (mapconcat
   'identity
   '("local loadstring = loadstring or load"
     "function tealmode_loadstring(str, displayname, lineoffset)"
     "  if lineoffset > 1 then"
     "    str = string.rep('\\n', lineoffset - 1) .. str"
     "  end"
     ""
     "  local x, e = loadstring(str, '@'..displayname)"
     "  if e then"
     "    error(e)"
     "  end"
     "  return x()"
     "end")
   " "))

(defun teal-make-teal-string (str)
  "Convert string to Teal literal."
  (save-match-data
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward "[\"'\\\t\\\n]" nil t)
        (cond
	 ((string= (match-string 0) "\n")
	  (replace-match "\\\\n"))
	 ((string= (match-string 0) "\t")
	  (replace-match "\\\\t"))
	 (t
          (replace-match "\\\\\\&" t))))
      (concat "'" (buffer-string) "'"))))

;;;###autoload
(defalias 'run-teal #'teal-start-process)

;;;###autoload
(defun teal-start-process (&optional name program startfile &rest switches)
  "Start a Teal process named NAME, running PROGRAM.
PROGRAM defaults to NAME, which defaults to `teal-default-application'.
When called interactively, switch to the process buffer."
  (interactive)
  (or switches
      (setq switches teal-default-command-switches))
  (setq name (or name (if (consp teal-default-application)
                          (car teal-default-application)
                        teal-default-application)))
  (setq program (or program teal-default-application))
  (setq teal-process-buffer (apply 'make-comint name program startfile switches))
  (setq teal-process (get-buffer-process teal-process-buffer))
  (set-process-query-on-exit-flag teal-process nil)
  (with-current-buffer teal-process-buffer
    ;; wait for prompt
    (while (not (teal-prompt-line))
      (accept-process-output (get-buffer-process (current-buffer)))
      (goto-char (point-max)))
    ;; send initialization code
    (teal-send-string teal-process-init-code)

    ;; enable error highlighting in stack traces
    (require 'compile)
    (setq teal--repl-buffer-p t)
    (make-local-variable 'compilation-error-regexp-alist)
    (setq compilation-error-regexp-alist
          (cons (list teal-traceback-line-re 1 2)
                compilation-error-regexp-alist))
    (compilation-shell-minor-mode 1)
    (setq-local comint-prompt-regexp teal-prompt-regexp))

  ;; when called interactively, switch to process buffer
  (if (called-interactively-p 'any)
      (switch-to-buffer teal-process-buffer)))

(defun teal-get-create-process ()
  "Return active Teal process creating one if necessary."
  (unless (comint-check-proc teal-process-buffer)
    (teal-start-process))
  teal-process)

(defun teal-kill-process ()
  "Kill Teal process and its buffer."
  (interactive)
  (when (buffer-live-p teal-process-buffer)
    (kill-buffer teal-process-buffer)
    (setq teal-process-buffer nil)))

(defun teal-set-teal-region-start (&optional arg)
  "Set start of region for use with `teal-send-teal-region'."
  (interactive)
  (set-marker teal-region-start (or arg (point))))

(defun teal-set-teal-region-end (&optional arg)
  "Set end of region for use with `teal-send-teal-region'."
  (interactive)
  (set-marker teal-region-end (or arg (point))))

(defun teal-send-string (str)
  "Send STR plus a newline to the Teal process.

If `teal-process' is nil or dead, start a new process first."
  (unless (string-equal (substring str -1) "\n")
    (setq str (concat str "\n")))
  (process-send-string (teal-get-create-process) str))

(defun teal-send-current-line ()
  "Send current line to the Teal process, found in `teal-process'.
If `teal-process' is nil or dead, start a new process first."
  (interactive)
  (teal-send-region (line-beginning-position) (line-end-position)))

(defun teal-send-defun (pos)
  "Send the function definition around point to the Teal process."
  (interactive "d")
  (save-excursion
    (let ((start (if (save-match-data (looking-at "^function[ \t]"))
                     ;; point already at the start of "function".
                     ;; We need to handle this case explicitly since
                     ;; teal-beginning-of-proc will move to the
                     ;; beginning of the _previous_ function.
                     (point)
                   ;; point is not at the beginning of function, move
                   ;; there and bind start to that position
                   (teal-beginning-of-proc)
                   (point)))
          (end (progn (teal-end-of-proc) (point))))

      ;; make sure point is in a function definition before sending to
      ;; the process
      (if (and (>= pos start) (< pos end))
          (teal-send-region start end)
        (error "Not on a function definition")))))

(defun teal-maybe-skip-shebang-line (start)
  "Skip shebang (#!/path/to/interpreter/) line at beginning of buffer.

Return a position that is after Teal-recognized shebang line (1st
character in file must be ?#) if START is at its beginning.
Otherwise, return START."
  (save-restriction
    (widen)
    (if (and (eq start (point-min))
             (eq (char-after start) ?#))
        (save-excursion
          (goto-char start)
          (forward-line)
          (point))
      start)))

(defun teal-send-region (start end)
  (interactive "r")
  (setq start (teal-maybe-skip-shebang-line start))
  (let* ((lineno (line-number-at-pos start))
         (teal-file (or (buffer-file-name) (buffer-name)))
         (region-str (buffer-substring-no-properties start end))
         (command
          ;; Print empty line before executing the code so that the first line
          ;; of output doesn't end up on the same line as current prompt.
          (format "print(''); tealmode_loadstring(%s, %s, %s);\n"
                  (teal-make-teal-string region-str)
                  (teal-make-teal-string teal-file)
                  lineno)))
    (teal-send-string command)
    (when teal-always-show (teal-show-process-buffer))))

(defun teal-prompt-line ()
  (save-excursion
    (save-match-data
      (forward-line 0)
      (if (looking-at comint-prompt-regexp)
          (match-end 0)))))

(defun teal-send-teal-region ()
  "Send preset Teal region to Teal process."
  (interactive)
  (unless (and teal-region-start teal-region-end)
    (error "teal-region not set"))
  (teal-send-region teal-region-start teal-region-end))

(defalias 'teal-send-proc 'teal-send-defun)

(defun teal-send-buffer ()
  "Send whole buffer to Teal process."
  (interactive)
  (teal-send-region (point-min) (point-max)))

(defun teal-restart-with-whole-file ()
  "Restart Teal process and send whole file as input."
  (interactive)
  (teal-kill-process)
  (teal-send-buffer))

(defun teal-show-process-buffer ()
  "Make sure `teal-process-buffer' is being displayed.
Create a Teal process if one doesn't already exist."
  (interactive)
  (display-buffer (process-buffer (teal-get-create-process))))


(defun teal-hide-process-buffer ()
  "Delete all windows that display `teal-process-buffer'."
  (interactive)
  (when (buffer-live-p teal-process-buffer)
    (delete-windows-on teal-process-buffer)))

(defun teal-funcname-at-point ()
  "Get current Name { '.' Name } sequence."
  ;; FIXME: copying/modifying syntax table for each call may incur a penalty
  (with-syntax-table (copy-syntax-table)
    (modify-syntax-entry ?. "_")
    (current-word t)))

(defun teal-search-documentation ()
  "Search Teal documentation for the word at the point."
  (interactive)
  (let ((url (concat teal-documentation-url "#pdf-" (teal-funcname-at-point))))
    (funcall teal-documentation-function url)))

(defun teal-toggle-electric-state (&optional arg)
  "Toggle the electric indentation feature.
Optional numeric ARG, if supplied, turns on electric indentation when
positive, turns it off when negative, and just toggles it when zero or
left out."
  (interactive "P")
  (let ((num_arg (prefix-numeric-value arg)))
    (setq teal-electric-flag (cond ((or (null arg)
                                       (zerop num_arg)) (not teal-electric-flag))
                                  ((< num_arg 0) nil)
                                  ((> num_arg 0) t))))
  (message "%S" teal-electric-flag))

(defun teal-forward-sexp (&optional count)
  "Forward to block end"
  (interactive "p")
  ;; negative offsets not supported
  (cl-assert (or (not count) (>= count 0)))
  (save-match-data
    (let ((count (or count 1))
          (block-start (mapcar 'car teal-sexp-alist)))
      (while (> count 0)
        ;; skip whitespace
        (skip-chars-forward " \t\n")
        (if (looking-at (regexp-opt block-start 'words))
            (let ((keyword (match-string 1)))
              (teal-find-matching-token-word keyword 'forward))
          ;; If the current keyword is not a "begin" keyword, then just
          ;; perform the normal forward-sexp.
          (forward-sexp 1))
        (setq count (1- count))))))

(defun teal-compile-to-lua ()
  "Compiles the current Teal file to Lua."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (save-buffer)
    (with-temp-buffer
      (apply 'call-process teal-default-application
             nil (current-buffer) nil
             (list "gen" file-name))
      (message (string-trim (buffer-string))))))

(defconst teal-compile-and-print-code
  (mapconcat
   'identity
   '("local tl = require('tl')"
     "local filename = arg[1]"
     "local env = tl.init_env(true)"
     "local result, err = tl.process(filename, env, nil, {})"
     "if err then"
     "  error(err)"
     "end"
     "print(tl.pretty_print_ast(result.ast))"
     )
   " "))

(defun teal-compile-to-lua-and-show ()
  "Compiles the current Teal file to Lua and opens the output in
a new buffer for inspection."
  (interactive)
  (let ((buffer (get-buffer-create "*teal-gen-output*"))
        (file-name (buffer-file-name)))
    (save-buffer)
    (with-current-buffer buffer
      (erase-buffer)
      (apply 'call-process-region teal-compile-and-print-code nil
             lua-default-application
             nil (current-buffer) nil
             (list "-" file-name))
      (lua-mode)
      (goto-char (point-min)))
    (pop-to-buffer buffer)
    (message "Compilation finished.")))

(defun teal-check ()
  "Typechecks the current Teal file."
  (interactive)
  (compile (format "tl check %s" (buffer-file-name))))


;; menu bar

(define-key teal-mode-menu [restart-with-whole-file]
  '("Restart With Whole File" .  teal-restart-with-whole-file))
(define-key teal-mode-menu [kill-process]
  '("Kill Process" . teal-kill-process))

(define-key teal-mode-menu [hide-process-buffer]
  '("Hide Process Buffer" . teal-hide-process-buffer))
(define-key teal-mode-menu [show-process-buffer]
  '("Show Process Buffer" . teal-show-process-buffer))

(define-key teal-mode-menu [end-of-proc]
  '("End Of Proc" . teal-end-of-proc))
(define-key teal-mode-menu [beginning-of-proc]
  '("Beginning Of Proc" . teal-beginning-of-proc))

(define-key teal-mode-menu [send-teal-region]
  '("Send Teal-Region" . teal-send-teal-region))
(define-key teal-mode-menu [set-teal-region-end]
  '("Set Teal-Region End" . teal-set-teal-region-end))
(define-key teal-mode-menu [set-teal-region-start]
  '("Set Teal-Region Start" . teal-set-teal-region-start))

(define-key teal-mode-menu [send-current-line]
  '("Send Current Line" . teal-send-current-line))
(define-key teal-mode-menu [send-region]
  '("Send Region" . teal-send-region))
(define-key teal-mode-menu [send-proc]
  '("Send Proc" . teal-send-proc))
(define-key teal-mode-menu [send-buffer]
  '("Send Buffer" . teal-send-buffer))
(define-key teal-mode-menu [search-documentation]
  '("Search Documentation" . teal-search-documentation))


(provide 'teal-mode)

;;; teal-mode.el ends here

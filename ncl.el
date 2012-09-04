;;; ncl.el --- Major mode for NCAR Command Language(NCL)
;;

;; Author: David Brown
;;    Sylvia Murphy,
;;    C. Schreck,
;;    A. Srock,
;;    T. Corti,
;;    ETH Zurich,
;; Maintainer:
;; Created:
;; Version: 0.97
;; Keywords: ncl, NCAR Command Language.

;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;********************************************
;; HOW TO USE
;;********************************************
;; 1) place this file somewhere on your local system e.g. ~your_home/bin
;; 2) in your .emacs or .xemacs/custom.el file, add and properly modify //
;; the following (without the comments):
;; (setq auto-mode-alist (cons '("\.ncl$" . ncl-mode) auto-mode-alist))
;; (autoload 'ncl-mode "LOCATION/ncl.el")
;; (add-hook 'ncl-mode-hook
;;           (lambda ()
;;             )
;;           )
;; 3) setup display colors for font-lock.  You may also want to set default
;; foreground and background colors.  Colors can be Xwindows names or #rrggbb.
;; These should also go somewhere in your .emacs or .xemacs/custom.el file.
;;     ; highlight comments
;;         (set-face-foreground font-lock-comment-face "FireBrick")
;;     ; highlight strings
;;         (set-face-foreground font-lock-string-face "Salmon")
;;     ; highlight keywords, array descriptors, and tests
;;         (set-face-foreground font-lock-keyword-face "Purple")
;;     ; highlight built-in functions
;;         (set-face-foreground font-lock-builtin-face "Blue")
;;     ; highlight gsn* functions
;;         (set-face-foreground font-lock-variable-name-face "SteelBlue")
;;     ; highlight shea_util and contributed functions
;;         (set-face-foreground font-lock-function-name-face  "CadetBlue")
;;     ; highlight resources
;;         (set-face-foreground font-lock-constant-face  "ForestGreen")
;;

;;; Change Log:
;;
;; September 20 2011
;; Revision 0.34
;;  - Updated to include new functions, resources, and
;;    keywords added in NCL 6.0.0
;;
;; Revision 0.33
;; Changes to 0.32 by T. Corti, ETH Zurich and David Brown,
;; Changes between 0.2 and 0.3 by C. Schreck and A. Srock, University at Albany
;; Changes between 0.1 and 0.2 Provided by Heiko Klein of Norway

;; August 19 2003 Sylvia Murphy
;; National Center for Atmospheric Research
;; Does text highlighting for NCL reserved words, built-in functions,
;; gsn* functions, contributed and shea-util functions, text, and comments.
;; Does automatic indenting between begin and end statments, and within
;; do loops and if statements.
;;
;; Emacs has a lot more power that these functions. I do not use that
;; functionality, so i did not spend any more time trying to add abbreviations,
;; special keymaps etc.
;;
;; Updates in version 0.32
;; Added Comment Handling (M-;).
;;  - Insert a comment at the end of the current line
;;  - Alternatively comment/uncomment selected region
;; Use syntactic fontification for comments and strings
;; Correct fontification of strings containing a semicolon (;)
;; Added highlightning for resources using font-lock-constant-face
;; All documented functions are now highlighted (modification by D. Brown)
;;
;; Updates in version 0.3:
;; Added more keywords (full list from NCL documentation)
;; Changed color mapping (font-lock) settings:
;;   - removed usage of font-lock-reference-face
;;   - added usage of font-lock-builtin-face
;;   - NCL built-in functions now use font-lock-builtin-face
;;   - contributed and shea_util functions now use font-lock-function-face
;;   - added boolean and value test keywords
;;   - added keywords for beginning and ending arrays: (/ and /)
;;   - all keywords now use font-lock-keyword-face
;;   - explicitly fontifies strings with font-lock-string-face
;; Changed syntax type of underscore to "word" instead of punctuation
;; Updated "How to Use" instructions for ease of inclusion with Xemacs
;;
;; KNOWN PROBLEMS in version 0.32:
;; 1) Comment Handling does not work in xemacs
;; 2) Comments may not fontify on file open in xemacs
;;
;; KNOWN PROBLEMS in version 0.3:
;; 1) Comments with embedded strings don't initially fontify properly, but
;;    do change if line modified somehow
;; 2) Strings containing a semicolon (;) do not fontify properly
;;
;; KNOWN PROBLEMS THAT VERSION 0.2 fixed
;; 1) Works with xemacs 21.*
;; 2) Works with emacs 20.*

;; KNOWN PROBLEMS in Version 0.1
;; 1) Only partially works with emacs version 20.3.2
;;    a) highlights only comments and text, and only after tabs
;;    b) indentation appears to work
;; 2) Does not work with xemacs
;; 3) Not all NCL built-in functions are highlighted. I listed MY favorite
;;    ones.
;; 4) Have not demonstrated how to change the indentation value in .emacs
;; 5) The ncl-in-comment function does not work. Its calls are commented out.

;;; Code:

(require 'ncl-mode-keywords)

;;=================================================================
;; user options
;;=================================================================
(defgroup ncl nil
  "major mode to edit Ncar Command Line(NCL) language "
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

(defcustom ncl-mode-hook nil
  "Hook run when entering NCL mode."
  :type    'hook
  ;; Not the only safe options, but some common ones.
  :safe    (lambda (value) (member value '((ncl-add-imenu-menu) nil)))
  :options '(ncl-add-imenu-menu)
  :group   'ncl)

(defcustom ncl-shell-program "ncl"
  "*The perl shell program location."
  :type 'string
  :group 'ncl)

(defvar ncl-var-re
  (concat
   "^[ \t]*"                            ;initial optional space
   "\\([a-z0-9_]*\\)"                   ;var
   "[@]?.*"                             ;optional "@" and space
   "[ \t]*=.*"                          ;and rest
   )
  "Regexp for matching variable.")

(defconst ncl-font-lock-keywords
  (eval-when-compile            ; for  faster loading (is it working?)
    `(;; ncl major keywords
      (,(concat
         "\\<" (regexp-opt
                (append ncl-key-keywords '("True" "False")) 'paren) "\\>")
       (1 font-lock-keyword-face))

      ;; operators
      (,(concat
         "\\(" (regexp-opt ncl-key-operators 'paren) "\\)")
       (1 font-lock-type-face))

      ;;"ncl built-in functions",
      (,(concat
         "\\<" (regexp-opt ncl-key-builtin 'paren) "\\>")
       (1 font-lock-builtin-face))

      ;; contrib functions
      (,(concat
         "\\<" (regexp-opt
                (append ncl-key-contrib ncl-key-shea ncl-key-pop
                        ncl-key-skewt ncl-key-diag ncl-key-user ncl-key-wrfarw
                        ncl-key-wrfcontrib ncl-key-windrose
                        ) 'paren) "\\>")
       (1 font-lock-function-face))

      ;; ncl gsn function-face
      (,(concat
         "\\<" (regexp-opt ncl-key-gsn 'paren) "\\>")
       (1 font-lock-variable-name-face))

      ;; ncl resources
      (,(concat
         "\\<" (regexp-opt ncl-key-resources t) "\\>")
       (1 font-lock-constant-face))

      ;; variable face seq`ncl-var-re'
      (,ncl-var-re (1 font-lock-variable-name-face))
      ))
  "Ncl font lock key words.")

(put 'ncl-mode 'font-lock-defaults 'ncl-font-lock-keywords)

;;=================================================================
;; imenu
;;=================================================================
;;; imenu support for ncl-mode
(defcustom ncl-imenu-generic-expression
  `(("functions" "^[[:blank:]]*function[[:blank:]]+\\(.*\\)(.*)" 1)
    ("procedures" "^[[:blank:]]*procedure[[:blank:]]+\\(.*\\)(.*)" 1)
    ,(list "variables" ncl-var-re 1))
  "Generic expression for matching functions and procedure."
  :type 'string
  :group 'ncl)

(defun ncl-add-imenu-menu ()
  "Add an `imenu' menu to the menubar. The look up can be customized with
`ncl-imenu-generic-expression'"
  (interactive)
  (imenu-add-to-menubar "Imenu")
  (redraw-frame (selected-frame)))

;;************************************************
;; some variables used in the creation of ncl-mode
;;************************************************
(defvar ncl-mode-map nil
  "Keymap used in NCL mode.")
(defvar ncl-startup-message t
  "*Non-nil displays a startup message when `ncl-mode' is first called.")
(defconst ncl-mode-version "v0.97")
;;************************************************
;; syntax table
;;************************************************
;; characters are preceeded by a ?.
;; "." indicates punctuation
;; "_" indicates a symbol
;; "\"" indicates a string (must escape the double-quote)
;; "<" indicates a comment
;; "w" indicates a word character

(defvar ncl-mode-syntax-table nil
  "Syntax table in use in `ncl-mode' buffers.")
(if ncl-mode-syntax-table ()
  (setq ncl-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\;  "<"  ncl-mode-syntax-table)
  (modify-syntax-entry ?+   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?-   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?*   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?/   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?^   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?#   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?=   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?%   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?<   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?>   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?\'  "\"" ncl-mode-syntax-table)
  (modify-syntax-entry ?\"  "\"" ncl-mode-syntax-table)
  (modify-syntax-entry ?\\  "." ncl-mode-syntax-table)
  (modify-syntax-entry ?_   "w"  ncl-mode-syntax-table)
  (modify-syntax-entry ?{   "\(}"  ncl-mode-syntax-table)
  (modify-syntax-entry ?}   "\){"  ncl-mode-syntax-table)
  (modify-syntax-entry ?$   "_"  ncl-mode-syntax-table)
  (modify-syntax-entry ?.   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?\n  ">"  ncl-mode-syntax-table)
  (modify-syntax-entry ?\f  ">"  ncl-mode-syntax-table))

(defvar ncl-find-symbol-syntax-table nil
  "Syntax table that treats symbol characters as word characters.")

(if ncl-find-symbol-syntax-table ()
  (setq ncl-find-symbol-syntax-table
        (copy-syntax-table ncl-mode-syntax-table))
  )
;;****************************************************************************
;; keymap
;;****************************************************************************
(defvar ncl-mode-map nil
  "Keymap used in NCL mode.")
(if ncl-mode-map ()
  (setq ncl-mode-map (make-sparse-keymap))
  (define-key ncl-mode-map "\t"       'ncl-indent-line))
;;****************************************************************************
;; indenting variables
;;****************************************************************************
(defvar ncl-main-block-indent 2
  "*Extra indentation for the main block of code. That is the block between
the begin statement and the end statement.")

(defvar ncl-main-block-end -2
  "*The offset that places the end statement back on the left margin. This is
the negative of `ncl-main-block-indent`")

(defvar ncl-block-indent 2
  "*Extra indentation for do loops.")

(defvar ncl-block-end -2
  "*The offset that places the `end do` statement back to it origin.")

(defconst ncl-comment-line-start-skip "^[ \t]*;"
  "Regexp to match the start of a full-line comment. That is the
_beginning_ of a line containing a comment delmiter `\;' preceded
only by whitespace.")

;; defconst are constants that never change
;; the \` matches only those at the beginning of the buffer and no other
(defconst ncl-begin "\\<\\(begin\\)\\>\\|\\`"
  "Regular expression to find the begin statement.")

;; the \' matches only those at the end of the buffer and no other
(defconst ncl-end "\\<\\(^end$\\)\\>\\|\\'"
  "Regular expression to find the line that indicates the end of a
script.")

(defconst ncl-begin-do "^[ /t]*do"
  "Regular expression to find the beginning of a do loop.")

(defconst ncl-else "^[ /t]*else"
  "Regular expression to find an else statment.")

(defconst ncl-begin-if "^[ /t]*if"
  "Regular expression to find the beginning of a if statment.")

(defconst ncl-enddo "end[ ]do"
  "Regular expression to find the end of a do loop")

(defconst ncl-endif "end[ ]if"
  "Regular expression to find the end of a if statement")

(defconst ncl-identifier "[a-zA-Z][a-zA-Z0-9$_]+"
  "Regular expression matching an NCL identifier.")

(defconst ncl-label (concat ncl-identifier ":")
  "Regular expression matching NCL labels.")

(defvar ncl-no-change-comment ";;"
  "*The indentation of a comment that starts with this regular
expression will not be changed. Note that the indentation of a comment
at the beginning of a line is never changed.")

(defvar ncl-begin-line-comment nil
  "*A comment anchored at the beginning of line.
A comment matching this regular expression will not have its
indentation changed.  If nil the default is \"^\;\", i.e., any line
beginning with a \"\;\".  Expressions for comments at the beginning of
the line should begin with \"^\".")

(defvar ncl-code-comment ";;[^;]"
  "*A comment that starts with this regular expression on a line by
itself is indented as if it is a part of NCL code.  As a result if
the comment is not preceded by whitespace it is unchanged.")
;;****************************************************************************
;; indenting functions
;;****************************************************************************
(defun ncl-beginning-of-statement ()
  "Move to beginning of the current statement. Skips back past statement
continuations. Point is placed at the beginning of the line whether or not
this is an actual statement."
  (if (save-excursion (forward-line -1) (ncl-is-continuation-line))
      (ncl-previous-statement)
    (beginning-of-line)))

(defun ncl-end-of-statement ()
  "Moves point to the end of the current NCL statement. If not in a statement
just moves to end of line. Returns position."
  (interactive)
  (while (and (ncl-is-continuation-line)
              (= (forward-line 1) 0)))
  (end-of-line) (point))

(defun ncl-previous-statement ()
  "Moves point to beginning of the previous statement. Returns t if the
current line before moving is the beginning of the first non-comment
statement in the file, and nil otherwise."
  (interactive)
  (let (first-statement)
    (if (not (= (forward-line -1) 0))
        ;; first line in file
        t
      ;; skip blank lines, label lines, include lines and line comments
      (while (and
              ;; The current statement is the first statement until we
              ;; reach another statement.
              (setq first-statement
                    (or
                     (looking-at ncl-comment-line-start-skip)
                     (looking-at "[ \t]*$")
                     (looking-at (concat "[ \t]*" ncl-label "[ \t]*$"))
                     (looking-at "^@")))
              (= (forward-line -1) 0)))
      ;; skip continuation lines
      (while (and
              (save-excursion
                (forward-line -1)
                (ncl-is-continuation-line))
              (= (forward-line -1) 0)))
      first-statement)))

(defun ncl-is-continuation-line ()
  "Tests if current line is continuation line."
  (save-excursion
    (ncl-look-at "\\<\\$")))

(defun ncl-look-at (regexp &optional cont beg)
  "Searches current line from current point for the regular expression REGEXP.
If optional argument CONT is non-nil, searches to the end of the current
statement. If optional arg BEG is non-nil, search starts from the beginning
of the current statement. Ignores matches that end in a comment or inside a
string expression. Returns point if successful, nil otherwise.  This function
produces unexpected results if REGEXP contains quotes or a comment delimiter.
The search is case insensitive.  If successful leaves point after the match,
otherwise, does not move point."
  (let ((here (point))
        (old-syntax-table (syntax-table))
        (case-fold-search t)
        eos
        found)
    (set-syntax-table ncl-find-symbol-syntax-table)
    (setq eos
          (if cont
              (save-excursion (ncl-end-of-statement) (point))
            (save-excursion (end-of-line) (point))))
    (if beg (ncl-beginning-of-statement))
    (while (and (setq found (re-search-forward regexp eos t))
                (ncl-quoted)))
    (set-syntax-table old-syntax-table)
    (if (not found) (goto-char here))
    found))

(defun ncl-in-quote ()
  "Returns location of the opening quote if point is in a NCL string constant,
nil otherwise. Ignores comment delimiters on the current line. Properly
handles nested quotation marks and octal constants - a double quote followed
by an octal digit."
;;; Treat an octal inside an apostrophe to be a normal string. Treat a
;;; double quote followed by an octal digit to be an octal constant
;;; rather than a string. Therefore, there is no terminating double
;;; quote.
  (save-excursion
    ;; Because single and double quotes can quote each other we must
    ;; search for the string start from the beginning of line.
    (let* ((start (point))
           (eol (progn (end-of-line) (point)))
           (bq (progn (beginning-of-line) (point)))
           (endq (point))
           (data (match-data))
           delim
           found)
      (while  (< endq start)
        ;; Find string start
        ;; Don't find an octal constant beginning with a double quote
        (if (re-search-forward "\"[^0-7]\\|'\\|\"$" eol 'lim)
            ;; Find the string end. In NCL, two consecutive delimiters
            ;; after the start of a string act as an escape for the
            ;; delimiter in the string. Two consecutive delimiters alone
            ;; (i.e., not after the start of a string) is the the
            ;; null string.
            (progn
              ;; Move to position after quote
              (goto-char (1+ (match-beginning 0)))
              (setq bq (1- (point)))
              ;; Get the string delimiter
              (setq delim (char-to-string (preceding-char)))
              ;; Check for null string
              (if (looking-at delim)
                  (progn (setq endq (point)) (forward-char 1))
                ;; Look for next unpaired delimiter
                (setq found (search-forward delim eol 'lim))
                (while (looking-at delim)
                  (forward-char 1)
                  (setq found (search-forward delim eol 'lim)))
                (if found
                    (setq endq (- (point) 1))
                  (setq endq (point)))
                ))
          (progn (setq bq (point)) (setq endq (point)))))
      (store-match-data data)
      ;; return string beginning position or nil
      (if (> start bq) bq))))


(defun ncl-quoted ()
  "Returns t if point is in a comment or quoted string. nil otherwise."
  ;;  (or (ncl-in-comment) (ncl-in-quote)))
  (or (ncl-in-quote)))

(defun ncl-in-comment ()
  "Returns t if point is inside a comment, nil otherwise."
  (save-excursion
    (let ((here (point)))
      (and (ncl-goto-comment) (> here (point))))))

(defun ncl-goto-comment ()
  "Move to start of comment delimiter on current line. Moves to end of line if
there is no comment delimiter. Ignores comment delimiters in strings. Returns
point if comment found and nil otherwise."
  (let ((eos (progn (end-of-line) (point)))
        (data (match-data))
        found)
    ;; Look for first comment delimiter not in a string
    (beginning-of-line)
    (setq found (search-forward comment-start eos 'lim))
    (while (and found (ncl-in-quote))
      (setq found (search-forward comment-start eos 'lim)))
    (store-match-data data)
    (and found (not (ncl-in-quote))
         (progn
           (backward-char 1)
           (point)))))

(defun ncl-current-statement-indent ()
  "Return indentation of the current statement. If in a statement, moves to
beginning of statement before finding indent."
  (ncl-beginning-of-statement)
  (ncl-current-indent))

(defun ncl-current-indent ()
  "Return the column of the indentation of the current line.  Skips any
whitespace. Returns 0 if the end-of-line follows the whitespace."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    ;; if we are at the end of blank line return 0
    (cond ((eolp) 0)
          ((current-column)))))

(defun ncl-calculate-indent ()
  "Return appropriate indentation for current line as NCL code."
  (save-excursion
    (beginning-of-line)
    (cond
     ;; if line is "begin" do nothing and exit
     ((ncl-look-at ncl-begin) 0)
     ;; calculate indent based on previous and current statements
     (t (let ((the-indent
               ;; calculate indent based on previous statement
               (save-excursion
                 (cond
                  ;; retreive the previous statement
                  ( (ncl-previous-statement) 0)

                  ;; indent if previous statment is begin
                  ((ncl-look-at ncl-begin t)
                   (+ (ncl-current-statement-indent) ncl-main-block-indent))

                  ;; indent if previous statment is do
                  ((ncl-look-at ncl-begin-do t)
                   (+ (ncl-current-statement-indent) ncl-block-indent))

                  ;; indent if previous statment is if
                  ((ncl-look-at ncl-begin-if t)
                   (+ (ncl-current-statement-indent) ncl-block-indent))

                  ;; indent if previous statment is else
                  ((ncl-look-at ncl-else t)
                   (+ (ncl-current-statement-indent) ncl-block-indent))

                  ((ncl-current-statement-indent))))))
          ;; adjust the indentation based on the current statement
          (cond
           ;; do loop
           ((ncl-look-at ncl-enddo t)
            (+ the-indent ncl-block-end))
           ;; if statement
           ((ncl-look-at ncl-endif t)
            (+ the-indent ncl-block-end))
           ;; else statement
           ((ncl-look-at ncl-else t)
            (+ the-indent ncl-block-end))

           ;; End block
           ((ncl-look-at ncl-end t)
            (+ the-indent ncl-main-block-end)) ;; end gets negative indent
           (the-indent))

          )))))

(defun ncl-indent-to (col &optional min)
  "Indent from point with spaces until column COL. Inserts space before
markers at point."
  (if (not min) (setq min 0))
  (insert-before-markers
   (make-string (max min (- col (current-column))) ? )))

(defun ncl-indent-left-margin (col)
  "Indent the current line to column COL. Indents such that first
non-whitespace character is at column COL. Inserts spaces before markers at
point."
  (save-excursion
    (beginning-of-line)
    (delete-horizontal-space)
    (ncl-indent-to col)))

(defun ncl-comment-hook ()
  "Compute indent for the beginning of the NCL comment delimiter."
  (if (or (looking-at ncl-no-change-comment)
          (if ncl-begin-line-comment
              (looking-at ncl-begin-line-comment)
            (looking-at "^\;")))
      (current-column)
    (if (looking-at ncl-code-comment)
        (if (save-excursion (skip-chars-backward " \t") (bolp))
            ;; On line by itself, indent as code
            (let ((tem (ncl-calculate-indent)))
              (if (listp tem) (car tem) tem))
          ;; after code - do not change
          (current-column))
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))
           comment-column))))

(defun ncl-indent-line ()
  "Indents current NCL line as code or as a comment."
  (interactive)
  ;; Move point out of left margin.
  (if (save-excursion
        (skip-chars-backward " \t")
        (bolp))
      (skip-chars-forward " \t"))
  (let ((mloc (point-marker)))
    (save-excursion
      (beginning-of-line)
      (if (looking-at ncl-comment-line-start-skip)
          ;; Indentation for a line comment
          (progn
            (skip-chars-forward " \t")
            (ncl-indent-left-margin (ncl-comment-hook)))
        ;; Indent for code line
        (beginning-of-line)
        (if (or
             ;; a label line
             (looking-at (concat "^" ncl-label "[ \t]*$"))
             ;; a batch command
             (looking-at "^[ \t]*@"))
            ;; leave flush left
            nil
          ;; indent the line
          (ncl-indent-left-margin (ncl-calculate-indent)))
        ;; Adjust parallel comment
        ;;        (end-of-line)
        ;;        (if (ncl-in-comment)
        ;;            (indent-for-comment))
        ))
    (goto-char mloc)
    ;; Get rid of marker
    (set-marker mloc nil)
    ))

;;****************************************************************************
;; the command to comment/uncomment text
;;****************************************************************************
(defun ncl-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((deactivate-mark nil) (comment-start ";") (comment-end ""))
    (setq comment-add 1)          ;default to `;;' in comment-region
    (comment-dwim arg)))

;;****************************************************************************
;; define ncl mode
;;****************************************************************************
(defun ncl-mode ()
  "Major mode for editing NCL .ncl files."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'ncl-mode)
  (setq mode-name "NCL")

  ;; modify the keymap
  (define-key ncl-mode-map [remap comment-dwim] 'ncl-comment-dwim)

  (if ncl-startup-message
      (message "Emacs NCL mode version %s." ncl-mode-version)
    )
  ;;**************************
  ;; indentation
  ;;**************************
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'ncl-indent-line)
  (use-local-map ncl-mode-map)
  ;;**************************
  ;; these ensure syntax hightlighting
  ;;**************************
  ;; font-lock setup for various emacs: XEmacs, Emacs 19.29+, Emacs <19.29.
  ;; taken from html-helper-mode, adapted to ncl
  (cond ((string-match "XEmacs\\|Lucid" (emacs-version)) ; XEmacs/Lucid
         (put major-mode 'font-lock-keywords-case-fold-search t)
         (put major-mode 'font-lock-syntactic-keywords t)
         (put major-mode 'font-lock-maximum-decoration 2)
         )
        ;; not sure if this is correct
        ;; XEmacs (19.13, at least) guesses the rest correctly.
        ;; If any older XEmacs don't, then tell me.
        ;;
        ((string-lessp "19.28.89" emacs-version) ; Emacs 19.29 and later
         (make-local-variable 'font-lock-defaults)
         (setq font-lock-defaults '(ncl-font-lock-keywords nil t)))
        ;;
        (t ; Emacs 19.28 and older
         (make-local-variable 'font-lock-keywords-case-fold-search)
         (make-local-variable 'font-lock-keywords)
         ;;(make-local-variable 'font-lock-no-comments)
         (setq font-lock-keywords-case-fold-search t)
         (setq font-lock-keywords ncl-font-lock-keywords)
         ;;(setq font-lock-no-comments t)
         ))

  ;; imenu thing
  (set (make-local-variable 'imenu-generic-expression)
       ncl-imenu-generic-expression)

  (font-lock-mode 1)
  ;;  (setq font-lock-maximum-decoration t)
  ;;  (make-local-variable 'font-lock-defaults)
  ;;  (setq font-lock-defaults 'ncl-keywords)
  ;;  (make-local-variable 'comment-start)
  ;;  (setq comment-start ";")
  ;; turn this on if debuging this code
  (setq debug_on_error t)
  (set-syntax-table ncl-mode-syntax-table)
  (run-hooks 'ncl-mode-hook))


(easy-menu-define ncl-menu ncl-mode-map "Menu for NCL mode."
  `("NCL"
    ("Customization"
     ,(custom-menu-create 'ncl))

    "--"
    ["Comment Region" comment-region mark-active]
    ["Uncomment Region"
     (comment-region (region-beginning) (region-end) 1)
     mark-active]
    ["Indent Region"     indent-region mark-active]

    "--"
    ["Narrow to Subprogram" narrow-to-defun t]
    ["Widen" widen t]

    ["Add Imenu Menu" imenu-add-menubar-index
     :active   (not (lookup-key (current-local-map) [menu-bar index]))
     :included (fboundp 'imenu-add-to-menubar)
     :help "Add an index menu to the menu-bar"]))


;;************************************************************************
(provide 'ncl)

;;; ncl.el ends here

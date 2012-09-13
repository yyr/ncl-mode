;;; ncl-mode.el -- Major Mode for editing NCL scripts

;; Copyright (C) 2012 Yagnesh Raghava Yakkala <http://yagnesh.org>

;; Author: Yagnesh Raghava Yakkala <yagnesh@live.com>
;; URL: https://github.com/yyr/ncl-mode
;; Maintainer: Yagnesh Raghava Yakkala <yagnesh@live.com>
;; Created: Tuesday, July 24 2012
;; Keywords: ncl, Major Mode, ncl-mode, atmospheric science.

;; This file is NOT part of GNU Emacs.

;; ncl-mode.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ncl-mode.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Major mode to edit ncl scripts.

;;; Code:
(require 'ncl-mode-keywords)

(defgroup ncl nil
  "major mode to edit Ncar Command Line(NCL) language "
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

(defgroup ncl-indent nil
  "major mode to edit Ncar Command Line(NCL) language "
  :prefix "ncl-"
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'ncl)

(defcustom ncl-mode-hook nil
  "Hook run when entering NCL mode."
  :type    'hook
  ;; Not the only safe options, but some common ones.
  :safe    (lambda (value) (member value '((ncl-add-imenu-menu) nil)))
  :options '(ncl-add-imenu-menu)
  :group   'ncl)

(defcustom ncl-shell-program "ncl"
  "*The ncl shell program location."
  :type 'string
  :group 'ncl)

(defcustom ncl-indent-tabs-mode nil
  "Indentation can insert tabs in Ncl mode if this is non-nil."
  :type 'boolean
  :group 'ncl-indent)

(defcustom ncl-block-indent 2
  "Indentation of Ncl blocks."
  :type 'integer
  :group 'ncl-indent)

(defcustom ncl-body-indent 0
  "Ncl Script body indentation."
  :type 'integer
  :group 'ncl-indent)

(defcustom ncl-continuation-indent 8
  "Extra indentation applied to continuation lines."
  :type  'integer
  :safe  'integerp
  :group 'ncl-indent)

(defcustom ncl-zero-indent-re
  "\\_<\\(begin\\|end[ \t]*$\\|function\\|local\\|procedure\\|undef\\)\\_>"
  "Extra indentation applied to continuation lines."
  :type  'regexp
  :group 'ncl-indent)

(defcustom ncl-indented-comment-re ";"
  "Regexp matching comments to indent as code"
  :type 'integer
  :group 'ncl-indent)

(defcustom ncl-comment-column 32
  "Indentation column of comments."
  :type 'integer
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

;;; syntax table
(defvar ncl-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\r " "  table) ; return is white space
    (modify-syntax-entry ?_  "w"  table) ; underscore in names
    (modify-syntax-entry ?/ "$"   table) ; paired delimiters
    (modify-syntax-entry ?\\ "\\" table) ; escape chars

    (modify-syntax-entry ?\; "<"  table) ; begin comment
    (modify-syntax-entry ?\n ">"  table) ; end comment
    (modify-syntax-entry ?\f ">"  table)

    (modify-syntax-entry ?$  "_"  table) ; symbol constituents
    (modify-syntax-entry ?\` "_"  table)

    (modify-syntax-entry ?\' "\"" table) ; string quote
    (modify-syntax-entry ?\" "\"" table)

    (modify-syntax-entry ?.   "." table) ; punctuation
    (modify-syntax-entry ?<   "." table)
    (modify-syntax-entry ?>   "." table)
    (modify-syntax-entry ?+  "."  table)
    (modify-syntax-entry ?-  "."  table)
    (modify-syntax-entry ?=  "."  table)
    (modify-syntax-entry ?*  "."  table)
    (modify-syntax-entry ?/  "."  table)
    (modify-syntax-entry ?%  "."  table)
    (modify-syntax-entry ?#  "."  table)
    table)
  "Syntax table used in ncl mode.")

(defvar ncl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-a") 'ncl-beginning-of-fun/proc)
    (define-key map (kbd "C-M-e") 'ncl-end-of-fun/proc)
    (define-key map (kbd "C-M-p") 'ncl-beginning-of-block)
    (define-key map (kbd "C-M-n") 'ncl-end-of-block)
    (define-key map (kbd "C-M-h") 'ncl-mark-defun)
    (define-key map (kbd "C-M-q") 'ncl-indent-exp)
    (define-key map (kbd "C-j")   'reindent-then-newline-and-indent)
    (define-key map (kbd "C-m")   'newline)
    map)
  "Key map for NCL mode.")

;;=================================================================
;; imenu
;;=================================================================
;;; imenu support for ncl-mode
(defcustom ncl-imenu-generic-expression
  `(("functions" "^[[:blank:]]*function[[:blank:]]+\\(.*\\)(.*)" 1)
    ("procedures" "^[[:blank:]]*procedure[[:blank:]]+\\(.*\\)(.*)" 1)
    ,(list "variables" "^[ \t]*\\([a-z0-9_]*\\)[ \t]*=.*" 1))
  "Generic expression for matching functions, procedure and
variable assignments."
  :type 'string
  :group 'ncl)

(defun ncl-add-imenu-menu ()
  "Add an `imenu' menu to the menubar. The look up can be customized with
`ncl-imenu-generic-expression'"
  (interactive)
  (imenu-add-to-menubar "Imenu")
  (redraw-frame (selected-frame)))

;;=================================================================
;; Menu
;;=================================================================
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

;;=================================================================
;; Indentation
;;=================================================================

;; start-string/regexp  indent         variable holding start-string/regexp
;;    ^; (re)              0
;;    ;;;                  0              ncl-comment-region
;;    ;  (re)            as code          ncl-indented-comment-re
;;    ;;                 as code          ncl-comment-region
;;    default            comment-column

(defconst ncl-block-beg-re (regexp-opt '("if" "do" "do while") 'symbols)
  "Regular expression to find beginning of \"if/do while/do\" block.")

(defconst ncl-block-end-re (regexp-opt '("end if"
                                         "end do") 'symbols)
  "Regular expression to find end of block.")

(defconst ncl-end-re "[ \t]*end[ \t]*$"
  "Regular expression to find end of \"end\" block.")

(defconst ncl-else-like-re
  (regexp-opt '("else" "else if") 'symbols)
  "Regexp matching an \"else\" or \"else if\".")

(defconst ncl-do "[ \t]*do"
  "Regular expression to find beginning of  \"do\"")

(defconst ncl-end-do "[ \t]*end[ ]do"
  "Regular expression to find beginning of  \"end do\"")

(defconst ncl-end-if "[ \t]*end[ ]if"
  "Regular expression to find beginning of  \"end if\"")

(defconst ncl-identifier "[a-zA-Z][a-zA-Z0-9$_]+[ \t]*:"
  "Regular expression to find Ncl identifiers. ")

(defconst ncl-fun/proc-block-re
  (regexp-opt '("function" "procedure") 'paren)
  "Regexp used to locate the start of a \"function/procedure\".")

(defconst ncl-indent-beg-re
  (concat "^\\s *" (regexp-opt '("if" "do" "do while"
                                 "begin")) "\\_>")
  "Regexp to match where the indentation gets deeper.")

;;; Inline functions
(defsubst ncl-in-string ()
  "Return non-nil if point is inside a string. Checks from `point-min'."
  (nth 3 (parse-partial-sexp (point-min)
                             (point))))

(defsubst ncl-in-comment ()
  "Return non-nil if point is inside a comment."
  (nth 4 (parse-partial-sexp (point-min)
                             (point))))

(defsubst ncl-line-continued ()
  "Return t if the current line is a continued one. This includes
  comment lines embedded in continued lines, but not the last
  line of a continued statements."
  (save-excursion
    (beginning-of-line)
    (while (and (looking-at "[ \t]*\\(;\\|$\\)"))
      (zerop (forward-line -1))) ; adjustment for empty line
    (end-of-line)
    (while (ncl-in-comment)
      (search-backward ";" (line-beginning-position))
      (skip-chars-backward ";"))        ; for a comment line
    (skip-chars-backward " \t")
    (= (preceding-char) ?\\)))

(defsubst ncl-current-indentation ()
  "Return indentation of current line."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")))

(defsubst ncl-indent-to (col)
  "Indent current line to column COL."
  (beginning-of-line)
  (back-to-indentation)
  (delete-horizontal-space)
  (indent-to col))

(defsubst ncl-get-present-comment-type ()
  "If point lies within a comment, return the string starting the comment."
  (save-excursion
    (when (ncl-in-comment)
      (beginning-of-line)
      (re-search-forward ";+[ \t]*" (line-end-position))
      (while (ncl-in-string)
        (re-search-forward ";+[ \t]*" (line-end-position)))
      (match-string-no-properties 0))))

(defsubst ncl-looking-at-fun/proc-start ()
  "Return (KIND NAME) if a fuction/procedure block with name NAME
starts after point. "
  (cond
   ((looking-at "\\(function\\)[ \t]+\\(\\sw+\\)\\>")
    (list (match-end 1) (match-end 2)))
   ((looking-at "\\(procedure\\)[ \t]+\\(\\sw+\\)\\>")
    (list (match-end 1) (match-end 2)))))

(defsubst ncl-looking-at-begin ()
  "Return \"begin\" if a begin statement starts after point"
  (when (looking-at "begin")
    "begin"))

(defsubst ncl-looking-at-if ()
  "Return \"if\" and next word (may be \"while\") if a do statement starts
after point."
  (if (looking-at "\\(\\(if\\)[ \t]*\\)")
      (list (match-string-no-properties 2))))

(defsubst ncl-looking-at-do ()
  "Return (DO NEXT) and next word (may be 'while) if a do statement starts
after point."
  (if (looking-at "\\(\\(do\\)[ \t]+\\(\\sw+\\)\\)")
      (list (match-string-no-properties 2) (match-string-no-properties 3))))

(defsubst ncl-looking-at-do-while ()
  "Return 'dowhile if a do statement starts after point."
  (if (and (ncl-looking-at-do)
           (equal "while" (cadr (ncl-looking-at-do))))
      'dowhile
    nil))

(defsubst ncl-looking-at-only-do ()
  "Return 'do if the statement starts with only do"
  (if (and (ncl-looking-at-do)
           (equal "while" (cadr (ncl-looking-at-do))))
      nil
    'do))

(defsubst ncl-looking-at-end ()
  "Return (KIND) of end after the point."
  (cond ((looking-at ncl-end-do)
         'enddo)
        ((looking-at ncl-end-if)
         'endif)
        ((looking-at ncl-end-re)
         'end)
        (t nil)))

(defsubst ncl-looking-at-only-end ()
  "Return t if only \"end\" present on the line."
  (when (eq 'end (ncl-looking-at-end))
    t))

(defsubst ncl-looking-at-end-x ()
  "Return t if \"end\" is not alone (end do end if...)."
  (when (memq  (ncl-looking-at-end) '(enddo endif))
    t))

(defsubst ncl-present-statement-cont ()
  "Return continuation properties of present statement.
Possible return values are:
single - statement is not continued.
begin  - current line is the first in a continued statement.
end    - current line is the last in a continued statement
middle - current line is neither first nor last in a continued statement.
Comment lines embedded amongst continued lines return 'middle."
  (let (pcont cont)
    (save-excursion
      (setq pcont (if (ncl-previous-statement) (ncl-line-continued))))
    (setq cont (ncl-line-continued))
    (cond ((and (not pcont) (not cont)) 'single)
          ((and (not pcont) cont)       'begin)
          ((and pcont       (not cont)) 'end)
          ((and pcont       cont)       'middle)
          (t (error "The impossible occurred")))))

;;; functions
(defun ncl-previous-statement ()
  "Move point to beginning of the previous statement.
If no previous statement is found (i.e. if called from the first statement in
buffer), move to the start of the buffer and return nil. "
  (interactive)
  (let (not-first-statement)
    (beginning-of-line)
    (while (and (setq not-first-statement (zerop (forward-line -1)))
                (looking-at "[ \t]*\\(;\\|$\\)")))
    not-first-statement))

(defun ncl-next-statement ()
  "Move point to beginning of the next ncl statement.
Return nil if no later statement is found."
  (interactive)
  (let (not-last-statement)
    (beginning-of-line)
    (while (and (setq not-last-statement
                      (and (zerop (forward-line 1))
                           (not (eobp))))
                (looking-at "[ \t]*\\(;\\|$\\)")))
    not-last-statement))

(defun ncl-move-to-block (n)
  "Move to the beginning (N < 0) or the end (N > 0) of the current block
or blocks."
  (let ((orig (point))
                                        ;        (start (ncl-calculate-indent))
        (down (looking-at
               (if (< n 0)
                   ncl-block-end-re
                 ncl-block-beg-re)))
        pos done)
    (while (and (not done)
                (not (if (< n 0)
                         (bobp)
                       (eobp))))
      (forward-line n)
      (cond
       ((looking-at "^\\s *$"))
       ((looking-at "^\\s *;"))
       ((and (> n 0)
             (looking-at ncl-block-beg-re))
        (re-search-forward ncl-block-end-re))
       ((and (< n 0)
             (looking-at ncl-block-end-re))
        (re-search-backward ncl-block-beg-re))
       (t
        (setq pos (current-indentation))
        (setq done t))))
    (back-to-indentation)))

(defun ncl-beginning-of-block (&optional arg)
  "Move backward to the beginning of the current block.
with ARG, move up multiple block."
  (interactive "p")
  (ncl-move-to-block (- (or arg 1))))

(defun ncl-end-of-block (&optional arg)
  "Move backward to the end of the current block.
with ARG, move up multiple block."
  (interactive)
  (ncl-move-to-block (or arg 1)))

;;;###autoload
(defun ncl-beginning-of-fun/proc ()
  "Move point to the beginning of the current function or procedure."
  (interactive)
  (let ((count 1)
        (matching-beg))
    (beginning-of-line)
    (while (and (> count 0)
                (re-search-backward ncl-fun/proc-block-re nil 'move))
      (beginning-of-line)
      (skip-chars-forward " \t")
      (cond ((setq matching-beg (ncl-looking-at-fun/proc-start))
             (setq count (1- count)))
            ((ncl-looking-at-end)
             (setq count (1+ count)))))
    (if (zerop count)
        matching-beg
      (if (called-interactively-p 'interactive)
          (message "No beginning found"))
      nil)))

;;;###autoload
(defun ncl-end-of-fun/proc ()
  "Move point to the beginning of the current function or procedure."
  (interactive)
  (let ((count 1)
        matching-end)
    (end-of-line)
    (while (and (> count 0)
                (re-search-forward ncl-end-re nil 'move))
      (beginning-of-line)
      (skip-chars-forward " \t")
      (cond ((ncl-looking-at-fun/proc-start)
             (setq count (1+ count)))
            ((setq matching-end (ncl-looking-at-end))
             (setq count (1- count))))
      (end-of-line))
    (if (zerop count)
        matching-end
      (if (called-interactively-p 'interactive)
          (message "No end found"))
      nil)))


(defun ncl-comment-indent ()
  "Return the indentation to be used for a comment starting at point.
Used for `comment-indent-function' by ncl mode.
if comment type \";;;\", \"^;\" this function return 0.
`ncl-indented-comment-re' (if not trailing code) calls `ncl-calculate-indent'.
All other return `comment-column', leaving at least one space after code."
  (cond ((looking-at ";;;") 0)
        ((save-excursion (looking-at ";")) 0)  ; for "^;"
        ((and (looking-at ncl-indented-comment-re)
              (save-excursion
                (skip-chars-forward " \t")
                (bolp)))
         (ncl-calculate-indent))
        (t (save-excursion
             (skip-chars-forward " \t")
             (max (if (bolp) 0 (1+ (current-column)))
                  comment-column)))))

(defun ncl-indent-line ()
  "Indent current line as ncl code."
  (interactive "*P")
  (let ((pos (point-marker))
        indent)
    (back-to-indentation)
    (if (looking-at ";") ; comment line
        (setq indent (ncl-comment-indent))
      (and (looking-at "end")           ; match struct find its indentation
           (ncl-match-end))
      (setq indent (ncl-calculate-indent)))
    (or (= indent (current-column))
        (ncl-indent-to indent))
    (and (< (point) pos)
         (goto-char pos))
    (set-marker pos nil)))

(defun ncl-match-end ()
  "From an end block statement, find the corresponding block and name."
  (interactive)
  (let ((count 1)
        (top-of-window (window-start))
        (end-point (point))
        beg-name  end-type
        match-pnt)
    (when (save-excursion (back-to-indentation)
                          (setq end-type (ncl-looking-at-end)))
      (save-excursion
        (beginning-of-line)
        (while (and (> count 0)
                    (or (re-search-backward "begin" top-of-window 'move)
                        (re-search-backward ncl-block-beg-re top-of-window 'move)))
          (back-to-indentation)
          (setq match-pnt (point))
          (cond ((or (ncl-in-string)
                     (ncl-in-comment)))
                ((setq beg-name
                       (or
                        (ncl-looking-at-do)
                        (ncl-looking-at-begin)
                        (ncl-looking-at-if)
                        (ncl-looking-at-fun/proc-start)))
                 (setq count (1- count)))
                ((ncl-looking-at-end)
                 (setq count (1+ count)))))
        (if (> count 0)
            (progn
              (setq match-pnt nil)
              (message "No matching beginning."))
          (goto-char end-point)
          (beginning-of-line))))
    match-pnt))

(defun ncl-calculate-indent ()
  "Calculate the indent column based on previous statements."
  (interactive)
  (let (icol cont (pnt (point)))
    (save-excursion
      (back-to-indentation)
      (if (looking-at ncl-zero-indent-re)
          (setq icol 0)
        (if (not (ncl-previous-statement))
            ;; if previous statement is nil, on or first line of first statement
            (setq icol 0)

          (setq cont (ncl-present-statement-cont))
          (if (eq cont 'end)
              (while (not (eq 'begin (ncl-present-statement-cont)))
                (ncl-previous-statement)))

          (cond ((eq cont 'begin)
                 (setq icol (+ (ncl-current-indentation)
                               ncl-continuation-indent)))
                ((eq cont 'middle) (setq icol (current-indentation)))

                (t (setq icol (ncl-current-indentation))
                   (skip-chars-forward " \t")
                   (if (or (ncl-looking-at-fun/proc-start)
                           (ncl-looking-at-if)
                           (ncl-looking-at-do)
                           (looking-at ncl-else-like-re))
                       (setq icol (+ icol ncl-block-indent)))

                   ;; hunt for decrement
                   (goto-char pnt)
                   (beginning-of-line)
                   (cond ((looking-at " \t*$") nil)
                         ((and (skip-chars-forward " \t")
                               (or
                                (looking-at ncl-else-like-re)
                                (ncl-looking-at-end-x)))
                          (setq icol (- icol ncl-block-indent)))
                         (t
                          (setq icol icol))))))))
    icol))

(defun ncl-indent-region ()
  ""
  )

;;;###autoload
(define-derived-mode ncl-mode prog-mode "Ncl"
  "Major mode for editing Ncl scripts.

\\{ncl-mode-map}
"
  (set (make-local-variable 'indent-line-function) 'ncl-indent-line)
  (set (make-local-variable 'indent-region-function) 'ncl-indent-region)
  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-start-skip) ";+ *")
  (set (make-local-variable 'comment-indent-function) 'ncl-comment-indent)
  (setq indent-tabs-mode nil)           ; auto buffer local
  (set (make-local-variable 'imenu-generic-expression)
       ncl-imenu-generic-expression)
  (set (make-local-variable 'font-lock-defaults)
       '(ncl-font-lock-keywords))
  (set (make-local-variable 'beginning-of-defun-function)
       'ncl-beginning-of-subprogram)
  (set (make-local-variable 'end-of-defun-function) 'ncl-end-of-subprogram))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.ncl\\'") 'ncl-mode))

(provide 'ncl-mode)
;;; ncl-mode.el ends here

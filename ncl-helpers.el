;;; ncl-helpers.el
;;
;;    File: ncl-helpers.el
;;  Author: Yagnesh Raghava Yakkala <yagnesh@NOSPAM.live.com>
;; Created: Friday, December  9 2011

;;; Description:
;; just place holder for now


;;=================================================================
;; User options
;;=================================================================
(defgroup ncl nil
  "major mode to edit Ncar Command Line(NCL) language "
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

(defgroup ncl-indent nil
  "Indentation variables in NCL mode."
  :prefix "ncl-"
  :group  'ncl-indent)

(defgroup ncl-comment nil
  "Comment-handling variables in NCL mode."
  :prefix "ncl-"
  :group  'ncl)

(defcustom ncl-block-indent 2
  "Extra indentation applied to do, if, where(?) blocks."
  :type  'integer
  :safe  'integerp
  :group 'ncl-indent)

(defcustom ncl--indent 2
  "Extra indentation applied to IF, SELECT CASE, WHERE and FORALL blocks."
  :type  'integer
  :safe  'integerp
  :group 'ncl-indent)


(defcustom ncl-tab-mode-default nil
  "Default tabbing/carriage control style for empty files in Ncl mode.
A non-nil value specifies tab-digit style of continuation control.
A value of nil specifies that continuation lines are marked
with a character in column 6."
  :type  'boolean
  :safe  'booleanp
  :group 'ncl-indent)

(defcustom ncl-continuation-string "\\"
  "Single-character string used for Ncl continuation lines.
In fixed format continuation style, this character is inserted in
column 6 by \\[ncl-split-line] to begin a continuation line.
Also, if \\[ncl-indent-line] finds this at the beginning of a
line, it will convert the line into a continuation line of the
appropriate style.  Normally \"$\"."
  :type  'string
  :safe  (lambda (value) (and (stringp value) (= (length value) 1)))
  :group 'ncl)

(defcustom ncl-comment-region ";;$"
  "String inserted by \\[ncl-comment-region] at start of each line in region."
  :type  'string
  :safe  'stringp
  :group 'ncl-indent)

(defcustom ncl-indented-comment-re ";"
  "Regexp matching comments to indent as code."
  :type  'regexp
  :safe  'stringp
  :group 'ncl-indent)

(defcustom ncl-mode-hook nil
  "Hook run when entering NCL mode."
  :type    'hook
  ;; Not the only safe options, but some common ones.
  :safe    (lambda (value) (member value '((ncl-add-imenu-menu) nil)))
  :options '(ncl-add-imenu-menu)
  :group   'ncl)

;; User options end here.

(defvar ncl-imenu-generic-expression
  ;; this one directly copied form fortran-mode
  ;; These patterns could be confused by sequence nos. in cols 72+ and
  ;; don't allow continuations everywhere.
  (list
   (list
    nil
    ;; [This will be fooled by `end function' allowed by G77.  Also,
    ;; it assumes sensible whitespace is employed.]
    (concat
     ;; leading whitespace:
     "^\\s-+\\("
     ;; function declaration with optional type, e.g. `real',
     ;; `real*4', character(*), `double precision':
     "\\(\\sw\\|\\s-\\|[*()+]\\)*"
     "\\<function\\|subroutine\\|entry\\|block\\s-*data\\|program\\)"
     ;; Possible statement continuation:
     "[ \t" ncl-continuation-string "]+"
     ;; Variable to index:
     "\\(\\sw+\\)")
    3)
   ;; Un-named block data.
   '(nil "^\\s-+\\(block\\s-*data\\)\\s-*$" 1))
  "Value for `imenu-generic-expression' in ncl mode.")


;;; ncl-helpers.el ends here

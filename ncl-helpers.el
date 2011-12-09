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
  "Default tabbing/carriage control style for empty files in NCL mode.
A non-nil value specifies tab-digit style of continuation control.
A value of nil specifies that continuation lines are marked
with a character in column 6."
  :type  'boolean
  :safe  'booleanp
  :group 'ncl-indent)

(defcustom ncl-continuation-string \\\\
  "Single-character string used for NCL continuation lines.
In fixed format continuation style, this character is inserted in
column 6 by \\[ncl-split-line] to begin a continuation line.
Also, if \\[ncl-indent-line] finds this at the beginning of a
line, it will convert the line into a continuation line of the
appropriate style.  Normally \\."
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
(easy-menu-define ncl-menu ncl-mode-map "Menu for NCL mode."
  `("NCL"
    ("Customization"
     ,(custom-menu-create 'ncl))

    "--"
    ["Comment Region" ncl-comment-region mark-active]
    ["Uncomment Region"
     (ncl-comment-region (region-beginning) (region-end) 1)
     mark-active]
    ["Indent Region"     indent-region mark-active]
    "--"

    ["Narrow to Subprogram" narrow-to-defun t]
    ["Widen" widen t]
    "--"
    ["Fill Statement/Comment" fill-paragraph t]

    "--"
    ["Toggle Auto Fill" auto-fill-mode :selected auto-fill-function
     :style toggle
     :help "Automatically fill text while typing in this buffer"]

    ["Toggle Abbrev Mode" abbrev-mode :selected abbrev-mode
     :style toggle :help "Expand abbreviations while typing in this buffer"]

    ["Add Imenu Menu" imenu-add-menubar-index
     :active   (not (lookup-key (current-local-map) [menu-bar index]))
     :included (fboundp 'imenu-add-to-menubar)
     :help "Add an index menu to the menu-bar"]))

;;; ncl-helpers.el ends here

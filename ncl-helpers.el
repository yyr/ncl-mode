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

;;=================================================================
;; Define major mode
;;=================================================================

;;; ncl-helpers.el ends here

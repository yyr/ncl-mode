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
(defvar ncl-doc-mode-hook nil
  "hook runs after enabling the ncl-doc-mode")
(defvar ncl-doc-return-window-config nil
  "previous window config")

(defvar ncl-doc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1]  'ncl-doc-lookup-and-leave)
    (define-key map [mouse-2]  'ncl-doc-lookup)
    (define-key map "\C-m"     'ncl-doc-lookup-and-leave)
    (define-key map " "        'ncl-doc-lookup-and-leave)
    (define-key map "f"        'ncl-doc-lookup)
    (define-key map "q"        'ncl-doc-quit-window)
    (define-key map "n"        'ncl-doc-next-line)
    (define-key map "p"        'ncl-doc-prev-line)

    (define-key map "/"        'isearch-forward)
    (define-key map "l"        'recenter)
    (define-key map "<"        'beginning-of-buffer)
    (define-key map ">"        'end-of-buffer)
    (define-key map "v"        'scroll-down)
    map)
  "Keymap for `ncl-doc-mode-mode'.")

(define-derived-mode ncl-doc-mode fundamental-mode "ncl-doc"
  "major mode to help read NCL docs from UCAR website"
  (kill-all-local-variables)
  (use-local-map ncl-doc-mode-map)
  (setq buffer-read-only t)
  (run-mode-hooks))

(defun ncl-doc-move-prev-line ()
  "Move to previous entry"
  (interactive)
  (when (< 3 (line-number-at-pos))
    (call-interactively 'previous-line)))

(defun ncl-doc-next-line ()
  "Move to next entry"
  (interactive)
  (when (< (line-number-at-pos)
           (- (line-number-at-pos (point-max)) 1))
    (call-interactively 'next-line)))

(defun ncl-doc-lookup ()
  "Lookup the current line in a browser."
  (interactive)

  (let ((url (get-text-property (point) 'ncl-doc-target-url)))
    (if url
        (progn
          (beginning-of-line)
          (message "Browsing: \"%s\"" url)
          (browse-url url))
      (error "No URL on this line"))))

(defun ncl-doc-quit-window ()
  "Leave the completions window."
  (interactive)
  (set-window-configuration ncl-doc-return-window-config))

;;; ncl-helpers.el ends here

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
;;

;;; Code:
;;

(require 'ncl-mode-keywords)

(defgroup ncl nil
  "major mode to edit Ncar Command Line(NCL) language "
  :prefix "ncl-"
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
  "*The ncl shell program location."
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
    (define-key map (kbd "C-M-a") 'ncl-beginning-of-defun)
    (define-key map (kbd "C-M-e") 'ncl-end-of-defun)
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
    ,(list "variables" ncl-var-re 1))
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
  (set (make-local-variable 'beginning-of-defun-function)
       'ncl-beginning-of-subprogram)
  (set (make-local-variable 'end-of-defun-function) 'ncl-end-of-subprogram))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.ncl\\'") 'ncl-mode))

(provide 'ncl-mode)
;;; ncl-mode.el ends here

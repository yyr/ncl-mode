;;; inf-ncl.el --- Inferior NCL mode

;; Copyright (C) 2011-2012 Yagnesh Raghava Yakkala <http://yagnesh.org>

;; Author: Yagnesh Raghava Yakkala <hi@yagnesh.org>
;; URL: https://github.com/yyr/ncl.el
;; Maintainer: Yagnesh Raghava Yakkala <hi@yagnesh.org>
;; Created: 2012-06-21
;; Keywords: run, ncl, inferior, shell

;; This file is NOT part of GNU Emacs.

;; ncl-doc.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ncl-doc.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with python.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  Inferior NCL mode to run NCL interactively with in the
;;  Emacs.  built above comint.el.  (see comint.el for more)

;; Put the following in the .emacs file
;;   (require 'inf-ncl)
;; Then call `M-x inf-ncl'

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'ncl) ;  in fact variables are defined there
(require 'comint)

(defgroup inf-ncl nil
  "*Run NCL with in Emacs buffer"
  :group 'processes
  :group 'ncl)

;;=================================================================
;; user options
;;=================================================================
(defcustom inf-ncl-first-prompt-pattern  "ncl 0> *"
  "*Regexp to match first  in shell."
  :type 'regexp
  :group 'inf-ncl)

(defcustom inf-ncl-prompt-pattern-regexp "^\\(ncl [0-9]+> *\\)+"
  "*Regexp to match single command."
  :type 'regexp
  :group 'inf-ncl)

(defcustom inf-ncl-mode-hook nil
  "*Hook for customising inf-ncl mode."
  :group 'inf-ncl)

;;=================================================================
;; internal variables
;;=================================================================
(defvar inf-ncl-ncl "ncl")

(defconst inf-ncl-error-regexp-alist
  '())

(defvar inf-ncl-buffer nil "*The current ncl process buffer.")

(defvar inf-ncl-mode-map nil
  "*Mode map for inf-ncl-mode")

(cond ((not inf-ncl-mode-map)
       (setq inf-ncl-mode-map
             (copy-keymap comint-mode-map))
       (define-key inf-ncl-mode-map "\C-c\C-l" 'ncl-load-file)))

;; keys for communication from ncl mode buffer
(define-key ncl-mode-map "\C-x\C-e" 'inf-ncl-send-line)
(define-key ncl-mode-map "\C-c\C-r" 'inf-ncl-send-region)
(define-key ncl-mode-map "\C-c\M-r" 'inf-ncl-send-region-and-go)
(define-key ncl-mode-map "\C-c\C-l" 'inf-ncl-load-file)
(define-key ncl-mode-map "\C-c\C-y" 'inf-ncl-switch-to-ncl)
(define-key ncl-mode-map "\C-c\C-z" 'inf-ncl-switch-to-end-ncl)
(define-key ncl-mode-map "\C-c\C-n" 'run-ncl)

(defvar inf-ncl-font-lock-keywords)

(define-derived-mode inf-ncl-mode comint-mode "Inferior NCL mode"
  "Major mode for interacting with an inferior NCL interpreter.

The following commands are available:
\\{inf-ncl-mode-map}

One major advantage of using inferior mode is to be able to send
pieces of code from other buffers to the interpreter. In this
case from ncl mode buffer to background running ncl process.

For that, The following functions are available for you.
 `run-ncl'
 `inf-ncl-load-file'

`inf-ncl-switch-to-ncl'
`inf-ncl-switch-to-end-ncl'

`inf-ncl-send-line'
`inf-ncl-send-region'
`inf-ncl-send-region-and-go'

Customizations: Edit
   `comint-mode-hook'  => before entering into inf-ncl-mode (for ex: custom key commands)
   `inf-ncl-mode-hook'  => after entering into inf-ncl-mode. (for ex: activation minor modes)
"
  (setq comint-prompt-regexp inf-ncl-prompt-pattern-regexp))


(defun inf-ncl-input-filter (str)
  "Don't save anything matching `inf-ncl-input-filter-regexp'."
  (not (string-match inf-ncl-input-filter-regexp str)))

(defalias 'run-ncl 'inf-ncl)

;;=================================================================
;; internal variables
;;=================================================================

;;;###autoload
(defun inf-ncl (cmd)
  "Run inferior ncl process. with prefix CMD arg choose "
  (interactive (list (if current-prefix-arg
                         (read-string "Run ncl:" ncl-shell-program)
                       ncl-shell-program)))
  (if (not (comint-check-proc "*ncl*"))
      (progn
        (set-buffer (apply 'make-comint "ncl" cmd nil))
        (inf-ncl-mode)))
  (setq ncl-shell-program cmd)
  (setq ncl-buffer "*ncl*")
  (pop-to-buffer "*ncl*"))


(provide 'inf-ncl)
;;; inf-ncl.el ends here

;;; inf-ncl.el --- Inferior NCL mode

;; Copyright (C) 2012, 2013 Yagnesh Raghava Yakkala <http://yagnesh.org>

;; Author: Yagnesh Raghava Yakkala <hi@yagnesh.org>
;; URL: https://github.com/yyr/ncl-mode
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
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  Inferior NCL mode to run NCL interactively with in the
;;  Emacs.  built above comint.el.  (see comint.el for more)

;; Why would anyone wants to run ncl within emacs.?

;; Because Its easy to type in Emacs. (hey we have even auto-completion support)
;; You can send pieces of code from your script to interpretor on fly.

;; Put the following in the .emacs file
;;   (require 'inf-ncl)
;; Then call `M-x run-ncl'

;;; Code:

(require 'ncl-mode)
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

(defvar inf-ncl-buffer nil
  "*The current ncl process buffer.")

(defvar inf-ncl-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map "\C-c\C-l" 'ncl-load-file)
    map)
  "*Mode map for inf-ncl-mode.")

(defvar inf-ncl-font-lock-keywords)

(defconst inf-ncl-error-regexp-alist
  '())

(defvar inf-ncl-ncarg-root (if (getenv "NCARG_ROOT")
                               (getenv "NCARG_ROOT")
                             (progn
                               (message "ncl-mode: Warning, ncarg_root is not set. falling back to home directory")
                               (getenv "HOME")))
  "NCARG_ROOT from envronemt")

(defvar inf-ncl-lib-root
  (concat (directory-file-name inf-ncl-ncarg-root) "/lib/ncarg/nclscripts/")
  "Ncl builtin library root")

(defvar inf-ncl-prev-dir/file nil
  "Catches the last loaded library or file.")

;;=================================================================
;; Define major mode
;;=================================================================

(define-derived-mode inf-ncl-mode comint-mode "*Inf NCL*"
  "Major mode for interacting with an inferior NCL interpreter.

One major advantage of using inferior mode is to be able to send
pieces of code from other buffers to the interpreter. In this
case from ncl mode buffer to background running ncl process.

For that, The following functions are available for you.
`run-ncl'
`inf-ncl-load-file'
`inf-ncl-switch-to-ncl'
`inf-ncl-send-line'
`inf-ncl-send-region'
`inf-ncl-send-region-and-go'

Customizations: Edit
   `comint-mode-hook'  => before entering into inf-ncl-mode (for ex: custom key commands)
   `inf-ncl-mode-hook'  => after entering into inf-ncl-mode. (for ex: activation minor modes)

The following commands are available:
\\{inf-ncl-mode-map}
"
  (setq comint-prompt-regexp inf-ncl-prompt-pattern-regexp))

;;=================================================================
;; Functions
;;=================================================================

(defun inf-ncl-keys ()
  "Key for inferior mode interaction from ncl buffer."
  ;; keys for communication from ncl mode buffer
  (define-key ncl-mode-map "\C-x\C-e" 'inf-ncl-send-line)
  (define-key ncl-mode-map "\C-c\C-r" 'inf-ncl-send-region)
  (define-key ncl-mode-map "\C-c\M-r" 'inf-ncl-send-region-and-go)
  (define-key ncl-mode-map "\C-c\C-l" 'inf-ncl-load-file)
  (define-key ncl-mode-map "\C-c\C-y" 'inf-ncl-switch-to-ncl)
  (define-key ncl-mode-map "\C-c\C-n" 'run-ncl))

(defun inf-ncl-proc ()
  "Returns current inferior process."
  (or (get-buffer-process (if (eq major-mode 'inf-ncl-mode)
                              (current-buffer)
                            inf-ncl-buffer))
      (error "No ncl process is running. Start one \"M-x run-ncl\" ")))

;; FIXME seems not working (bug in comint-mode?)
;;;###autoload
(defun inf-ncl-load-file (fn)
  "Load ncl library into inferior process."
  (interactive (comint-source-default inf-ncl-prev-dir/file 'ncl-mode))
  (comint-check-source fn)              ;checks if the buffer needs to be saved
  (setq inf-ncl-prev-dir/file (cons (file-name-directory fn)
                                    (file-name-nondirectory fn)))
  (comint-send-string (inf-ncl-proc) (concat "load \""
                                             fn
                                             "\"\n")))

;;;###autoload
(defun inf-ncl-switch-to-ncl (eob-p)
  "Switch to inferior process. with EOB-P go to the end of the buffer."
  (interactive "P")
  (if (get-buffer inf-ncl-buffer)
      (pop-to-buffer inf-ncl-buffer)
    (error "No inferior process is running."))
  (cond (eob-p
         (push-mark)
         (goto-char (point-max)))))

;;;###autoload
(defun inf-ncl-send-line (switch)
  "Send a line to inferior process. With prefix SWITCH switches
  to the process buffer"
  (interactive "P")
  (let ((ln (buffer-substring-no-properties
             (line-beginning-position) (line-end-position))))
    (comint-send-string (inf-ncl-proc) (concat ln "\n")))
  (if switch
      (inf-ncl-switch-to-ncl t)))

;;;###autoload
(defun inf-ncl-send-region (start end)
  "Send a region to inferior ncl process."
  (interactive "r")
  (comint-send-region (inf-ncl-proc) start end))

(defun inf-ncl-send-region-and-go (start end)
  "Send a region to inferior ncl process and switch buffer."
  (comint-send-region (inf-ncl-proc) start end)
  (inf-ncl-switch-to-ncl t))

;;;###autoload
(defun inf-ncl (arg)
  "Run inferior ncl process.  With prefix ARG interactively
choose the interpreter"
  (interactive (list (if current-prefix-arg
                         (read-string "Run ncl: " ncl-shell-program)
                       ncl-shell-program)))
  (if (not (comint-check-proc inf-ncl-buffer))
      (progn
        (set-buffer (apply 'make-comint "ncl" arg nil))
        (inf-ncl-mode)))
  (setq ncl-shell-program arg)
  (pop-to-buffer (setq inf-ncl-buffer (format "*%s*" "ncl"))))

(defalias 'run-ncl 'inf-ncl)

;;;###autoload
(eval-after-load 'ncl-mode
  ;; load the keys for ncl
  '(add-hook 'ncl-mode-hook 'inf-ncl-keys))

;;;###autoload
(eval-after-load 'auto-complete
  '(progn
     (require 'find-func)
     (defun ac-ncl-mode-setup ()
       (setq ac-sources
             (append '(ac-source-yasnippet
                       ac-source-words-in-buffer) ac-sources)))
     (add-hook 'ncl-mode-hook 'ac-ncl-mode-setup)
     (add-to-list 'ac-modes 'ncl-mode)
     (add-to-list 'ac-dictionary-directories
                  (concat (file-name-as-directory
                           (file-name-directory
                            (find-library-name "ncl-mode")))
                          "dict/"))))

(provide 'inf-ncl)
;;; inf-ncl.el ends here

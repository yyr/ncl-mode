;;; ncl-mode-load.el --- loading script for ncl-mode (and other things)

;; Copyright (C) 2012 Yagnesh Raghava Yakkala <http://yagnesh.org>

;; Author: Yagnesh Raghava Yakkala <hi@yagnesh.org>
;; URL: https://github.com/yyr/ncl-mode
;; Maintainer: Yagnesh Raghava Yakkala <hi@yagnesh.org>
;; Created: Sunday, June 24 2012
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

;; User initialization file.

;; To load all this the stuff in this package simply add the following
;; line in your .emacs file
;; (load "path/to/this/file/emacs-ncl-load.el")
;; That's it.

;;; Code:

;; find this file path and it to load-path
(defconst ncl-mode-dir (file-name-directory
                        (or load-file-name buffer-file-name)))
(add-to-list 'load-path ncl-mode-dir)


;;; General autoload ncl mode
;;=================================================================
(autoload 'ncl-mode "ncl" "ncl-mode for editing ncar graphics" t)
(setq auto-mode-alist (cons '("\.ncl$" . ncl-mode) auto-mode-alist))
(setq ncl-startup-message nil)

;;; load inf-ncl, ncl-doc
(require 'ncl-doc)
(require 'inf-ncl)

(add-hook 'ncl-mode-hook (lambda () (ncl-doc-minor-mode 1)))
(add-hook 'ncl-mode-hook 'inf-ncl-keys)

;; Auto Complete setup
;;=================================================================
;;; if user has auto-complete installed then set it up for ncl as well
;;; ncl-mode
(defun ac-ncl-mode-setup ()
  (setq ac-sources
        (append '(ac-source-yasnippet
                  ac-source-words-in-buffer)
                ac-sources)))

(when (require 'auto-complete nil t)
  (add-to-list 'ac-modes 'ncl-mode)
  ;; add dictionary
  (add-to-list 'ac-dictionary-directories
               (concat ncl-mode-dir "dict/"))
  (add-hook 'ncl-mode-hook 'ac-ncl-mode-setup))

;; Yasnippet setup
;;=================================================================
(when (require 'yasnippet nil t)
  (add-to-list 'yas/snippet-dirs (concat ncl-mode-dir "snippets/")))

;;; ncl-mode-load.el ends here

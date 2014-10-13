;;; ncl-mode-load.el --- loading script for ncl-mode (and other things)

;; Copyright (C) 2012, 2013 Yagnesh Raghava Yakkala <http://yagnesh.org>

;; Author: Yagnesh Raghava Yakkala <hi@yagnesh.org>
;; URL: https://github.com/yyr/ncl-mode
;; Maintainer: Yagnesh Raghava Yakkala <hi@yagnesh.org>
;; Created: Sunday, June 24 2012
;; Keywords: ncl

;; This file is NOT part of GNU Emacs.

;; ncl-mode-load.el is free software: you can redistribute it and/or modify
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

(add-to-list 'load-path (file-truename
                         (concat ncl-mode-dir "lisp")))


;;; General autoload ncl mode
;;=================================================================
(add-to-list 'auto-mode-alist (cons (purecopy "\\.ncl\\'") 'ncl-mode))
(autoload 'ncl-mode "ncl-mode.el" "ncl-mode for editing ncar graphics" t)

;;; load inf-ncl, ncl-doc
(require 'ncl-doc)
(require 'inf-ncl)

(dolist (hook '(ncl-mode-hook
                inf-ncl-mode-hook))
  (add-hook hook 'ncl-doc-minor-mode))
(add-hook 'ncl-mode-hook 'inf-ncl-keys)

;; Auto Complete setup
;;=================================================================
;;; if user has auto-complete installed then set it up for both
;;; ncl-mode and inf-ncl-mode
(defun ac-ncl-mode-setup ()
  (setq ac-sources
        (append '(ac-source-yasnippet
                  ac-source-words-in-buffer)
                ac-sources)))

(mapc (lambda (x)
        (when (require 'auto-complete nil t)
          (add-to-list 'ac-modes x)
          (add-hook (intern (format "%s-hook" x)) 'ac-ncl-mode-setup)
          (add-to-list 'ac-dictionary-directories
                       (concat ncl-mode-dir "dict/"))))
      '(ncl-mode inf-ncl-mode))

;; Yasnippet setup
;;=================================================================
(when (require 'yasnippet nil t)
  (add-to-list 'yas-snippet-dirs (concat ncl-mode-dir "snippets/")))

(provide 'ncl-mode-load)
;;; ncl-mode-load.el ends here

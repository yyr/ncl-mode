;;; company-ncl-mode.el --- company-mode completion backend for ncl-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2012-2017 Yagnesh Raghava Yakkala <http://yagnesh.org>

;; Author: Yagnesh Raghava Yakkala. http://yagnesh.org
;; Created: Thursday, September 22 2016

;; This file is part of ncl-mode.

;; ncl-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ncl-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'company)
(require 'cl-lib)
(require 'ncl-mode-keywords)

(defcustom company-ncl-enable-fuzzy nil nil)
(setq ncl-all-keywords (delete-dups (append ncl-key-builtin ncl-key-contrib
                                            ncl-key-diag ncl-key-gsn
                                            ncl-key-keywords ncl-key-operators
                                            ncl-key-pop ncl-key-resources
                                            ncl-key-shea ncl-key-skewt
                                            ncl-key-user ncl-key-windrose
                                            ncl-key-wrfarw)))

(defun company-ncl-fuzzy-match (prefix candidate)
  (if company-ncl-enable-fuzzy
      (cl-subsetp (string-to-list prefix)
                  (string-to-list candidate))
    (string-prefix-p prefix candidate)))

;;;###autoload
(defun company-ncl-mode (command &optional arg &rest ignored)
  "`company-mode' completion backend for `ncl-mode'."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-ncl-mode))
    (prefix (and (member major-mode '(ncl-mode inf-ncl-mode))
                 (company-grab-symbol)))
    (candidates (cl-remove-if-not
                 (lambda (c) (company-ncl-fuzzy-match arg c))
                 ncl-all-keywords))
    (sorted t)))

;;;###autoload
(add-hook 'ncl-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '(company-ncl-mode))))

;;;###autoload
(add-hook 'inf-ncl-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '(company-ncl-mode))))


(provide 'company-ncl-mode)
;;; company-ncl-mode.el ends here

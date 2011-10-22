;;; ncl-doc.el
;;
;; Copyright (C) Yagnesh Raghava Yakkala. http://yagnesh.org
;;    File: ncl-doc.el
;;  Author: Yagnesh Raghava Yakkala <yagnesh@NOSPAM.live.com>
;; Created: Saturday, September 24 2011
;; License: GPL v3 or later. <http://www.gnu.org/licenses/gpl.html>

;;; Description:
;; this is a minor moder to look
;; ncl-doc-thing-at-point => collects the appropriate thing at point
;; ncl-doc-classify => should look where to look
;; ncl-doc-fetch => bring the doc from server (local?)
;; ncl-doc-crop-header => should crop the headers
;; ncl-doc-render => should render html doc

;;; code starts here

;;; requires
(require 'ncl)                          ; to bring all the
                                        ; function/resources names
;; Variables
;; FIXME
(defcustom ncl-doc-baseurl
  "http://www.ncl.ucar.edu/Document/"
  "Ncl documentation website base url. To construct URLs individual pages"
  :group 'ncl-doc
  :type 'string)

(defcustom ncl-doc-cache-dir
  "~/.ncl-doc/"
  "Directory to store the cache"
  :group 'ncl-doc
  :type 'string)

(define-minor-mode ncl-doc
  "Minor mode to help to read on line documentation of ncl
  functions and resources" nil )

(defvar ncl-doc-mode-hook nil
  "hook runs after enabling the ncl-doc-mode")

(defvar ncl-doc-mode-map nil
  "key bindings")

;;; functions
(defun ncl-doc-cache-dir-create ()
  "creates cache dir"
  (interactive)
  (unless (file-directory-p ncl-doc-cache-dir)
    (make-directory ncl-doc-cache-dir)))

(defun ncl-doc-thing-at-point ()
  "collect the thing at point tell if its a resource"
  (interactive)
  (let ((tap (thing-at-point 'symbol)))
    (message tap)))

;;; construction of doc by removing header

(provide 'ncl-doc)
;;; ncl-doc.el ends here

;;; ncl-doc.el
;;
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
;;=================================================================
;; user options
;;=================================================================

(defcustom ncl-doc-url-base
  "http://www.ncl.ucar.edu/Document"
  "Ncl documentation website base url. To construct URLs individual pages"
  :group 'ncl-doc
  :type 'string)

(defcustom ncl-doc-cache-dir
  "~/.ncl-doc/"
  "Directory to store the cache"
  :group 'ncl-doc
  :type 'string)


;;=================================================================
;; internal variables
;;=================================================================

(defvar ncl-doc-url-builtin-base
  (concat ncl-doc-url-base "/" "Functions/Built-in")
  "for eg: `ncl-doc-url-base/`Functions/Built-in/dpres_hybrid_ccm.shtml")

(defvar ncl-doc-url-suffix
  ".shtml"
  "suffix of URLs. haven't checked if its same for all")

(defvar ncl-doc-mode-map nil
  "key bindings")

(define-minor-mode ncl-doc
  "Minor mode to help to read on line documentation of ncl
  functions and resources" nil
  :group 'ncl-doc
  :init-value nil
  :keymap ncl-doc-mode-map)

(defvar ncl-doc-mode-hook nil
  "hook runs after enabling the ncl-doc-mode")

;;; keywords
(defvar ncl-test-builtins
  [angmom_atm test dpres_hybrid_ccm]
  "just for test must derive from the ncl.el")

;;; functions
(defun ncl-doc-cache-dir-create ()
  "creates cache dir"
  (interactive)
  (unless (file-directory-p ncl-doc-cache-dir)
    (make-directory ncl-doc-cache-dir)))

(defun ncl-doc-construct-url-for-builtin (KWORD)
  "construct url for ncl built in function. `ncl-doc-builtin-function-base`
is the base url"
  (let ((kwd KWORD))
    (message  (concat
               ncl-doc-url-builtin-base  "/"  (format "%s" `,KWORD)
               ncl-doc-url-suffix))))

;;; construction of doc by removing header
(defun ncl-doc-construct-url (KWORD)
  "construct a url from the KWORD"
  (interactive "SNCL kwd: ")
  (let ((kwd KWORD))
    (if (find kwd ncl-test-builtins :test 'string=)
        (print (stringp kwd))
      (message "Not Found"))))

(defun ncl-doc-thing-at-point ()
  "collect the thing at point tell if its a resource"
  (interactive)
  (let ((tap (thing-at-point 'symbol)))
    (message tap)))


(provide 'ncl-doc)
;;; ncl-doc.el ends here

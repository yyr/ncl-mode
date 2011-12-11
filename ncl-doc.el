;;; ncl-doc.el
;;
;;    File: ncl-doc.el
;;  Author: Yagnesh Raghava Yakkala <yagnesh@NOSPAM.live.com>
;; Created: Saturday, September 24 2011
;; License: GPL v3 or later. <http://www.gnu.org/licenses/gpl.html>

;;; Commentary:
;;

;;=================================================================
;;; code starts here
(require 'ncl)
(eval-when-compile
  (require 'cl))

;;=================================================================
;; user options
;;=================================================================
(defcustom ncl-doc-url-base
  "http://www.ncl.ucar.edu"
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

(defvar ncl-doc-url-alist
  ;;  ("keywords" . "Document/Manuals/Ref_Manual/")
  '(("builtin" . "/Document/Functions/Built-in/")
    ("contrib" .  "/Document/Functions/Contributed/")
    ("diag" .  "/Document/Functions/Diagnostics/")
    ("pop" .  "/Document/Functions/Pop_remap/")
    ("shea" .  "/Document/Functions/Shea_util/")
    ("skewt" .  "/Document/Functions/Skewt_func/")
    ("user" .  "/Document/Functions/User_contributed/")
    ("wrfarw" .  "/Document/Functions/WRF_arw/")
    ("wrfcontrib" .  "/Document/Functions/WRF_contributed/")
    ("windrose" .  "/Document/Functions/Wind_rose/")
    ("gsn" .  "/Document/Graphics/Interfaces/"))
  "url alist for different categories")

(defvar ncl-doc-mode-hook nil
  "hook runs after enabling the ncl-doc-mode")

;;; functions
(defun ncl-doc-cache-dir-create ()
  "creates cache dir"
  (interactive)
  (unless (file-directory-p ncl-doc-cache-dir)
    (make-directory ncl-doc-cache-dir)))

;;; construction of doc by removing header
(defun ncl-doc-construct-url (KWORD)
  "construct a url from the KWORD"
  (interactive "SNCL kwd: ")
  (let ((kwd KWORD))
    (cond                               ; FIXME simplify mapcar?
     ((find (format "%s" kwd) ncl-key-builtin :test 'string=)
      (format "%s%s%s%s" ncl-doc-url-base (cdr (assoc "builtin" ncl-doc-url-alist))
              kwd ncl-doc-url-suffix))

     ((find (format "%s" kwd) ncl-key-diag :test 'string=)
      (format "%s%s%s%s" ncl-doc-url-base (cdr (assoc "diag" ncl-doc-url-alist))
              kwd ncl-doc-url-suffix))

     ((find (format "%s" kwd) ncl-key-pop :test 'string=)
      (format "%s%s%s%s" ncl-doc-url-base (cdr (assoc "pop" ncl-doc-url-alist))
              kwd ncl-doc-url-suffix))

     ((find (format "%s" kwd) ncl-key-shea :test 'string=)
      (format "%s%s%s%s" ncl-doc-url-base (cdr (assoc "shea" ncl-doc-url-alist))
              kwd ncl-doc-url-suffix))

     ((find (format "%s" kwd) ncl-key-skewt :test 'string=)
      (format "%s%s%s%s" ncl-doc-url-base (cdr (assoc "skewt" ncl-doc-url-alist))
              kwd ncl-doc-url-suffix))

     ((find (format "%s" kwd) ncl-key-user :test 'string=)
      (format "%s%s%s%s" ncl-doc-url-base (cdr (assoc "user" ncl-doc-url-alist))
              kwd ncl-doc-url-suffix))

     ((find (format "%s" kwd) ncl-key-wrfarw :test 'string=)
      (format "%s%s%s%s" ncl-doc-url-base (cdr (assoc "wrfarw" ncl-doc-url-alist))
              kwd ncl-doc-url-suffix))

     ((find (format "%s" kwd) ncl-key-wrfcontrib :test 'string=)
      (format "%s%s%s%s" ncl-doc-url-base (cdr (assoc "wrfarw" ncl-doc-url-alist))
              kwd ncl-doc-url-suffix))

     ((find (format "%s" kwd) ncl-key-windrose :test 'string=)
      (format "%s%s%s%s" ncl-doc-url-base (cdr (assoc "windrose" ncl-doc-url-alist))
              kwd ncl-doc-url-suffix))

     ((find (format "%s" kwd) ncl-key-gsn :test 'string=)
      (format "%s%s%s%s" ncl-doc-url-base (cdr (assoc "gsn" ncl-doc-url-alist))
              kwd ncl-doc-url-suffix))

     ((find (format "%s" kwd) ncl-key-contrib :test 'string=)
      (format "%s%s%s%s" ncl-doc-url-base (cdr (assoc "contrib" ncl-doc-url-alist))
              kwd ncl-doc-url-suffix))
     (t
      nil))))


;;=================================================================
;; user fictions
;;=================================================================
;;;###autoload
(defun ncl-doc-keyword-open-in-browser ()
  "asks for keyword and calls the opens in browser"
  (interactive)
  (let* ((default-word (thing-at-point 'symbol))
         (default-prompt
           (concat "Keyword: "
                   (if default-word
                       (concat "[" default-word "]") nil) ": "))
         (default-query
           (funcall #'(lambda (str)
                        "Remove Whitespace from beginning and end of a string."
                        (replace-regexp-in-string "^[ \n\t]*\\(.*?\\)[ \n\t]*$"
                                                  "\\1"
                                                  str))
                    (read-string default-prompt nil nil default-word))))
    (let ((url (ncl-doc-construct-url default-query)))
      (if url
          (browse-url-default-browser url)
        (message "could not find \" %s\" keyword :(" default-query)))))


(provide 'ncl-doc)
;;; ncl-doc.el ends here

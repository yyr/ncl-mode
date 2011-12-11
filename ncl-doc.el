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

(defvar ncl-doc-resources-page
  "list_alpha_res.shtml"
  "suffix of URLs. haven't checked if its same for all")

(defvar ncl-doc-url-alist
  ;;
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
    ("gsn" .  "/Document/Graphics/Interfaces/")
    ("resources" . "/Document/Graphics/Resources/")
    ("keywords" . "Document/Manuals/Ref_Manual/"))
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

     ((find (format "%s" kwd) ncl-resources :test 'string=)
      (format "%s%s%s#%s" ncl-doc-url-base (cdr (assoc "resources" ncl-doc-url-alist))
              ncl-doc-resources-page kwd))

     ((find (format "%s" kwd) ncl-keywords :test 'string=)
      "keyword")

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
           (concat "NCL Keyword "
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
      (if (string= url "keyword")       ; if its a ncl keyword its in the ref manual
          (message "\"%s\" is a NCL builtin keyword and has no specific page to look at
Consult User Manual Here: http://www.ncl.ucar.edu/Document/Manuals/Ref_Manual/"
                   default-query)
        (if url
            (browse-url-default-browser url)
          (message "could not find \"%s\" keyword in ncl-doc database :("
                   default-query))))))

;;=================================================================
;; Define mode
;;=================================================================
(defvar ncl-doc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s")
      'ncl-doc-keyword-open-in-browser)
    map)
  "key bindings ncl-doc-minor-mode")

;;;###autoload
(define-minor-mode ncl-doc-minor-mode
  "Minor mode to help to read on line documentation of ncl
  functions and resources"
  nil " ncl-doc"
  :set 'custom-set-minor-mode
  :initialize 'custom-initialize-default
  :type    'boolean
  :keymap ncl-doc-mode-map
  :group 'ncl-doc
  :require 'ncl)

(provide 'ncl-doc)
;;; ncl-doc.el ends here

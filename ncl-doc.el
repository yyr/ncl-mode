;;; ncl-doc.el
;;
;;    File: ncl-doc.el
;;  Author: Yagnesh Raghava Yakkala <yagnesh@NOSPAM.live.com>
;; Created: Saturday, September 24 2011
;; License: GPL v3 or later. <http://www.gnu.org/licenses/gpl.html>

;;; Commentary:
;; ncl-doc minor mode helps you read NCL documentation from UCAR website
;;
;;
;;
;; acknowledgment: parts in the major mode section based on pylookup.el

;;=================================================================
;;; code starts here
(require 'ncl) ;  in fact variables are defined there
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
    ("keywords" . "Document/Manuals/Ref_Manual/")
    ("contrib" .  "/Document/Functions/Contributed/"); FIXME search matches with contrib list
    )  "url alist for different categories")

;;;
(defvar ncl-doc-key-cat
  (mapcar (lambda (x)
            (car x))
          ncl-doc-url-alist)
  "Categories in the ncl. this variable is prepared from
`ncl-doc-url-alist'")

(defvar ncl-doc-key-all
  (apply 'append (mapcar (lambda (x)
                           (symbol-value (intern (format "ncl-key-%s" x))))
                         ncl-doc-key-cat))
  "all keys in ncl")

;;=================================================================
;; Internal functions
;;=================================================================
(defun ncl-doc-cache-dir-create ()
  "creates cache dir"
  (interactive)
  (unless (file-directory-p ncl-doc-cache-dir)
    (make-directory ncl-doc-cache-dir)))

;;; search functions
(defun ncl-doc-keys-search-cat (CAT TERM)
  "searches for TERM in CAT"
  (let ((cat-keys CAT))
    (remove nil
            (mapcar (lambda (x)
                      (if (string-match TERM x)
                          x))
                    cat-keys))))

(defun ncl-doc-keys-search (TERM)
  "takes search TERM returns alist with matches; alist is category and matched
keywords"
  (let* ((st TERM)
         (cats  (mapcar (lambda (x)
                          (car x))
                        ncl-doc-url-alist))
         (cats (mapcar (lambda (x)           ;make list of keyword vars
                         (concat "ncl-key-" x))
                       cats)))
    (mapcar (lambda (cat)
              (let ((klst (symbol-value (intern cat))))
                (ncl-doc-keys-search-cat klst TERM)))
            cats)))

;;; URL construction function
(defun ncl-doc-construct-url (KWORD)
  "construct a url from the KWORD"
  (let ((kwd KWORD)
        ;; cats: all categories in the ncl-doc-url-alist
        (cats (mapcar (lambda (cat)
                        (car cat))
                      ncl-doc-url-alist)))
    (catch 'break
      (while (cdr cats)                 ; loop on all categories
        (setq ct (car cats))
        (setq cats `,(cdr cats))
        (if (member (format "%s" kwd)
                    (symbol-value (intern (concat "ncl-key-" ct))))
            (throw 'break
                   (if (string= ct "resources")
                       (format "%s%s%s#%s"
                               ncl-doc-url-base (cdr (assoc ct ncl-doc-url-alist))
                               ncl-doc-resources-page kwd)
                     (if (string= ct "keywords")
                         "keyword"
                       (format "%s%s%s%s"
                               ncl-doc-url-base (cdr (assoc ct ncl-doc-url-alist))
                               kwd ncl-doc-url-suffix)))))))))

;;=================================================================
;; Define minor mode
;;=================================================================
(defvar ncl-doc-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-p") 'ncl-doc-query-at-point)
    (define-key map (kbd "C-c C-o") 'ncl-doc-query-open)
    map)
  "key bindings ncl-doc-minor-mode")

;;;###autoload
(define-minor-mode ncl-doc-minor-mode
  "Minor mode to help to read on line documentation of ncl
  functions and resources
see the functions `ncl-doc-query-open' and `ncl-doc-query-at-point'
"
  nil " ncl-doc"
  :set 'custom-set-minor-mode
  :initialize 'custom-initialize-default
  :type    'boolean
  :keymap ncl-doc-minor-mode-map
  :group 'ncl-doc
  :require 'ncl)

(defvar ncl-doc-completing-read
  (if (null ido-mode) 'completing-read 'ido-completing-read)
  "Ido support with convenience")

;;=================================================================
;; Major Mode stuff
;;=================================================================

(defvar ncl-doc-temp-buffer-name
  "*ncl doc buffer*")

;;=================================================================
;; user functions
;;=================================================================
;;;###autoload
(defun ncl-doc-query-at-point ()
  "asks for keyword while picking up one at point if available
and calls the browser if it matches any of ncl keywords
For completion support call `ncl-doc-query-open'"
  (interactive)
  (let* ((default-word (thing-at-point 'word))
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
                    (completing-read default-prompt ncl-doc-key-all))))

    (if (= 0 (string-width default-query)) ;protect if the user input is empty
        (set 'default-query default-word)) ;then it should be default word only

    (if (string= ""default-query)
        (message "default-query is nothing "))

    (let ((url (ncl-doc-construct-url default-query)))
      ;; if url is a keyword tell that there is no url for that
      (if (string= url "keyword")       ; if its a ncl keyword its in the ref manual
          (message "\"%s\" is a NCL builtin keyword and has no specific page to look at
Consult User Manual Here: http://www.ncl.ucar.edu/Document/Manuals/Ref_Manual/"
                   default-query)

        (if url
            ;; gets an url call the browser
            (progn
              (message "Browsing: \"%s\"" url)
              (browse-url url))

          ;; lets try if search bring anything
          (let ((matches (ncl-doc-keys-search default-query)))
            (cond

             ((eq (remove nil matches) nil)
              ;; no search results; leave with fun message
              (message
               ":( May NCL GOD will help you; NO search results for \"%s\" from all keywords"
               default-query))

             ;; if any matches call the major mode to display
             (t
              (let* ((cur-window-conf (current-window-configuration))
                     (tmpbuf (get-buffer-create ncl-doc-temp-buffer-name)))

                (display-buffer tmpbuf)
                (pop-to-buffer tmpbuf)

                (setq buffer-read-only nil)
                (erase-buffer)

                ;; Write what we are looking for
                (insert (format "NCL keyword matches for \"%s\":\n"
                                default-query))
                (insert "\n")

                ;; insert text
                (let ((idx 0)
                      (ln (list-length ncl-doc-key-cat)))
                  (while (> ln idx)
                    (let ((ct (nth idx ncl-doc-key-cat))
                          (mts (nth idx matches)))
                      (insert (format ";;Search matches in \"%s\":\n" ct))

                      ;; loop over matches
                      (mapc (lambda (x)
                              (insert (format "%s\n" x))

                              ;; put url as property
                              (put-text-property
                               (line-beginning-position) (line-end-position)
                               'ncl-doc-key-url (ncl-doc-construct-url x)))
                            mts))
                    (insert "\n")
                    (goto-line 3)
                    ;; Major mode stuff

                    (incf idx)
                    )))))))))))

;;;###autoload
(defun ncl-doc-query-open (KEY)
  "Query for a keyword from the database with completion support
and calls browser with corresponding URL"
  (interactive
   (list
    (let ((initial (thing-at-point 'word)))
      (funcall ncl-doc-completing-read
               "Query: " ncl-all-keys
               nil nil nil nil))))
  (let ((url (ncl-doc-construct-url KEY)))
    (progn
      (message "Browsing: \"%s\"" url)
      (browse-url url))))

(provide 'ncl-doc)
;;; ncl-doc.el ends here

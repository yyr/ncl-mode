;;; ncl-doc.el --- NCL doc lookup helper

;; Copyright (C) 2011-2018 Yagnesh Raghava Yakkala <http://yagnesh.org>

;; Author: Yagnesh Raghava Yakkala <hi@yagnesh.org>
;; URL: https://github.com/yyr/ncl-mode
;; Maintainer: Yagnesh Raghava Yakkala <hi@yagnesh.org>
;; Package-Requires: ((ncl-mode "0.99a"))
;; Created: Saturday, September 24 2011
;; Keywords: doc,lookup,ncl

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
;;
;; ncl-doc minor mode helps you read NCL documentation from UCAR website
;; ----------------------------------------------------------
;; Setup: put the following your .emacs/init.el
;; (require 'ncl-doc)
;; (add-hook 'ncl-mode-hook
;;   (lambda ()
;;     (ncl-doc-minor-mode 1)))
;; ----------------------------------------------------------
;; Usage:
;; M-x ncl-doc-query-at-point ( C-c C-s )
;;    This function does few things.
;;     1) It prompts for a string/keyword and collects a given string
;;     2) It tries to find a URL for the given string
;;         + if it finds URL for the string call the browser to open that URL
;;         + If no URL is found, then goes on to search for the given
;;           string.
;;         + Displays all search matches in separate buffer category wise.
;;           In that buffer RET in any search match will call browser for
;;           take you to that page.
;;         + If no matches found for a given string it leaves you there
;;
;; M-x ncl-doc-query-open  ( C-c C-o )
;;     Use this function you lets you choose from the all keywords ncl-doc
;;     has in its database and takes you to that page.
;;
;; ----------------------------------------------------------
;; Customization:
;; M-x customize-group RET ncl-doc RET
;; check out the following variables
;; `ncl-doc-url-base'  defaults to "http://www.ncl.ucar.edu"
;; `ncl-doc-minor-mode-hook'
;; README.org file has little more info
;; ----------------------------------------------------------
;; acknowledgment:
;; Parts of the major mode section is based on pylookup.el

;;; Code:
(require 'ncl-mode-keywords)

(eval-when-compile
  (require 'cl))

(defgroup ncl-doc nil
  "*Ncl documentation lookup helper"
  :group 'ncl
  :group 'docs)

;;=================================================================
;; user options
;;=================================================================
(defcustom ncl-doc-url-base
  "http://www.ncl.ucar.edu"
  "Ncl documentation website base url.  To construct URLs individual pages."
  :group 'ncl-doc
  :type 'string)

(defcustom ncl-doc-cache-dir            ; cache is not yet implemented
  "~/.ncl-doc/"
  "Directory to store the cache."
  :group 'ncl-doc
  :type 'string)

(defcustom ncl-doc-mode-hook nil        ; major mode hook
  "Hook runs after enabling the `ncl-doc-mode' major mode."
  :group 'ncl-doc)

(defcustom ncl-doc-minor-mode-hook nil
  "Hook runs after enabling the ncl-doc-minormode-hook."
  :group 'ncl-doc)

;;=================================================================
;; internal variables
;;=================================================================
(defvar ncl-doc-url-suffix
  ".shtml"
  "Suffix of URLs.  haven't checked if its same for all.")

(defvar ncl-doc-resources-page
  "list_alpha_res.shtml"
  "Suffix of URLs.  haven't checked if its same for all.")

(defvar ncl-doc-url-alist
  ;;
  '(("builtin"    . "/Document/Functions/Built-in/")
    ("contrib"    . "/Document/Functions/Contributed/")
    ("diag"       . "/Document/Functions/Diagnostics/")
    ("pop"        . "/Document/Functions/Pop_remap/")
    ("shea"       . "/Document/Functions/Shea_util/")
    ("skewt"      . "/Document/Functions/Skewt_func/")
    ("user"       . "/Document/Functions/User_contributed/")
    ("wrfarw"     . "/Document/Functions/WRF_arw/")
    ("wrfcontrib" . "/Document/Functions/WRF_contributed/")
    ("windrose"   . "/Document/Functions/Wind_rose/")
    ("gsn"        . "/Document/Graphics/Interfaces/")
    ("resources"  . "/Document/Graphics/Resources/")
    ("keywords"   . "Document/Manuals/Ref_Manual/")
    )  "Url alist for different categories.")

;;;
(defvar ncl-doc-key-cat
  (mapcar (lambda (x)
            (car x))
          ncl-doc-url-alist)
  "Categories in the ncl.  This variable is prepared from `ncl-doc-url-alist'.")

(defvar ncl-doc-key-all
  (apply 'append (mapcar (lambda (x)
                           (symbol-value (intern (format "ncl-key-%s" x))))
                         ncl-doc-key-cat))
  "All keys in ncl.")

;;=================================================================
;; Internal functions
;;=================================================================
(defun ncl-doc-cache-dir-create ()
  "Create cache dir."
  (interactive)
  (unless (file-directory-p ncl-doc-cache-dir)
    (make-directory ncl-doc-cache-dir)))

;;; search functions
(defun ncl-doc-keys-search-cat (cat term)
  "Search all CAT for a TERM."
  (let ((cat-keys cat))
    (remove nil
            (mapcar (lambda (x)
                      (if (string-match term x)
                          x))
                    cat-keys))))

(defun ncl-doc-keys-search (term)
  "Take a search TERM, Return an alist with all matched items.
Here alist is category and matched keywords."
  (let* ((st term)
         (cats  (mapcar (lambda (x)
                          (car x))
                        ncl-doc-url-alist))
         (cats (mapcar (lambda (x)           ;make list of keyword vars
                         (concat "ncl-key-" x))
                       cats)))
    (mapcar (lambda (cat)
              (let ((klst (symbol-value (intern cat))))
                (ncl-doc-keys-search-cat klst term)))
            cats)))

;;; URL construction function
(defun ncl-doc-construct-url (kword)
  "Construct a url from the KWORD."
  (let ((kwd kword)
        ;; cats: all categories in the ncl-doc-url-alist
        (cats (mapcar (lambda (cat)
                        (car cat))
                      ncl-doc-url-alist)))
    (catch 'break
      (while  cats                 ; loop on all categories
        (let ((ct (car cats)))
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
                                 kwd ncl-doc-url-suffix))))))
        (setq cats (cdr cats))))))

;;=================================================================
;; Define minor mode
;;=================================================================
(defvar ncl-doc-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") 'ncl-doc-query-at-point)
    (define-key map (kbd "C-c C-p") 'ncl-doc-query-at-point)
    (define-key map (kbd "C-c C-o") 'ncl-doc-query-open)
    map)
  "Key bindings function `ncl-doc-minor-mode'.")

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
  :require 'ncl
  (run-mode-hooks))

(defvar ncl-doc-completing-read
  (if (null ido-mode) 'completing-read 'ido-completing-read)
  "Ido support with convenience.")

;;=================================================================
;; Major Mode stuff
;;=================================================================
(defvar ncl-doc-temp-buffer-name
  "*ncl doc buffer*")

(defvar ncl-doc-return-window-config nil
  "Previous window config.")

(defvar ncl-doc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m"     'ncl-doc-browse-url)
    (define-key map " "        'ncl-doc-browse-url)
    (define-key map "f"        'ncl-doc-browse-url)
    (define-key map "q"        'ncl-doc-quit-window)
    (define-key map "Q"        'ncl-doc-exit-window)
    (define-key map "n"        'ncl-doc-move-next-line)
    (define-key map "p"        'ncl-doc-move-prev-line)
    (define-key map "s" 'ncl-doc-query-at-point)
    (define-key map "o" 'ncl-doc-query-open)

    (define-key map "/"        'isearch-forward)
    (define-key map "l"        'recenter)
    (define-key map "<"        'beginning-of-buffer)
    (define-key map ">"        'end-of-buffer)
    (define-key map "v"        'scroll-down)
    map)
  "Keymap for `ncl-doc-mode'.")

(define-derived-mode ncl-doc-mode fundamental-mode "ncl-doc"
  "major mode to help read NCL docs from UCAR website"
  :group 'ncl-doc
  (kill-all-local-variables)
  (setq mode-name "ncl-doc")
  (use-local-map ncl-doc-mode-map)
  (setq buffer-read-only t)
  (run-mode-hooks))

(defun ncl-doc-move-prev-line ()
  "Move to previous entry."
  (interactive)
  (when (< 3 (line-number-at-pos))
    (call-interactively 'previous-line)))

(defun ncl-doc-move-next-line ()
  "Move to next entry."
  (interactive)
  (when (< (line-number-at-pos)
           (- (line-number-at-pos (point-max)) 1))
    (call-interactively 'next-line)))

(defun ncl-doc-browse-url ()
  "Lookup the current line in a browser."
  (interactive)
  (beginning-of-line)
  (let* ((wd (thing-at-point 'symbol))
         (url (ncl-doc-construct-url wd)))
    (if url
        (if (string= url "keyword")
            (message "\"%s\" is a NCL builtin keyword and has no specific page to look at
Consult User Manual Here: http://www.ncl.ucar.edu/Document/Manuals/Ref_Manual/"
                     wd)
          (progn
            (message "Browsing: \"%s\"" url)
            (browse-url url)))
      (error "No URL on this line"))))


(defun ncl-doc-exit-window ()
  "Leave the completions window."
  (interactive)
  (let ((buf (current-buffer)))
    (set-window-configuration ncl-doc-return-window-config)
    (kill-buffer buf)))

(defun ncl-doc-quit-window ()
  "Leave the completions window."
  (interactive)
  (set-window-configuration ncl-doc-return-window-config))


;;=================================================================
;; user functions
;;=================================================================
;;;###autoload
(defun ncl-doc-query-at-point ()
  "Ask for a keyword to search and match inbuilt function."
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
                    (completing-read default-prompt ncl-doc-key-all))))

    (if (= 0 (string-width default-query)) ;protect if the user input is empty
        (set 'default-query default-word)) ;then it should be default word only

    (if (string= "" default-query)
        (message "default-query is nothing "))

    (let ((url (ncl-doc-construct-url default-query)))
      ;; if url is a keyword tell that there is no url for that
      (if (string= url "keyword") ; if its a ncl keyword its in the ref manual
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
                (insert (format "NCL keyword Search for \"%s\":"
                                default-query))
                (insert "\n")

                ;; insert text
                (let ((idx 0)
                      (ln (length ncl-doc-key-cat)))
                  (while (> ln idx)
                    (let ((ct (nth idx ncl-doc-key-cat))
                          (mts (nth idx matches)))
                      (if mts
                          (insert (format "\n;; Matches in the category \"%s\":\n" ct)))

                      ;; loop over matches
                      (mapc (lambda (x)
                              (insert (format "%s\n" x)))
                            mts))
                    (incf idx)))

                (goto-char (point-min))
                (forward-line (1- 3))

                ;; Major mode stuff
                (ncl-doc-mode)

                (font-lock-add-keywords
                 nil
                 '("\\(^;.*\\)"
                   (1 font-lock-comment-face)))

                (font-lock-add-keywords
                 nil `((,(format "\\(%s\\|%s\\|%s\\)"
                                 default-query
                                 (upcase default-query)
                                 (upcase-initials default-query))
                        1
                        font-lock-warning-face prepend)))

                (setq font-lock-keywords-case-fold-search t)

                ;; store window conf
                (set (make-local-variable 'ncl-doc-return-window-config)
                     cur-window-conf)

                ;; make fit to screen
                (shrink-window-if-larger-than-buffer (get-buffer-window tmpbuf)))))))))))

;;;###autoload
(defun ncl-doc-query-open (keyword)
  "Queries for a KEYWORD and search the database for it to gather
its documentation url. Then calls web browser to open that url."
  (interactive
   (list
    (let ((initial (thing-at-point 'symbol)))
      (funcall ncl-doc-completing-read
               "Query: " ncl-doc-key-all nil nil initial t))))
  (let ((url (ncl-doc-construct-url keyword)))
    (progn
      (message "Browsing: \"%s\"" url)
      (browse-url url))))

;;;###autoload
(eval-after-load 'ncl-mode
  ;; load the keys for ncl
  '(add-hook 'ncl-mode-hook 'ncl-doc-minor-mode))

;;;###autoload
(eval-after-load 'inf-ncl
  ;; load the keys for ncl
  '(add-hook 'ncl-mode-hook 'ncl-doc-minor-mode))

(provide 'ncl-doc)
;;; ncl-doc.el ends here

;;; run-test.el
;;
;; Description:
;;  emacs -batch -Q -l test/run-test.el


(defvar ncl-test-dir (file-name-directory (or load-file-name
                                              buffer-file-name)))
(defvar ncl-root-dir (file-name-as-directory (concat ncl-test-dir "..")))


;;; load-path
(mapc (lambda (p) (add-to-list 'load-path p))
      (list ncl-test-dir ncl-root-dir))

;; load tests
(load "ncl-mode-tests")


;; Use ERT from github when this Emacs does not have it
(unless (locate-library "ert")
  (add-to-list
   'load-path
   (file-name-directory (concat ncl-root-dir "/lib/ert/lisp/emacs-lisp")))
  (require 'ert-batch)
  (require 'ert-ui))

;;; run
(if noninteractive
    (ert-run-tests-batch-and-exit)
  (ert t))

;;; run-test.el ends here

;;; run-test.el
;;
;; Description:
;;  emacs -batch -Q -l test/run-test.el


(defvar ncl-test-dir (file-name-directory load-file-name))
(defvar ncl-root-dir (concat ncl-test-dir ".."))


;;; load-path
(mapc (lambda (p) (add-to-list 'load-path p))
      (list ncl-test-dir ncl-root-dir))

;; load tests
(load "ncl-mode-tests")

;;; run
(if noninteractive
    (ert-run-tests-batch-and-exit)
  (ert t))

;;; run-test.el ends here

;;; run-test.el
;;
;; Description:
;;  emacs -batch -Q -l test/run-test.el


(defconst ncl-test-dir (file-name-directory
                        (or load-file-name buffer-file-name)))
(defconst ncl-root-dir (expand-file-name ".." ncl-test-dir))
(setq debug-on-error t)

;;; load-path
(mapc (lambda (p) (add-to-list 'load-path p))
      (list ncl-test-dir ncl-root-dir))

;; load tests
(load "ncl-mode-tests")

;;; run
(if noninteractive
    (progn
      (princ emacs-version)
      (ert-run-tests-batch-and-exit))
  (ert t))

;;; run-test.el ends here

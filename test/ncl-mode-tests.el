;;; ncl-mode-tests.el
;;
;;; Description: tests for ncl-mode.
;; based on ruby-mode tests

(require 'ert)
(require 'ncl-mode)

(defun ncl-should-indent (content column)
  "Assert indentation COLUMN on the last line of CONTENT."
  (with-temp-buffer
    (insert content)
    (ncl-mode)
    (ncl-indent-line)
    (should (= (current-indentation) column))))

(defun ncl-should-indent-buffer (expected content)
  "Assert that CONTENT turns into EXPECTED after the buffer is re-indented.

The whitespace before and including \"|\" on each line is removed."
  (with-temp-buffer
    (cl-flet ((fix-indent (s) (replace-regexp-in-string "^[ \t]*|" "" s)))
             (insert (fix-indent content))
             (ncl-mode)
             (indent-region (point-min) (point-max))
             (should (string= (fix-indent expected) (buffer-string))))))


;;; tests

;;; ncl-mode-tests.el ends here

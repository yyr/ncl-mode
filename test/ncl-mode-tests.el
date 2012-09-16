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

;;; indentation
(ert-deftest ncl-indent-continued-lines ()
  (ncl-should-indent "a = 1 + \\\n2" ncl-continuation-indent)
  (ncl-should-indent "  a = 1 + \\\n2 + \\\n4" 0)
  (ncl-should-indent "  a = 1 + \\\n  2 + \\\n4" 2))

(ert-deftest ncl-indent-simple ()
  (ncl-should-indent-buffer
   "if (foo)
   |  bar
   |end if
   |zot
   |"
   "if (foo)
   |bar
   |  end if
   |    zot
   |"))

(ert-deftest ncl-multiple-loop-indent ()
  (ncl-should-indent-buffer
   "do it = 0, 4, 1
   |  do while ( some_exp )
   |    a = \"b\"
   |  end do
   |end do
   |"
   "    do it = 0, 4, 1
   |do while ( some_exp )
   |a = \"b\"
   |  end do
   | end do
   |"))

(ert-deftest ncl-procedure-indent ()
  (ncl-should-indent-buffer
   "undef(\"some_proc\")
   |procedure some_proc(a:numeric, b:numeric)
   |local a, b, c
   |
   |begin
   |  a = \"b\"
   |
   |  return
   |end"
   "undef(\"some_proc\")
   |procedure some_proc(a:numeric, b:numeric)
   |local
   |
   |begin
   |
   |
   |return
   |end"
   ))

;;; ncl-mode-tests.el ends here

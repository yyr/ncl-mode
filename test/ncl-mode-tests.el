;;; ncl-mode-tests.el
;;
;;; Description: tests for ncl-mode.
;; based on ruby-mode tests

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
(ert-deftest ncl-test-file-beginning ()
  (ncl-should-indent "   a = 1" 0)
  (ncl-should-indent ";\n   a = 1" 0)
  (ncl-should-indent ";\n\n    a = 1" 0))

(ert-deftest ncl-test-indent-comment-simple ()
  (ncl-should-indent "  a = 1 + 2\n  ; two" 2)
  (ncl-should-indent "  a = 1 + 2\n; two" 0)
  (ncl-should-indent "  a = 1 + 2\n  ;;; two" 0))

(ert-deftest ncl-test-indent-comment ()
  (ncl-should-indent-buffer
   "; some if
   |if ( choice  ) then
   |  a@attnames = -9999.0
   |;;; some else if
   |  ;
   |end if
   |"
   "; some if
   |if ( choice  ) then
   |  a@attnames = -9999.0
   |   ;;; some else if
   | ;
   |end if
   |"))

(ert-deftest ncl-test-indent-continued-lines ()
  (ncl-should-indent "a = 1 + \\\n2" 4)
  (ncl-should-indent "  a = 1 + \\\n2 + \\\n4" 0)
  (ncl-should-indent "  a = 1 + \\\n  2 + \\\n4" 2))

(ert-deftest ncl-test-indent-if ()
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

(ert-deftest ncl-test-indent-if-else ()
  (ncl-should-indent-buffer
   "if (foo)
   |  bar
   |else
   |  baz
   |end if
   |zot
   |"
   "if (foo)
   |  bar
   |else
   |baz
   |  end if
   |zot
   |"
   ))

(ert-deftest ncl-test-indent-if-elseif-else ()
  (ncl-should-indent-buffer
   "if (foo)
   |  bar
   |  else if (condition)
   |    i = \"ok\"
   |  else
   |    i = \"Not ok\"
   |  end if
   |else
   |  baz
   |end if
   |zot
   |"
   "if (foo)
   |bar
   |else if (condition)
   |i = \"ok\"
   |else
   |    i = \"Not ok\"
   |end if
   |  else
   |  baz
   |  end if
   |zot
   |"))

(ert-deftest ncl-test-indent-multiple-loop ()
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

(ert-deftest ncl-test-indent-procedure ()
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
   |  local a, b, c
   |
   |    begin
   |  a = \"b\"
   |
   |        return
   |  end"
   ))

(ert-deftest ncl-test-indent-setvalues-block ()
  (ncl-should-indent-buffer
   "a = res
   |setvalues a
   |  a : b
   |end setvalues"
   "a = res
   |  setvalues a
   |  a : b
   |  end setvalues"
   ))


(ert-deftest ncl-test-indent-getvalues-block ()
  (ncl-should-indent-buffer
   "a = res
   |getvalues a
   |  a : b
   |end getvalues"
   "a = res
   |  getvalues a
   |  a : b
   |  end getvalues"
   ))

;;; ncl-mode-tests.el ends here

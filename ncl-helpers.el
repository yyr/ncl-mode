;;; ncl-helpers.el
;;
;;    File: ncl-helpers.el
;;  Author: Yagnesh Raghava Yakkala <yagnesh@NOSPAM.live.com>
;; Created: Friday, December  9 2011

;;; Description:
;; just place holder for now

;;=================================================================
;; User options
;;=================================================================
(defgroup ncl-indent nil
  "Indentation variables in NCL mode."
  :prefix "ncl-"
  :group  'ncl-indent)

(defgroup ncl-comment nil
  "Comment-handling variables in NCL mode."
  :prefix "ncl-"
  :group  'ncl)

(defcustom ncl-block-indent 2
  "Extra indentation applied to do, if, where(?) blocks."
  :type  'integer
  :safe  'integerp
  :group 'ncl-indent)

(defcustom ncl--indent 2
  "Extra indentation applied to IF, SELECT CASE, WHERE and FORALL blocks."
  :type  'integer
  :safe  'integerp
  :group 'ncl-indent)

;;; ncl-helpers.el ends here

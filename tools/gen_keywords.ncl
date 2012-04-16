;    File: gen_keywords.ncl
; Created: Sunday, December 11 2011
;

; Description:
; copied from gen_editor_utils.ncl

load "gen_editor_utils.ncl"

begin
  ofile="ncl-mode"
  all_keywords=new(3000,string)

; collect and append all keywords
  add_lines(all_keywords,get_ncl_keywords())
  add_lines(all_keywords,get_ncl_resources())
  add_lines(all_keywords,get_ncl_procs_and_funcs())

  asciiwrite(ofile,all_keywords)

; delete missing lines
  system("perl -i -ne 'print unless /^missing/' " + ofile)

end

;;; gen_keywords.ncl ends here

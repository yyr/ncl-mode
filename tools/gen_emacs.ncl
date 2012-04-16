load "gen_editor_utils.ncl"

;----------------------------------------------------------------------
; This function returns the line number of the first missing line.
;----------------------------------------------------------------------
  undef("get_next_line_num")
  function get_next_line_num(lines)
  local ii
begin
  ii = ind(ismissing(lines))
  if(all(ismissing(ii))) then
    print("get_next_line_num: Error, no lines left")
    return(new(1,integer))
  else
    return(ii(0))
  end if
end

;---------------------------------------------------------------------
; Procedure: write_as_defvar
; Doc: frame defvar from varname, list, docstring,
;---------------------------------------------------------------------
undef("write_as_defvar")
procedure write_as_defvar(ll,klist,varname,dstr)
local j,k,dq,fl,mtmpstr,nlpr,ev

begin
  ni = get_next_line_num(ll)
  add_lines(ll, "")

  dq = str_get_dq()
  fl = "(defvar " + varname
  add_lines(ll, fl)
  add_lines(ll,"  '(")       ;

  nlpr = 200                    ; no of words per row
  do i=0,dimsizes(klist)-1,nlpr
    mtmpstr = "   "
;---Only do 200 names in a single line
    do j=i,min((/i+nlpr-1,dimsizes(klist)-1/))
      mtmpstr = mtmpstr + " " + dq + klist(j) + dq
;      mtmpstr = mtmpstr + " " + klist(j)

    end do
    add_lines(ll,mtmpstr)
  end do
  ev = "    ) " + dq + dstr + dq + ")"
  add_lines(ll, ev)
  add_lines(ll, "")

end
;- |write_as_defvar| ends here --

;----------------------------------------------------------------------
; This procedure reads in the header from the existing emacs file
; and adds it to the new array of lines for the new emacs file.
;----------------------------------------------------------------------
undef("write_header")
procedure write_header(elines,lines)
local find_line, ii, header
begin
;---Get header
  find_line = ";;;; COOKIE: STARTS HERE =DO NOT DELETE="
  ii        = str_match_ind(elines,find_line)
  header    = elines(0:ii(0))

  add_lines(lines,header)
end

;----------------------------------------------------------------------
; This procedure generates the keywords and adds them
; to the lines for the new emacs file.
;----------------------------------------------------------------------
undef("write_keywords")
procedure write_keywords(lines)
local key_names,vname,dstring
begin
  dstring="Reserved Keywords in ncl"
  vname="ncl-key-keywords"
  key_names = get_ncl_keywords()
  write_as_defvar(lines,key_names,vname,dstring)
end

;----------------------------------------------------------------------
; This procedure generates the operators and adds them
; to the lines for the new emacs file.
;----------------------------------------------------------------------
undef("write_operators")
procedure write_operators(lines)
local op_names, ni,dstr,vname
begin

  vname = "ncl-key-operators"
  dstr="Operators in NCL"

;---Get list of operators
  op_names = get_ncl_operators()

  do i=0,dimsizes(op_names)-1
    if ( op_names(i) .eq. "\" ) then
      op_names(i)="\\"
    end if
  end do
  write_as_defvar(lines,op_names,vname,dstr)

end

;----------------------------------------------------------------------
; This procedure gets the function/procedure names for each
; category (gsn, built-in, etc) and writes them out by
; section.
;----------------------------------------------------------------------
undef("write_ncl_procs_and_funcs_by_cat")
procedure write_ncl_procs_and_funcs_by_cat(lines)
local i, j, k, cat_names, btmpstr, mtmpstr, dstr, closer, etmpstr, fnames, dq, nlpr
begin
  dq = str_get_dq()
  cat_names = get_valid_cat_names()
  nlpr = 200
  do i=0,dimsizes(cat_names(:,0))-1
    ;; write defvarline
    dstr=cat_names(i,1)
    vname="ncl-key-" + cat_names(i,0)
    fnames = get_ncl_procs_and_funcs_by_cat(cat_names(i,0))
    write_as_defvar(lines,fnames,vname,dstr)
;---Clean up
    delete(fnames)
  end do
end

;----------------------------------------------------------------------
; This procedure writes out the NCL resource names.
;----------------------------------------------------------------------
undef("write_ncl_resources")
procedure write_ncl_resources(lines)
local dstr,rnames,vname
begin
  rnames = get_ncl_resources()
  dstr="Ncl resources"
  vname="ncl-key-resources"
  write_as_defvar(lines,rnames,vname,dstr)
end

;----------------------------------------------------------------------
; This procedure reads in the footer from the existing emacs file
; and adds it to the new array of lines for the new emacs file.
;----------------------------------------------------------------------
undef("write_footer")
procedure write_footer(elines,lines)
local dq, end_lines, find_line, ii, footer
begin
  dq = str_get_dq()

;---Get footer
  find_line = ";;;; COOKIE: ENDS HERE =DO NOT DELETE="
  ii        = str_match_ind(elines,find_line)
  if(ismissing(ii(0))) then
    print("Couldn't find '" + find_line + "'")
    return
  end if
  footer    = elines(ii(0):)
  add_lines(lines,footer)
end

;----------------------------------------------------------------------
; This procedure writes an array of lines to the emacs file
;----------------------------------------------------------------------
undef("write_emacs_file")
procedure write_emacs_file(fname,lines)
begin
  ii = ind(.not.ismissing(lines))

  if(.not.any(ismissing(ii))) then
    print("Writing output to " + fname)
    system("/bin/rm " + fname)
    asciiwrite(fname,lines(ii))
  else
    print("No output to write to " + fname)
  end if
end


;----------------------------------------------------------------------
; Main code for generating "ncl.el"
;----------------------------------------------------------------------
begin
  emacs_fname = "ncl.el"

;---Make backup copy of emacs file
  system("/bin/cp " + emacs_fname + " " + emacs_fname + ".bkp")

  all_lines = new(1000,string)

;---Read in current ncl.el file
  emacs_lines = asciiread(emacs_fname,-1,"string")

  write_header(emacs_lines,all_lines)

;---Reconstruct everything in-between
  write_keywords(all_lines)
  write_operators(all_lines)
  write_ncl_procs_and_funcs_by_cat(all_lines)
  write_ncl_resources(all_lines)

  write_footer(emacs_lines,all_lines)
;  print(all_lines)
  write_emacs_file(emacs_fname + ".new",all_lines)

end

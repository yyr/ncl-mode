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

;----------------------------------------------------------------------
; This procedure adds an array of strings to an existing array of
; strings. Since this is a procedure, the array of lines to be
; added to has to be preallocated.
;----------------------------------------------------------------------
undef("add_lines")
procedure add_lines(lines,l)
begin
;---Error checking
  if(all(ismissing(l))) then
    print("add_lines: lines to add are all missing. None added.")
    return
  end if

  nlines = dimsizes(lines)
  nl     = dimsizes(l)

  ii = ind(.not.ismissing(lines))
  if(all(ismissing(ii))) then
    ii(0) = -1
    nii   = 0
  else
    nii = dimsizes(ii)-1
  end if

;---Make sure we can add the line(s).
  if((ii(nii)+nl).lt.nlines) then
    lines(ii(nii)+1:ii(nii)+nl) = l
  else
    print("Error: no room left to add lines to array. Quitting...")
    exit
  end if
end

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
local key_names, ni, tmpstr
begin
  dq = str_get_dq()
  ni = get_next_line_num(lines)
  add_lines(lines,";; NCL keywords")
  add_lines(lines,"(defvar ncl-keywords")
  add_lines(lines,"  '(")
  tmpstr = "    "
  key_names = get_ncl_keywords()
  do i=0,dimsizes(key_names)-1
    tmpstr = tmpstr + " " + dq + key_names(i) + dq
  end do
  tmpstr = tmpstr + " )"
  add_lines(lines,tmpstr)
  closer=")"
  dstr= "  " + dq + "Reserved Keywords in ncl" + dq
  add_lines(lines, dstr + closer)
end

;----------------------------------------------------------------------
; This procedure generates the operators and adds them
; to the lines for the new emacs file.
;----------------------------------------------------------------------
undef("write_operators")
procedure write_operators(lines)
local op_names, ni, btmpstr, mtmpstr, etmpstr
begin
  ni = get_next_line_num(lines)
  add_lines(lines,";; Array definition chars and tests - couldn't get to work in list above...")

;---Get list of operators
  op_names = get_ncl_operators()

  dq = str_get_dq()
  btmpstr = "    (" + dq + "\\("
  etmpstr = "\\)" + dq + " 1 font-lock-keyword-face )"
  do i=0,dimsizes(op_names)-1
    mtmpstr = str_sub_str(op_names(i),".","\\.")
    mtmpstr = str_sub_str(mtmpstr,"(","\(")
    mtmpstr = str_sub_str(mtmpstr,")","\)")
    mtmpstr = str_sub_str(mtmpstr,"/","\/")
    add_lines(lines,btmpstr + mtmpstr + etmpstr)
  end do
  add_lines(lines,"")
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
    ;; cat description
    add_lines(lines,"")
    ;; write defvarline
    defstr = "(defvar ncl-key-" + cat_names(i,0)
    add_lines(lines,defstr)
    add_lines(lines,"  '(")

    fnames = get_ncl_procs_and_funcs_by_cat(cat_names(i,0))
    do j=0,dimsizes(fnames)-1,nlpr
      mtmpstr = "   "
;---Only do 200 names in a single line
      do k=j,min((/j+nlpr-1,dimsizes(fnames)-1/))
        mtmpstr=mtmpstr + " " + dq + fnames(k) + dq
      end do
      mtmpstr = mtmpstr + ")"
      add_lines(lines,mtmpstr)
    end do
    closer=")"
    dstr= "  " + dq + cat_names(i,1)  + dq
    add_lines(lines, dstr + closer)

;---Clean up
    delete(fnames)
  end do
end

;----------------------------------------------------------------------
; This procedure writes out the NCL resource names.
;----------------------------------------------------------------------
undef("write_ncl_resources")
procedure write_ncl_resources(lines)
local i, j, btmpstr, mtmpstr, etmpstr, dstr, rnames, dq, nlpr,closer
begin
  dq = str_get_dq()
  rnames = get_ncl_resources()
;  print(rnames)
  nlpr = 200
  add_lines(lines,";; ncl resources 200 names in each line")

  ;; write defvarline
  defstr = "(defvar ncl-resources"
  add_lines(lines,defstr)
  add_lines(lines,"  '(")

  do i=0,dimsizes(rnames)-1,nlpr
    mtmpstr = "  "
;---Only do 200 names in a single line
    do j=i,min((/i+nlpr-1,dimsizes(rnames)-1/))
      mtmpstr = mtmpstr + " " + dq + rnames(j) + dq
    end do
    add_lines(lines,mtmpstr+")")
  end do
  closer=")"
  dstr= "  " + dq + "Ncl resources"  + dq
  add_lines(lines, dstr + closer)
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
;  write_operators(all_lines)
  write_ncl_procs_and_funcs_by_cat(all_lines)
  write_ncl_resources(all_lines)

  write_footer(emacs_lines,all_lines)
  print(all_lines)
;  write_emacs_file(emacs_fname + ".new",all_lines)

end

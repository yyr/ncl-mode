;    File: indent.ncl

a = "b"

;_____________________________________________________________________________
; procedure: some_proc
;_____________________________________________________________________________
undef("some_proc")
procedure some_proc(a:numeric, b:numeric)
local

begin


return
end

;_____________________________________________________________________________
; function: some_proc
;_____________________________________________________________________________
undef("some_fun")
function some_fun(f:file, n:numeric, \
                       s:string)
local m

begin

m = s + ""


return()
end

begin

varname = "a"
dimname = "Dimension1"

a&Dimension1 = (/.1,.2,.3,.4,.5,.../)
a&$dimname$ = (/.1,.2,.3,.4,.5,.../)
a@units = "Degrees C"
a@_FillValue = -9999.0

thefile->a&Dimension1 = (/.1,.2,.3,.4,.5,.../)
thefile->$varname$&$dimname$ = (/.1,.2,.3,.4,.5,.../)
thefile->a@units = "Degrees C"

attnames = (/"units","_FillValue"/)
a@$attnames(0)$ = "Degrees C"
a@$attnames(1)$ = -9999.0

do it = 0, 4, 1

if ( choice  ) then

do while ( some_logical_exp )

end do

else

end if


end do



end
;;; indent.ncl ends here

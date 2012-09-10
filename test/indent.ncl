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

do it = 0, 4, 1

if ( choice  ) then

do while ( some_logical_exp )

end do

else

end if


end do



end
;;; indent.ncl ends here

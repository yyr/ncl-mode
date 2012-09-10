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




end
;;; indent.ncl ends here

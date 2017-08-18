; getcomf.pro
;  check existence of comfile and return its content value
;  comfile should contain one binaly integer
;	'97/08/31	k.i.
function getcomf,comfile,delete=delete

count=0
while count eq 0 do begin
	f=findfile(comfile,count=count)
	wait,0.005
endwhile
m=0
wait,0.01
openr,unit,comfile,/get_lun
readu,unit,m
close,unit
free_lun,unit
if keyword_set(delete) then $
	dmy=call_external('c:\nkrprj\cprog\oscomdll.dll', $
		'Dremove',comfile,value=[0d],/S_VALUE)

return,m
end

;******************************************************
pro putcomf,comfile,m

get_lun,unit
openw,unit,comfile
writeu,unit,m
close,unit
free_lun,unit

end


;+
; timestr.pro (function)
;	get time string 'hh:mm:ss'  (from float time)
;	!! un-understandable, old string is replaced by a new call ???
;	97/09/29	k.i.
;	98/11/11	k.i.	ftime parameter
;	07/08/04	k.i.	b32 keyword
;-
function timestr,ftime,b32=b32

if n_elements(ftime) then begin
	time_ret=string(long(ftime),format='(i2.2)')+':' $
		+string(long(ftime*60) mod 60,format='(i2.2)')+':' $
		+string(long(ftime*3600) mod 60,format='(i2.2)')
endif else begin
	time1='11:11:11     '
	if keyword_set(b32) then begin
		oscomdll='c:\nkrprj\cprog\oscom32\debug\oscom32.dll'
		time1=call_external(oscomdll,'gettimestr',/S_VALUE)
	endif else begin
		oscomdll='c:\nkrprj\cprog\oscomdll.dll'
		dmy=call_external(oscomdll,'gettimestr',time1,value=[0d],/S_VALUE)
	endelse
	time_ret=time1
endelse
return,time_ret
end

;---------- init camera ----------
dllfile='C:\Projects\cprog\VS2005\Xenics\cam_set5\debug\cam_set5.dll'
m_hCam=call_external(dllfile,'createcam',/all_value,/cdecl)
;---------- start camera cooling ----------
Temp=260	; Cooling Temp. (K)
stfan=call_external(dllfile,'startfan',m_hCam,/all_value,/cdecl)
ctemp=call_external(dllfile,'settemp',m_hCam,Temp,/all_value,/cdecl)

for i=0,9 do begin
	print,'wait'+string(10-i)+' sec ...'
	wait,1
endfor

;---------- stop camera cooling ----------;
stfan=call_external(dllfile,'stopfan',m_hCam,/all_value,/cdecl)
;---------- stop camera ----------;
m_hCam=call_external(dllfile,'stopcap',m_hCam,/all_value,/cdecl)

print,'-OK-'

END

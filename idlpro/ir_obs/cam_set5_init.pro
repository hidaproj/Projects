; initialize IR camera

dllfile='C:\Documents and Settings\corona\My Documents\Visual Studio 2005\Projects\cam_set5\debug\cam_set5.dll'
 ;---------- initcamera ----------;
  m_hCam=call_external(dllfile,'createcam',/all_value,/cdecl)
 ;---------- camera cooling ----------;
 stfan=call_external(dllfile,'startfan',m_hCam,/all_value,/cdecl)
 Temp=255	; Cooling Temp. (K)
 stfan=call_external(dllfile,'settemp',m_hCam,Temp,/all_value,/cdecl)
 print,'Cooling Temp.='+string(Temp,form='(i5)')+'K'

end

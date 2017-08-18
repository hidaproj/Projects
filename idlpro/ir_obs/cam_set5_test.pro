;pro cam_set5_test

window,0,xs=640,ys=512

;dllfile='C:\Documents and Settings\isuzuki\My Documents\Visual Studio 2005\Projects\cam_set5\debug\cam_set5.dll'
;dllfile='C:\Documents and Settings\corona\My Documents\Visual Studio 2005\Projects\cam_set5\debug\cam_set5.dll'
 ;---------- initcamera ----------;
 ; m_hCam=call_external(dllfile,'createcam',/all_value,/cdecl)
 ;---------- camera cooling ----------;
; stfan=call_external(dllfile,'startfan',m_hCam,/all_value,/cdecl)
; exp=10
; gain=0
; integ=2
;---------- start cap ----------;
;  m_hCam=call_external(dllfile,'startcap',m_hCam,exp,gain,/all_value,/cdecl)

;dark=intarr(640,512)
 ;  mm=call_external(dllfile,'InitParam',integ,/cdecl,/all_value)
 ; mm=call_external(dllfile,'singleshot',dark,integ,value=[0b],/cdecl)
;dark=float(dark)
;tvscl,dark
;wait,5

;---------- single shot ----------;
img2=intarr(640,512,100)
for i=0,100-1 do begin
img=intarr(640,512)
;   mm=call_external(dllfile,'InitParam',integ,/cdecl,/all_value)
  mm=call_external(dllfile,'singleshot',img,integ,value=[0b],/cdecl)
tvscl,rotate(img,7)
	xyouts,10,10,string(i),/dev
img2(*,*,i)=img
endfor

;---------- stop obs ----------;
;	r3=call_external(dllfile,'stopfan',m_hCam,/all_value,/cdecl);
;	m_hCam=call_external(dllfile,'stopcap',m_hCam,/all_value,/cdecl)
stop
end

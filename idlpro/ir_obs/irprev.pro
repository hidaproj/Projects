
integ=1
expo=1000	; msec
gain=3      ;0='low',3='high'
window,0,xs=640,ys=512
;window,2
;---------- start cap ----------;
  m_hCam=call_external(dllfile,'startcap',m_hCam,expo,gain,/all_value,/cdecl)
  setp=call_external(dllfile,'setpower',m_hCam,/all_value,/cdecl)
;================================== add 2008/10/20
  rr=call_external(dllfile,'initboard',/all_value,/cdecl)
  rr=call_external(dllfile,'InitParam',integ,/all_value,/cdecl)
  rr=call_external(dllfile,'AllocMemory',/all_value,/cdecl)
;-=================================

nx=640 &	ny=512
img1=intarr(nx,ny)
ans=''
;while ans ne 'q' do begin

!err=0
while !err ne 4 do begin
		tvrdc,x,y,2,/dev
		if (!err and 3) ne 0 then begin
			while (!err ne 0 ) do begin wait,float(expo)/10. & tvrdc,x,y,0,/dev & end 
		endif 
	mm=call_external(dllfile,'get_img',img1,value=[0b],/cdecl)
	temp=call_external(dllfile,'checktemp',m_hCam,/all_value,/cdecl)
	pwr=call_external(dllfile,'checkpower',m_hCam,/all_value,/cdecl)
;wset,0
	tvscl,img1
	xyouts,10,10,'temp : '+strtrim(temp,2)+'K('+strtrim(temp-273,2)+'C)   PWR : '+strtrim(pwr,2),/dev,size=2
	;read,ans 
;wset,2
;	plot,img(x,*)

endwhile

rr=call_external(dllfile,'ReleaseMemory',/all_value,/cdecl)
rr=call_external(dllfile,'closeboard',/all_value,/cdecl)

end

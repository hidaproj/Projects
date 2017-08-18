
integ=1
expo=100	; msec
gain=3
nx=640 &	ny=512
;window,0,xs=nx,ys=ny
;---------- start cap ----------;
  m_hCam=call_external(dllfile,'startcap',m_hCam,expo,gain,/all_value,/cdecl)
  setp=call_external(dllfile,'setpower',m_hCam,/all_value,/cdecl)
;================================== add 2008/10/20
  rr=call_external(dllfile,'initboard',/all_value,/cdecl)
  rr=call_external(dllfile,'InitParam',integ,/all_value,/cdecl)
  rr=call_external(dllfile,'AllocMemory',/all_value,/cdecl)
;-=================================

img1=intarr(nx,ny)
ans=''
;while ans ne 'q' do begin

;Create a base widget. 
base1 = WIDGET_BASE(title='IR_CAM',/column)

xpdmenu,['/EXIT/'],base1,/frame,title='irprev2'

;Attach a 256 x 256 draw widget. 
draw = WIDGET_DRAW(base1, XSIZE = nx, YSIZE = ny) 
 
;Realize the widgets. 
WIDGET_CONTROL, /REALIZE, base1

;Obtain the window index. 
WIDGET_CONTROL, draw, GET_VALUE = index 

;Set the new widget to be the current graphics window 

uvalue=''
ii=0
	while (uvalue ne "EXIT") do begin
		WSET, index 
		mm=call_external(dllfile,'get_img',img1,value=[0b],/cdecl)
		temp=call_external(dllfile,'checktemp',m_hCam,/all_value,/cdecl)
		pwr=call_external(dllfile,'checkpower',m_hCam,/all_value,/cdecl)
		tvscl,img1
		xyouts,10,10,'temp : '+strtrim(temp,2)+'K('+strtrim(temp-273,2)+'C)   PWR : '+strtrim(pwr,2),/dev,size=2
		event = widget_event(base1,/nowait)
		if event.id ne 0 then $
		WIDGET_CONTROL, get_uvalue=uvalue, event.id
	endwhile
WIDGET_CONTROL, /destroy, base1

rr=call_external(dllfile,'ReleaseMemory',/all_value,/cdecl)
rr=call_external(dllfile,'closeboard',/all_value,/cdecl)

end

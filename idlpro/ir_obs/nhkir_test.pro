

integ=10
expo=100	; msec
 gain=3

;---------- start cap ----------;
  m_hCam=call_external(dllfile,'startcap',m_hCam,expo,gain,/all_value,/cdecl)
  rr=call_external(dllfile,'initboard',/all_value,/cdecl)
  rr=call_external(dllfile,'InitParam',integ,/all_value,/cdecl)
  rr=call_external(dllfile,'AllocMemory',/all_value,/cdecl)

nx=640 &	ny=512
pol=fltarr(nx,ny,6)
img1=intarr(nx,ny,integ)
for j=0,5 do begin
	caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
	DATE1=strmid(strtrim(year,2),2,2)+'/'+strtrim(mon,2)+'/'+strtrim(day,2)	; "yy/mm/dd"
	TIME1=strtrim(hour,2)+':'+strtrim(minu,2)+':'+strmid(strtrim(seco,2),0,6)	; "hh:mm:ss.sss"

 mm=call_external(dllfile,'get_img',img1,value=[0b],/cdecl)
print,j
	caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
	DATE2=strmid(strtrim(year,2),2,2)+'/'+strtrim(mon,2)+'/'+strtrim(day,2)	; "yy/mm/dd"
	TIME2=strtrim(hour,2)+':'+strtrim(minu,2)+':'+strmid(strtrim(seco,2),0,6)	; "hh:mm:ss.sss"
endfor
rr=call_external(dllfile,'ReleaseMemory',/all_value,/cdecl)
rr=call_external(dllfile,'closeboard',/all_value,/cdecl)

for i=0,integ-1 do begin 
 tvscl,img1(*,*,i) 
 xyouts,10,10,strtrim(i,2),/dev,size=3
 wait,0.2 
endfor

print,time1,'   ',time2
end
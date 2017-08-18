; nhkir.pro

outfile_path='C:\anan\10_21\'
seq=15		; # of sequence
integ=50	; # of integ at each pol.
expo=50	; msec
gain=3
fov='roi'	; -> all image
;fov='full'	; -> sum
nx=640 &	ny=512
x1=250 &	y1=80
nxp=150 &	nyp=300
window,0,xs=nx,ys=ny
;---------- start cap ----------;
  m_hCam=call_external(dllfile,'startcap',m_hCam,expo,gain,/all_value,/cdecl)

;================================== add 2008/10/20
  rr=call_external(dllfile,'initboard',/all_value,/cdecl)
  rr=call_external(dllfile,'InitParam',integ,/all_value,/cdecl)
  rr=call_external(dllfile,'AllocMemory',/all_value,/cdecl)
;-=================================

h=nkrhead()
h.OBSERVATORY='nkr'
h.TELESCOPE='c25'
h.OBS_MODE=1
h.TIME_SYS='JST'
h.EXP=expo/10
h.INTEG=integ
h.PROG='nhkir'
if fov eq 'roi' then begin
	h.nx=nxp &	h.ny=nyp
	h.pix_x1=x1 &	h.pix_y1=y1
	h.nbyt=2
	h.data_type=2
	h.nf=integ*6
	pol=uintarr(nxp,nyp,integ*6)
endif else begin
	h.nx=nxp &	h.ny=nyp
	h.pix_x1=0 &	h.pix_y1=0
	h.nbyt=4
	h.data_type=4
	h.nf=6
	pol=fltarr(nx,ny,6)
endelse
img1=intarr(nx,ny,integ)
com=['I+Q','I-Q','I+U','I-U','I+V','I-V']
ans=''
fnam=dialog_input(prompt='OUT FILE NAME:')

for k=0,seq-1 do begin
	outfile=outfile_path+fnam+'_'+strtrim(k,2)+'.dat'

	for j=0,5 do begin
		print,j,'   ',com[j]
		read,ans
		if j eq 0 then begin
			caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
			h.DATE1=strmid(strtrim(year,2),2,2)+'/'+strtrim(mon,2)+'/'+strtrim(day,2)	; "yy/mm/dd"
			h.TIME1=strtrim(hour,2)+':'+strtrim(minu,2)+':'+strmid(strtrim(seco,2),0,6)	; "hh:mm:ss.sss"
		endif

		;================================== modif 2008/10/20
		;	for i=0,integ-1 do begin
		;		mm=call_external(dllfile,'singleshot',img1,value=[0b],/cdecl)

		mm=call_external(dllfile,'get_img',img1,value=[0b],/cdecl)

		if fov eq 'roi' then begin
			pol[*,*,j*integ:(j+1)*integ-1]=img1[x1:x1+nxp-1,y1:y1+nyp-1,*]
			for i=0,integ-1 do begin
				tvscl,img1(*,*,i)
				xyouts,10,10,string(i),/dev,size=3
				draw,[x1,x1+nxp,x1+nxp,x1,x1],[y1,y1,y1+nyp,y1+nyp,y1]
			endfor
		endif else begin
			for i=0,integ-1 do begin
				tvscl,img1(*,*,i)
				xyouts,10,10,string(i),/dev,size=3
				pol[*,*,j]=pol[*,*,j]+float(img1(*,*,i))
			endfor
		endelse
		;-=================================
		if j eq 5 then begin
			caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
			h.DATE2=strmid(strtrim(year,2),2,2)+'/'+strtrim(mon,2)+'/'+strtrim(day,2)	; "yy/mm/dd"
			h.TIME2=strtrim(hour,2)+':'+strtrim(minu,2)+':'+strmid(strtrim(seco,2),0,6)	; "hh:mm:ss.sss"
		endif
	endfor

	nkrsave,outfile,h,pol
	if fov eq 'roi' then begin
		pol5=rebin(float(pol[*,*,5*integ:6*integ-1]),nxp,nyp,1)
		pol4=rebin(float(pol[*,*,4*integ:5*integ-1]),nxp,nyp,1)
		tvscl,((pol5-pol4)/(pol5+pol4))>(-0.01)<0.01
	endif else begin
		tvscl,((pol[*,*,5]-pol[*,*,4])/(pol[*,*,5]+pol[*,*,4]))>(-0.01)<0.01
	endelse
	print,outfile
endfor

rr=call_external(dllfile,'ReleaseMemory',/all_value,/cdecl)
rr=call_external(dllfile,'closeboard',/all_value,/cdecl)


end
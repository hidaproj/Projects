;  mmsp_calm.pro
;  calibrate MMSP data for multi measure
;  
;  2011.03.25   k.i.
;  2012.03.31   k.i.
@mmsplib

;********************************************************************
dir='C:\data\MMSP\20110309\' &	cfile='c20110309_115853.sav'	; clear, 1ms x 100
files='LC2_T20_Ip00_h'+['1','2']+'_v01000.sav' 


wl0s=[500.]
wbin=4

rad=!pi/180.
cd,dir
nf=n_elements(files)

restore_mmsp,dir+cfile,wl,p,cdat,wbin=wbin
imgsize,cdat,nw,nn
th1=dindgen(nn)*p.dth1
th2=th1/5.

;----  waveplate calibration  ----
mmsp_calwps,th1,th2,cdat,c,verb=0,thresh=1000.

iiv=where(c.valid eq 1)
stretch,255,0
ofm1=mean(c[iiv].offset1)
ofm2=mean(c[iiv].offset2)

;--------------  Mueller matrix  ----
s1=mmdecmp(emat(4))
s=replicate(s1,nw)
ss=replicate(s1,nw,nf)
niv=n_elements(iiv)
volt=fltarr(nf)
window,2,xs=500,ys=700
x0=0.15 & 	x1=0.95
y0=0.12 &	y1=0.95
nplt=4
dy1=(y1-y0)/nplt
xrange=[400,1000]
blank=replicate(' ',10)
chars=1.2
for k=0,nf-1 do begin
	file=files[k]
	ID=strmid(file,0,3)
	ip=strpos(file,'_v')
;	volt[k]=float(strmid(file,ip+2,5))/1000.
	restore_mmsp,file,wl,p,dat,wbin=wbin

	mmsp_mueller,th1,th2,dat,c,MM,fit=fitv

	for j=0,niv-1 do begin
		s[iiv[j]]=mmdecmp(MM[*,*,iiv[j]])
	endfor
	if ID eq 'LC1' then	ii=where(s.vr[1] lt 0., count) $
	else	ii=where(s.vr[1] gt 0., count)
	if count ne 0 then begin
		s[ii].vr=-s[ii].vr
		s[ii].ret=2*!pi-s[ii].ret
	endif
	ss[*,k]=s
	ret=s.ret/!pi*180.
	ang=atan(s.vr[1],s.vr[0])/2/!pi*180.

	Vr2=s.vr^2
	th=atan(sqrt(Vr2[2,*]),sqrt(Vr2[0,*]+Vr2[1,*]))/!pi*180
	;------------------------------------------------------------
	yp=y0+dy1*3
	plot,wl,s.tu,pos=[x0,yp,x1,yp+dy1-0.005],xrange=xrange,xstyle=1,xtickname=blank, $
		ytitle='transmission',yrange=[0,1.2],charsize=chars,title=file
	oplot,xrange,[1,1],line=1
	yp=y0+dy1*2
	plot,wl,ret,/noerase,pos=[x0,yp,x1,yp+dy1-0.005],xrange=xrange,xstyle=1,xtickname=blank, $
		ytitle='retardation',charsize=chars,ystyle=1,yticks=4,yrange=[0,360.]
	yp=y0+dy1*1
	plot,wl,ang,/noerase,pos=[x0,yp,x1,yp+dy1-0.005],xrange=xrange,xstyle=1,xtickname=blank, $
		ytitle='r-ax [deg.]',charsize=chars;,yrange=median(ang)+2*[-1,1]
	oplot,wl,th,line=1
	yp=y0+dy1*0
	plot,wl,s.dia,/noerase,pos=[x0,yp,x1,yp+dy1-0.005],xrange=xrange,xstyle=1,xtitle='wavelength [nm]', $
		ytitle='diattenuation',charsize=chars,yrange=[0,0.1]

endfor

stop



;----   arrange rets[*,*]. ----
rets=ss.ret/!pi*180.
angs=atan(ss.vr[1],ss.vr[0])/2/!pi*180.
Vr2=ss.vr^2
ths=atan(sqrt(Vr2[2,*,*]),sqrt(Vr2[0,*,*]+Vr2[1,*,*]))/!pi*180.

	yp=y0+dy1*3
	plot,wl,ss[*,0].tu,pos=[x0,yp,x1,yp+dy1-0.005],xrange=xrange,xstyle=1,xtickname=blank, $
		ytitle='transmission',yrange=[0,1.2],charsize=chars,title=file
	for i=1,nf-1 do oplot,wl,ss[*,i].tu,line=i
	oplot,xrange,[1,1],line=1
	yp=y0+dy1*2
	plot,wl,rets[*,0],/noerase,pos=[x0,yp,x1,yp+dy1-0.005],xrange=xrange,xstyle=1,xtickname=blank, $
		ytitle='retardation',charsize=chars,ystyle=1,yticks=4,yrange=[0,360.]
	for i=1,nf-1 do oplot,wl,rets[*,i],line=i
	yp=y0+dy1*1
	plot,wl,angs[*,0],/noerase,pos=[x0,yp,x1,yp+dy1-0.005],xrange=xrange,xstyle=1,xtickname=blank, $
		ytitle='r-ax [deg.]',charsize=chars;,yrange=median(ang)+2*[-1,1]
	for i=1,nf-1 do oplot,wl,angs[*,i],line=i
;	oplot,wl,th,line=1
	yp=y0+dy1*0
	plot,wl,ss[*,0].dia,/noerase,pos=[x0,yp,x1,yp+dy1-0.005],xrange=xrange,xstyle=1,xtitle='wavelength [nm]', $
		ytitle='diattenuation',charsize=chars,yrange=[0,0.1]
	for i=1,nf-1 do oplot,wl,ss[*,i].dia,line=i



end

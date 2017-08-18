;  mmsp_cal_incli.pro
;  calibrate MMSP data for multi measure, incidence angle
;  
;  2011.03.10   k.i.
@mmsplib

;********************************************************************
dir='C:\data\MMSP\20110308\' &	cfile='c20110308_133654.sav'	; clear, 1ms x 100
fnam='LC1_T20_I' &	vv1='_v06000' &	rrange=[0.25,0.3]
dir='C:\data\MMSP\20110309\' &	cfile='c20110309_085144.sav'	; clear, 1ms x 100
fnam='LC2_T20_I' &	vv1='_v06000' &	rrange=[0.31,0.39]
fnam='LC2_T20_I' &	vv1='_v04000' &	rrange=[0.57,0.65]
;fnam='LC2_T20_I' &	vv1='_v02000' &	rrange=[0.81,0.89]
;fnam='LC2_T20_I' &	vv1='_v00000' &	rrange=[0.51,0.59]
;fnam='LC3_T20_I' &	vv1='_v06000' &	rrange=[0.28,0.33]	; no d data
;fnam='WP1_T20_I' &	vv1='' &	rrange=[0.4,0.45]	; no d data


files=fnam+['l15','l10','l05','r00','r05','r10','r15']+'_00'+vv1+'.sav'
files2=fnam+['d15','d10','d05','r00','u05','u10','u15']+'_00'+vv1+'.sav'
xx=[-1.5,-1.,-0.5,0.,0.5,1.0,1.5]
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


;   arrange rets[*,*].
rets=ss.ret/2./!pi
angs=atan(ss.vr[1],ss.vr[0])/2/!pi*180.
Vr2=ss.vr^2
ths=atan(sqrt(Vr2[2,*,*]),sqrt(Vr2[0,*,*]+Vr2[1,*,*]))/!pi*180.

window,0,xs=600,ys=600
window,0,xs=500,ys=500
nw0=n_elements(wl0s)
title='wl='+string(wl0s[0],form='(i4)')
for i=1,nw0-1 do title=title+', '+string(wl0s[i],form='(i4)')
title=title+'nm'
plot,xx,rets[200,*],xtitle='Inc.angl (deg)',ytitle='Retardation (wav)',chars=1.2,/nodata, $
	title=fnam+'  '+vv1+', '+title,yrange=rrange,ystyle=1
for j=0,nw0-1 do begin
	wl0=wl0s[j]
	dmy=min(abs(wl-wl0),iw0)
	oplot,xx,rets[iw0,*],psym=1+j
	oplot,xx,rets[iw0,*],line=j
endfor

stop
;----------------------------  2nd path  -----------------
for k=0,nf-1 do begin
	file=files2[k]
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

endfor


;   arrange rets[*,*].
rets=ss.ret/2./!pi
angs=atan(ss.vr[1],ss.vr[0])/2/!pi*180.
Vr2=ss.vr^2
ths=atan(sqrt(Vr2[2,*,*]),sqrt(Vr2[0,*,*]+Vr2[1,*,*]))/!pi*180.

wset,0
for j=0,nw0-1 do begin
	wl0=wl0s[j]
	dmy=min(abs(wl-wl0),iw0)
	oplot,xx,rets[iw0,*],psym=4
	oplot,xx,rets[iw0,*],line=j
endfor

end

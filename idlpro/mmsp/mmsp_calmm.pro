;  mmsp_calmm.pro
;  calibrate MMSP data for multi measure
;  
;  2012.03.31   k.i.
@mmsplib

;********************************************************************
dir='C:\data\MMSP\20120331\' &	cfile='c20120331_130252.sav'	; clear, 1ms x 100
files='m20120331_'+[ $
	'132621', $.sav	15+16 1/2 for 
	'133506', $	21+22 1/2 for 4A elem.
	'134318', $	26+27 1/2 for 4A tune.
	'134831', $	32+33 1/2 for 2A elem.
	'140006', $	37+38 1/2 for 2A tune
	'140426', $	43+44 1/2 for 1A elem.
	'141125', $	48+49 1/4 for 1A elem. <-- should be 1/2.
	'141718', $	54+55 1/4 for 1A elem. <-- should be 1/2.
	'144202', $	60+61 1/2 for 0.5A elem.
	'144804', $	65+66 1/2 for 0.5A tune.
	'145109'] $	71+72 1/2 for 0.25A elem.
	+'.sav'
files='m20120331_'+[ $
	'133908', $	24+25 1/4 for 4A elem.
	'135200', $	35+36 1/2 for 2A elem. <-- should be 1/4.
	'140819', $	46+47 1/4 for 1A elem.
	'144511', $	63+64 1/4 for 0.5A elem.
	'145345', $	74+75 1/4 for 0.25A elem.
	'151433', $	5+7 1/4 for 8A elem.?
	'151911'] $	6+8 1/4 for 8A elem.?
	+'.sav'

dir='C:\data\MMSP\20120710\' &	cfile='c20120710_103231.sav'	; clear, 6ms x 10
files='m20120710_'+[ $
	'103645', $	1-1
	'103906', $	1-2
	'104115' $	1-3
	] $
	+'.sav'


wl0=656.28
wbin=1

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
iiw=where(wl ge 500 and wl lt 800)
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

	dret=s[iiw].ret-shift(s[iiw].ret,1)
	jj=where(dret gt 0., cn1)
	jj=where(dret lt 0., cn2)
	if cn1 gt cn2 then begin
		s.vr=-s.vr
		s.ret=2*!pi-s.ret
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

stop

ret0=90.
plot,wl,ss[*,0].ret/!pi*180.,xrange=wl0+10*[-1,1],xtitle='wl [nm]', $
	yrange=ret0+20.*[-1,1]
oplot,wl0+10*[-1,1],ret0*[1,1]
for k=0,nf-1 do begin
	oplot,wl,ss[*,k].ret/!pi*180.
	ret1=spline(wl[iiw],ss[iiw,k].ret/!pi*180.,wl0)
	print,files[k],'  ',string(ret1,form='(f8.1)')
endfor

end

;  mmsp_cal.pro
;  2011.01.26   a.n., k.i.
@mmsplib

;********************************************************************
dir='C:\data\MMSP\20101125\'
cfile='c20101125_163957.sav'	; clear, 1ms x 200
file='m20101125_164203.sav'	; DST wp
;cfile='c20101125_170226.sav'	
;file='m20101125_171812.sav'	; Meadowlark versalite
;file='m20101125_170554.sav'	; RM90
;file='m20101125_171307.sav'	; NAL625s
file='m20101125_171530.sav'	; sheet pol 1ms x 200

;dir='C:\data\MMSP\20101127\'
;cfile='c20101127_124005.sav'	; clear, 1ms x 200
;file='m20101127_124359.sav'	; DST wp

dir='C:\data\MMSP\20110126\'
cfile='c20110126_105636.sav'	; clear, 1ms x 100
file='m20110126_110010.sav'	; DST wp

dir='C:\data\MMSP\20110203\'
cfile='c20110203_101558.sav'	; clear, 1ms x 100
file='m20110203_102652.sav'	; DST wp

dir='C:\data\MMSP\20110207\'
cfile='c20110207_171428.sav'	; clear, 1ms x 100
file='m20110207_171630.sav'	; Meadowlark LC

dir='C:\data\MMSP\20110216\'
cfile='c20110216_113425.sav'	; clear, 1ms x 100
file='LC1_v00000.sav'		; AROptics LCVR

;-----------------------------------------------------------------
rad=!pi/180.

restore,dir+cfile	; p,wl[*],dat[*,*]
cdat=dat
restore,dir+file	; p,wl[*],dat[*,*]
imgsize,cdat,nw,nn
th1=dindgen(nn)*p.dth1
th2=th1/5.

wbin=4
nw=nw/wbin
wl=rebin(wl,nw)
cdat=rebin(cdat,nw,nn)
dat=rebin(dat,nw,nn)

;----  waveplate calibration  ----
mmsp_calwps,th1,th2,cdat,c,verb=0,thresh=1000.

iiv=where(c.valid eq 1)
stretch,255,0
ofm1=mean(c[iiv].offset1)
ofm2=mean(c[iiv].offset2)
if 0 then begin
	window,xs=700,ys=750
	plot,wl[iiv],c[iiv].ret1,ytitle='retardation [deg.]',xtickname=replicate(' ',10), $
		charsize=1.5,title='MMSP waveplates',yrange=[80,140],pos=[0.15,0.5,0.95,0.9]
	oplot,wl[iiv],c[iiv].ret2,line=2
	plot,wl[iiv],c[iiv].offset1-ofm1,xtitle='wavelength [nm]',ytitle='axis [deg.]', $
		charsize=1.5,yrange=15.*[-1,1],pos=[0.15,0.1,0.95,0.497],/noerase
	oplot,wl[iiv],c[iiv].offset2-ofm2,line=2
endif

;--------------  Mueller matrix  ----

mmsp_mueller,th1,th2,dat,c,MM,fit=fitv

s1=mmdecmp(MM[*,*,nw/2])
s=replicate(s1,nw)
niv=n_elements(iiv)
for j=0,niv-1 do begin
	s[iiv[j]]=mmdecmp(MM[*,*,iiv[j]])
endfor

ii=where(s.vr[1] lt 0., count)
if count ne 0 then begin
	s[ii].vr=-s[ii].vr
	s[ii].ret=2*!pi-s[ii].ret
endif

ret=s.ret/!pi*180.
ang=atan(s.vr[1],s.vr[0])/2/!pi*180.

Vr2=s.vr^2
th=atan(sqrt(Vr2[2,*]),sqrt(Vr2[0,*]+Vr2[1,*]))/!pi*180

;------------------------------------------------------------
window,2,xs=600,ys=790
x0=0.15 & 	x1=0.95
y0=0.12 &	y1=0.95
nn=4
dy1=(y1-y0)/nn
xrange=[200,1200]
xrange=[400,1000]
blank=replicate(' ',10)
yp=y0+dy1*3
chars=1.5
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

;xtitle='wavelength',ytitle='transmission',yrange=[0,1.2]



i0=400
print,mm[*,*,i0],form='(4f10.4)'

stop
window,3
plot,dat[i0,*],psym=2
oplot,fitv[i0,*]

stop

;------  misc plot  ------
surface,dat,wl,th1,charsize=2,xtitle='wavelength [nm]',ytitle='!7h!31'


end

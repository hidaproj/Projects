;  mmsp_callcvr.pro
;  calibrate MMSP data for LCVR
;  
;  2011.03.06   k.i.
;  2011.04.05   k.i.,  rotMM
;  2012.03.31   k.i.,	for 32mm LC

@mmsplib

;********************************************************************
o = mmsp_calctl()
dir0='C:\data\MMSP\'
dir=dir0+'20110308\' &	cfile='c20110308_133654.sav'	; clear, 1ms x 100
fnam='LC1_T20_Ip00_v' &		o.rotang=-45.24	; <-- get from mmsp_cal.pro with ax=0
;fnam='LC2_T20_Ip00_v' &		o.rotang=-45.06
;dir=dir0+'20110309\' &	cfile='c20110309_173055.sav'	; clear, 1ms x 100
;fnam='LC2_T30_Ip00_00_v'
;dir=dir0+'20110310\' &	cfile='c20110310_104259.sav'	; clear, 1ms x 100
;fnam='LC2_T10_Ip00_00_v'

dir=dir0+'20110729\'	; 1ms x 100
cfile='c20110729_175224.sav' &	fnam='LC2_T25_Ip00_A0'	; LC2 1atm
cfile='c20110729_175002.sav' &	fnam='LC2_T25_Ip00_V0' 	; LC2 vac


dir=dir0+'20110730\'
cfile='c20110730_111627.sav' &	fnam='LC1_T25_Ip00_A0'	; LC1 1atm
cfile='c20110730_112941.sav' &	fnam='LC1_T25_Ip00_V0'	; LC1 vac
flip=1

cfile='c20110730_160454.sav' &	fnam='LC3_T25_Ip00_A1'	; LC3 1atm
;cfile='c20110730_161539.sav' &	fnam='LC3_T25_Ip00_V0'	; LC3 vac
flip=0

dir=dir0+'20120331\'	; 1ms x 100
cfile='c20120331_161816.sav' &	fnam='LC1_T20_Ip00_A1_v'	; LC1 1atm
flip=1 &	retmax=6

dir=dir0+'20120411\'	; 6ms x 10
cfile='c20120411_100024.sav' &	fnam='LC3_T20_Ip00_01_v'	; LC4 1atm
flip=1 &	retmax=6

dir=dir0+'20120412\'	; 6ms x 10
cfile='c20120412_140113.sav' &	fnam='LC5_T30_Ip00_01_v'	; LC1 1atm
flip=1 &	retmax=6

;dir=dir0+'20121230\'	; 6ms x 10
;cfile='c20121230_131043.sav' &	fnam='LC6_T45_Ip00_02_v'	; LC1 1atm
;flip=1 &	retmax=6

;-----------------------------------------------------------------
o.mmdir= 'C:\data\MMSP\mm\tf_bbmelm'
o.dir0=dir0
o.dir=dir
o.ax=0.
o.file=fnam

wl0s=[525,656,855,1083]	; wavelength to plot ret. vs. Volt
;-----------------------------------------------------------------

;goto,rmap

rad=!pi/180.
cd,dir
files=findfile(fnam+'*.sav')	; 
nf=n_elements(files)

restore,dir+cfile	; p,wl[*],dat[*,*]
cdat=dat
imgsize,cdat,nw,nn
th1=dindgen(nn)*p.dth1
th2=th1/5.

wbin=2
nw=nw/wbin
wl=rebin(wl,nw)
cdat=rebin(cdat,nw,nn)

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

window,2,xs=500,ys=700
x0=0.15 & 	x1=0.95
y0=0.12 &	y1=0.95
nplt=4
dy1=(y1-y0)/nplt
xrange=[200,1200]
xrange=[400,1100]
blank=replicate(' ',10)
chars=1.2
s1=mmdecmp(emat(4))
ss=replicate(s1,nw,nf)
MM=dblarr(4,4,nw,nf)
niv=n_elements(iiv)
volt=fltarr(nf)
restore,files[0]
ps=replicate(p,nf)
ID=strmid(fnam,0,3)
for k=0,nf-1 do begin
	file=files[k]
	ip=strpos(file,'_v')
	volt[k]=float(strmid(file,ip+2,5))/1000.
	restore,file	; p,wl[*],dat[*,*]
	wl=rebin(wl,nw)
	dat=rebin(dat,nw,nn)
	ps[k]=p

	mmsp_mueller,th1,th2,dat,c,MM1,fit=fitv
	MM[*,*,*,k]=MM1

;	if ID eq 'LC1' then flip=0 else flip=1
	s=mmdecmp2(MM1,iiv=where(c.valid eq 1), flip=flip)
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

	;xtitle='wavelength',ytitle='transmission',yrange=[0,1.2]
	if volt[k] eq 1.6 then win2gif,'c:\tmp\lcv.gif'
endfor

;   arrange rets[*,*].
rets=ss.ret/2./!pi
angs=atan(ss.vr[1],ss.vr[0])/2/!pi*180.
Vr2=ss.vr^2
ths=atan(sqrt(Vr2[2,*,*]),sqrt(Vr2[0,*,*]+Vr2[1,*,*]))/!pi*180.

for j=0,niv-1 do begin
	iw=iiv[j]
	ret1=rets[iw,*]
	for i=nf-2,0,-1 do if ret1[i] lt ret1[i+1] then ret1[i]=ret1[i]+1.
	for i=nf-2,0,-1 do if ret1[i] lt ret1[i+1] then ret1[i]=ret1[i]+1.
	for i=nf-2,0,-1 do if ret1[i] lt ret1[i+1] then ret1[i]=ret1[i]+1.
	for i=nf-2,0,-1 do if ret1[i] lt ret1[i+1] then ret1[i]=ret1[i]+1.
	for i=nf-2,0,-1 do if ret1[i] lt ret1[i+1] then ret1[i]=ret1[i]+1.
	rets[iw,*]=ret1
endfor

window,0,xs=600,ys=750
nw0=n_elements(wl0s)
title='wl='+string(wl0s[0],form='(i4)')
for i=1,nw0-1 do title=title+', '+string(wl0s[i],form='(i4)')
title=title+'nm'
;plot,volt,rets[200,*],yrange=[0,3.],xtitle='Volt',ytitle='Retardation (wav)',chars=1.2,/nodata, $
;	title=title
plot,volt,rets[200,*],pos=[0.12,0.35,0.95,0.9],yrange=[0,retmax],ytitle='Retardation (wav)', $
	chars=1.2,/nodata,title=title,xtickname=replicate(' ',20)
for j=0,nw0-1 do begin
	wl0=wl0s[j]
	dmy=min(abs(wl-wl0),iw0)
	oplot,volt,rets[iw0,*],psym=1+j
	oplot,volt,rets[iw0,*],line=j
endfor

dmy=min(abs(wl-wl0s[0]),iw0)
ang0=mean(angs[iw0,*])
plot,volt,angs[200,*],pos=[0.12,0.1,0.95,0.345],yrange=ang0+5*[-1,1],xtitle='Volt',ytitle='Orientation (deg)', $
	chars=1.2,/nodata,/noerase
for j=0,nw0-1 do begin
	wl0=wl0s[j]
	dmy=min(abs(wl-wl0),iw0)
	oplot,volt,angs[iw0,*],psym=1+j
	oplot,volt,angs[iw0,*],line=j
endfor

win2gif,'c:\tmp\lcvv.gif'


stop

rmap:
;---------------------------------------------------------------------------
;  ret on (V,wl) plane
dv2=0.02	; delta-volt
wl1=400. &	wl2=1100.

nv2=fix(max(volt)/dv2)+1
volt2=findgen(nv2)/nv2*max(volt)
ret2=fltarr(nw,nv2)
for i=0,nw-1 do begin
	ret2[i,*]=spline(volt,rets[i,*],volt2)
endfor
ii=where(wl ge wl1 and wl le wl2)
wlp=wl[ii]
ret2p=rotate(ret2[ii,*],4)
imgsize,ret2p,nvp,nwp
dx0=100 &	dy0=70
pos=[dx0,dy0,dx0+nvp,dy0+nwp]
window,3,xs=nvp+dx0+10,ys=nwp+dy0+50
loadct,2
stretch,255,0
tvscl,alog(ret2p),dx0,dy0
plot,volt2,wlp,pos=pos,/noerase,/nodata,/dev,xtitle='Volt',ytitle='wl [nm]', $
	xrange=[0,max(volt2)],xstyle=1,yrange=[wl1,wl2],ystyle=1, $
	title=fnam,charsize=1.5
blank=replicate(' ',10)
levels=findgen(20)*0.25
c_anno=string(levels,form='(f4.2)')
contour,ret2p,pos=pos,level=levels,c_anno=c_anno,/spline,/dev,/noerase, $
	xstyle=1+4,ystyle=1+4,charsize=1.5


stop

MM=rotmm(MM,o.rotang*rad)
outf=fnam+'_MM.sav'
outfile=dialog_pickfile(path=o.mmdir,file=outf,title='out MM file')
save,p,c,o,wl,volt,MM,file=outfile

stop

;------  misc plot  ------
surface,dat,wl,th1,charsize=2,xtitle='wavelength [nm]',ytitle='!7h!31'


end

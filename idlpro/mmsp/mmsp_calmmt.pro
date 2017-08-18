;  mmsp_calmmt.pro
;  calibrate MMSP data for multi measure, dependence on T
;  
;  2013.06.25   k.i.
@mmsplib
@pollib

;********************************************************************
dir='C:\data\MMSP\20130624\' &	cfile='c20130624_163504.sav'
dir='C:\data\MMSP\20130707\' &	cfile='c20130624_163504.sav'
;dir='C:\data\MMSP\20131006\' &	cfile='c20131006_110824.sav'
;T0=[18,18,25,28.5,30.5,26,22.5,20]	; T(C) for 2013.10.06
dir='C:\data\MMSP\20131230\' &	cfile='c20131013_132613.sav'
dir='C:\data\MMSP\20150131\' &	cfile='c20141031_095201.sav'
;dir='C:\data\MMSP\20140411 U-fiber-1\' &	cfile='c20140301_141047.sav'
T0=''

files=findfile(dir+'m*.sav')
;files=files[1:6] & T0=T0[1:6]	; for 20131006
;files=files[5:n_elements(files)-1]	; for 20140411


wl0s=[525.,854.2, 1083.0]
wbin=4

rad=!pi/180.
cd,dir
nf=n_elements(files)
nw0=n_elements(wl0s)

restore_mmsp,dir+cfile,wl,p,cdat,wbin=wbin
imgsize,cdat,nw,nn
th1=dindgen(nn)*p.dth1
th2=th1/5.

;----  waveplate calibration  ----
mmsp_calwps,th1,th2,cdat,c,verb=0,thresh=1000.

iiv=where(c.valid eq 1)
iiw=where(wl ge 500 and wl lt 1100)
stretch,255,0
ofm1=mean(c[iiv].offset1)
ofm2=mean(c[iiv].offset2)

;--------------  Mueller matrix  ----
window,xs=600,ys=600
MM=fltarr(4,4,nw,nf)
ps=replicate(p,nf)
temp=fltarr(nf)
MMt=fltarr(4,4,nf,nw0)
for k=0,nf-1 do begin
	file=files[k]
	restore_mmsp,file,wl,p,dat,wbin=wbin
	mmsp_mueller,th1,th2,dat,c,MM1,fit=fitv
	mmplot,wl,mm1,chs=chars,title=file,xrange=[500,1100]
	MM[*,*,*,k]=MM1
	ps[k]=p
	;--- 
	tt=float(strsep(p.temp,sep=','))
	temp[k]=total(tt)/n_elements(tt)
;	temp[k]=T0[k]
	for j=0,3 do begin
	for i=0,3 do begin
		MMt[i,j,k,*]=interpol(MM1[i,j,*],wl,wl0s)
	endfor
	endfor
endfor


omat=m_limit(eps=0.0001, a=0.05, plmax=0.04, pcmax=0.1)	; SUVIT

for iw=0,2 do begin
for k=0,nf-1 do begin
	mmt[*,*,k,iw]=mmt[*,*,k,iw]/mmt[0,0,k,iw]
endfor
endfor

iw=1
xr=[15,35] &	tc=16.
xr=[10,30] &	tc=28.
mmplot,temp,mmt[*,*,*,iw],xtitle='Temp. [C]',psym=4,$
	title='wl='+string(wl0s[iw],form='(f6.1)')+'nm',chs=1.2,xrange=xr


stop

;--- 
tols=[0.0527,  0.0500,  0.0033,  0.0033] ; SUVIT tolerance for first row
window,2,xs=1200,ys=700
!p.multi=[0,4,2]
psym=[4,2,1]

for m=0,1 do begin
    for l=0,3 do begin
	yr=0.1*[-1,1]
	if m eq 0 and l eq 0 then yr=[0.2,1.0]
	if m eq 0 and (l eq 2 or l eq 3) then yr=0.04*[-1,1]
	if m eq 1 and (l eq 2 or l eq 3) then yr=0.2*[-1,1]
	if m eq 1 and l eq 1 then yr=[0.2,1.0]
	plot,temp,mmt[l,m,*,0],xtitle='T [C]',yrange=yr,chars=2,xr=xr, $
		xmargin=[5,3],/nodata
	for iw=0,2 do begin
		oplot,temp,mmt[l,m,*,iw],psym=psym[iw]
		oplot,temp,mmt[l,m,*,iw],line=iw
	endfor
	oplot,tc*[1,1],mean(yr)+tols[l]*[-1,1]
	oplot,tc+0.5*[-1,1],mean(yr)+tols[l]*[1,1]
	oplot,tc+0.5*[-1,1],mean(yr)-tols[l]*[1,1]
    endfor
endfor

!p.multi=[0,1,1]

end

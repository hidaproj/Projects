;  mmsplib.pro
;  2010.10.26	k.i.
;  2010.12.10	anan
;  2011.01.26   a.n., k.i.
;  2011.03.11   k.i.	restore_mmsp
;  2011.05.04   k.i.	bug
;  2012.04.08   k.i., m.h.	mmsp_param(), get_temp()
;  2015.04.21   t.a.	mmip_calwps, mmip_mueller, ip keyword in mmdecmp2
;  2016.07.19   k.i.	mmsp_mueller - pg90 keyword

;********************************************************************
pro sinfit,th,dat,av=av,a1=a1,b1=b1,amp=amp,ph=ph,n=n,fit=fit
;  dat(th) = av+a*sin(n*th)+b*cos(n*th) = av+amp*sin(n*th-ph)
;  n: # of wave
;  th: in radian
;  cosph: is set, ph is cos(n*th-ph)
if not keyword_set(n) then n=1

nn=n_elements(dat)
av=total(dat)/nn
dat0=dat-av
a1=total(dat0*sin(n*th))/total(sin(n*th)^2)
b1=total(dat0*cos(n*th))/total(cos(n*th)^2)
amp=sqrt(a1^2+b1^2)
ph=-atan(b1,a1)
fit=av+amp*sin(n*th-ph)

end

;***************************************************************************
pro mmsp_calwp1,th1,th2,Im0,ret1,ret2,offset1,offset2,fit=fitv,c=c,thp=thp, $
	l0=l0,verb=verb
;   calib. WP-1 and 2
;   th1[*],th2[*] -- angle of WP1 and 2 from origin sensor 
;   Im0[*]   -- dark subtracted, but non-calibrated intensity
;   ret1 & 2   -- retardations in deg.
;   offset1 & 2 -- offset angle of waveplates from orogin
;   verb  -- if set, plot fitting

if not keyword_set(verb) then verb=0
rad=!pi/180.
av = mean(Im0)
Im=Im0-av
sinfit,th1*rad,Im,n=4,amp=amp1,ph=ph1,fit=fit1
sinfit,th2*rad,Im,n=4,amp=amp2,ph=ph2,fit=fit2
sinfit,(th1-th2)*rad,Im,n=4,amp=ampd,ph=phd,fit=fitd
sinfit,(th1-th2)*rad,Im,n=2,amp=ampd2,ph=phd2,fit=fitd2
sinfit,(th1+th2)*rad,Im,n=2,amp=amps2,ph=phs2,fit=fits2
;--- sin phase to cos phase
ph1=ph1/rad+90. &	ph2=ph2/rad+90.
phd=phd/rad+90. &	phd2=phd2/rad+90. &	phs2=phs2/rad+90.

fitv=av+fit1+fit2+fitd+fitd2+fits2

r1=amp2/ampd &	cd1=(r1-1.)/(r1+1.) &	ret1=acos(cd1)/rad
r2=amp1/ampd &	cd2=(r2-1.)/(r2+1.) &	ret2=acos(cd2)/rad

dth1=(phd+ph2)/4.
thp=(ph1-ph2-phd)/2.
dth2=(ph2-2.*thp)/4.
;plot,th1,Im,title=string(ph1)+ string(thp)
;oplot,th1,fit1,line=1
;print,ph1,ph2,phd,dth1,dth2
;if dth2 lt 90 then stop
;wait,0.1

if verb then begin
	print,'ret1, ret2 =',ret1,ret2
	print,'dth1,dth2,thp=',dth1,dth2,thp
	plot,th1,Im0,psym=4,xtickv=360*indgen(6),xticks=5,xtitle='!7h!3-1'
	oplot,th1,fitv
	wait,0.01
endif
;l0=av/(1.+(1+cd1)*(1.+cd2)*cos(thp*rad)/4.)*2.
l0=av/(1.+(1.+cd1)*(1.+cd2)*cos(2.*thp*rad)/4.)*4.            ;anan

offset1=dth1
offset2=dth2
if keyword_set(c) then begin
	c.ret1=ret1 &	c.offset1=offset1
	c.ret2=ret2 &	c.offset2=offset2
	c.thp=thp &	c.l0=l0
endif


end

;********************************************************************
pro mmsp_calwps,th1,th2,dat,c,verb=verb,thresh=thresh
;  th1[*],th2[*] -- angle of WP1 and 2 from origin sensor, deg. 
;  dat[nw,*]	- MMSP measurement
;  c[nw]	- return results
;  thresh	- threshhold to make fitting, max intensity

if not keyword_set(verb) then verb=0
;if not keyword_set(threch) then thresh=1000.
if not keyword_set(thresh) then thresh=1000.		;ichimotosanhe
imgsize,dat,nw,nn
c1= {MMSP_CAL, $
	valid:		0, 	$ ; 1 or 0, if light level < thresh then valid=0
	l0:		0.d,	$ ; light level, 'c' in ppt
	ret1:		0.d,	$ ; retardation of WP-1 (deg.)
	offset1:	0.d,	$ ; offset angle of WP-1 ( " )
	ret2:		0.d,	$ ; retardation of WP-2 (deg.)
	offset2:	0.d,	$ ; offset angle of WP-2 ( " )
	thp:		0.d	$ ; offset angle of 1-st polarizer
	}
c=replicate(c1,nw)

for i=0,nw-1 do begin
	Im=transpose(dat[i,*])
	if max(Im) gt thresh then begin
		mmsp_calwp1,th1,th2,Im,c=c1,verb=verb
		c[i]=c1
		c[i].valid=1
	endif else begin
		c[i].valid=0
	endelse
endfor

end
;********************************************************************
pro mmip_calwps,th1,th2,dat,c,verb=verb,thresh=thresh
;  th1[*],th2[*] -- angle of WP1 and 2 from origin sensor, deg. 
;  dat[nx,ny,*]	- MMSP measurement
;  c		- return results
;  thresh	- threshhold to make fitting, max intensity

if not keyword_set(verb) then verb=0
if not keyword_set(thresh) then thresh=1000.
imgsize,dat,nx,ny,nn
c1= {MMSP_CAL, $
	valid:		0, 	$ ; 1 or 0, if light level < thresh then valid=0
	l0:		0.d,	$ ; light level, 'c' in ppt
	ret1:		0.d,	$ ; retardation of WP-1 (deg.)
	offset1:	0.d,	$ ; offset angle of WP-1 ( " )
	ret2:		0.d,	$ ; retardation of WP-2 (deg.)
	offset2:	0.d,	$ ; offset angle of WP-2 ( " )
	thp:		0.d	$ ; offset angle of 1-st polarizer
	}
c=replicate(c1,nx,ny)

for i=0,nx-1 do begin
	for j=0,ny-1 do begin
		Im=transpose(dat[i,j,*])
		if max(Im) gt thresh then begin
			mmsp_calwp1,th1,th2,Im,c=c1,verb=verb
			c[i,j]=c1
			c[i,j].valid=1
		endif else begin
			c[i,j].valid=0
		endelse
	endfor
endfor
end
;****************************************************************
function Cmat,d1,t1,d2,t2,tp=tp,a=a,b=b

;  (1,1,0,0) -> WP2(d2,t2) -> M -> WP1(d1,t1) -> PL(0) -> I_out
;  I_out = SUM_ij [ c_ij*M(i,j) ]
;  d1,d2  -- retardation of WP1,WP2
;  t1,t2  -- angle of WP1,WP2
;  tp     -- angle of incidence polarizer,  all in rad
;  a,b    -- a-sensor (PL(0)), b-sensor (PL(Pi/2))

if not keyword_set(tp) then tp=0.

    cd1=Cos(double(d1)) &	sd1=Sin(double(d1))
    ct1=Cos(double(2*t1)) &	st1=Sin(double(2*t1))
    cd2=Cos(double(d2)) &	sd2=Sin(double(d2))
    ct2=Cos(double(2*t2)) &	st2=Sin(double(2*t2))
    ctp=Cos(double(2*tp)) &	stp=Sin(double(2*tp))

  nn=n_elements(t1)

  c11 = replicate(1.,nn)
  c12 = (ct2^2 + cd2*st2^2)*ctp + (ct2*st2 - cd2*ct2*st2)*stp
  c13 = (ct2*st2 - cd2*ct2*st2)*ctp + (cd2*ct2^2 + st2^2)*stp
  c14 = sd2*st2*ctp - ct2*sd2*stp
  c21 = (ct1^2 + cd1*st1^2) 
  c22 = (ct1^2 + cd1*st1^2)*(ct2^2 + cd2*st2^2)*ctp + (ct1^2 + cd1*st1^2)*(ct2*st2 - cd2*ct2*st2)*stp
  c23 = (ct1^2 + cd1*st1^2)*(ct2*st2 - cd2*ct2*st2)*ctp + (ct1^2 + cd1*st1^2)*(cd2*ct2^2 + st2^2)*stp
  c24 = sd2*(ct1^2 + cd1*st1^2)*st2*ctp - ct2*sd2*(ct1^2 + cd1*st1^2)*stp
  c31 = ct1*st1 - cd1*ct1*st1 
  c32 = (ct1*st1 - cd1*ct1*st1)*(ct2^2 + cd2*st2^2)*ctp + (ct1*st1 - cd1*ct1*st1)*(ct2*st2 - cd2*ct2*st2)*stp
  c33 = (ct1*st1 - cd1*ct1*st1)*(ct2*st2 - cd2*ct2*st2)*ctp + (ct1*st1 - cd1*ct1*st1)*(cd2*ct2^2 + st2^2)*stp
  c34 = sd2*(ct1*st1 - cd1*ct1*st1)*st2*ctp - ct2*sd2*(ct1*st1 - cd1*ct1*st1)*stp
  c41 = -sd1*st1
  c42 = -sd1*st1*(ct2^2 + cd2*st2^2)*ctp - sd1*st1*(ct2*st2 - cd2*ct2*st2)*stp
  c43 = -sd1*st1*(ct2*st2 - cd2*ct2*st2)*ctp - sd1*st1*(cd2*ct2^2 + st2^2)*stp
  c44 = -sd1*sd2*st1*st2*ctp + ct2*sd1*sd2*st1*stp

if keyword_set(b) then begin
  c21=-c21 &	c22=-c22 &	c23=-c23 &	c24=-c24
  c31=-c31 &	c32=-c32 &	c33=-c33 &	c34=-c34
  c41=-c41 &	c42=-c42 &	c43=-c43 &	c44=-c44
endif

   C=[[c11], [c12], [c13], [c14], $
      [c21], [c22], [c23], [c24], $
      [c31], [c32], [c33], [c34], $
      [c41], [c42], [c43], [c44]]
;C=C/2.
   C=C/4.              ;anan
return,C
end

;***************************************************************************
pro mmsp_mueller1,th1,th2,Im,ret1,ret2,Mul,rm,thp=thp,fit=fitv
;   calib. Mueller matrix
;   th1,th2(*)   --  offset corrected WP angle, th=0 is fast ax. in +Q,deg.
;   ret1,ret2    --  retardation, deg.
;   Im(*)        --  gain corrected intensity
if not keyword_set(thp) then thp=0.

rad=!pi/180.
nn=n_elements(th1)

Cm=Cmat(ret1*rad,th1*rad,ret2*rad,th2*rad,tp=thp*rad)
;Cm=Cmat(ret1*rad,th1*rad,ret2*rad,th2*rad,tp=thp*rad,/b)
Rm=Cm#invert(transpose(Cm)#Cm)
Mv=Im#Rm &	Mul=reform(Mv,4,4)

fitv=Cm#transpose(Mv)

end

;***************************************************************************
pro mmsp_mueller,th1o,th2o,dat,c,MM,fit=fitv,pg90=pg90
;   obtain Mueller matrix
;   th1o,th2o[*]   --  WP angle from origin sensor, deg.
;   dat[nw,*]      --  MMSP data
;   c[nw]   	   --  calibration parameter,  ret1,2, offset1,2, thp
;   MM[4,4,nw]    --  output
;   pg90	   --  if 1, polarization generator (th2) rotate 90deg.

imgsize,dat,nw,nn
mm=dblarr(4,4,nw)
fitv=dblarr(nw,nn)
for i=0,nw-1 do begin
	if c[i].valid then begin
		Im=transpose(dat[i,*])/c[i].l0
		thp=c[i].thp
		th1=th1o-c[i].offset1
		th2=th2o-c[i].offset2
		ret1=c[i].ret1
		ret2=c[i].ret2
		ret2=-ret2
		if keyword_set(pg90) then begin
			th2=th2+90.
			;print,'pg90'
		endif
		mmsp_mueller1,th1,th2,Im,ret1,ret2,M1,rm,thp=thp,fit=fit1
                ;if i eq 500 then print,rm,form='(80f6.2)'
		MM[*,*,i]=M1
		fitv[i,*]=fit1*c[i].l0
	endif
endfor


end
;***************************************************************************
pro mmip_mueller,th1o,th2o,dat,c,MM,fit=fitv
;   obtain Mueller matrix
;   th1o,th2o[*]   --  WP angle from origin sensor, deg.
;   dat[nw,*]      --  MMSP data
;   c[nw]   	   --  calibration parameter,  ret1,2, offset1,2, thp
;   MM[4,4,nw]    --  output

imgsize,dat,nx,ny,nn
mm=dblarr(4,4,nx,ny)
fitv=dblarr(nx,ny,nn)
for i=0,nx-1 do begin
	for j=0,ny-1 do begin
		if c[i,j].valid then begin
			Im=transpose(dat[i,j,*])/c[i,j].l0
			thp=c[i,j].thp
			th1=th1o-c[i,j].offset1
			th2=th2o-c[i,j].offset2
			ret1=c[i,j].ret1
			ret2=c[i,j].ret2
			mmsp_mueller1,th1,th2,Im,ret1,ret2,M1,rm,thp=thp,fit=fit1
                	;if i eq 500 then print,rm,form='(80f6.2)'
			MM[*,*,i,j]=M1
			fitv[i,j,*]=fit1*c[i,j].l0
		endif
	endfor
endfor


end
;********************************************************************
function ta_mmsp_cal,dat=dat,cdat,p,wl,thresh=thresh

if not keyword_set(thresh) then thresh=1000.

rad=!pi/180.


imgsize,cdat,nw,nn
th1=dindgen(nn)*p.dth1
th2=th1/5.

wbin=2
wbin=1
nw=nw/wbin
wl=rebin(wl,nw)
cdat=rebin(cdat,nw,nn)

;----  waveplate calibration  ----
mmsp_calwps,th1,th2,cdat,c,verb=0,thresh=thresh

;----------------------------
if nw eq 1 then help,c,/str else begin
	window,xs=700,ys=750
	if 0 then ii=where(c.valid eq 1) else ii=indgen(nw)
	stretch,255,0
	plot,wl[ii],c[ii].ret1,ytitle='retardation [deg.]',xtickname=replicate(' ',10), $
		charsize=1.5,title='MMSP waveplates',yrange=[80,140],pos=[0.15,0.5,0.95,0.9]
	oplot,wl[ii],c[ii].ret2,line=2
	ofm1=mean(c[ii].offset1)
	ofm2=mean(c[ii].offset2)
	plot,wl[ii],c[ii].offset1-ofm1,xtitle='wavelength [nm]',ytitle='axis [deg.]', $
		charsize=1.5,yrange=15.*[-1,1],pos=[0.15,0.1,0.95,0.497],/noerase
	oplot,wl[ii],c[ii].offset2-ofm2,line=2
endelse	
if not keyword_set(dat) then begin
	mm=c
	goto,jump
endif
;----  Mueller matrix of clear  ----
dat=rebin(dat,nw,nn)
mmsp_mueller,th1,th2,dat,c,MM,fit=fitv

jump:

if 0 then begin
    s1=ta_mmdecmp(MM[*,*,500])
    s=replicate(s1,nw)
    for i=0,nw-1 do begin
        s[i]=ta_mmdecmp(MM[*,*,i])
    endfor
    ret=s.ret/!pi*180.
    
    wset,2
    plot,wl,mm[0,0,*],xtitle='wavelength',ytitle='transmission'
    
    print,mm[*,*,200],form='(4f10.4)'
    
;------  misc plot  ------
    surface,dat,wl,th1,charsize=2,xtitle='wavelength [nm]',ytitle='!7h!31'
endif

return,mm

end

;********************************************************************
pro restore_mmsp,file,wl,p,dat,wbin=wbin

restore,file	; p,wl[*],dat[*,*]
imgsize,wl,nw1
imgsize,dat,nw,nn
if nw1 lt nw then wl=[wl,wl[nw1-1]]	; nw1 = 2047 sometime..
if keyword_set(wbin) then begin
	nw=nw/wbin
	wl=rebin(wl,nw)
	dat=rebin(dat,nw,nn)
endif

end
;********************************************************************
function mmsp_calctl

dir0='C:\data\MMSP\'
dir=dir0+'20110308\'
mmdir=dir0+'mm\tf_bbmelm'

o = {Cntl_mmsp_cal, $
	dir0:	dir0, $
	dir:	dir, $		; MMSP original data dir
	mmdir:	mmdir, $	; MM output dir
	cfile:	'', $		; calib. file
	file:	'', $		; MMSP data
	wmin:	400., $		; wavelength min
	wmax:	1000., $	; wavelength min
	wbin:	fix(4), $	; wavelength bin
	wlp:	656.3, $	; wavelength for print
	ax:	float(0.), $	; axis angle after rot, deg.
	rotang:	float(0.) $	; rotation angle, deg.
	}
return,o
end


;********************************************************************
function mmsp_param,pm,spname

expo=1	; msec
integ=100
dth1=22.5
nth=360./dth1*5
date='20'+yymmdd(get_time=time)
time=strmid(time,0,2)+strmid(time,3,2)+strmid(time,6,2)
p = {mmsp_prm, $
	m:		pm, $ ; 
	spectrograph:	spname, $	; 'MayaPro2000',,
	expo:		expo, $	; exsposure, msec
	integ:		integ, $	; exsposure, msec
	dth1:		dth1, $	; angle step of WP
	nth:		nth, $	; # of sampling position
	wbin:		1, $	; wavelength binning
	date:		date, $
	time:		time,$	; hhmmss
	optics:  	'', $	; 'ex. 'LC1'
	ftemp:  	'20', $	; temperature for file name [C]
	incang:  	'p00', $	; incidence angle, deg.
	seqn: 	 	'00', $
	T_mode:		'manual', $	; T-measurement, 'manual', '
	temp:	  	'20', $	; temperature[s], measured
	outdir:  	'c:\data\MMSP\'+date+'\', $
	outfile:	'a.sav' $
	}

return,p
end

;********************************************************************
function mmdecmp2,MM,iiv=iiv0,flip=flip			; 2015.4.20 T.A. comment out
;function mmdecmp2,MM,iiv=iiv,flip=flip,ip=ip			; 2015.4.20 T.A., iiv, ip
; flip

if not keyword_set(ip) then begin				; 2015.4.21 T.A.
	imgsize,MM,n4,n4,nw
	if n_elements(iiv) eq 0 then iiv=indgen(nw) else iiv=iiv0		; 2015.4.20 T.A. comment out
	;if n_elements(iiv) eq 0 then iiv=indgen(nw) else iiv=iiv		; 2015.4.20 T.A.

	s1=mmdecmp(MM[*,*,nw/2])
	s=replicate(s1,nw)
	niv=n_elements(iiv)
	for j=0,niv-1 do begin
		s[iiv[j]]=mmdecmp(MM[*,*,iiv[j]])
	endfor
	if keyword_set(flip) then ii=where(s.vr[1] gt 0., count) else ii=where(s.vr[1] lt 0., count)
	if count ne 0 then begin
		print,'flip in mmdecmp2, count=',count
		s[ii].vr=-s[ii].vr
		s[ii].ret=2*!pi-s[ii].ret
	endif
endif else begin
	imgsize,MM,n4,n4,nx,ny
	if n_elements(iiv) eq 0 then iiv=indgen(nx*ny) else iiv=iiv
	iix=iiv mod nx
	iiy=iiv/nx
	niv=n_elements(iiv)

	s1=mmdecmp(MM[*,*,iix[niv/2],iiy[niv/2]])
	s=replicate(s1,nx,ny)
	j=0l
	while j le niv-1 do begin
		s[iix[j],iiy[j]]=mmdecmp(reform(MM[*,*,iix[j],iiy[j]]))
		j=j+1l
	endwhile
	if keyword_set(flip) then ii=where(s.vr[1] gt 0., count) else ii=where(s.vr[1] lt 0., count)
	if count ne 0 then begin
		print,'flip in mmdecmp2, count=',count
		s[ii mod nx,ii/nx].vr=-s[ii mod nx, ii/nx].vr
		s[ii mod nx,ii/nx].ret=2*!pi-s[ii mod nx,ii/nx].ret
	endif
endelse


return,s
end

;**************************************************************************
@caiolib2
function get_temp,text=text,init=init
;  get Temperature from contec AD
common get_temp_com,p

	if keyword_set(init) then begin	; init contex AD
		p=caio_ctl()
		caio_init,p=p,st=st,dllfile=caiodll
		p.rate=5000	; sampling, Hz
		p.n_rec1=100l	; length of one record
		p.nch=1
		p=caio_pSet(p)
	endif

	caio_start,n_sample=p.n_rec1
	ndat=0
	while ndat lt p.n_rec1 do begin
		ndat=caio_count()
		;st=caio_status(BUSY=busy,START_TRG=START_TRG,DATA_NUM=data_num,OFERR=oferr)
	endwhile
	temps=caio_read(p.nch,p.n_rec1)
	T=reform(rebin(temps,1,p.nch),p.nch)
	if keyword_set(text) then begin
		cT=string(T[0],form='(f7.3)')
		for i=1,p.nch-1 do cT=cT+','+string(T[i],form='(f7.3)')
		T=cT
	endif
return,T
end


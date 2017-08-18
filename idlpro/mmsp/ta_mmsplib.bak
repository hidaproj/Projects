;  ta_mmsplib.pro
;  2010.10.26	k.i.
;  2011.02.17   t.a.
;  2011.03.7   t.a.

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

dth1=(phd+ph2)/4.
thp=(ph1-ph2-phd)/2.
dth2=(ph2-2.*thp)/4.
r1=amp2/ampd &	cd1=(r1-1.)/(r1+1.) &	ret1=acos(cd1)/rad
r2=amp1/ampd &	cd2=(r2-1.)/(r2+1.) &	ret2=acos(cd2)/rad
if (abs(phs2-2.*(dth1+dth2+thp)) ge 90) then ret2=-ret2   ;anan 11.02.17

if verb then begin
	print,'ret1, ret2 =',ret1,ret2
	print,'dth1/rad,dth2,thp=',dth1,dth2,thp
	plot,th1,Im0,psym=4,xtickv=360*indgen(6),xticks=5,xtitle='!7h!3-1'
	oplot,th1,fitv
endif
l0=av/(1.+(1+cd1)*(1.+cd2)*cos(thp*rad)/4.)*2.
l0=av/(1.+(1.+cd1)*(1.+cd2)*cos(2.*thp*rad)/4.)*4.            ;anan

offset1=dth1
offset2=dth2
if keyword_set(c) then begin
    c.ret1=ret1 &	c.offset1=offset1
    c.ret2=ret2 &	c.offset2=offset2
    c.thp=thp &	c.l0=l0
endif

;== fitting error ==;
del     =Im0-fitv
Idc   =replicate(1.,n_elements(Im0))
Idmp  =cos((4.*th1-ph1)*rad)
Idpm  =cos((4.*th2-ph2)*rad)
Idmm  =cos((4.*(th1-th2)-phd)*rad)
Idd2  =cos((2.*(th1-th2)-phd2)*rad)
Ids2  =cos((2.*(th1+th2)-phs2)*rad)
Idph1 =amp1*sin((4.*th1-ph1)*rad)
Idph2 =amp2*sin((4.*th2-ph2)*rad)
Idphd =ampd*sin((4.*(th1-th2)-phd)*rad)
Idphd2=ampd2*sin((2.*(th1-th2)-phd2)*rad)
Idphs2=amps2*sin((2.*(th1+th2)-phs2)*rad)

mat=invert([$
 [total(Idc*Idc),total(Idc*Idmp),total(Idc*Idpm),total(Idc*Idmm),total(Idc*Idd2),total(Idc*Ids2),total(Idc*Idph1),total(Idc*Idph2),total(Idc*Idphd),total(Idc*Idphd2),total(Idc*Idphs2)],$
 [total(Idmp*Idc),total(Idmp*Idmp),total(Idmp*Idpm),total(Idmp*Idmm),total(Idmp*Idd2),total(Idmp*Ids2),total(Idmp*Idph1),total(Idmp*Idph2),total(Idmp*Idphd),total(Idmp*Idphd2),total(Idmp*Idphs2)],$
 [total(Idpm*Idc),total(Idpm*Idmp),total(Idpm*Idpm),total(Idpm*Idmm),total(Idpm*Idd2),total(Idpm*Ids2),total(Idpm*Idph1),total(Idpm*Idph2),total(Idpm*Idphd),total(Idpm*Idphd2),total(Idpm*Idphs2)],$
 [total(Idmm*Idc),total(Idmm*Idmp),total(Idmm*Idpm),total(Idmm*Idmm),total(Idmm*Idd2),total(Idmm*Ids2),total(Idmm*Idph1),total(Idmm*Idph2),total(Idmm*Idphd),total(Idmm*Idphd2),total(Idmm*Idphs2)],$
 [total(Idd2*Idc),total(Idd2*Idmp),total(Idd2*Idpm),total(Idd2*Idmm),total(Idd2*Idd2),total(Idd2*Ids2),total(Idd2*Idph1),total(Idd2*Idph2),total(Idd2*Idphd),total(Idd2*Idphd2),total(Idd2*Idphs2)],$
 [total(Ids2*Idc),total(Ids2*Idmp),total(Ids2*Idpm),total(Ids2*Idmm),total(Ids2*Idd2),total(Ids2*Ids2),total(Ids2*Idph1),total(Ids2*Idph2),total(Ids2*Idphd),total(Ids2*Idphd2),total(Ids2*Idphs2)],$
 [total(Idph1*Idc),total(Idph1*Idmp),total(Idph1*Idpm),total(Idph1*Idmm),total(Idph1*Idd2),total(Idph1*Ids2),total(Idph1*Idph1),total(Idph1*Idph2),total(Idph1*Idphd),total(Idph1*Idphd2),total(Idph1*Idphs2)],$
 [total(Idph2*Idc),total(Idph2*Idmp),total(Idph2*Idpm),total(Idph2*Idmm),total(Idph2*Idd2),total(Idph2*Ids2),total(Idph2*Idph1),total(Idph2*Idph2),total(Idph2*Idphd),total(Idph2*Idphd2),total(Idph2*Idphs2)],$
 [total(Idphd*Idc),total(Idphd*Idmp),total(Idphd*Idpm),total(Idphd*Idmm),total(Idphd*Idd2),total(Idphd*Ids2),total(Idphd*Idph1),total(Idphd*Idph2),total(Idphd*Idphd),total(Idphd*Idphd2),total(Idphd*Idphs2)],$
 [total(Idphd2*Idc),total(Idphd2*Idmp),total(Idphd2*Idpm),total(Idphd2*Idmm),total(Idphd2*Idd2),total(Idphd2*Ids2),total(Idphd2*Idph1),total(Idphd2*Idph2),total(Idphd2*Idphd),total(Idphd2*Idphd2),total(Idphd2*Idphs2)],$
 [total(Idphs2*Idc),total(Idphs2*Idmp),total(Idphs2*Idpm),total(Idphs2*Idmm),total(Idphs2*Idd2),total(Idphs2*Ids2),total(Idphs2*Idph1),total(Idphs2*Idph2),total(Idphs2*Idphd),total(Idphs2*Idphd2),total(Idphs2*Idphs2)]$
])

rmat=-transpose([total(del*Idc),$    ;0
                 total(del*Idmp),$
                 total(del*Idpm),$   ;2
                 total(del*Idmm),$
                 total(del*Idd2),$   ;4
                 total(del*Ids2),$
                 total(del*Idph1),$  ;6
                 total(del*Idph2),$
                 total(del*Idphd),$  ;8
                 total(del*Idphd2),$ 
                 total(del*Idphs2)]) ;10
fiterr=mat ## rmat

;== error of eace parameter==;

if keyword_set(c) then begin
    c.dret1=(-ampd*fiterr[2]+amp2*fiterr[3])/(amp2+ampd)/sqrt(amp2*ampd)/rad
    c.dret2=(-ampd*fiterr[1]+amp1*fiterr[3])/(amp1+ampd)/sqrt(amp1*ampd)/rad
    c.doffset1=(fiterr[8]+fiterr[7])/4./rad
    c.dthp=(fiterr[6]-fiterr[7]-fiterr[8])/2./rad
    c.doffset2=(fiterr[7]-2.*c.dthp)/4./rad
endif

end
;********************************************************************
pro mmsp_calwps,th1,th2,dat,p,c,verb=verb,thresh=thresh,$
                rms=rms                                           ;anan
;  th1[*],th2[*] -- angle of WP1 and 2 from origin sensor, deg. 
;  dat[nw,*]	- MMSP measurement
;  p            - structure of observation parameters
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
	thp:		0.d,	$ ; offset angle of 1-st polarizer
	dret1:		0.d,	$ ; error of retardation of WP-1 (deg.)
	doffset1:	0.d,	$ ; error of offset angle of WP-1 ( " )
	dret2:		0.d,	$ ; error of retardation of WP-2 (deg.)
	doffset2:	0.d,	$ ; error of offset angle of WP-2 ( " )
	dthp:		0.d	$ ; error of offset angle of 1-st polarizer
	}
c=replicate(c1,nw)
rms=fltarr(nw)  ;anan

for i=0,nw-1 do begin
    Im=transpose(dat[i,*])
    if max(Im) gt thresh then begin
;    if (max(Im) gt thresh) and (max(Im) lt p.integ*2.^14) then begin   ;anan 11'03'04
        mmsp_calwp1,th1,th2,Im,c=c1,verb=verb,fit=fit
        c[i]=c1
        c[i].valid=1
        rms[i]=sqrt(total((Im-fit)^2)/n_elements(fit))
    endif else begin
        c[i].valid=0
    endelse
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
Rm=Cm#invert(transpose(Cm)#Cm)
Mv=Im#Rm &	Mul=reform(Mv,4,4)

fitv=Cm#transpose(Mv)

end

;***************************************************************************
pro mmsp_mueller,th1o,th2o,dat,p,c,MM,dMM,fit=fitv
;   obtain Mueller matrix
;   th1o,th2o[*]   --  WP angle from origin sensor, deg.
;   dat[nw,*]      --  MMSP data
;   c[nw]   	   --  calibration parameter,  ret1,2, offset1,2, thp
;   MM[4,4,nw]    --  output

imgsize,dat,nw,nn
mm=dblarr(4,4,nw)
dmm=dblarr(4,4,nw,2)   ; +- of (fitting error & SN) ; anan 2011/02/12
fitv=dblarr(nw,nn)
for i=0,nw-1 do begin
	if c[i].valid then begin
		Im=transpose(dat[i,*])/c[i].l0
		thp=c[i].thp
		th1=th1o-c[i].offset1
		th2=th2o-c[i].offset2
		ret1=c[i].ret1
		ret2=c[i].ret2
		mmsp_mueller1,th1,th2,Im,ret1,ret2,M1,rm,thp=thp,fit=fit1
                ;if i eq 500 then print,rm,form='(80f6.2)'
		MM[*,*,i]=M1
		fitv[i,*]=fit1*c[i].l0

                 ;==anan 2011/02/12
                th1=th1o-(c[i].offset1+c[i].doffset1)
		th2=th2o-(c[i].offset2+c[i].doffset2)
		othp=thp   & thp=c[i].thp+c[i].dthp
		oret1=ret1 & ret1=c[i].ret1+c[i].dret1
		oret2=ret2 & ret2=c[i].ret2+c[i].dret2
		mmsp_mueller1,th1,th2,Im,ret1,ret2,M1,tmp,thp=thp,fit=fit1
		dMM[*,*,i,0]=(M1-MM[*,*,i])>0.
                dMM[*,*,i,1]=(M1-MM[*,*,i])<0.
                sn=1./300./sqrt(p.integ)*(2.^16*p.integ/c[i].l0)^.5
                rm1=total(rm^2,1)
                ilv=1.+(1.+cos(oret1*!dtor))*(1.+cos(oret2*!dtor))*cos(2.*othp*!dtor)/4.
                dMM[*,*,i,0]=abs(sn*ilv*sqrt(reform(rm1,4,4)))>dMM[*,*,i,0]
                dMM[*,*,i,1]=-abs(sn*ilv*sqrt(reform(rm1,4,4)))<dMM[*,*,i,1]
	endif
endfor


end
;*******************************************************************
pro plotresult,wl,mm,dmm,c

loadct,0 & stretch,255,0
nw=n_elements(wl)
ii=where((c.valid eq 1) and $
         (mm[0,0,*] ge 0) and $
         (mm[0,0,*]^2 ge total(mm[1:3,0,*]^2,1)) and $
         (mm[0,0,*]^2 ge total(mm[0,1:3,*]^2,2)) )
ni=n_elements(ii)
wl0=wl & mm0=mm & dmm0=dmm & c0=c
if ni ge 2 then begin
wl=wl[ii] & mm=mm[*,*,ii] & dmm=dmm[*,*,ii,*] & c=c[ii]
endif
nw=n_elements(wl)

;s1=ta_mmdecmp(MM[*,*,nw/2])          ;s1=mmdecmp(MM[*,*,nw/2])
s1=mmdecmp(MM[*,*,nw/2])          ;s1=mmdecmp(MM[*,*,nw/2])
s=replicate(s1,nw)
for j=0,ni-1 do begin
    s[j]=mmdecmp(MM[*,*,j])    ;s[ii[j]]=mmdecmp(MM[*,*,ii[j]])
    ;s[j]=ta_mmdecmp(MM[*,*,j])    ;s[ii[j]]=mmdecmp(MM[*,*,ii[j]])
endfor

chs=0.8
;=== fig1 ==
ang=atan(s.vd[1],s.vd[0])/2/!pi*180.
iim=where(ang-mean(ang)+45. lt 0.)
iip=where(ang-mean(ang)+45. gt 90.)    ;ii=where(ang lt 0)
ang[iim]=ang[iim]+90.
ang[iip]=ang[iip]-90.

window,1,xs=500,ys=500           ;dia
x0=0.12 & 	x1=0.95   &  xc=(x1+x0)/2.
y0=0.1 &	y1=0.95
nn=3
dy1=(y1-y0)/nn
xrange=[200,1200]
blank=replicate(' ',10)
xxx=0.05

yp=y0+dy1*2
plot,wl,s.dia*sqrt(s.vd[0]^2+s.vd[1]^2),$
  pos=[x0,yp,xc-xxx,yp+dy1-0.005],xrange=xrange,xstyle=1,xtickname=blank, $
;  ytitle='linear dia.',yrange=[0,1],charsize=1.2,title=file
  ytitle='linear dia.',yrange=[0,0.1],charsize=chs,title=file
yp=y0+dy1*1
plot,wl,ang>(-90)<90,$
  /noerase,pos=[x0,yp,xc-xxx,yp+dy1-0.005],xrange=xrange,xstyle=1,xtickname=blank, $
  ytitle='d-ax [deg.]',charsize=chs,ystyle=1,yticks=4;,yrange=[-90,90]
yp=y0+dy1*0
plot,wl,s.dia*s.vd[2],$
  /noerase,pos=[x0,yp,xc-xxx,yp+dy1-0.005],xrange=xrange,xstyle=1,xtitle='wavelength [nm]', $
  ytitle='circ. dia.',charsize=chs,yrange=[-1,1]

yp=y0+dy1*2
plot,wl,s.pd[0],$
  /noerase,pos=[xc+xxx,yp,x1,yp+dy1-0.005],xrange=xrange,xstyle=1,xtickname=blank, $
  ytitle='polarizance Q',yrange=[0,1],charsize=chs,title=file
yp=y0+dy1*1
plot,wl,s.pd[1],$
  /noerase,pos=[xc+xxx,yp,x1,yp+dy1-0.005],xrange=xrange,xstyle=1,xtickname=blank, $
  ytitle='polarizance U',charsize=chs,yrange=[0,1],ystyle=1,yticks=4
yp=y0+dy1*0
plot,wl,s.pd[2],$
  /noerase,pos=[xc+xxx,yp,x1,yp+dy1-0.005],xrange=xrange,xstyle=1,xtitle='wavelength [nm]', $
  ytitle='polarizance V',charsize=chs,yrange=[0,1]

;=== fig2 ==
ret=s.ret/!pi*180.

ii=where(s.vr[1] lt 0., count)
if count ne 0 then begin
	s[ii].vr=-s[ii].vr
	s[ii].ret=2*!pi-s[ii].ret
endif
ang=atan(s.vr[1],s.vr[0])/2/!pi*180.
ret=s.ret/!pi*180.
;iim=where(ang-mean(ang)+45. lt 0.)
;iip=where(ang-mean(ang)+45. gt 90.)    ;ii=where(ang lt 0)
;ang[iim]=ang[iim]+90.
;ang[iip]=ang[iip]-90.
;ang[ii]=90.-ang[ii]
;ret[[iim,iip]]=360.-ret[[iim,iip]]                   ;ret[ii]=360.-ret[ii]

window,4,xs=500,ys=500           ;tu,ret
x0=0.12 & 	x1=0.95   &  xc=(x1+x0)/2.
y0=0.1 &	y1=0.95
nn=4
dy1=(y1-y0)/nn
xrange=[200,1200]
blank=replicate(' ',10)
xxx=0.05

yp=y0+dy1*3
plot,wl,s.tu,$
  pos=[x0,yp,xc-xxx,yp+dy1-0.005],xrange=xrange,xstyle=1,xtickname=blank, $
  ytitle='transmission',yrange=[0,1.2],charsize=chs,title=file
oplot,xrange,[1,1],line=1
yp=y0+dy1*2
plot,wl,ret*sqrt(s.vr[0]^2+s.vr[1]^2),$
  /noerase,pos=[x0,yp,xc-xxx,yp+dy1-0.005],xrange=xrange,xstyle=1,xtickname=blank, $
  ytitle='linear ret. [deg.]',charsize=chs,yrange=[0,360],ystyle=1,yticks=4
yp=y0+dy1*1
plot,wl,ang>0<180,$
  /noerase,pos=[x0,yp,xc-xxx,yp+dy1-0.005],xrange=xrange,xstyle=1,xtickname=blank, $
  ytitle='r-ax [deg.]',charsize=chs ;,yrange=median(ang)+2*[-1,1]
yp=y0+dy1*0
plot,wl,s.vr[2]*ret,$
  /noerase,pos=[x0,yp,xc-xxx,yp+dy1-0.005],xrange=xrange,xstyle=1,xtitle='wavelength [nm]', $
  ytitle='circ. ret. [deg.]',charsize=chs,yrange=[-180,180]

;yp=y0+dy1*3
;plot,bwl,replicate(0,n_elements(bwl)),psym=7,yr=[-1,1],$
;  /noerase,pos=[xc+xxx,yp,x1,yp+dy1-0.02],xrange=xrange,xstyle=1,xtickname=blank, $
;  ytitle='bad data for decomp.',charsize=chs,ytickformat='(a1)',ystyle=1,yticks=4
yp=y0+dy1*2
plot,wl,s.dep*s.depv[0],$
  /noerase,pos=[xc+xxx,yp,x1,yp+dy1-0.005],xrange=xrange,xstyle=1,xtickname=blank, $
  ytitle='depolarization Q',charsize=chs,yrange=[0,1],ystyle=1,yticks=4
yp=y0+dy1*1
plot,wl,s.dep*s.depv[1],$
  /noerase,pos=[xc+xxx,yp,x1,yp+dy1-0.005],xrange=xrange,xstyle=1,xtickname=blank, $
  ytitle='depolarization U',charsize=chs ,yrange=[0,1]
yp=y0+dy1*0
plot,wl,s.dep*s.depv[2],$
  /noerase,pos=[xc+xxx,yp,x1,yp+dy1-0.005],xrange=xrange,xstyle=1,xtitle='wavelength [nm]', $
  ytitle='depolarization V',charsize=chs,yrange=[0,1]

;=== fig3 ==
window,3,xs=500,ys=600           ;tu,ret
x0=0.12 & 	xc=0.95
y0=0.1 &	y1=0.95
nn=5
dy1=(y1-y0)/nn
xrange=[200,1200]
blank=replicate(' ',10)
xxx=0.05

yp=y0+dy1*4
plot,wl,s.tu,$
  pos=[x0,yp,xc-xxx,yp+dy1-0.005],xrange=xrange,xstyle=1,xtickname=blank, $
  ytitle='Transmission',yrange=[0,1.2],charsize=chs,title=file
oplot,xrange,[1,1],line=1
yp=y0+dy1*3
plot,wl,ret,$
  /noerase,pos=[x0,yp,xc-xxx,yp+dy1-0.005],xrange=xrange,xstyle=1,xtickname=blank, $
  ytitle='Retardance [deg.]',charsize=chs,yrange=[0,360],ystyle=1,yticks=4
yp=y0+dy1*2
plot,wl,ang>0<180,$
  /noerase,pos=[x0,yp,xc-xxx,yp+dy1-0.005],xrange=xrange,xstyle=1,xtickname=blank, $
  ytitle='r-ax [deg.]',charsize=chs ;,yrange=median(ang)+2*[-1,1]
yp=y0+dy1*1
plot,wl,s.dia,$
  /noerase,pos=[x0,yp,xc-xxx,yp+dy1-0.005],xrange=xrange,xstyle=1,xtickname=blank, $
  ytitle='Diattenuation',yrange=[0,0.1],charsize=chs,title=file
yp=y0
plot,wl,s.dep,$
  /noerase,pos=[x0,yp,xc-xxx,yp+dy1-0.005],xrange=xrange,xstyle=1,xtitle='wavelength [nm]', $
  ytitle='Depolarization',charsize=chs,yrange=[0,1]
;=== fig1 ==
xtitle='wavelength [nm]'
yr=[-1.2,1.2]
xx=[0.12,0.325,0.535,0.745,0.95]
yy=[0.95,0.7375,0.525,0.3125,0.1]
blank=replicate(' ',10)
xtickname=[' ','400','600','800','1000',' ']

xticks=5
window,0,xs=600,ys=600
;!p.multi=[0,4,4]
for i=0,3 do begin
	for j=0,3 do begin
            if (i eq 0) and (j eq 0) then begin
                plot,wl,mm[j,i,*],$
                  pos=[xx[j],yy[i+1],xx[j+1],yy[i]],$
                  xtitle='',xtickname=blank,charsize=chs,line=0,yr=yr,psym=3,xticks=xticks
                errplot,wl,(mm[j,i,*]+dmm[j,i,*,0])<yr[1]>yr[0],$
                  mm[j,i,*]+dmm[j,i,*,1]<yr[1]>yr[0],$
                  width=0
            endif else begin
                if (j eq 0) and (i ne 3) then begin
                    plot,wl,mm[j,i,*],/noerase,$
                      pos=[xx[j],yy[i+1],xx[j+1],yy[i]],$
                      xtitle='',xtickname=blank,charsize=chs,line=0,yr=yr,psym=3,xticks=xticks
                    errplot,wl,(mm[j,i,*]+dmm[j,i,*,0])<yr[1]>yr[0],$
                      mm[j,i,*]+dmm[j,i,*,1]<yr[1]>yr[0],$
                      width=0
                endif
                if (j ne 0) and (i eq 3) then begin
                    plot,wl,mm[j,i,*],/noerase,$
                      pos=[xx[j],yy[i+1],xx[j+1],yy[i]],$
                      xtitle=xtitle,xtickname=xtickname,$
                      charsize=chs,line=0,yr=yr,psym=3,ytickname=blank,xticks=xticks
                    errplot,wl,(mm[j,i,*]+dmm[j,i,*,0])<yr[1]>yr[0],$
                      mm[j,i,*]+dmm[j,i,*,1]<yr[1]>yr[0],$
                      width=0
                endif
                if (j eq 0) and (i eq 3) then begin
                    plot,wl,mm[j,i,*],/noerase,$
                      pos=[xx[j],yy[i+1],xx[j+1],yy[i]],$
                      xtitle=xtitle,xtickname=xtickname,$
                      charsize=chs,line=0,yr=yr,psym=3,xticks=xticks
                    errplot,wl,(mm[j,i,*]+dmm[j,i,*,0])<yr[1]>yr[0],$
                      mm[j,i,*]+dmm[j,i,*,1]<yr[1]>yr[0],$
                      width=0
                endif 
                if (j ne 0) and (i ne 0) then begin
                    plot,wl,mm[j,i,*],/noerase,$
                      pos=[xx[j],yy[i+1],xx[j+1],yy[i]],$
                      xtitle='',charsize=chs,line=0,yr=yr,psym=3,$
                      xtickname=blank,ytickname=blank,xticks=xticks
                    errplot,wl,(mm[j,i,*]+dmm[j,i,*,0])<yr[1]>yr[0],$
                      mm[j,i,*]+dmm[j,i,*,1]<yr[1]>yr[0],$
                      width=0
                endif
                if (j ne 0) and (i eq 0) then begin
                    plot,wl,mm[j,i,*],/noerase,$
                      pos=[xx[j],yy[i+1],xx[j+1],yy[i]],$
                      xtitle='',charsize=chs,line=0,yr=yr,psym=3,$
                      xtickname=blank,ytickname=blank,xticks=xticks
                    errplot,wl,(mm[j,i,*]+dmm[j,i,*,0])<yr[1]>yr[0],$
                      mm[j,i,*]+dmm[j,i,*,1]<yr[1]>yr[0],$
                      width=0
                endif
            endelse
		oplot,!x.crange,[1.,1.],line=1
		oplot,!x.crange,[-1.,-1.],line=1
        endfor
endfor
;!p.multi=0

;------  misc plot  ------
if 0 then begin
    window,3,xs=600,ys=600
    surface,dat,wl,th1,charsize=2,xtitle='wavelength [nm]',ytitle='!7h!31'
endif

wl=wl0 & mm=mm0 & dmm=dmm0 & c=c0


i0=400
print,'wl = '+strtrim(fix(wl[i0]),1)+' nm'
print,mm[*,*,i0],form='(4f10.4)'



END
;********************************************************************
function ta_mmsp_cal,dat=dat,cdat,p,wl,thresh=thresh,clrrms=rms,dMM=dMM

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
mmsp_calwps,th1,th2,cdat,p,c,verb=0,thresh=thresh,rms=rms    ;p   anan 11'03'04

if 0 then begin
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
endif

if not keyword_set(dat) then begin
	mm=c
	goto,jump
endif
;----  Mueller matrix of sample  ----
dat=rebin(dat,nw,nn)
mmsp_mueller,th1,th2,dat,p,c,MM,dMM,fit=fitv

plotresult,wl,mm,dmm,c

jump:

return,mm

end

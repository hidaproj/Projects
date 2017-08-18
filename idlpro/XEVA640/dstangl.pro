 pro dstangl,hangl,zd,radius,pangl,incli,ga

; dstangl.pro
;   make array about Zenith distance and hour angle
;	2009.1.22	T.A.
;       2012.11.06     Addition of r,p,i,ga

e=lonarr(12)
;openr,1,'c:\tmp\dstangl.dat'
openr,1,'C:\ftpdstangl\dstangl.dat'
readf,1,e
close,1

;e[0]=hour up  e[1]=hour low e[2]=Zenith up e[3]=Zenith low 
;e[4]=r up     e[5]=r low    e[6]=p up      e[7]=p low
;e[8]=i up     e[9]=i low    e[10]=GA up    e[11]=GA low

haarr=fltarr(100)
zdarr=fltarr(100)

a=e[0]*1d
b=e[1]*1d
c=e[2]*1d
d=e[3]*1d
ru=e[4]*1d
rl=e[5]*1d
pu=e[6]*1d
pl=e[7]*1d
iu=e[8]*1d
il=e[9]*1d
gu=e[10]*1d
gl=e[11]*1d

;--------- Hour Angle --------
if (a lt 0) then begin
 if (b lt 0) then begin
  ;print,'Case A'
  hangl=-((a-2^15)*2.^16 + (b-2^15)+2.^15)
 endif else begin
  ;print,'Case B'
  hangl=-((a-2^15)*2.^16+b)
 endelse
endif else begin
 if (b lt 0) then begin
  ;print,'Case C'
  hangl=a*2.^16+ (b-2^15)+2.^15
 endif else begin
  ;print,'Case D'
  hangl=a*2.^16 + b
 endelse
endelse

;--------- Zenith Distance --------
if (c lt 0) then begin
 if (d lt 0) then begin
  ;print,'Case E'
  zd=-((c-2^15)*2.^16 + (d-2^15)+2.^15)
 endif else begin
  ;print,'Case F'
  zd=-((c-2^15)*2.^16+d)
 endelse
endif else begin
 if (d lt 0) then begin
  ;print,'Case G'
  zd=c*2.^16+ (d-2^15)+2.^15
 endif else begin
  ;print,'Case H'
  zd=c*2.^16 + d
 endelse
endelse

;-------- Radius ---------
if (ru lt 0) then begin
 if (rl lt 0) then begin
  ;print,'Case A'
  radius=-((ru-2^15)*2.^16 + (rl-2^15)+2.^15)
 endif else begin
  ;print,'Case B'
  radius=-((ru-2^15)*2.^16+rl)
 endelse
endif else begin
 if (rl lt 0) then begin
  ;print,'Case C'
  radius=ru*2.^16+ (rl-2^15)+2.^15
 endif else begin
  ;print,'Case D'
  radius=ru*2.^16 + rl
 endelse
endelse

;--------- Polar Angle --------
if (pu lt 0) then begin
 if (pl lt 0) then begin
  ;print,'Case A'
  pangl=-((pu-2^15)*2.^16 + (pl-2^15)+2.^15)
 endif else begin
  ;print,'Case B'
  pangl=-((pu-2^15)*2.^16+pl)
 endelse
endif else begin
 if (pl lt 0) then begin
  ;print,'Case C'
  pangl=pu*2.^16+ (pl-2^15)+2.^15
 endif else begin
  ;print,'Case D'
  pangl=pu*2.^16 + pl
 endelse
endelse

;--------- Inclination --------
if (iu lt 0) then begin
 if (il lt 0) then begin
  ;print,'Case A'
  incli=-((iu-2^15)*2.^16 + (il-2^15)+2.^15)
 endif else begin
  ;print,'Case B'
  incli=-((iu-2^15)*2.^16+il)
 endelse
endif else begin
 if (il lt 0) then begin
  ;print,'Case C'
  incli=iu*2.^16+ (il-2^15)+2.^15
 endif else begin
  ;print,'Case D'
  incli=iu*2.^16 + il
 endelse
endelse
;--------- Grating Angle --------
if (gu lt 0) then begin
 if (gl lt 0) then begin
  ;print,'Case A'
  ga=-((gu-2^15)*2.^16 + (gl-2^15)+2.^15)
 endif else begin
  ;print,'Case B'
  ga=-((gu-2^15)*2.^16+gl)
 endelse
endif else begin
 if (gl lt 0) then begin
  ;print,'Case C'
  ga=gu*2.^16+ (gl-2^15)+2.^15
 endif else begin
  ;print,'Case D'
  ga=gu*2.^16 + gl
 endelse
endelse

;================================

ha0=abs(hangl)
hah=long(ha0)/3600
ham=long(ha0-hah*3600)/60
has=long(ha0-hah*3600-ham*60)

zd0=abs(zd)
zdd=long(zd0)/3600
zdm=long(zd0-zdd*3600)/60
zds=long(zd0-zdd*3600-zdm*60)

rd0=abs(radius)
rdd=long(rd0)/3600
rdm=long(rd0-rdd*3600)/60
rds=long(rd0-rdd*3600-rdm*60)

pa0=abs(pangl)
pad=long(pa0)/3600
pam=long(pa0-pad*3600)/60
pas=long(pa0-pad*3600-pam*60)

in0=abs(incli)
ind=long(in0)/3600
inm=long(in0-ind*3600)/60
ins=long(in0-ind*3600-inm*60)

ga0=abs(ga)
gad=long(ga0)/3600
gam=long(ga0-gad*3600)/60
gas=long(ga0-gad*3600-gam*60)

print,'Hour Angle [sec]: ',hangl
print,'         [h,m,s]:',hah,ham,has
print,' '
print,'Zenith Distance [arcsec]: ',zd
print,'         [d,m,s]:',zdd,zdm,zds
print,' '
print,'Radius [arcsec]: ',radius
print,'         [m,s]:',rdm,rds
print,' '
print,'Polar Angle [arcsec]: ',pangl
print,'         [d,m]:',pad,pam
print,' '
print,'Inclination [arcsec]: ',incli
print,'         [d,m]:',ind,inm
print,' '
print,'Grating Angle [arcsec]: ',incli
print,'         [d,m]:',gad,gam


hangl=long(hangl)
zd=long(zd)

end


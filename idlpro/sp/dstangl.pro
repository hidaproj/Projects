 pro dstangl,hangl,zd,radius,pangl,incli,ga,num_grating,im,noprint=noprint,file_size=file_size

; dstangl.pro
;   make array about Zenith distance and hour angle
;	2009.1.22	T.A.
;       2012.11.06     Addition of r,p,i,ga
;       2015.05.03	noprint
;	2016.04.30	T.A. H.YW. num_grating
;	2016.07.18	T.A. image rotator angle
;       2016.08.12	T.A. change the limit of file size 

e=lonarr(15)
;openr,1,'c:\tmp\dstangl0.dat'
dstfile='C:\ftpdstangl\dstangl0.dat'
openr,1,dstfile
fst=fstat(1)
help,fstatstop
;while(fst.size ne 87 and fst.size ne 84 and fst.size ne 88) do begin
;while(fst.size le 80) do begin
while(fst.size le 100) do begin
	close,1
	print,'size=',fst.size,'    retry opening....'
	wait,0.1
	openr,1,dstfile
	fst=fstat(1)
	;stop
endwhile
readf,1,e
close,1
file_size=fst.size

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
ng=e[12]
mu=e[13]*1d
ml=e[14]*1d

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
;--------- angle of image rotator --------
if (mu lt 0) then begin
 if (ml lt 0) then begin
  ;print,'Case A'
  im=-((mu-2^15)*2.^16 + (ml-2^15)+2.^15)
 endif else begin
  ;print,'Case B'
  im=-((mu-2^15)*2.^16+ml)
 endelse
endif else begin
 if (ml lt 0) then begin
  ;print,'Case C'
  im=mu*2.^16+ (ml-2^15)+2.^15
 endif else begin
  ;print,'Case D'
  im=mu*2.^16 + ml
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

print,im
im0=abs(im)
; measure 20160718
;im00=48548.000		; incli +0d
;im90=27812.000		; incli=+180
;imlimit1=13672.000	; incli=+302d44m, limit
;imlimit1=62256.000	; incli=+241d00m, limit
;im=90.*3600./(im90-im00)*(im0-im00) ;arcsec
im=-0.00434025*im0+210.709
im0=abs(im)
imd=long(im)/3600
imm=long(im0-imd*3600)/60
ims=long(im0-imd*3600-imm*60)

case ng of
	'000000':num_grating='VS#1'
	'000001':num_grating='HS#1'
	'000002':num_grating='HS#2'
	'000004':num_grating='HS#3'
	else:begin
		print,ng
		num_grating='else'
	end
endcase

if not keyword_set(noprint) then begin
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
print,'Grating number: ',num_grating
print,'Image rotator Angle [arcsec]: ',im
print,'         [d,m]:',imd,imm
print,' '
print,'------------------------------------------'

endif

hangl=long(hangl)
zd=long(zd)

end


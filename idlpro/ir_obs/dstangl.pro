 pro dstangl,hangl,zd

; dstangl.pro
;   make array about Zenith distance and hour angle
;	2009.1.22	T.A.


e=lonarr(4)
;openr,1,'c:\tmp\dstangl.dat'
openr,1,'C:\ftpdstangl\dstangl.dat'
readf,1,e
close,1

;e[0]=hour up  e[1]=hour low e[2]=Zenith up e[3]=Zenith low 

haarr=fltarr(100)
zdarr=fltarr(100)

a=e[0]
b=e[1]
c=e[2]
d=e[3]


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

ha0=abs(hangl)
hah=long(ha0)/3600
ham=long(ha0-hah*3600)/60
has=long(ha0-hah*3600-ham*60)

zd0=abs(zd)
zdd=long(zd0)/3600
zdm=long(zd0-zdd*3600)/60
zds=long(zd0-zdd*3600-zdm*60)

print,'Hour Angle [sec]: ',hangl
print,'         [h,m,s]:',hah,ham,has
print,' '
print,'Zenith Distance [arcsec]: ',zd
print,'         [d,m,s]:',zdd,zdm,zds

hangl=long(hangl)
zd=long(zd)

end


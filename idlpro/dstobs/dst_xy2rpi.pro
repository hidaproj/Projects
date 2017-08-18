; Conversion Solar (X,Y) corrdination into DST's (r,p) and inclination
; 2007.04.10 by S.UeNO

posstr=' '
read,"Today's position angle [dd, mm]:",posstr
read,"Target position (x,y) [arcsec]:", x0,y0

tmp=STRSPLIT(posstr,',',/EXTRACT)

if (tmp(0) eq '-0') then begin
 pos=-1.*abs(float(tmp(1)))/60.  ; [deg]
endif else begin
 if (tmp(0) eq '0') then begin
  pos=abs(float(tmp(1)))/60.  ; [deg]
 endif else begin
  posdd=float(tmp(0))
  posmm=float(tmp(1))
  pos=(posdd/abs(posdd)) * (abs(posdd)+abs(posmm)/60.)  ; [deg]
 endelse
endelse

r=sqrt(x0^2+y0^2) ; [arcsec]
rmm=fix(r/60.) ; [arcmin]
rss=r-rmm*60.  ; [arcsec]

;-------------------------------
if (x0 ge 0) then begin
 theta0=270.+pos ; [deg]
endif else begin
 theta0=90.+pos ; [deg]
endelse
theta=180./!pi*atan(1.*y0/x0)  ; [deg]
pol=theta0+theta ; [deg]

poldd=fix(pol) ; [dd]
polmm=(pol-poldd)*60. ; [mm]

;-------------------------------
if (pos ge 0.) then begin
 snp=pos
endif else begin
 snp=360.+pos
endelse

snpdd=fix(snp) ; [dd]
snpmm=(snp-snpdd)*60. ; [mm]

;-------------------------------

print,"DST's  r    [arcmin, arcsec]          :", rmm, rss
print,"DST's  p    [deg, min]                :", poldd, polmm
print,"DST's i of Solar North Pole [deg, min]:", snpdd, snpmm

end

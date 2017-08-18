; Conversion DST's (r,p) into Solar (X,Y) corrdinates and inclination
; 2007.04.10 by S.UeNo

read,"Today's position angle [dd, mm]:",posdd,posmm
read,"DST Target position r [arcmin, arcsec]:", rmin, rsec
read,"DST Target position p [deg, min]:", pdeg, pmin


pos=(posdd/abs(posdd)) * (abs(posdd)+abs(posmm)/60.)  ; [deg]

r=rmin*60.+rsec*1. ; [arcsec]

p=pdeg*1.+pmin/60. ; [deg]
theta=p-pos

xx=-1.*r*sin(theta/180.*!pi)
yy=r*cos(theta/180.*!pi)

;-------------------------------

print," X  [arcsec]   :", xx
print," Y  [arcsec]   :", yy

end

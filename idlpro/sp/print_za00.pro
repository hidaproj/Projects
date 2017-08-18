
;PRO rec_hazd
;
;	record time(yyyymmddhhmmss), hour angle(sec), and zenith distance(sec)
;       time cadence 1sec
;20110808  T.A.
;20110927  T.A.		save in each 1 minute.
;       2012.11.06     Addition of r,p,i,ga

;==========================headder============================;
;PRO rec_hazd_event, ev
;PRO rec_hazd_
;
;-

while 1 do begin
	caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
	yyyymmddhhmmss=string(year,form='(i4.4)')+string(mon,form='(i2.2)')+string(day,form='(i2.2)') $
	+string(hour,form='(i2.2)')+string(minu,form='(i2.2)')+string(seco,form='(i2.2)')

	dstangl,hangl,zd,radius,pangl,incli,ga

ha0=abs(hangl)
hah=long(ha0)/3600
ham=long(ha0-hah*3600)/60
has=long(ha0-hah*3600-ham*60)

zd0=abs(zd)
zdd=long(zd0)/3600
zdm=long(zd0-zdd*3600)/60
zds=long(zd0-zdd*3600-zdm*60)


ha=ha0/3600.*15.*!dtor
zd=zd0/3600.*!dtor
lat=36.252*!dtor
za=asin(cos(lat)*sin(ha)/sin(zd))



print,'Hour Angle [sec]: ',hangl
print,'         [h,m,s]:',hah,ham,has
print,' '
print,'Zenith Distance [arcsec]: ',zd
print,'         [d,m,s]:',zdd,zdm,zds
print,' '

wait,1.
enddo

END

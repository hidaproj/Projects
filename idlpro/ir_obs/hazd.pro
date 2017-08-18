a=0 & b=0 & c=0 & d=0
haarr=fltarr(100)
zdarr=fltarr(100)
window,0
window,1

print,'If you want to stop this program, push Ctrl+C. '

start:

time2=systime(0)

openr,1,'~fmt/hau.dat'
readf,1,a
close,1

openr,1,'~fmt/hal.dat'
readf,1,b
close,1

openr,1,'~fmt/zdu.dat'
readf,1,c
close,1

openr,1,'~fmt/zdl.dat'
readf,1,d
close,1

if (a lt 0) then begin
 if (b lt 0) then begin
  ;print,'Case A'
  ha=-((a-2^15)*2.^16 + (b-2^15)+2.^15)
 endif else begin
  ;print,'Case B'
  ha=-((a-2^15)*2.^16+b)
 endelse
endif else begin
 if (b lt 0) then begin
  ;print,'Case C'
  ha=a*2.^16+ (b-2^15)+2.^15
 endif else begin
  ;print,'Case D'
  ha=a*2.^16 + b
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

ha0=abs(ha)
hah=long(ha0)/3600
ham=long(ha0-hah*3600)/60
has=long(ha0-hah*3600-ham*60)

zd0=abs(zd)
zdd=long(zd0)/3600
zdm=long(zd0-zdd*3600)/60
zds=long(zd0-zdd*3600-zdm*60)

print,'Hour Angle [sec]: ',ha
print,'         [h,m,s]:',hah,ham,has
print,' '
print,'Zenith Distance [arcsec]: ',zd
print,'         [d,m,s]:',zdd,zdm,zds
hastr=string(format='(i3.3)',hah)+'h '+string(format='(i2.2)',ham)+'m '+string(format='(i2.2)',has)+'s'
zdstr=string(format='(i3.3)',zdd)+'d '+string(format='(i2.2)',zdm)+'m '+string(format='(i2.2)',zds)+'s'


haarr=shift(haarr,-1)
haarr(99)=ha

zdarr=shift(zdarr,-1)
zdarr(99)=zd

wh=where(haarr ne 0.)
wz=where(zdarr ne 0.)

if (wh(0) eq -1) then wh=indgen(100)
if (wz(0) eq -1) then wz=indgen(100)

wset,0 & plot,wh,haarr(wh),xrange=[0,100],psym=2,/ynozero,title='Hour Angle [sec]',xtitle='Time [3sec]',chars=1.5 & oplot,wh,haarr(wh)
xyouts,100,450,time2,chars=1.5,/device
wset,1 & plot,wz,zdarr(wz),xrange=[0,100],psym=4,/ynozero,title='Zenith Distance [arcsec]',xtitle='Time [3sec]',chars=1.5 & oplot,wz,zdarr(wz)
xyouts,100,450,time2,chars=1.5,/device
window,2


xyouts,50,410,'HOUR ANGLE',chars=5,/dev,charthick=2
xyouts,80,310,hastr,chars=7,/dev,charthick=4
xyouts,50,190,'ZENITH DISTANCE',chars=5,/dev,charthick=2
xyouts,80,90,zdstr,chars=7,/dev,charthick=4

time0=systime(1)
gettime:
time1=systime(1)
if ( (time1-time0) lt 3.0 ) then goto,gettime
 
goto,start

end


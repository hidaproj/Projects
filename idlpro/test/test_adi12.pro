retall
; test_adi12.pro
@adi12_8lib
nchan=1

caio_init
caio_Set,nchan=nchan,rate=400			;maximum sampling rate (1ch) = 1666 (=1sec/0.6msec)
p=caio_getinfo() &	help,p,/st ;&	stop

caio_start
	;ans='' & read,ans

	caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
	yyyymmdd_hhmmss=string(year,form='(i4.4)')+string(mon,form='(i2.2)')+string(day,form='(i2.2)') $
		+'_'+string(hour,form='(i2.2)')+string(minu,form='(i2.2)')+string(seco,form='(i2.2)')
	print,'trig_start',yyyymmdd_hhmmss
;caio_trig,nchan
	caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
	yyyymmdd_hhmmss=string(year,form='(i4.4)')+string(mon,form='(i2.2)')+string(day,form='(i2.2)') $
		+'_'+string(hour,form='(i2.2)')+string(minu,form='(i2.2)')+string(seco,form='(i2.2)')
	print,'trig_end',yyyymmdd_hhmmss

caio_Set,nchan=nchan,rate=3600			;maximum sampling rate (1ch) = 1666 (=1sec/0.6msec)
caio_start

	;thresfold_V=1
	;caio_trig,thresfold_V

;for i=0,100 do begin
;	print,i,caio_status(),caio_count()
;endfor
;for i=0,50 do begin
;	dt=caio_read(nchan,1)
;	print,dt
;endfor

wait,2.0
;ans='' & read,ans
caio_stop
st=caio_status()
ndat=caio_count()
print,i,st,ndat

dd = caio_read(nchan,ndat,ret=ret)

caio_exit




end

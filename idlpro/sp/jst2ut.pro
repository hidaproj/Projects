;********************************************************************
;NAME       : jst2ut (procedure)
;FUNCTION   : translate JST -> UT
;DATE       : 91/09/27       1991,7,19
;PROGRAMMER : k.i.  m.m.
;********************************************************************
pro jst2ut,date,time
; date : 'yy/mm/dd'
; time : 'hh:mm:ss'

yy=fix(strmid(date,0,2))
mo=fix(strmid(date,3,2))
dd=fix(strmid(date,6,2))
hh=fix(strmid(time,0,2))
mi=fix(strmid(time,3,2))
ss=fix(strmid(time,6,2))

uyy=yy
umo=mo
udd=dd
uhh=hh-9
umi=mi
uss=ss

if(uhh lt 0) then begin
	uhh=uhh+24
	udd=dd-1
	if(udd eq 0) then begin
		umo=mo-1
		case mo of
		    1:  begin
			udd=31
			umo=12
			uyy=yy-1
			end
	 	    2:	udd=31
		    3:	begin
			udd=28
			if( (yy mod 4) eq 0 ) then udd=29
			end
		    4:	udd=31	
		    5:	udd=30
		    6:	udd=31
		    7:	udd=30
		    8:	udd=31
		    9:	udd=31
		    10:	udd=30
		    11:	udd=31
		    12:	udd=30
		endcase
	endif
endif

date=string(uyy,format='(I2.2)')+'/'+string(umo,format='(I2.2)')+ $ 		
			'/'+string(udd,format='(I2.2)')
time=string(uhh,format='(I2.2)')+':'+string(umi,format='(I2.2)')+ $
			':'+string(uss,format='(I2.2)')

return
end


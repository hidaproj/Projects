;	makolib.pro

;	2016.01.28	t.a.
; 2016.07.14  K. Otsuji no convert to float
; 2016.07.25  K. Otsuji add mako_startstream/mako_stopstream/mako_getimg

;=============================================
function p_mako
;---------------------------------------------
p={mako_param,		$
	SIMPLE:		'T',					$;
	BITPIX:		12,					$;
	NAXIS:		3,					$;
	NAXIS1:		2048,					$; Width
	NAXIS2:		2048,					$; Height
	NAXIS3:		1,					$; number of images
	EXTEND:		'F',					$;
	BSCALE:		1.,					$;
	BZERO:		0.,					$;
	ORIGIN:		'Kwasan and Hida Observatories',	$;
	OBSERVAT:	'Kwasan Observatory',			$;
	TELESCOP:	'The 18cm Refractor Telescope',		$;
	TIMESYS:	'UTC',					$
	DATE_OBS:	'',					$; 
	EXPTIME:	1000l,					$; exposure time (micro sec)
	CAMGAIN:	0,					$; gain
	FGBINX:		1,					$; Binning X
	FGBINY:		1,					$; Binning Y
	X0:		0,					$;
	X1:		2047,					$;
	Y0:		0,					$;
	Y1:		2047,					$;
	DETNAM:		'Mako G-419B',				$
	TRIGMODE:	'Freerun',				$
	ACQUMODE:	'Continuous',				$;Acquisition mode
	COMMENT:	'',					$
	HISTORY:	'',					$
	status:	1	$
	}

return,p
END
	
;=============================================
function mako_init
;---------------------------------------------
common mako,dllfile
dllfile='C:\projects\cprog\VS2010\mako_g419b\x64\Debug\mako_g419b.dll'

r=call_external(dllfile,'CamInit')
p=p_mako()
if r eq 0 then p.status=0

return,p
END
;=============================================
pro mako_fin
;---------------------------------------------
common mako,dllfile

r=call_external(dllfile,'CamFin')

END

;=============================================
pro set_parameters,p
;---------------------------------------------
common mako,dllfile

help,p,/str
r=call_external(dllfile,'SetParam',p.EXPTIME,p.CAMGAIN,/all_value,/cdecl)

END

;=============================================
function mako_obs,p,img,header=header
;---------------------------------------------
common mako,dllfile

caldat,systime(/JULIAN,/UTC),mon,day,year,hour,minu,seco

r=call_external(dllfile,'GrabImg',p.NAXIS3,/all_value,/cdecl)

imgs=!null
if r eq 0 then return,0
for i=0,p.NAXIS3-1 do begin
	r=call_external(dllfile,'DivBuf',i,/all_value,/cdecl)
	r=call_external(dllfile,'GoIdl',img)
	imgs=[[[imgs]],[[img]]]
endfor

p.DATE_OBS=string(year,format='(i4.4)')+'-'+string(mon,format='(i2.2)')+'-'+string(day,format='(i2.2)')+'T'+	$
	string(hour,format='(i2.2)')+':'+string(minu,format='(i2.2)')+':'+string(seco,format='(i2.2)')

header=p
return,imgs

END

;=============================================
pro mako_startstream,p
;---------------------------------------------

  common mako,dllfile

  r=call_external(dllfile,'AcquisitionModeContinuous',p.NAXIS3,/all_value,/cdecl)
  
end

;=============================================
pro mako_stopstream
;---------------------------------------------

  common mako,dllfile

  r=call_external(dllfile,'StopAcquisition')

end

;=============================================
function mako_getimg,p,img,header=header
  ;---------------------------------------------
  common mako,dllfile

  caldat,systime(/JULIAN,/UTC),mon,day,year,hour,minu,seco

  r=call_external(dllfile,'QueueFrame',p.NAXIS3,/all_value,/cdecl)
  
  imgs=!null
  if r eq 0 then return,0
  for i=0,p.NAXIS3-1 do begin
    r=call_external(dllfile,'DivBuf',i,/all_value,/cdecl)
    r=call_external(dllfile,'GoIdl',img)
    imgs=[[[imgs]],[[img]]]
  endfor

  p.DATE_OBS=string(year,format='(i4.4)')+'-'+string(mon,format='(i2.2)')+'-'+string(day,format='(i2.2)')+'T'+  $
    string(hour,format='(i2.2)')+':'+string(minu,format='(i2.2)')+':'+string(seco,format='(i2.2)')

  header=p
  return,imgs

END





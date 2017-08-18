; HR2000lib.pro

;  Library for controling HR2000

;   2010.6.20	k.i., n.k.
;   2012.4.03   m.h., ;modif for Maya 2000 PRO
;   2015.4.05	k.i.,t.a.   HR2000_64.dll
;   2016.5.15	k.i.	    flame USB4000

;**************************************************************************
pro hr_init,spname
common hr2000lib,dllfile,npix

	;dllfile='C:\Projects\cprog\VS2005\HR2000\Debug\HR2000.dll'
	dllfile='D:\Projects\cprog\VS2010\HR2000_64\x64\Debug\HR2000_64.dll'
	spname = call_external(dllfile,'hr_init',/s_value )
	;print,nsp
	if spname eq 'USB4000' then 	wl=lonarr(4000)	$ ;;;; modif for flame USB4000 (2016.05.15)
	else 				wl=lonarr(3000)	;;;; modif for Maya 2000 PRO (2012.04.03)
	npix = call_external(dllfile,'hr_getwl',wl,/cdecl )

end


;**************************************************************************
pro hr_close
common hr2000lib,dllfile,npix
	dmy = call_external(dllfile,'hr_close',/cdecl )

end

;**************************************************************************
function hr_getwl
common hr2000lib,dllfile,npix

	wl=lonarr(npix)	;;;; modif for Maya 2000 PRO (2012.04.03)
	npix = call_external(dllfile,'hr_getwl',wl,/cdecl )
	wl=double(wl)/1000.
	return,wl
end

;**************************************************************************
pro hr_setexpo,expo
; expo in msec
common hr2000lib,dllfile,npix

	expou=expo*1000l;
	dmy = call_external(dllfile,'hr_setexpo',expou,value=[1d],/cdecl )

end

;**************************************************************************
function hr_getsp1
common hr2000lib,dllfile,npix

sp1=lonarr(npix)	;;;; modif for Maya 2000 PRO (2012.04.03)
npix = call_external(dllfile,'hr_getsp1',sp1,value=[0d],/cdecl )
sp1=double(sp1)/10.
return,sp1

end

;**************************************************************************
function hr_getsps,nn	; get nn spectra
common hr2000lib,dllfile,npix

sp1=lonarr(npix)	;;;; modif for Maya 2000 PRO (2012.04.03)
sp=uintarr(npix,nn)
for i=0,nn-1 do begin
	npix = call_external(dllfile,'hr_getsp1',sp1,value=[0d],/cdecl )
	sp[*,i]=uint(double(sp1)/10.)
endfor

return,sp

end

;**************************************************************************
pro hr_gpio,bitpos,val,ret
; control GPIO
;  val -  0,1 for output,(-1 for input)
;  ret -  return
common hr2000lib,dllfile,npix


ret = call_external(dllfile,'hr_gpio',bitpos,val,value=[1d,1d],/cdecl )

end

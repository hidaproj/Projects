; edge_appod_filt.pro
;  2013.5.22	k.i.

function edge_appod_filt, nx,ny,wap=waps

;--- 1D case ---
if not keyword_set(ny) then begin
	wapx=waps
	csx=(cos(findgen(wapx)/wapx*!pi)+1.)/2
	appo=replicate(1.,nx)
	appo[0:wapx-1]=appo[0:wapx-1]*(1.-csx)
	appo[nx-wapx:nx-1]=appo[nx-wapx:nx-1]*csx
	return,appo
endif

;--- 2D case ---
if n_elements(waps) eq 1 then begin
	wapx=waps
	wapy=waps
endif
if n_elements(waps) eq 2 then begin
	wapx=waps[0]
	wapy=waps[1]
endif

;-----  appodization filter for a segment  --------
csx=(cos(findgen(wapx)/wapx*!pi)+1.)/2
csy=(cos(findgen(wapy)/wapy*!pi)+1.)/2
appo=fltarr(nx,ny) &	appo[*,*]=1.
for j=0,ny-1 do begin
	appo[0:wapx-1,j]=appo[0:wapx-1,j]*(1.-csx)
	appo[nx-wapx:nx-1,j]=appo[nx-wapx:nx-1,j]*csx
endfor
if ny ge 2 then begin
for i=0,nx-1 do begin
	appo[i,0:wapy-1]=appo[i,0:wapy-1]*(1.-csy)
	appo[i,ny-wapy:ny-1]=appo[i,ny-wapy:ny-1]*csy
endfor
endif

return,appo


end

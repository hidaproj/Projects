;+
;  NAME:     nkrfilt.pro
;  PURPOSE:  return Norikura filter transmission 
;  USAGE:
;	tr=nkrfilt('filter',wl=wl)
;  PARAMETERS:
;	filter	-- filename (without extension)
;	wl	-- wavelength in nm
;	tr	-- transmission
;  PROCEDURE:
;	'filter.bin' must exist in c:\nkrprj\calib\filters\c25\'
;  HISTORY:
;	k.i. '98/11/08
;	k.i. '00/06/15
;-
function nkrfilt,filter,wl=wl,ifset=ifset
;  wl	- wavelength in nm, if not set, return wl(*)

dir='c:\nkrprj\calib\filters\25cm\'
if keyword_set(ifset) then dir='c:\nkrprj\calib\filters\ifset\'

ff=findfile(dir+filter+'.bin',count=count)
if count eq 0 then begin
	print,dir+filter+'.bin'+' not found!'
	return,0
endif

spget,dir+filter+'.bin',wls,trs
if keyword_set(wl) then spln=1 else spln=0

if n_elements(wl) eq n_elements(wls) then begin
	if max(abs(wl-wls)) eq 0. then spln=0
endif

if spln then begin
	wmin=min(wl) &	wmax=max(wl)
	dmy=min(abs(wls-wmin),i1) &	i1=max([i1-2,0])
	dmy=min(abs(wls-wmax),i2) &	i2=min([i1+2,n_elements(wls)-1])
	tr=spline(wls(i1:i2),trs(i1:i2),wl)*0.01
endif else begin
	wl=wls
	tr=trs*0.01
endelse
return,tr

end

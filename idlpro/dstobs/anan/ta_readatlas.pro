;*************************************************************************
;NAME       : atlas (function)
;FUNCTION   : read Sac Peak Solar Flux Atlas by Kurucz et al.(1984)
;               or Liege Atlas of solar disc center spectra
;             /usr/local/lib/atlas/atlas1.dat  should exist
;				   liege.dat
;DATE       : 92/05/04
;	     '95/01/08	k.i.
;	     '95/01/14	k.i.
;	     '97/05/14	k.i.	automatic OS detection
;	     '98/09/27  k.i.    bug fix for wl > 8000A for SacPeak
;	     '98/10/08  k.i.    (wstep+1e-6)/dw1
;	     '00/01/17  k.i.    byteorder for liege
;	     '00/10/25  k.i.    !version.os_family
;	     '02/07/16  k.i.    !version.os eq 'Win32'
;	     '02/10/27  k.i.    !version.os_family
;	     '05/10/24  k.i.    spot keyword, use SacPeak spot4atl by spconv.pro
;	     '06/07/20  k.i.    for linux
;	     '06/08/17  k.i.    wl -> double
;	     '07/03/06  k.i.    wmin,wmax can be omitted, bug fix
;PROGRAMMER : k.i.   I'm not sure if wavelength is very strict.
;	     '14/08/06  t.a.    directory
; ========================================================================
function ta_readatlas,wmin,wmax,wstep,wl=wl,liege=liege,spot=spot

;  wmin, wmax  : wave length range in A
;  wstep       : wave length step in A
;  wl          : return wave length array

if not keyword_set(wmin) then begin
	wmin=0
	wmax=100000.
endif
if wmin ge wmax then return,-1
if keyword_set(liege) then file='liege.dat' $
else file='ATLAS1.DAT'
if !version.os_family eq 'Windows' or !version.os eq 'Win32' then begin
	dir='\Projects\IDLPRO\DSTobs\anan\'	; T.A. 20140806
	dir='\solar\atlas\'	; T.A. 20140806
	drv=['c:','d:','e:']
	for i=0,n_elements(drv)-1 do begin
		ff=findfile(drv(i)+dir+file,count=cc)
		if cc ne 0 then begin
			drv0=drv(i) 
			goto,pp
		endif
	endfor
	print,'Atlas file not found!!'
	stop
   pp:	afile=drv0+dir+file
endif else begin
	;afile='/usr/local/lib/atlas/'+file
        ;afile='~/data/solar/atlas/'+file
        ;afile='/work1/anan/works/program_anan/atlas/'+file
        afile='./'+file				; T.A. 20140806
endelse

close,1
if keyword_set(spot) then begin	; SacPeak spot atlas converted by \data\download\NSO\spconv.pro
	afile=drv0+dir+'spot4atl.dat'
	nn=0l
	openr,1,afile
	readu,1,nn
	wlf=dblarr(nn) &	spf=fltarr(nn)
	readu,1,wlf
	readu,1,spf
	close,1
	if not keyword_set(wstep) then begin
		ii=where(wlf ge wmin and wlf le wmax)
		wl=wlf(ii) &	sp=spf(ii)
	endif else begin
		wl=wmin+findgen((wmax-wmin)/wstep+1)*wstep
		sp=spline(wlf,spf,wl)
	endelse
	return,sp
endif

if keyword_set(liege) then begin	; Liege Atlas
	dw1=0.002d	; Liege Atlas 4006-6860A
	wmin0=4006.d
	wmax0=6860.d
	wmax=min(wmax,wmax0)
	if n_elements(wstep) eq 0 then wstep=dw1
	nn=long((wmax-wmin)/wstep)+1
	wl=wmin+findgen(nn)*wstep
	openr,1,afile
	seek=long((wmin-wmin0)/dw1)*2
	len=long((wmax-wmin)/dw1)+1
	buf=intarr(len)
	point_lun,1,seek
	readu,1,buf
	close,1
	;if !version.os_family eq 'Windows' then byteorder,buf
	if strmid(!version.arch,0,3) eq 'x86' then byteorder,buf

endif else begin	; Sac Peak Atlas
	dw1=0.01d	; wave-length step in A for 2960-8000 A 
	dw2=0.02d	;              "            8000-13000 A 
	wmin0=2960.d
	wmax0=13000.d
	wmin=max([wmin,wmin0])
	wmax=min([wmax,wmax0])
	if n_elements(wstep) eq 0 then wstep=dw1
	nn=long((wmax-wmin)/wstep)+1
	wl=wmin+dindgen(nn)*wstep
	openr,1,afile
	if wmax le 8000. then begin
		np=fix((wstep+1e-6)/dw1)
		seek=long((wmin-wmin0)/dw1)-np/2
		len=long(nn*np)
		buf=bytarr(len)
		point_lun,1,seek
		readu,1,buf
	endif
	if wmin ge 8000. then begin
		seek=long((8000.-wmin0)/dw1 + (wmin-8000.)/dw2)
		len=long((wmax-wmin)/dw2)+1
		buf=bytarr(len)
		point_lun,1,seek
		readu,1,buf
		buf=rebin(buf,2*len)
	endif
	if wmin le 8000. and wmax gt 8000. then begin
		seek=(wmin-wmin0)/dw1
		len1=long((8000-wmin)/dw1)+1
		len2=long((wmax-8000)/dw2)+1
		buf1=bytarr(len1)
		buf2=bytarr(len2)
		point_lun,1,seek
		readu,1,buf1
		readu,1,buf2
		buf=[buf1,rebin(buf2,2*len2)]
	endif
	close,1
endelse

len=n_elements(buf)
if len/nn*nn eq len then return,rebin(float(buf),nn)
buf1=fltarr(len,2)
buf1(*,0)=buf
buf1=congrid(buf1,nn,2,/interp)
buf=buf1(*,0)
	
return,buf

end

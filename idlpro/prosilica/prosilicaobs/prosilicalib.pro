;  prosilicalib.pro

;  2012.11.02	t.k., k.i.,  for 64bit using prosilica_64.dll (from CameraLib.pro)
;  2013.10.23	k.i.,  gettime
;  2014.06.07	k.i.,  many from orcalib
;  2014.08.01	k.i.,  'BIN     = ',p.binx

;**************************************************************
function p_prosilica
;--------------------------------------------------------------
p={prosilica_param,   $
   expo:        long(50000l),  	$   ; exposure time [usec]
   framerate:   float(30),     	$   ; frame rate [frame/sec]
   gain:        0,      	$   ; gain 0-28
   binx:        1,      	$   ; Binning X 1-8
   biny:        1,           	$   ; Binning Y 1-1200
   Width:       1600,        	$   ; Width  max=1600 (binx=1)
   Height:      1200,         	$   ; Height  max=1200 (biny=1)
   RegionX:     0,           	$   ; start of region read out,pixel,left edge
   RegionY:     0,           	$   ; start of region read out,pixel,top edge
   date_obs:   	'',           	$   ; yyyy-mm-ddThh:mm:ss.sss
   date_obs2:  	'',           	$   ; yyyy-mm-ddThh:mm:ss.sss
   timesys :   	'JST',         	$   ; yyyy-mm-ddThh:mm:ss.sss
   observat:   	'Hida',        	$   ;
   telescop:   	'DST',         	$   ;
   instrume:   	'VS',          	$   ;
   camera:     	'Prosilica',   	$   ; 
   wave:  	'',           	$   ; wavelength
   data_typ:	'OBJECT',	$   ; 'OBJECT','DARK', 'FLAT'
   clock:       79861111l,  	$   ; TimeStanmpFrequency [Hz]
   timelo:      0,           	$   ; Time stamp, lower 32-bits
   timehi:      0,           	$   ; Time stamp, upper 32-bits
   status:      0           	$   ; Status
   }

return,p
end

;**************************************************************
function CamSetParam,expo=expo,gain=gain,binx=binx,biny=biny, $
	width=width,height=height,regionx=regionx,regiony=regiony, $
	framerate=framerate
;--------------------------------------------------------------
common prosilica,prodll,p,img,msec0

;  expo	-	exposure, usec, long
;  gain	-	0,1,2,,,,20
;  binx,biny -	1,2,4,,
;  

if n_elements(expo) ne 0 then begin
	err=call_external(prodll,'SetCamExpo', expo, /all_value,/cdecl)
	p.expo=expo
endif

if n_elements(gain) then begin
	err=call_external(prodll,'SetCamGain', gain, /all_value,/cdecl)
	p.gain=gain
endif

if keyword_set(width) then p.width=width
if keyword_set(height) then p.height=height
if keyword_set(width) or keyword_set(height) then begin
	err=call_external(prodll,'SetCamSize', p.width, p.height, /all_value,/cdecl)
	img=intarr(p.width,p.height)
endif

if keyword_set(binx) then p.binx=binx
if keyword_set(biny) then p.biny=biny
if keyword_set(binx) or keyword_set(biny) then begin
	err=call_external(prodll,'SetCamBin', p.binx, p.biny, /all_value,/cdecl)
	img=intarr(p.width/p.binx,p.height/p.biny)
endif

set_region=0
if n_elements(regionx) ne 0 then begin
	p.regionx=regionx
	set_region=1
endif
if n_elements(regiony) ne 0 then begin
	p.regiony=regiony
	set_region=1
endif
if set_region then begin
	err=call_external(prodll,'SetCamRegion', p.regionx, p.regiony, /all_value,/cdecl)
endif

if keyword_set(framerate) then begin
	err=call_external(prodll,'SetCamFrate', framerate, /all_value,/cdecl)
	p.framerate=framerate
endif




return,p

END

;**************************************************************
function CamInit
;--------------------------------------------------------------
common prosilica,prodll,p,img,msec0
prodll='C:\Projects\cprog\VS2010\prosilica_64\x64\Debug\prosilica_64.dll'
err=call_external(prodll,'CamInit',/all_value,/cdecl)
p=p_prosilica()
p.status=err
img=intarr(p.width,p.height)
p=CamSetParam(expo=p.expo,gain=p.gain,width=p.width,height=p.height, $
	binx=p.binx,biny=p.biny,regionx=p.regionx,regiony=p.regiony)

return,p

END


;**************************************************************
PRO CamFin
;--------------------------------------------------------------
common prosilica,prodll,p,img,msec0

err=call_external(prodll,'CamFin',/all_value,/cdecl)

END


;**************************************************************
PRO GrubImg,nimg
;--------------------------------------------------------------
common prosilica,prodll,p,img,msec0

err=call_external(prodll,'GrabImg',nimg,/all_value,/cdecl)

END


;**************************************************************
PRO PrevImg
;--------------------------------------------------------------
common prosilica,prodll,p,img,msec0

r=call_external(prodll,'GrabImg',1,/all_value,/cdecl)
r=call_external(prodll,'DivBuf',0,/all_value,/cdecl)
r=call_external(prodll,'GoIdl',img,/cdecl)

if n_elements(img[*,0]) gt 1000 then  $
  tvscl,congrid(img,n_elements(img[*,0])/2,n_elements(img[0,*])/2) $
  else tvscl,img
plot,findgen(50)/50*4096,histogram(img,max=4096,min=0,nbins=50), $
    /noerase,/xstyle,charsize=0.5,position=[0.05,0.05,0.25,0.2],color=0

end

;**************************************************************
function Gigobs1
;--------------------------------------------------------------
common prosilica,prodll,p,img,msec0

r=call_external(prodll,'GrabImg',1,/all_value,/cdecl)
r=call_external(prodll,'DivBuf',0,/all_value,/cdecl)
r=call_external(prodll,'GoIdl',img,/cdecl)
;	img[*,*,*] store only 1 image even if nimg>1

return,img

end

;**************************************************************
function Gigobs,nimg
;--------------------------------------------------------------
common prosilica,prodll,p,img,msec0

imgs=intarr(p.width,p.height,nimg)
r=call_external(prodll,'GrabImg',nimg,/all_value,/cdecl)
for i=0,nimg-1 do begin
	r=call_external(prodll,'DivBuf',i,/all_value,/cdecl)
	r=call_external(prodll,'GoIdl',img,/cdecl)
	imgs[*,*,i]=img
endfor

return,imgs

end

;**************************************************************
function timeinit	; obsolate  use get_systime()
;--------------------------------------------------------------
common prosilica,prodll,p,img,msec0

caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
second=string(seco,form='(i2.2)')	&	second0=second
while (second eq second0) do begin
	caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
	second=string(seco,form='(i2.2)')
endwhile

msec0=call_external(prodll,'getime',/all_value,/cdecl)
return,msec0
end

;**************************************************************
function gettime	; obsolate  use get_systime()
;--------------------------------------------------------------
common prosilica,prodll,p,img,msec0

caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
yyyymmdd_hhmmss=string(year,form='(i4.4)')+string(mon,form='(i2.2)')+string(day,form='(i2.2)') $
	+'_'+string(hour,form='(i2.2)')+string(minu,form='(i2.2)')+string(seco,form='(i2.2)')
msec=call_external(prodll,'getime',/all_value,/cdecl)

time0=msec-msec0
length=strlen(string(time0))
msec=strmid(string(time0),(length-3),3)

time=yyyymmdd_hhmmss+'.'+msec
return,time

end

;**************************************************************
pro savefits_p,imgs,p,file=file

;p={orca4_param,   $
;   expo:        0.1,	  	$   ; exposure time [sec]
;   framerate:   float(30),     	$   ; frame rate [frame/sec]
;   gain:        0,      	$   ; gain 0-28
;   bin:	        1,      	$   ; Binning XY 1-8
;   Width:       2048,        	$   ; Width  max=2048 (binx=1)
;   Height:      2048,         	$   ; Height  max=2048 (biny=1)
;   RegionX:     0,           	$   ; start of region read out,pixel,left edge
;   RegionY:     0,           	$   ; start of region read out,pixel,top edge
;   clock:       79861111l,  	$   ; TimeStanmpFrequency [Hz]
;   timelo:      0,           	$   ; Time stamp, lower 32-bits
;   timehi:      0,           	$   ; Time stamp, upper 32-bits
;   status:      0           	$   ; Status

case size(imgs,/type) of
   1: bitpix=8
   2: bitpix=16
   3: bitpix=32
   4: bitpix=32
   5: bitpix=64
   6: bitpix=64	 ; ? complex
   7: bitpix=-1	 ; string
   8: bitpix=-1  ; struct
   9: bitpix=128 ; ? dcomplex
  10: bitpix=-1	 ; pointer
  11: bitpix=-1  ; objref
  12: bitpix=16  ; uint
  13: bitpix=32  ; ulong
  14: bitpix=64  ; long64
  15: bitpix=64  ; ulong64
endcase
nax=size(imgs,/n_dimension)
dim=size(imgs,/dimension)
nax1=dim[0]
if nax ge 2 then nax2=dim[1] else nax2=1
if nax ge 3 then nax3=dim[2] else nax3=1

fh=strarr(36)
        fh[0] =string('SIMPLE  = ','T',format='(a10,a20," /")')
	fh[1] =string('BITPIX  = ',bitpix,format='(a10,i20," /")')
	fh[2] =string('NAXIS   = ',nax,format='(a10,i20," /")')
	fh[3] =string('NAXIS1  = ',nax1,format='(a10,i20," /")')
	fh[4] =string('NAXIS2  = ',nax2,format='(a10,i20," /")')
	fh[5] =string('NAXIS3  = ',nax3,format='(a10,i20," /")')
	fh[6] =string('DATE_OBS= ',p.date_obs,format='(a10,a20," /")')
	fh[7] =string('DATE_OB2= ',p.date_obs2,format='(a10,a20," /")')	; end time of integ
	fh[8] =string('TIMESYS = ',p.timesys,format='(a10,a20," /")')
	fh[9] =string('OBSERVAT= ',p.observat,format='(a10,a20," /")')
	fh[10]=string('TELESCOP= ',p.telescop,format='(a10,a20," /")')
	fh[11]=string('CAMERA  = ',p.camera,format='(a10,a20," /")')
	fh[12]=string('EXP     = ',p.expo,format='(a10,f20.10," /")')
	fh[13]=string('DATA_TYP= ',p.data_typ,format='(a10,a20," /")')
	fh[14]='VARTYPE = UINT'
	fh[15]=string('X0      = ',p.RegionX,format='(a10,i20," /")')
	fh[16]=string('Y0      = ',p.RegionY,format='(a10,i20," /")')
	fh[17]=string('BIN     = ',p.binx,format='(a10,i20," /")')
	fh[18]='COMMENT = none'
	fh[19]='HISTORY = RAW'
	fh[20:34]=string(' ',format='(a10)')
	fh[35]=string('END       ',format='(a10)')
	blnk80=string(' ',format='(a80)')
	fh=strmid(fh+blnk80,0,80)

	get_lun,Unit
	openw,Unit,file
	for j=0,35 do begin
		writeu,Unit,fh[j]
	endfor
	byteorder,imgs
	writeu,Unit,imgs
	close,Unit
	free_lun,Unit
	byteorder,imgs


end

;**************************************************************
pro set_wdroi,wd,p1	; set ROI widget
;--------------------------------------------------------------
	widget_control,wd.X0,set_value=string(p1.RegionX,form='(i4)')
	widget_control,wd.Y0,set_value=string(p1.RegionY,form='(i4)')
	widget_control,wd.WIDTH,set_value=string(p1.Width,form='(i4)')
	widget_control,wd.HEIGHT,set_value=string(p1.Height,form='(i4)')
end

;**************************************************************
pro prosil_handle, ev, wd, p, img1
;--------------------------------------------------------------

dbin=1600./wd.p.wx/p.binx

case (ev.id) of
   wd.PRV_START: begin
	nbins=128 &	imax=2l^12 -1
	ii=findgen(nbins)/nbins*imax
	x0=p.regionx/dbin
	y0=p.regiony/dbin
	hpos=float([x0,y0,x0,y0])/wd.p.wx+[0.05,0.05,0.2,0.17]
	while ev.id ne wd.PRV_STOP do begin
		ev = widget_event(wd.PRV_STOP,/nowait)
		img1=Gigobs1()
		tvscl,rebin(img1,p.Width/dbin,p.Height/dbin)>0,x0,y0
		if wd.p.hist_on then begin
			h=histogram(img1,max=imax,min=0,nbins=nbins)
			plot,ii,h,psym=10,/noerase,/xstyle,charsize=0.5, $
				pos=hpos,color=0
		endif
	endwhile
	end
   wd.EXPO: begin
	p.expo=float(gt_wdtxt(ev.id))
	p=CamSetParam(expo=p.expo)
	end
   wd.BIN: begin
	p.binx=fix(gt_wdtxt(ev.id)) & p.biny=p.binx
	dbin=1600./wd.p.wx/p.binx
	p=CamSetParam(binx=p.binx,biny=p.biny)
	set_wdroi,wd,p
	end
   wd.X0: begin
	p=CamSetParam(regionx=fix(gt_wdtxt(ev.id)))
	end
   wd.Y0: begin
	p=CamSetParam(regiony=fix(gt_wdtxt(ev.id)))
	end
   wd.WIDTH: begin
	p=CamSetParam(width=fix(gt_wdtxt(ev.id)))
	end
   wd.HEIGHT: begin
	p=CamSetParam(height=fix(gt_wdtxt(ev.id)))
	end
   wd.CBOX: begin
	box_cur1,x0, y0, nx, ny
	p=CamSetParam(regionx=x0*dbin)
	p=CamSetParam(regiony=y0*dbin)
	p=CamSetParam(width=nx*dbin)
	p=CamSetParam(height=ny*dbin)
	set_wdroi,wd,p
	end
   wd.FULL: begin
	p=CamSetParam(regionx=0)
	p=CamSetParam(regiony=0)
	p=CamSetParam(width=1600/p.binx)
	p=CamSetParam(height=1200/p.biny)
	set_wdroi,wd,p
	end
  else:
endcase

end

;************************************************************************
function prosil_widget,base,p
;--------------------------------------------------------------

wd={wd_prosil_V01,	$
	p: {prosil_gui, $
		wx:		800, 	$	; window x-size for image display
		wy:		600, 	$	;        y-size
		hist_on:	1	$	; histgram on/off
	},$
	PRV_START:	0l, 	$
	PRV_STOP:	0l, 	$
	EXPO:		0l, 	$
	BIN:		0l,	$
	X0:		0l,	$
	Y0:		0l,	$
	WIDTH:		0l,	$
	HEIGHT:		0l,	$
	FULL:		0l,	$
	CBOX:		0l,	$
	HIST:		0l	$
	}

	b2=widget_base(base, /colum, /frame)
	dmy = widget_label(b2, value='>>> Prosilica <<<')
	b21=widget_base(b2, /row)
	dmy = widget_label(b21, value='exp:')
	wd.EXPO = widget_text(b21,value=string(p.expo,form='(i8)'), xsize=7, uvalue='EXPO',/edit)
	dmy = widget_label(b21, value='usec, Prev.')
	wd.PRV_START=widget_button(b21, value="Start", uvalue = "PRV_START")
	wd.PRV_STOP=widget_button(b21, value="Stop", uvalue = "PRV_STOP")
	b22=widget_base(b2, /row)
	dmy = widget_label(b22, value='ROI: ')
	wd.FULL=widget_button(b22, value="full", uvalue = "FULL")
	wd.CBOX=widget_button(b22, value="Cbox", uvalue = "CBOX")
	dmy = widget_label(b22, value='s,  bin:')
	wd.BIN = widget_text(b22,value=string(p.binx,form='(i2)'), xsize=2, uvalue='BIN',/edit)

	b23=widget_base(b2, /row)
	dmy = widget_label(b23, value='x0:')
	wd.X0 = widget_text(b23,value=string(p.RegionX,form='(i4)'), xsize=4, uvalue='X0',/edit)
	dmy = widget_label(b23, value='y0:')
	wd.Y0 = widget_text(b23,value=string(p.RegionY,form='(i4)'), xsize=4, uvalue='Y0',/edit)
	dmy = widget_label(b23, value='nx:')
	wd.WIDTH = widget_text(b23,value=string(p.Width,form='(i4)'), xsize=4, uvalue='WIDTH',/edit)
	dmy = widget_label(b23, value='ny:')
	wd.HEIGHT = widget_text(b23,value=string(p.Height,form='(i4)'), xsize=4, uvalue='HEIGHT',/edit)

return,wd
end


;**************************************************************
pro test
common prosilica,prodll,p,img,msec0


p=CamInit()

stop
nimg=1
img=Gigobs(nimg)

stop

p=CamSetParam(expo=200000l)


stop
if p.Width gt 1000 then  window,0,xs=p.Width/2,ys=p.Height/2 $
else  window,0,xs=p.Width,ys=p.Height
img=intarr(p.Width,p.Height)
PrevImg,img



end

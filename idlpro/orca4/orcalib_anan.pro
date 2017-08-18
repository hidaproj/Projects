;  orcalib.pro

;  2013.5.11	k.i., t.k.
;  2013.6.10	k.i., 	OrcaSetParm  Binning ‚É–â‘è
;  2013.11.3	k.i., t.k.,	savefits_p, p-struct
;  2014.04.01	k.i., m.s.,	partial frame
;  2014.05.06	k.i.,     	'VARTYPE = UINT' key
;  2014.05.11	k.i.,     	X0, Y0, BIN FITS keyword
;  2014.05.18	k.i.,     	bug fix, noDev
;  2014.05.21	k.i.,     	orca_widget, orca_handle
;  2014/05/24	k.i.		preview hist minor change
;  2015/06/20	k.i.		orca_settrigmode, savefits_p
;  2015/07/24	k.i.		orca_widget
;  2015/08/23	k.i.,t.s,a.t.	orcaobs nowait, orca_getimg
;  2015.9.01    k.i.,a.t.	savefits_p  DATE_OBS,OB2  (a23)
;  2016.7.04    t.a.            OrcaOutTriggerProgramable, OrcaOutTriggerDefault


;**************************************************************
function p_orca4
;--------------------------------------------------------------
p={orca4_param,   $
   expo:        0.1,	  	$   ; exposure time [sec]
   framerate:   float(30),     	$   ; frame rate [frame/sec]
   gain:        0,      	$   ; gain 0-28
   bin:	        1,      	$   ; Binning XY 1-8
   Width:       2048,        	$   ; Width  max=2048 (binx=1), (cam pixel)/bin 
   Height:      2048,         	$   ; Height max=2048 (biny=1), (cam pixel)/bin
   RegionX:     0,           	$   ; left edge of ROI, pixel, (cam pixel)/bin
   RegionY:     0,           	$   ; top edge of ROI, pixel, (cam pixel)/bin
   TrigMode:    'Internal',    	$   ; trigger mode, 'Internal' or 'Start'
   TrigPol:	'Negative',	$   ; trigger polarity, 'Negative' or 'Positive'
   date_obs:   	'',           	$   ; yyyy-mm-ddThh:mm:ss.sss
   date_obs2:  	'',           	$   ; yyyy-mm-ddThh:mm:ss.sss
   timesys :   	'JST',         	$   ; yyyy-mm-ddThh:mm:ss.sss
   observat:   	'Hida',        	$   ;
   telescop:   	'DST',         	$   ;
   instrume:   	'VS',          	$   ;
   camera:     	'ORCA4',     	$   ; 
   wave:  	'',           	$   ; wavelength
   data_typ:	'OBJECT',	$   ; 'OBJECT','DARK', 'FLAT'
   clock:       0l,  		$   ; TimeStanmpFrequency [Hz]
   timelo:      0,           	$   ; Time stamp, lower 32-bits
   timehi:      0,           	$   ; Time stamp, upper 32-bits
   status:      0           	$   ; Status
   }

return,p
end

;**************************************************************
function OrcaInit,noDev=noDev0
;--------------------------------------------------------------
common orca4,orcadll,p,img,noDev

orcadll='C:\Projects\cprog\VS2010\orca4\x64\Debug\orca4.dll'
if keyword_set(noDev0) then noDev=1 else noDev=0
if not noDev then err=call_external(orcadll,'OrcaInit',/all_value,/cdecl)

p=p_orca4()
img=intarr(p.width,p.height)

return,p

end


;**************************************************************
pro OrcaFin
;--------------------------------------------------------------
common orca4,orcadll,p,img,noDev

if not noDev then err=call_external(orcadll,'OrcaFin',/all_value,/cdecl)

end
;**************************************************************
pro OrcaOutTriggerDefault
;--------------------------------------------------------------
common orca4,orcadll,p,img,noDev

err=call_external(orcadll,'SetOutputTriggerProgramableHSP',0,/all_value,/cdecl)

end
;**************************************************************
pro OrcaOutTriggerExposure
;--------------------------------------------------------------
common orca4,orcadll,p,img,noDev

rr=call_external(orcadll,'SetOutputTriggerProgramableHSP',1,/all_value,/cdecl)

end


;**************************************************************
function OrcaSetParam,expo=expo,gain=gain,bin=bin, $
	width=width,height=height,regionx=regionx,regiony=regiony, $
	framerate=framerate
;--------------------------------------------------------------
common orca4,orcadll,p,img,noDev

;  expo	-	exposure, sec, float
;  bin  -	1,2,4,,
;  gain	-	0,1,2,,,,20	(not implemented)
;  width, height, regionx,y, framerate  (not implemented)

;ret=call_external(orcadll,'CamIdle',/all_value,/cdecl)


if n_elements(expo) ne 0 then begin
	if not noDev then ret=call_external(orcadll,'SetOrcaExpo',expo,/all_value,/cdecl)
	p.expo=expo
endif

if n_elements(bin) ne 0 then begin
	if bin ne p.bin then begin
		if not noDev then begin
			err=call_external(orcadll,'OrcaFin',/all_value,/cdecl)
			wait,0.1
			err=call_external(orcadll,'OrcaInit',/all_value,/cdecl)
			wait,0.2
			ret=call_external(orcadll,'SetOrcaExpo',p.expo,/all_value,/cdecl)
			ret=call_external(orcadll,'SetOrcaBin',long(bin),/all_value,/cdecl)
		endif
		p.Width=p.Width/bin*p.bin
		p.Height=p.Height/bin*p.bin
		p.RegionX=p.RegionX/bin*p.bin
		p.RegionY=p.RegionY/bin*p.bin
		p.bin=bin
	endif
endif

if n_elements(regionx) ne 0 then p.RegionX=regionx
if n_elements(regiony) ne 0 then p.RegionY=regiony
if n_elements(width) ne 0 then p.Width=width
if n_elements(height) ne 0 then p.Height=height
; print,'OrcaSetParm:' &	help,p,/st


;img=intarr(p.width,p.height)
img=intarr(2048/p.bin,2048/p.bin)

return,p
end


;**************************************************************
function orcaobs,file=filename,nimg=nimg,nowait=nowait
;--------------------------------------------------------------
common orca4,orcadll,p,img,noDev

if not keyword_set(nimg) then nimg=1

if keyword_set(filename) then begin
	if not noDev then r=call_external(orcadll,'GrabImgToFile',nimg,filename,/all_value,/cdecl)
	return,0
endif


if not noDev then r=call_external(orcadll,'GrabImg',nimg)
if keyword_set(nowait) then return,-1


if not noDev then r=call_external(orcadll,'OrcaWait')

timstmp=lonarr(1)
;help,img,imgs &	help,p,/st

imgs=uintarr(p.width,p.height,nimg)
for i=0,nimg-1 do begin
	if not noDev then r=call_external(orcadll,'GoIdl',i,img,timstmp)	; timstmp not word 
	;help,img[p.RegionX:p.RegionX+p.Width-1,p.RegionY:p.RegionY+p.Height-1], imgs
	imgs[*,*,i]=img[p.RegionX:p.RegionX+p.Width-1,p.RegionY:p.RegionY+p.Height-1]
endfor

return,imgs


end

;**************************************************************
function orca_getimg,file=filename,nimg=nimg
;--------------------------------------------------------------
common orca4,orcadll,p,img,noDev

if not keyword_set(nimg) then nimg=1

if not noDev then r=call_external(orcadll,'OrcaWait')

timstmp=lonarr(1)
;help,img,imgs &	help,p,/st
imgs=uintarr(p.width,p.height,nimg)
for i=0,nimg-1 do begin
	if not noDev then r=call_external(orcadll,'GoIdl',i,img,timstmp)	; timstmp not word 
	;help,img[p.RegionX:p.RegionX+p.Width-1,p.RegionY:p.RegionY+p.Height-1], imgs
	imgs[*,*,i]=img[p.RegionX:p.RegionX+p.Width-1,p.RegionY:p.RegionY+p.Height-1]
endfor

return,imgs


end

;**************************************************************
pro orcaprev_event,ev
;--------------------------------------------------------------
common orcprv_com, wd, p, img1


case (ev.id) of
   wd.START: begin
	nbins=128 &	imax=2l^16 -1
	ii=findgen(nbins)/nbins*imax
	while ev.id ne wd.STOP do begin
		ev = widget_event(wd.STOP,/nowait)
		img1=OrcaObs(nimg=1)
		tvscl,rebin(img1,wd.nx,wd.ny)>0
		h=histogram(img1,max=imax,min=0,nbins=nbins)
		plot,ii,h,psym=10, $
		    /noerase,/xstyle,charsize=0.5,pos=[0.05,0.05,0.4,0.3],color=0
	endwhile
	end
   wd.STOP: begin
	end
   wd.SNAP: begin
	img1=OrcaObs(nimg=1)
	tvscl,rebin(img1,wd.nx,wd.ny)
	end
   wd.PROFS: begin
	profiles,img1
	end
   wd.EXPO: begin
	p.expo=float(gt_wdtxt(ev.id))
	p=OrcaSetParam(expo=p.expo)
	end
   wd.BIN: begin
	p.bin=fix(gt_wdtxt(ev.id))
	p=OrcaSetParam(bin=p.bin)
	end
   wd.Exit:  begin
	WIDGET_CONTROL, /destroy, ev.top
	return
	end
   else:
endcase


end


;**************************************************************
pro orcaprev,p0,img=img0
;--------------------------------------------------------------
common orcprv_com, wd, p, img1

p=p0
p=OrcaSetParam(expo=p.expo,bin=p.bin)

wd={wd_prv, $
	START:		0l, $
	STOP:		0l, $
	SNAP:		0l, $
	PROFS:		0l, $
	EXPO:		0.01, $
	BIN:		1, $
	nx:		512, $
	ny:		512, $
	Exit:		0l $
	}

window,0,xs=wd.nx,ys=wd.ny

base = WIDGET_BASE(title='ORCA4 prev.', /column) 
b1=widget_base(base, /row )
wd.START=widget_button(b1, value="Start", uvalue = "START")
wd.STOP=widget_button(b1, value="Stop", uvalue = "STOP")
wd.SNAP=widget_button(b1, value="Snap", uvalue = "SNAP")
wd.PROFS=widget_button(b1, value="Profs", uvalue = "PROFS")
b2=widget_base(base, /row )
dmy = widget_label(b2, value='exp:')
wd.EXPO = widget_text(b2,value=string(p.expo,form='(f5.3)'), xsize=5, uvalue='EXPO',/edit)
dmy = widget_label(b2, value='s  bin:')
wd.BIN = widget_text(b2,value=string(p.bin,form='(i2)'), xsize=2, uvalue='BIN',/edit)
b3=widget_base(base, /row )
wd.Exit=widget_button(b3, value="Exit", uvalue = "EXIT")


widget_control, base, /realize
XMANAGER, 'orcaprev', base;, /modal

p0=p
img0=img1

end


;**************************************************************
pro orca_settrigmode,mode,polarity=polarity
;--------------------------------------------------------------
common orca4,orcadll,p,img,noDev

case mode of
  'Internal': 	r=call_external(orcadll,"SetTriggerModeInternal",/all_value,/cdecl)
  'Start': 	r=call_external(orcadll,"SetTriggerModeStart",/all_value,/cdecl)
  else:  	print,'Orca trig mode ',mode,' not defined!'
endcase

if keyword_set(polarity) then begin
	case polarity of
	  'Negative': 	r=call_external(orcadll,"SetTriggerPolarityNegative",/all_value,/cdecl)
	  'Positive': 	r=call_external(orcadll,"SetTriggerPolarityPositive",/all_value,/cdecl)
	  else:  	print,'Polarity',polarity,' not defined!'
	endcase

endif

end


;**************************************************************
pro settriggermode
;--------------------------------------------------------------
common orca4,orcadll,p,img,noDev

r=call_external(orcadll,"SetTriggerMode",/all_value,/cdecl)

end

;**************************************************************
pro settriggerpolarity
;--------------------------------------------------------------
common orca4,orcadll,p,img,noDev

r=call_external(orcadll,"SetTriggerPolarity",/all_value,/cdecl)

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
;   TrigMode:    'Internal',   	$   ; trigger mode, 'Internal' or 'Start'
;   TrigPol:	'Negative',	$   ; trigger polarity, 'Negative' or 'Positive'
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
	fh[6] =string('DATE_OBS= ',p.date_obs,format='(a10,a23," /")')
	fh[7] =string('DATE_OB2= ',p.date_obs2,format='(a10,a23," /")')	; end time of integ
	fh[8] =string('TIMESYS = ',p.timesys,format='(a10,a20," /")')
	fh[9] =string('OBSERVAT= ',p.observat,format='(a10,a20," /")')
	fh[10]=string('TELESCOP= ',p.telescop,format='(a10,a20," /")')
	fh[11]=string('CAMERA  = ',p.camera,format='(a10,a20," /")')
	fh[12]=string('EXP     = ',p.expo,format='(a10,f20.10," /")')
	fh[13]=string('DATA_TYP= ',p.data_typ,format='(a10,a20," /")')
	fh[14]='VARTYPE = UINT'
	fh[15]=string('X0      = ',p.RegionX,format='(a10,i20," /")')
	fh[16]=string('Y0      = ',p.RegionY,format='(a10,i20," /")')
	fh[17]=string('BIN     = ',p.bin,format='(a10,i20," /")')
	fh[18]=string('TRIGMODE= ',p.TrigMode,format='(a10,a20," /")')
	fh[19]=string('TRIGPOL = ',p.TrigPol,format='(a10,a20," /")')
	fh[20]='COMMENT = none'
	fh[21]='HISTORY = RAW'
	fh[22:34]=string(' ',format='(a10)')
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
pro orca_handle, ev, wd, p, img1
;--------------------------------------------------------------

dbin=2048./wd.p.wx/p.bin

case (ev.id) of
   wd.PRV_START: begin
	nbins=128 &	imax=2l^16 -1
	ii=findgen(nbins)/nbins*imax
	x0=p.regionx/dbin
	y0=p.regiony/dbin
	hpos=float([x0,y0,x0,y0])/wd.p.wx+[0.05,0.05,0.2,0.17]
	while ev.id ne wd.PRV_STOP do begin
		ev = widget_event(wd.PRV_STOP,/nowait)
		img1=OrcaObs(nimg=1)
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
	p=OrcaSetParam(expo=p.expo)
	end
   wd.BIN: begin
	p.bin=fix(gt_wdtxt(ev.id))
	dbin=2048./wd.p.wx/p.bin
	p=OrcaSetParam(bin=p.bin)
	set_wdroi,wd,p
	end
   wd.X0: begin
	p=OrcaSetParam(regionx=fix(gt_wdtxt(ev.id)))
	end
   wd.Y0: begin
	p=OrcaSetParam(regiony=fix(gt_wdtxt(ev.id)))
	end
   wd.WIDTH: begin
	p=OrcaSetParam(width=fix(gt_wdtxt(ev.id)))
	end
   wd.HEIGHT: begin
	p=OrcaSetParam(height=fix(gt_wdtxt(ev.id)))
	end
   wd.CBOX: begin
	box_cur1,x0, y0, nx, ny
	p=OrcaSetParam(regionx=x0*dbin)
	p=OrcaSetParam(regiony=y0*dbin)
	p=OrcaSetParam(width=nx*dbin)
	p=OrcaSetParam(height=ny*dbin)
	set_wdroi,wd,p
	end
   wd.FULL: begin
	p=OrcaSetParam(regionx=0)
	p=OrcaSetParam(regiony=0)
	p=OrcaSetParam(width=2048/p.bin)
	p=OrcaSetParam(height=2048/p.bin)
	set_wdroi,wd,p
	end
  else:
endcase

end

;************************************************************************
function orca_widget,base,p
;--------------------------------------------------------------

wd={wd_orca_V01,	$
	p: {orca_gui, $
		wx:		1024, 	$	; window x-size for image display
		wy:		1024, 	$	;        y-size
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
	dmy = widget_label(b2, value='>>> ORCA <<<')
	b21=widget_base(b2, /row)
	dmy = widget_label(b21, value='exp:')
	wd.EXPO = widget_text(b21,value=string(p.expo,form='(f5.3)'), xsize=5, uvalue='EXPO',/edit)
	dmy = widget_label(b21, value='sec, Prev.')
	wd.PRV_START=widget_button(b21, value="Start", uvalue = "PRV_START")
	wd.PRV_STOP=widget_button(b21, value="Stop", uvalue = "PRV_STOP")
	b22=widget_base(b2, /row)
	dmy = widget_label(b22, value='ROI: ')
	wd.FULL=widget_button(b22, value="full", uvalue = "FULL")
	wd.CBOX=widget_button(b22, value="Cbox", uvalue = "CBOX")
	dmy = widget_label(b22, value='   bin:')
	wd.BIN = widget_text(b22,value=string(p.bin,form='(i2)'), xsize=2, uvalue='BIN',/edit)

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


;PRO DSTPOL_ObsLib
;+
;
;  DSTPOL_ObsLib.pro
;
;Functions for observation
;based on DST Poralization script by T. Anan and SMART T3 prosilica observation by T. Kawate
;
;20100908  T.A.
;20101125  T.A.		;hard trigger,MotorPulse,PrevObs,Get1ImageArray
;20131031  T.A.		;exclude AIO from PolIbs
;20160901  t.a.         ;POLOBS IDL_IDLBridge
;20161212  T.A.		;file name of POLOBS, image size in prevobs
;20161213  T.A.         ; saturated in prevobs
;
;
;========================headder==============================;
;function 	timeinit
;function 	gettime
;function	PrevObs
;function	Get1ImageArray
;function 	NormalObs
;function 	PolObs,polstate
;function 	CalibObs
;pro 		MessageBox,kotoba
;Function	MotorPulse,wp
;
;-

;========================include==============================;
	;@Prosilica_Lib
@C:\Projects\IDLPRO\dio8\CDIO_64_lib.pro

	;@AIO_Lib
@orcalib
;=========================main================================;



;**************************************************************
FUNCTION timeinit
;--------------------------------------------------------------
common obslib,msec0

caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
second=string(seco,form='(i2.2)')	&	second0=second
while (second eq second0) do begin
	caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
	second=string(seco,form='(i2.2)')
endwhile

	;msec0=GeTime()
return,msec0
END
;**************************************************************
FUNCTION gettime
;--------------------------------------------------------------
common obslib,msec0

caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
yyyymmdd_hhmmss=string(year,form='(i4.4)')+string(mon,form='(i2.2)')+string(day,form='(i2.2)') $
	+'_'+string(hour,form='(i2.2)')+string(minu,form='(i2.2)')+string(seco,form='(i2.2)')

	;msec=GeTime()

time0=msec-msec0
length=strlen(string(time0))
msec=strmid(string(time0),(length-3),3)

time=yyyymmdd_hhmmss+msec
return,time

END
;**************************************************************
FUNCTION PrevObs,wp,xy,mxmi=mxmi
;--------------------------------------------------------------

if not keyword_set(mxmi) then mxmi=[0,2.^12]
nimg=1

case wp.camera of
	0:begin		;GE1650
		img0=img2iDL(nimg)
		thresh=2.^12
		wx=800.;201612112 TA
		wy=600.;201612112 TA
	end
	1:begin		;ORCA-Flasch4.0
		img0=OrcaObs(nimg=nimg)
		thresh=2.^16
		wx=600.;201612112 TA
		wy=600.;201612112 TA
	end
endcase

pos=where(img0 ge thresh,npos)
img=congrid(img0,wx,wy)
;xy1=[xy[0]*2./wp.binx,xy[1]*2./wp.biny]
xy1=[xy[0]*2048./wx/wp.binx,xy[1]*2048./wx/wp.biny]
mxmi=[0,max(img0[*,xy1[1]])>max(img0[xy1[0],*])>mxmi[1]]


wset,1
chs=1.5
plot,indgen(wp.Width),img0[*,xy1[1]],charsize=chs,yr=mxmi,xstyle=1,		$
	xtitle='X [pix]'+' binx='+string(wp.binx,format='(i1)'),	$
	ytitle='[DN]',title='Y = '+string(xy[1],format='(i4)')
oplot,!x.crange,[thresh,thresh],line=1
plot,indgen(wp.Height),img0[xy1[0],*],charsize=chs,yr=mxmi,xstyle=1,		$
	xtitle='Y [pix]'+' biny='+string(wp.biny,format='(i1)'),	$
	ytitle='[DN]',title='X = '+string(xy[0],format='(i4)')
oplot,!x.crange,[thresh,thresh],line=1

wset,0
tvscl,img
if npos ge 1 then xyouts,0.1,0.1,/norm,'Saturated!!',charsize=10

return,img

END
;**************************************************************
FUNCTION Get1ImageArray
;--------------------------------------------------------------

nimg=1
img=img2iDL(nimg)
tvscl,img

return,img

END
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
	fh[17]=string('BIN     = ',p.bin,format='(a10,i20," /")')
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
PRO savefits_pp,imgs,p,wp,obs_type,polstate,sttime,file=file
;20161111  t.a.         ;add index 'AZ' and 'IMGROT'
;--------------------------------------------------------------

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
	;fh[6] =string('DATE_OBS= ',sttime,format='(a10,a23," /")')     ;comment out 20161125 TA
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

		;fh[18]=string('EXTEND  = ','F'		,format='(a10,a20," /")')
		;fh[19]=string('BSCALE  = ','1.0'	,format='(a10,f20.1," /")')
		;fh[20]=string('BZERO   = ','0.0'	,format='(a10,f20.1," /")')
		;fh[21]=string('ORIGIN  = ','HIDA OBSERVATORY'	,format='(a10,a20," /")')
		;fh[22]=string('INSTRUME= ',''		,format='(a10,a20," /")')
		;fh[23]=string('PROGRAM = ','dstvspol_pro',format='(a10,a20," /")')
		;fh[24]=string('PROG_VER= ','1'		,format='(a10,i20," /")')
	fh[18]=string('OBS_TYPE= ',obs_type	,format='(a10,a20," /")')
	fh[19]=string('POLSTATE= ',polstate	,format='(a10,a20," /")')
		;fh[27]=string('DATE    = ',p.date_obs	,format='(a10,a23," /")')
		;fh[28]=string('DATE_END= ',p.date_obs2	,format='(a10,a23," /")')
	fh[20]=string('WVPLATE = ',wp.waveplate	,format='(a10,a20," /")')
	fh[21]=string('PERIOD  = ',wp.period	,format='(a10,f20.10," /")')
	fh[22]=string('DETNAM  = ',p.camera	,format='(a10,a20," /")')
		;fh[32]=string('DET_TMP = ',-1		,format='(a10,i20," /")')
		;fh[33]=string('DET_PWR = ',-1		,format='(a10,i20," /")')
		;fh[34]=string('EXPTIME = ',p.expo	,format='(a10,f20.10," /")')
	fh[23]=string('CAMGAIN = ',wp.gain	,format='(a10,i20," /")')
		;fh[36]=string('FGBINX  = ',p.bin	,format='(a10,i20," /")')
		;fh[37]=string('FGBINY  = ',p.bin	,format='(a10,i20," /")')
		;fh[38]=string('X1      = ',p.RegionX+wp.Width-1,format='(a10,i20," /")')
		;fh[39]=string('Y1      = ',p.RegionY+wp.Height-1,format='(a10,i20," /")')
	fh[24]=string('WAVE    = ',wp.wavelength,format='(a10,i20," /")')
	fh[25]=string('R       = ',0.0		,format='(a10,f20.10," /")')
	fh[26]=string('P       = ',0.0		,format='(a10,f20.10," /")')
	fh[27]=string('I       = ',0.0		,format='(a10,f20.10," /")')
	fh[28]=string('HA      = ',0.0		,format='(a10,f20.10," /")')
	fh[29]=string('ZD      = ',0.0		,format='(a10,f20.10," /")')
	fh[30]=string('AZ      = ',0.0		,format='(a10,f20.10," /")')
	fh[31]=string('IMGROT  = ',0.0		,format='(a10,f20.10," /")')
	fh[32]='COMMENT = none'
	fh[33]='HISTORY = RAW'
	fh[34]=string(' ',format='(a10)')
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
PRO savefits_pp_nostr_tmp,imgs,fh,file=file
;--------------------------------------------------------------

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
print,'end'


end
;**************************************************************
FUNCTION NormalObs,wp,firstimg=firstimg
;20161111  t.a.         ;add index 'AZ' and 'IMGROT'
;--------------------------------------------------------------
COMMON bridge,bridge

case wp.camera of
	0:begin		;GE1650
		pro_init
		pro_setparam,wp
		sttime=gettime()
		getimg,wp.nimg
		entime=gettime()
		;savefits,'NORMAL','',sttime,entime,wp
		savefits,'NOM','',sttime,entime,wp
		savestmpfits,sttime,wp
		pro_exit
		filename=wp.svdir+wp.fname+sttime+'.fits'
	end
	1:begin		;ORCA-Flasch4.0
		;p=orcainit() ;comment out 20160709 T.A.
		p=OrcaSetParam(expo=wp.expo*1e-6,bin=wp.binx)
		p.date_obs=get_systime(ctime=stctime)

		if 1 then begin;- BRIDGE --------------------------


		obs_type='NOM'
		polstate=''
		done=0b
		while done ne 1 do begin
			for i=0,n_elements(bridge)-1 do begin
        			if bridge[i]->status() eq 0 then begin
					  ;print,get_systime(ctime=enctime)   ;~0sec
					imgs=OrcaObs(nimg=wp.nimg,/grabimg_wait)
					  ;print,get_systime(ctime=enctime)   ;~23sec
					bridge[i]->SetVar,'imgs',imgs
					;shmmap,get_os_handle=handle,template=imgs,/destroy_segment,get_name=seg_int
					;z=shmvar(seg_int)
					;for j=0,99 do z[*,*,j]=imgs[*,*,j]
					  ;print,get_systime(ctime=enctime)   ;~25sec
					p.date_obs2=get_systime(ctime=enctime)
					sttime=strmid(stctime,0,15)+strmid(stctime,16,3)
					filename=wp.svdir+wp.fname+sttime+'.fits'
					;filename=wp.svdir+wp.fname+sttime+'.sav'
		; PREPARING HEADER

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
		fh[6] =string('DATE_OBS= ',sttime,format='(a10,a23," /")')
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

		;fh[18]=string('EXTEND  = ','F'		,format='(a10,a20," /")')
		;fh[19]=string('BSCALE  = ','1.0'	,format='(a10,f20.1," /")')
		;fh[20]=string('BZERO   = ','0.0'	,format='(a10,f20.1," /")')
		;fh[21]=string('ORIGIN  = ','HIDA OBSERVATORY'	,format='(a10,a20," /")')
		;fh[22]=string('INSTRUME= ',''		,format='(a10,a20," /")')
		;fh[23]=string('PROGRAM = ','dstvspol_pro',format='(a10,a20," /")')
		;fh[24]=string('PROG_VER= ','1'		,format='(a10,i20," /")')
		fh[18]=string('OBS_TYPE= ',obs_type	,format='(a10,a20," /")')
		fh[19]=string('POLSTATE= ',polstate	,format='(a10,a20," /")')
		;fh[27]=string('DATE    = ',p.date_obs	,format='(a10,a23," /")')
		;fh[28]=string('DATE_END= ',p.date_obs2	,format='(a10,a23," /")')
		fh[20]=string('WVPLATE = ',wp.waveplate	,format='(a10,a20," /")')
		fh[21]=string('PERIOD  = ',wp.period	,format='(a10,f20.10," /")')
		fh[22]=string('DETNAM  = ',p.camera	,format='(a10,a20," /")')
		;fh[32]=string('DET_TMP = ',-1		,format='(a10,i20," /")')
		;fh[33]=string('DET_PWR = ',-1		,format='(a10,i20," /")')
		;fh[34]=string('EXPTIME = ',p.expo	,format='(a10,f20.10," /")')
		fh[23]=string('CAMGAIN = ',wp.gain	,format='(a10,i20," /")')
		;fh[36]=string('FGBINX  = ',p.bin	,format='(a10,i20," /")')
		;fh[37]=string('FGBINY  = ',p.bin	,format='(a10,i20," /")')
		;fh[38]=string('X1      = ',p.RegionX+wp.Width-1,format='(a10,i20," /")')
		;fh[39]=string('Y1      = ',p.RegionY+wp.Height-1,format='(a10,i20," /")')
		fh[24]=string('WAVE    = ',wp.wavelength,format='(a10,i20," /")')
		fh[25]=string('R       = ',0.0		,format='(a10,f20.10," /")')
		fh[26]=string('P       = ',0.0		,format='(a10,f20.10," /")')
		fh[27]=string('I       = ',0.0		,format='(a10,f20.10," /")')
		fh[28]=string('HA      = ',0.0		,format='(a10,f20.10," /")')
		fh[29]=string('ZD      = ',0.0		,format='(a10,f20.10," /")')
		fh[30]=string('AZ      = ',0.0		,format='(a10,f20.10," /")')
		fh[31]=string('IMGROT  = ',0.0		,format='(a10,f20.10," /")')
		fh[32]='COMMENT = none'
		fh[33]='HISTORY = RAW'
		fh[34]=string(' ',format='(a10)')
		fh[35]=string('END       ',format='(a10)')
		blnk80=string(' ',format='(a80)')
		fh=strmid(fh+blnk80,0,80)

		bridge[i]->SetVar,'fh',fh
		bridge[i]->SetVar,'filename',filename
		bridge[i]->Execute,	$
			'savefits_pp_nostr,imgs,fh,file=filename',/nowait
		  ;print,get_systime(ctime=enctime)   ;~28sec
		wait,0.1
					print,'saved   '+filename
					done=1b
					break      
				endif
			endfor
		endwhile
		  ;print,get_systime(ctime=enctime)   ;~26sec

		endif else begin;---------------------------
			;print,get_systime(ctime=enctime)   ;0sec
			imgs=OrcaObs(nimg=wp.nimg,/grabimg_wait)
			;print,get_systime(ctime=enctime)   ;~23sec
			p.date_obs2=get_systime(ctime=enctime)
			;orcafin  ;comment out 20160709 T.A.
			sttime=strmid(stctime,0,15)+strmid(stctime,16,3)
			filename=wp.svdir+wp.fname+sttime+'.fits'
			savefits_pp,imgs,p,wp,'NOM','',p.date_obs,file=filename
			;print,get_systime(ctime=enctime)   ;~24sec
			print,filename
		endelse
	end
endcase

firstimg=imgs[*,*,0]
;stop
return,filename

END
;**************************************************************
FUNCTION PolObs,polstate,wp,firstimg=firstimg
;20161111  t.a.         ;add index 'AZ' and 'IMGROT'
;--------------------------------------------------------------
COMMON bridge,bridge

case wp.camera of
	0:begin		;GE1650
		pro_init
		pro_setparam,wp
		;p=AIO_start()						;2013.10.31, anan
		sttime=gettime()
		hardtrigger_getimg,wp.nimg
		;AIO_stop						;2013.10.31, anan
		entime=gettime()
		savefits,'POL',polstate,sttime,entime,wp
		savestmpfits,sttime,wp
		pro_exit
		;rtwv=AIO_read();	&plot,rtwv			;2013.10.31, anan
		;save,file=wp.svdir+'wvplt'+sttime+'.sav',p,rtwv	;2013.10.31, anan

		filename=wp.svdir+wp.fname+sttime+'.fits'
	end
	1:begin		;ORCA-Flasch4.0
			;p=orcainit() 					;comment out 20160709 T.A.
		p=OrcaSetParam(expo=wp.expo*1e-6,bin=wp.binx)
		orca_settrigmode,'Start',polarity='Negative'
		p.date_obs=get_systime(ctime=stctime)
		print,'trigger waiting, ',p.date_obs & wait,0.1

		if 0 then begin;- BRIDGE --------------------------

		done=0b
		while done ne 1 do begin
			for i=0,n_elements(bridge)-1 do begin
        			if bridge[i]->status() eq 0 then begin
					  ;print,get_systime(ctime=enctime)   ;~0sec
					imgs=OrcaObs(nimg=wp.nimg,/grabimg_wait)
					p.date_obs2=get_systime(ctime=enctime)
					print,'complete taking images',p.date_obs2

					  ;print,get_systime(ctime=enctime)   ;~23sec
					bridge[i]->SetVar,'imgs',imgs
					;shmmap,get_os_handle=handle,template=imgs,/destroy_segment,get_name=seg_int
					;z=shmvar(seg_int)
					;for j=0,99 do z[*,*,j]=imgs[*,*,j]
					  ;print,get_systime(ctime=enctime)   ;~25sec
					;sttime=strmid(stctime,0,15)+strmid(stctime,16,3) ;comout 20161212 TA
					;filename=wp.svdir+wp.fname+sttime+'.fits'        ;comout 20161212 TA
					entime=strmid(enctime,0,15)+strmid(enctime,16,3)  ;20161212 TA
					filename=wp.svdir+wp.fname+entime+'.fits'         ;20161212 TA
		; PREPARING HEADER
		obs_type='POL'
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
		;fh[6] =string('DATE_OBS= ',sttime,format='(a10,a23," /")')     ;comment out 20161125 TA
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

		;fh[18]=string('EXTEND  = ','F'		,format='(a10,a20," /")')
		;fh[19]=string('BSCALE  = ','1.0'	,format='(a10,f20.1," /")')
		;fh[20]=string('BZERO   = ','0.0'	,format='(a10,f20.1," /")')
		;fh[21]=string('ORIGIN  = ','HIDA OBSERVATORY'	,format='(a10,a20," /")')
		;fh[22]=string('INSTRUME= ',''		,format='(a10,a20," /")')
		;fh[23]=string('PROGRAM = ','dstvspol_pro',format='(a10,a20," /")')
		;fh[24]=string('PROG_VER= ','1'		,format='(a10,i20," /")')
		fh[18]=string('OBS_TYPE= ',obs_type	,format='(a10,a20," /")')
		fh[19]=string('POLSTATE= ',polstate	,format='(a10,a20," /")')
		;fh[27]=string('DATE    = ',p.date_obs	,format='(a10,a23," /")')
		;fh[28]=string('DATE_END= ',p.date_obs2	,format='(a10,a23," /")')
		fh[20]=string('WVPLATE = ',wp.waveplate	,format='(a10,a20," /")')
		fh[21]=string('PERIOD  = ',wp.period	,format='(a10,f20.10," /")')
		fh[22]=string('DETNAM  = ',p.camera	,format='(a10,a20," /")')
		;fh[32]=string('DET_TMP = ',-1		,format='(a10,i20," /")')
		;fh[33]=string('DET_PWR = ',-1		,format='(a10,i20," /")')
		;fh[34]=string('EXPTIME = ',p.expo	,format='(a10,f20.10," /")')
		fh[23]=string('CAMGAIN = ',wp.gain	,format='(a10,i20," /")')
		;fh[36]=string('FGBINX  = ',p.bin	,format='(a10,i20," /")')
		;fh[37]=string('FGBINY  = ',p.bin	,format='(a10,i20," /")')
		;fh[38]=string('X1      = ',p.RegionX+wp.Width-1,format='(a10,i20," /")')
		;fh[39]=string('Y1      = ',p.RegionY+wp.Height-1,format='(a10,i20," /")')
		fh[24]=string('WAVE    = ',wp.wavelength,format='(a10,i20," /")')
		fh[25]=string('R       = ',0.0		,format='(a10,f20.10," /")')
		fh[26]=string('P       = ',0.0		,format='(a10,f20.10," /")')
		fh[27]=string('I       = ',0.0		,format='(a10,f20.10," /")')
		fh[28]=string('HA      = ',0.0		,format='(a10,f20.10," /")')
		fh[29]=string('ZD      = ',0.0		,format='(a10,f20.10," /")')
		fh[30]=string('AZ      = ',0.0		,format='(a10,f20.10," /")')
		fh[31]=string('IMGROT  = ',0.0		,format='(a10,f20.10," /")')
		fh[32]='COMMENT = none'
		fh[33]='HISTORY = RAW'
		fh[34]=string(' ',format='(a10)')
		fh[35]=string('END       ',format='(a10)')
		blnk80=string(' ',format='(a80)')
		fh=strmid(fh+blnk80,0,80)

		bridge[i]->SetVar,'fh',fh
		bridge[i]->SetVar,'filename',filename
		bridge[i]->Execute,	$
			'savefits_pp_nostr,imgs,fh,file=filename',/nowait
		  ;print,get_systime(ctime=enctime)   ;~28sec
		print,'saved   '+filename
		wait,0.1
		done=1b
					break      
				endif
			endfor
		endwhile
		;print,get_systime(ctime=enctime)   ;~26sec

		endif else begin;---------------------------
			imgs=OrcaObs(nimg=wp.nimg,/grabimg_wait)
			p.date_obs2=get_systime(ctime=enctime)
			print,'complete taking images',p.date_obs2

			  ;print,'finish takeing image',p.date_obs2 & wait,0.1   ;~26sec
			  ;orcafin					;comment out 20160709 T.A.
			;sttime=strmid(stctime,0,15)+strmid(stctime,16,3) ;comout 20161212 TA
			;filename=wp.svdir+wp.fname+sttime+'.fits'        ;comout 20161212 TA
			entime=strmid(enctime,0,15)+strmid(enctime,16,3)  ;20161212 TA
			filename=wp.svdir+wp.fname+entime+'.fits'         ;20161212 TA

			savefits_pp,imgs,p,wp,'POL',polstate,p.date_obs,file=filename
			print,'saved   '+filename
			wait,0.1
		endelse
	end
endcase

firstimg=imgs[*,*,0]



return,filename

END
;**************************************************************
FUNCTION CalibObs,wp,wd
;--------------------------------------------------------------

fn=strarr(9)
waittime=10.
cdio_init
						;wp.input=cdio_input()
  widget_CONTROL,wd.in,set_value=wp.input
tmp=''
cdio_o45,wp,wd	&	purpose='45'
while (tmp eq '') do begin
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output

	if (wp.input eq '45°') and (purpose eq '45') then begin
		print,'45'	&wait,5		;wait,5がないと入力信号の重なりが起きる＊ポイント！！
		fn[0]=PolObs('45',wp)
		wait,waittime
		cdio_o90,wp,wd	&	purpose='90'
print,'===== 45°終了 ====='
	endif
	if (wp.input eq '90°') and (purpose eq '90')  then begin
		print,'90'	&wait,5		;wait,5がないと入力信号の重なりが起きる＊ポイント！！
		fn[1]=PolObs('90',wp)
		wait,waittime
		cdio_o135,wp,wd	&	purpose='135'
print,'===== 90°終了 ====='
	endif
	if (wp.input eq  '135°') and (purpose eq '135')  then begin
		print,'135'	&wait,5	;wait,5がないと入力信号の重なりが起きる＊ポイント！！
		fn[2]=PolObs('135',wp)
		wait,waittime
		cdio_o180,wp,wd	&	purpose='180'
print,'===== 135°終了 ====='
	endif
	if (wp.input eq  '180°') and (purpose eq '180') then begin
		print,'180'	&wait,5		;wait,5がないと入力信号の重なりが起きる＊ポイント！！
		fn[3]=PolObs('180',wp)
		wait,waittime
		cdio_o225,wp,wd	&	purpose='225'
print,'===== 180°終了 ====='
	endif
;	if (wp.input eq  '225°') and (purpose eq '225') then begin
	if (wp.input eq  '135°') and (purpose eq '225') then begin
		print,'225'	&wait,5		;wait,5がないと入力信号の重なりが起きる＊ポイント！！
		fn[4]=PolObs('225',wp)
		wait,waittime
		cdio_o270,wp,wd	&	purpose='270'
		;wait,2
					;		while (wp.input eq  '135°') do wp.input=cdio_input()

print,'===== 225°終了 ====='
	endif
;	if (wp.input eq  '270°') and (purpose eq '270') then begin
	if (wp.input eq  '135°') and (purpose eq '270') then begin
		print,'270'	&wait,5		;wait,5がないと入力信号の重なりが起きる＊ポイント！！
		fn[5]=PolObs('270',wp)
		wait,waittime
		cdio_o315,wp,wd	&	purpose='315'
print,'===== 270°終了 ====='
	endif
	if (wp.input eq  '315°') and (purpose eq '315') then begin
		print,'315'	&wait,5		;wait,5がないと入力信号の重なりが起きる＊ポイント！！
		fn[6]=PolObs('315',wp)
		wait,waittime
		cdio_o360,wp,wd	&	purpose='360'
print,'===== 315°終了 ====='
	endif
	if (wp.input eq  '360°') and (purpose eq '360') then begin
		print,'0'	&wait,5		;wait,5がないと入力信号の重なりが起きる＊ポイント！！
		fn[7]=PolObs('0',wp)
		wait,waittime
		cdio_op25,wp,wd	&	purpose='22.5'
print,'===== 0°終了 ====='
	endif
	if (wp.input eq  '+22.5°') and (purpose eq '22.5')  then begin
;MessageBox,'無偏光撮影OK ??'
		print,'non'	&wait,5.
		fn[8]=PolObs('',wp)
		wait,waittime
		tmp='finish'
print,'===== 22.5°終了 ====='
	endif

	;print,wp.input
endwhile
cdio_exit

return,fn
END
;**************************************************************
PRO MessageBox,kotoba
;--------------------------------------------------------------
common obslib

cd,'C:\Projects\cprog\VS2005\prosilica'
prodll='C:\Projects\cprog\VS2005\prosilica\Debug\prosilica.dll'
r=call_external(prodll,'CamOpen', kotoba, /PORTABLE )

END
;**************************************************************
Function MotorPulse,wp			;imcomplete
;--------------------------------------------------------------
common obslib

bin0 = wp.binx > wp.biny
bin1 = wp.binx < wp.biny

case bin0 of				; threshfold of frame rate [frames/sec]
	1:thr0=25.
	2:thr0=25.
	4:thr0=60.
	8:thr0=75.
	else:thr0=25.
endcase
case bin1 of				; threshfold of frame rate [frames/sec]
	1:thr1=35.
	2:thr1=35.
	4:thr1=70.
	8:thr1=80.
	else:thr1=80.
endcase

framerate0 = (1.e6/wp.expo) < thr0	; [frames/sec] s
framerate1 = (1.e6/wp.expo) < thr1	; [frames/sec] l

period0 = 16.*(1./framerate0)		; [sec]
period1 = wp.nimg*(1./framerate1)	; [sec]
pulse = string(long([507904./period1,507904./period0]))		; [pulse/sec]

return,pulse

END

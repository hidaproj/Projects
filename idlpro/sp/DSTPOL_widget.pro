
;PRO DSTPOL_widget
;+
;
;  DSTPOL_widget.pro
;
;Main debug procedure by widget for DST Spectro-Polarimetric prosilica observation
;based on DST Poralization script by T. Anan and SMART T3 prosilica observation by T. Kawate
;
;20100908  T.A.
;20101125  T.A.			;etc.,window,preview,profile
;20110827  T.A.			; telescope position, pulse of mortor, waveplate
;20121107  T.A.			; exclude telescope position, r, p, i, change motor pulse to period
;20131031  T.A.			; makee save directory automaticaly
;20140222  T.A.			; add 'LUCEO127#2' to waveplate
;
;==========================headder============================;
;PRO DSTPOL_widget_event, ev
;PRO DSTPOL_widget
;
;-

;=========================include=============================;
@DSTPOL_ObsLib
@CDIO_Lib
;=========================main================================;

;**************************************************************
pro DSTPOL_widget_event, ev
;--------------------------------------------------------------
common widgetlib,wp,wd,svwp1,svwp2,svwp3,svwp4

rdio=1
stprm=1
windex_prev=0
windex_prof=1

widget_control, ev.id, get_uvalue=uvalue,get_value=value

print,'uvalue=',uvalue

	;r=cdio_init()
	;wp.input=''
	;for i=0,200 do widget_CONTROL,wd.in,set_value=wp.input
	;wp.input=cdio_input()
	;for i=0,200 do widget_CONTROL,wd.in,set_value=wp.input
	;cdio_exit


	if (uvalue eq "nimg") then begin
		if (fix(value) lt 0) or (fix(value) ge 101) then begin
			;print,'integ must be from 0 to 100'
			MessageBox,'integ must be from 0 to 100'
		endif else begin	&	wp.nimg=fix(value)	&	endelse		&	endif
	if (uvalue eq "expo") then wp.expo=float(value)
	if (uvalue eq "gain") then wp.gain=fix(value)
	if (uvalue eq "binx") then begin
		wp.binx=fix(value)
		wp.Width=1600./wp.binx
		widget_CONTROL,wd.Width,set_value=string(wp.Width, form='(i5)')
	endif
	if (uvalue eq "biny") then begin
		wp.biny=fix(value)
		wp.Height=1200./wp.biny
		widget_CONTROL,wd.height,set_value=string(wp.Height, form='(i5)')
	endif
	if (uvalue eq "height") then begin
		if (fix(value) gt 1200/wp.binx) then begin
  			widget_CONTROL,wd.height,set_value=string(wp.Height, form='(i5)')
		endif else begin  &  wp.Height=fix(value)  &  endelse  &  endif
	if (uvalue eq "width") then begin
		if (fix(value) gt 1600/wp.biny) then begin
  			widget_CONTROL,wd.width,set_value=string(wp.Width, form='(i5)')
		endif else begin  &  wp.Width=fix(value)  &  endelse  &  endif
	if (uvalue eq "regionx") then begin
		print,'RegionX Help : Start of region readout, in pixels; left edge.'
		if (fix(value) gt (1600./wp.binx-wp.Width)) then begin
  			widget_CONTROL,wd.regionx,set_value=string(wp.RegionX, form='(i5)')
		endif else begin  &  wp.RegionX=fix(value)  &  endelse  &  endif
	if (uvalue eq "regiony") then begin
		print,'RegionY Help : Start of region readout, in pixels; top edge.'
		if (fix(value) gt (1200./wp.biny-wp.Height)) then begin
  			widget_CONTROL,wd.regiony,set_value=string(wp.RegionY, form='(i5)')
		endif else begin  &  wp.RegionY=fix(value)  &  endelse  &  endif
	if (uvalue eq "svprm1") then begin & svwp1=wp & help,svwp1,/st & endif
	if (uvalue eq "svprm2") then begin & svwp2=wp & help,svwp2,/st & endif
	if (uvalue eq "svprm3") then begin & svwp3=wp & help,svwp3,/st & endif
	if (uvalue eq "svprm4") then begin & svwp4=wp & help,svwp4,/st & endif
	if (uvalue eq "svprm") then begin
		wp.svprm=value
		case wp.svprm of
			'1': begin
				wp.expo=svwp1.expo
				wp.gain=svwp1.gain
				wp.nimg=svwp1.nimg
				wp.binx=svwp1.binx
				wp.biny=svwp1.biny
				wp.Width=svwp1.Width
				wp.Height=svwp1.Height
				wp.RegionX=svwp1.RegionX
				wp.RegionY=svwp1.RegionY
			end
			'2': begin
				wp.expo=svwp2.expo
				wp.gain=svwp2.gain
				wp.nimg=svwp2.nimg
				wp.binx=svwp2.binx
				wp.biny=svwp2.biny
				wp.Width=svwp2.Width
				wp.Height=svwp2.Height
				wp.RegionX=svwp2.RegionX
				wp.RegionY=svwp2.RegionY
			end
			'3': begin
				wp.expo=svwp3.expo
				wp.gain=svwp3.gain
				wp.nimg=svwp3.nimg
				wp.binx=svwp3.binx
				wp.biny=svwp3.biny
				wp.Width=svwp3.Width
				wp.Height=svwp3.Height
				wp.RegionX=svwp3.RegionX
				wp.RegionY=svwp3.RegionY
			end
			'4': begin
				wp.expo=svwp4.expo
				wp.gain=svwp4.gain
				wp.nimg=svwp4.nimg
				wp.binx=svwp4.binx
				wp.biny=svwp4.biny
				wp.Width=svwp4.Width
				wp.Height=svwp4.Height
				wp.RegionX=svwp4.RegionX
				wp.RegionY=svwp4.RegionY
			end
			else:print,' check the # of load'
		endcase

		widget_CONTROL,wd.expo,set_value=string(wp.expo, form='(i5)')
		widget_CONTROL,wd.gain,set_value=string(wp.gain, form='(i5)')
		widget_CONTROL,wd.nimg,set_value=string(wp.nimg, form='(i5)')
		widget_CONTROL,wd.binx,set_value=string(wp.binx, form='(i5)')
		widget_CONTROL,wd.biny,set_value=string(wp.biny, form='(i5)')
		widget_CONTROL,wd.Width,set_value=string(wp.Width, form='(i5)')
		widget_CONTROL,wd.height,set_value=string(wp.Height, form='(i5)')
		widget_CONTROL,wd.regionx,set_value=string(wp.RegionX, form='(i5)')
		widget_CONTROL,wd.regiony,set_value=string(wp.RegionY, form='(i5)')
		help,wp,/st
	endif



	if (uvalue eq "svdir") then wp.svdir=value
	if (uvalue eq "fname") then wp.fname=value
	if (uvalue eq "nf") then wp.nf=value
	if (uvalue eq "wavelength") then wp.wavelength=value
	if (uvalue eq "r_m") then wp.r_m=strcompress(string(value),/remove_all)
	if (uvalue eq "r_s") then wp.r_s=strcompress(string(value),/remove_all)
	if (uvalue eq "p_d") then wp.p_d=strcompress(string(value),/remove_all)
	if (uvalue eq "p_m") then wp.p_m=strcompress(string(value),/remove_all)
	if (uvalue eq "i_d") then wp.i_d=strcompress(string(value),/remove_all)
	if (uvalue eq "i_m") then wp.i_m=strcompress(string(value),/remove_all)
	if (uvalue eq "i_s") then wp.i_s=strcompress(string(value),/remove_all)
	if (uvalue eq "position") then begin
		case value of
			0:wp.position='west'
			1:wp.position='east'
		endcase
	endif
;	if (uvalue eq "mtpulse") then wp.mtpulse=strcompress(string(value),/remove_all)
	if (uvalue eq "period") then wp.period=strcompress(string(value),/remove_all)
	if (uvalue eq "waveplate") then begin
		case value of
			0:wp.waveplate='LUCEO127#2'
			1:wp.waveplate='APSAW'
			2:wp.waveplate='Quarts'
		endcase
	endif

	ev_prevstop=widget_event(wd.bt_pren,/nowait)
	ev_opend=widget_event(wd.opend,/nowait)
	ev_opst=widget_event(wd.bs_wv_op,/nowait)

	;------ Save -------;
	if (uvalue eq "svo") then begin
		fn=strarr(wp.nf)
		for i=0,wp.nf-1 do begin
print,'===== # '+strcompress(string(i+1),/remove_all)+' ====='
			fn[i]=NormalObs(wp)
		endfor
		;==表示==;
		if (wp.binx ne 1) and (wp.biny ne 1) then begin 
			wx=wp.Width/2	& wy=wp.Height/2
			window,0,ys=wy,xs=wx
			for i=0,wp.nf-1 do begin
				mreadfits,fn[i],h,img
				img=rebin(img,wx,wy,wp.nimg)
				for j=0,wp.nimg-1 do begin
					tvscl,img[*,*,j]
					xyouts,0.05,0.05,string(j)+' binx='+	$
						string(wp.binx,format='(i1)')+	$
						' biny='+string(wp.biny,format='(i1)'),/norm
				endfor
			endfor
		endif else print,'not display observed images because << binning 1>>'
MessageBox,'!!FINISH NORMAL OBSERVATION!!'
	endif

	if (uvalue eq "svp") then begin
		fn=strarr(wp.nf)
		for i=0,wp.nf-1 do begin
print,'===== # '+strcompress(string(i+1),/remove_all)+' ====='
			fn[i]=PolObs('',wp)
		endfor
		;==表示==;
		if (wp.binx ne 1) and (wp.biny ne 1) then begin 
			wx=wp.Width/2	& wy=wp.Height/2
			window,0,ys=wy,xs=wx
			for i=0,wp.nf-1 do begin
				mreadfits,fn[i],h,img
				img=rebin(img,wx,wy,wp.nimg)
				for j=0,wp.nimg-1 do begin
					tvscl,img[*,*,j]
					xyouts,0.05,0.05,string(j)+' binx='+	$
						string(wp.binx,format='(i1)')+	$
						' biny='+string(wp.biny,format='(i1)'),/norm
				endfor
			endfor
		endif else print,'not display observed images because << binning 1>>'
MessageBox,'!!FINISH POLARIMETRIC OBSERVATION!!'
	endif

	if (uvalue eq "calib") then begin
		fn=CalibObs(wp,wd)
		;==表示==;
		if (wp.binx ne 1) and (wp.biny ne 1) then begin 
			wx=wp.Width/2	& wy=wp.Height/2
			window,0,ys=wy,xs=wx
			for i=0,8 do begin
				mreadfits,fn[i],h,img
				img=rebin(img,wx,wy,wp.nimg)
				for j=0,wp.nimg-1 do begin
					tvscl,img[*,*,j]
					xyouts,0.05,0.05,string(j)+' binx='+	$
						string(wp.binx,format='(i1)')+	$
						' biny='+string(wp.biny,format='(i1)'),/norm
				endfor
			endfor
		endif else print,'not display observed images because << binning 1>>'
MessageBox,'!!FINISH GETING CALIBRATION DATA!!'
	endif
;=========== preview ==========;


	if (uvalue eq 'prev_st') then begin
		wdelete,windex_prof
		window,0,xs=800,ys=600
		window,1,xs=800,ys=350
		!p.multi=[0,2,1]

		pro_init
		pro_setparam,wp
		xy=[400,300]
		img=PrevObs(wp,xy)
		print,'Click where you want to look profiles!!'
		cursor,xx,yy,/device	& xy=[xx,yy]	& print,xy
		mxmi=[0,max(img[*,xy[1]])>max(img[xy[0],*])]
		while ev_prevstop.id eq 0 do begin
			img=PrevObs(wp,xy,mxmi=mxmi)

			ev_prevstop=widget_event(wd.bt_pren,/nowait)
			ev_emp=widget_event(/nowait)
		endwhile
		pro_exit
		!p.multi=0
		wdelete,0	& wdelete,1
	endif

;========= profiles ==============;
	if (uvalue eq 'prof') then begin
		wdelete,windex_prev
		binmin=min([wp.binx,wp.biny],nn)
		if binmin eq 1 then begin
			print,''
			print,'>> compress image becasuse binning = 1 <<'
			print,''
			if nn eq 0 then begin
				wx=800	& wy=600/(wp.biny/wp.binx)
			endif else begin
				wx=800/(wp.binx/wp.biny)	& wy=600
			endelse
		endif else begin
			wx=wp.Width	& wy=wp.Height
		endelse
		window,windex_prof,xs=wx,ys=wy
		wset,windex_prof
		!p.multi=0
		pro_init
		pro_setparam,wp
		img=rebin(Get1ImageArray(),wx,wy)
		pro_exit
		profiles,img
	endif

;========= contec dio==============;
	if (uvalue eq 'diostart') then begin
		;while (ev_opst ne 0) and ev_opend.id eq 0 do begin
		while ev_opend.id eq 0 do begin
			cdio_init
			wp.input=cdio_input()
  			  widget_CONTROL,wd.in,set_value=wp.input
			wp.output=cdio_outstate()
			  widget_CONTROL,wd.out,set_value=wp.output


			if (uvalue eq 'o45') then cdio_o45,wp,wd
			if (uvalue eq 'o90') then cdio_o90,wp,wd
			if (uvalue eq 'o135') then cdio_o135,wp,wd
			if (uvalue eq 'o180') then cdio_o180,wp,wd
			if (uvalue eq 'o225') then cdio_o225,wp,wd
			if (uvalue eq 'o270') then cdio_o270,wp,wd
			if (uvalue eq 'o315') then cdio_o315,wp,wd
			if (uvalue eq 'o360') then cdio_o360,wp,wd
			if (uvalue eq 'op25') then cdio_op25,wp,wd
			if (uvalue eq 'opjg') then begin
				while ev_opend.id eq 0 do begin
					ev_dio = widget_event(wd.bs_wv_op,/nowait)
					if ev_dio.id ne 0 then $
					WIDGET_CONTROL, get_uvalue=uvalue, ev_dio.id
					if (uvalue ne '') and (uvalue ne 'opjg') then break
					cdio_opjg,wp,wd
				endwhile
			endif
			if (uvalue eq 'omjg') then begin
				while ev_opend.id eq 0 do begin
					ev_dio = widget_event(wd.bs_wv_op,/nowait)
					if ev_dio.id ne 0 then $
					WIDGET_CONTROL, get_uvalue=uvalue, ev_dio.id
					if (uvalue ne '') and (uvalue ne 'omjg') then break
					cdio_omjg,wp,wd
				endwhile
			endif
			if (uvalue eq 'o0') then cdio_o0,wp,wd
			if (uvalue eq 'o0set') then cdio_o0set,wp,wd
			if (uvalue eq 'ow0ad') then cdio_ow0ad,wp,wd
			if (uvalue eq 'ostop') then cdio_ostop,wp,wd
			uvalue=''

			ev_dio = widget_event(wd.bs_wv_op,/nowait)
			if ev_dio.id ne 0 then $
			WIDGET_CONTROL, get_uvalue=uvalue, ev_dio.id

			ev_opend=widget_event(wd.opend,/nowait)
			ev_emp=widget_event(/nowait)
		endwhile
		cdio_exit
	endif

;========= extra ==============;
	if (uvalue eq 'close1') then begin	; ref. C:\Projects\IDLPRO\IR_Obs\dstangle.pro
		close,1
		pro_exit
	endif
	wp.motorpulse=MotorPulse(wp)
	widget_CONTROL,wd.motorpulse,set_value=string(wp.motorpulse[0], form='(i8)')+' ～ '+	$
		string(wp.motorpulse[1], form='(i8)')+'  pulse/sec '
	if (uvalue eq 'rot_freq') then begin
		wp.rot_freq=float(value)
		wp.inpulse=Ulong(507904.*wp.rot_freq)
		widget_CONTROL,wd.inpulse,set_value=string(wp.inpulse,form='(i8)')+'  pulse/sec '
	endif
;====================================;
if (is_dir(wp.svdir) eq 0) then spawn,'mkdir '+wp.svdir

if (uvalue eq "EXIT") then WIDGET_CONTROL, /destroy, ev.top

end

;************************************************************************
pro DSTPOL_widget
;--------------------------------------------------------------
common widgetlib,wp,wd,svwp1,svwp2,svwp3,svwp4

time=gettime()


tmp='                                                             '
wp={widget_param, $
	wavelength:	'10830',	$		; wave length observed [A]
	expo:		50000l,		$		; exposure time [μsec]
	gain:		0,		$		; gain 0～28
	nimg:		100, 		$		;  # of image
	binx:		2, 		$		; Binning X 1～8
	biny:		2, 		$		; Binning Y 1～1200
	Height:		600,	 	$		; Height  max=1200 (biny=1)
	Width:		800,	 	$		; Width  max=1600 (binx=1)
	RegionX:	0, 		$		; start of region read out,pixel,left edge
	RegionY:	0, 		$		; start of region read out,pixel,top edge
	svprm:		'1',		$		; # of saved parameter set
	clock: 		79861111l, 	$		; TimeStanmpFrequency [Hz]
	timelo:		0,		$		; Time stamp, lower 32-bits
	timehi:		0,		$		; Time stamp, upper 32-bits
;	svdir:		'C:/data/dst/'+	strmid(time,0,8)+	$
;				'/',	$		; save directory
	svdir:		'C:\data\dst\'+	strmid(time,0,8)+	$
				'\',	$		; save directory
	fname:		'fname',	$		; head of file name
	nf:		1,		$		; number of files
	input:		tmp,		$		; contec cdio input 　＊モニター要の文字列メモリ（？）
	output:		tmp,		$		; contec cdio output　＊を確保する必要あり
	r_m:		' ',		$		; RADIUS [arcmin]
	r_s:		' ',		$		; RADIUS [arcsec]
	p_d:		' ',		$		; POLAR ANGLE [arcdeg]
	p_m:		' ',		$		; POLAR ANGLE [arcmin]
	i_d:		' ',		$		; INCLINATION [arcdeg]
	i_m:		' ',		$		; INCLINATION [arcmin]
	i_s:		' ',		$		; INCLINATION [arcsec]
	motorpulse:	strarr(2),	$		; motor pulse [pulses/sec]
	period:		'1',	$		; period of rotating waveplate [sec]
	rot_freq:	1.,		$		; waveplate roteting frequency [Hz}
	inpulse:	507904,		$		; motor pulse [pulses/sec]
	position:	'west',		$		; telescope position			;20110827
	mtpulse:	'507904',	$		; motor pulse [pulses/sec]		;20110827
	waveplate:	'LUCEO127#2',	$		; waveplate				;20110827
	n_evsample: 	0l 		$		; omake
	}

svwp1=wp
svwp2=wp
svwp3=wp
wp.motorpulse=MotorPulse(wp)
if is_dir(wp.svdir) eq 0 then spawn,'mkdir '+wp.svdir

wd={wd_cdio,	$
	bt_pren:	0l,	$		; プレビューストップ
	in:		0l,	$		; contec DIO 入力情報
	out:		0l,	$		; contec DIO 出力状況
	opend:		0l,	$		; contec DIO 操作ストップ
	bs_wv_op:	0l,	$		; contec DIO 操作スタート
	expo:		0l,	$		; exposure time [μsec]
	gain:		0,	$		; gain 0～28
	nimg:		0, 	$		;  # of image
	binx:		0, 	$		; Binning X 1～8
	biny:		0, 	$		; Binning Y 1～1200
	height:		0l,	$		; prosilica Height
	width:		0l,	$		; prosilica Width
	regionx:	0l,	$		; prosilica RegionX
	regiony:	0l,	$		; prosilica RegionY
	motorpulse:	0l,	$		; motor pulse [pulses/sec]
	period:		0l,	$		; period of rotating waveplate [sec]
	rot_freq:	0l,	$		; waveplate roteting frequency [Hz}
	inpulse:	0l,	$		; motor pulse [pulses/sec]
	position:	0l,	$		; telescope position			;20110827
	mtpulse:	0l,	$		; motor pulse [pulses/sec]		;20110827
	waveplate:	0l,	$		; waveplate				;20110827
	Exit:		0l	$
}
main = WIDGET_BASE(title='Prosilica Observation',/column)

	;== Set Parameter ==;
lab= widget_label(main,value='>>> Set Parameter <<<');,font=2)
bs_sp=widget_base(main, /column, /frame)
	bs_sp2=widget_base(bs_sp, /row)
		lab=widget_label(bs_sp2,value='      expo     : ')
		wd.expo=widget_text(bs_sp2,value=string(wp.expo, form='(i5)'), xsize=6, ysize=1, uvalue='expo',/edit)
		gains=strcompress(string(indgen(29)),/remove_all)
		wd.gain=cw_bselector(bs_sp2,gains,label_left='us        gain    : ', uvalue="gain",set_value=0, ysize=1)
		lab=widget_label(bs_sp2,value='      integ   : ')
		wd.nimg=widget_text(bs_sp2,value=string(wp.nimg, form='(i5)'), xsize=6, ysize=1, uvalue='nimg',/edit)
	bs_sp3=widget_base(bs_sp, /row)
		lab=widget_label(bs_sp3,value='      binx      : ')
		wd.binx=widget_text(bs_sp3,value=string(wp.binx, form='(i5)'), xsize=6, ysize=1, uvalue='binx',/edit)
		lab=widget_label(bs_sp3,value='      　   biny 　 : ')
		wd.biny=widget_text(bs_sp3,value=string(wp.biny, form='(i5)'), xsize=6, ysize=1, uvalue='biny',/edit)
	bs_sp4=widget_base(bs_sp, /row)
		lab=widget_label(bs_sp4,value='   　 Width　  : ')
		wd.width=widget_text(bs_sp4,value=string(wp.Width, form='(i5)'), xsize=6, ysize=1, uvalue='width',/edit)
		lab=widget_label(bs_sp4,value='pix      Height   : ')
		wd.height=widget_text(bs_sp4,value=string(wp.Height, form='(i5)'), xsize=6, ysize=1, uvalue='height',/edit)
		lab=widget_label(bs_sp4,value='pix')
	bs_sp5=widget_base(bs_sp, /row)
		lab=widget_label(bs_sp5,value='    RegionX　: ')
		wd.regionx=widget_text(bs_sp5,value=string(wp.RegionX, form='(i5)'), xsize=6, ysize=1, uvalue='regionx',/edit)
		lab=widget_label(bs_sp5,value='    　　　RegionY : ')
		wd.regiony=widget_text(bs_sp5,value=string(wp.RegionY, form='(i5)'), xsize=6, ysize=1, uvalue='regiony',/edit)
	bs_sp6=widget_base(bs_sp, /row)
		lab=widget_label(bs_sp6,		$
			value='______________ save parameters ____________________________________________________')
	bs_sp7=widget_base(bs_sp, /row)
		bt=widget_button(bs_sp7, value="Save 1", uvalue = "svprm1",/align_center,xsize=60)
		bt=widget_button(bs_sp7, value="Save 2", uvalue = "svprm2",/align_center,xsize=60)
		bt=widget_button(bs_sp7, value="Save 3", uvalue = "svprm3",/align_center,xsize=60)
		bt=widget_button(bs_sp7, value="Save 4", uvalue = "svprm4",/align_center,xsize=60)
		lab=widget_label(bs_sp7,value='    　   　Load : ')
		text=widget_text(bs_sp7,value=wp.svprm, xsize=6, ysize=1, uvalue='svprm',/edit)



	;== Observation ==;
lab_ob = widget_label(main,value='>>> Observation <<<');,font=2)
bs_ob=widget_base(main, /column, /frame)
	bs_sv=widget_base(bs_ob, /column)
		bs_sv0=widget_base(bs_sv, /row)
			lab=widget_label(bs_sv0,value='Save Directory : ')
			text=widget_text(bs_sv0,value=wp.svdir, xsize=45, uvalue='svdir',/edit)
		bs_sv1=widget_base(bs_sv, /row)
			lab=widget_label(bs_sv1,value='   File Name    : ')
			text=widget_text(bs_sv1,value=wp.fname, xsize=10, uvalue='fname',/edit)
			lab=widget_label(bs_sv1,value='          Number of Set : ')
			text=widget_text(bs_sv1,value=string(wp.nf, form='(i5)'), xsize=6, uvalue='nf',/edit)
		bs_sv2=widget_base(bs_sv, /row)		;20110827
			;positions=['west','east']
			;wd.position=cw_bselector(bs_sv2,positions,label_left='Telescope position : ', uvalue="position",set_value=0, ysize=1)

			;lab=widget_label(bs_sv2,value='            Wave Length   : ')
			lab=widget_label(bs_sv2,value='Wave Length   : ')
			text=widget_text(bs_sv2,value=wp.wavelength, xsize=6, uvalue='wavelength',/edit,	$
					xoffset=100000)
			lab=widget_label(bs_sv2,value='[A]')
		bs_sv3=widget_base(bs_sv, /row)
			;lab=widget_label(bs_sv3,value='r :')
			;text=widget_text(bs_sv3,value=wp.r_m, xsize=4, uvalue='r_m',/edit,	$
			;		xoffset=100000)
			;lab=widget_label(bs_sv3,value='m')
			;text=widget_text(bs_sv3,value=wp.r_s, xsize=4, uvalue='r_s',/edit,	$
			;		xoffset=100000)
			;lab=widget_label(bs_sv3,value='s')

			;lab=widget_label(bs_sv3,value='  p :')
			;text=widget_text(bs_sv3,value=wp.p_d, xsize=4, uvalue='p_d',/edit,	$
			;		xoffset=100000)
			;lab=widget_label(bs_sv3,value='d')
			;text=widget_text(bs_sv3,value=wp.p_m, xsize=4, uvalue='p_m',/edit,	$
			;		xoffset=100000)
			;lab=widget_label(bs_sv3,value='m')

			;lab=widget_label(bs_sv3,value='   i :')
			;text=widget_text(bs_sv3,value=wp.i_d, xsize=4, uvalue='i_d',/edit,	$
			;		xoffset=100000)
			;lab=widget_label(bs_sv3,value='d')
			;text=widget_text(bs_sv3,value=wp.i_m, xsize=4, uvalue='i_m',/edit,	$
			;		xoffset=100000)
			;lab=widget_label(bs_sv3,value='m')
			;text=widget_text(bs_sv3,value=wp.i_s, xsize=4, uvalue='i_s',/edit,	$
			;		xoffset=100000)
			;lab=widget_label(bs_sv3,value='s')
			waveplates=['LUCEO127#2','APSAW','Quarts']
			wd.waveplate=cw_bselector(bs_sv3,waveplates,label_left='     Waveplate : ', uvalue="waveplate",set_value=0, ysize=1)
		bs_sv35=widget_base(bs_sv, /row)
			lab=widget_label(bs_sv35,value='Period of rotating waveplate :')
			text=widget_text(bs_sv35,value=wp.period, xsize=6, uvalue='period',/edit,	$
					xoffset=100000)
			lab=widget_label(bs_sv35,value=' [sec]')
			;lab=widget_label(bs_sv35,value='Pulse of motor :')
			;text=widget_text(bs_sv35,value=wp.mtpulse, xsize=6, uvalue='mtpulse',/edit,	$
			;		xoffset=100000)
			;lab=widget_label(bs_sv35,value=' [pulse/sec]')
			;waveplates=['APSAW','Quarts']
			;wd.waveplate=cw_bselector(bs_sv35,waveplates,label_left='     Waveplate : ', uvalue="waveplate",set_value=0, ysize=1)
		bs_sv4=widget_base(bs_sv, /row)
			bt=widget_button(bs_sv4, value="Get (NOM)", uvalue = "svo",/align_center,xsize=120)
			bt=widget_button(bs_sv4, value="Get (POL)", uvalue = "svp",/align_center,xsize=120)
			bt=widget_button(bs_sv4, value="Get (CAL)", uvalue = "calib",/align_center,xsize=120)
	bs_pr=widget_base(bs_ob, /row)
		bt_prst=WIDGET_BUTTON(bs_pr,uvalue='prev_st',value='Preview Start',/align_center,xsize=123)
		wd.bt_pren=WIDGET_BUTTON(bs_pr,uvalue='prev_en',value='Preview Stop',/align_center,xsize=120)
		bt_prof=WIDGET_BUTTON(bs_pr,uvalue='prof',value='Profiles',/align_center,xsize=123)

lab_ob = widget_label(main,value='>>> Auto-Rotate Pol. <<<');,font=2)
bs_wv=widget_base(main, /column, /frame)
	bs_wv1=widget_base(bs_wv,/row)
		lab = widget_label(bs_wv1,value='OPERATION : ');,font=2)
		bt=widget_button(bs_wv1, value='Initialize', uvalue = 'diostart',/align_center,xsize=60)
		wd.opend=widget_button(bs_wv1, value='Close', uvalue = 'diostop',/align_center,xsize=60)
	lab = widget_label(bs_wv,value='<< STATUS >>');,font=2)
	bs_wv_ip=widget_base(bs_wv, /column)
		bs_wv_ip1=widget_base(bs_wv_ip, /row)
			lab = widget_label(bs_wv_ip1,value='     Status     = ');,font=2)
			wd.in = widget_label(bs_wv_ip1,value=wp.input)
		bs_wv_ip2=widget_base(bs_wv_ip, /row)
			lab = widget_label(bs_wv_ip2,value='Output Status = ');,font=2)
			wd.out = widget_label(bs_wv_ip2,value=wp.output)
	lab = widget_label(bs_wv,value='<< OUTPUT >>');,font=2)
	wd.bs_wv_op=widget_base(bs_wv, /column)
		bs_wv_op1=widget_base(wd.bs_wv_op, /row)
			bt=widget_button(bs_wv_op1, value='45deg', uvalue = 'o45',/align_center,xsize=60)
			bt=widget_button(bs_wv_op1, value='90deg', uvalue = 'o90',/align_center,xsize=60)
			bt=widget_button(bs_wv_op1, value='135deg', uvalue = 'o135',/align_center,xsize=60)
			bt=widget_button(bs_wv_op1, value='180deg', uvalue = 'o180',/align_center,xsize=60)
			bt=widget_button(bs_wv_op1, value='225deg', uvalue = 'o225',/align_center,xsize=60)
			bt=widget_button(bs_wv_op1, value='270deg', uvalue = 'o270',/align_center,xsize=60)
		bs_wv_op2=widget_base(wd.bs_wv_op, /row)
			bt=widget_button(bs_wv_op2, value='315deg', uvalue = 'o315',/align_center,xsize=60)
			bt=widget_button(bs_wv_op2, value='360deg', uvalue = 'o360',/align_center,xsize=60)
			bt=widget_button(bs_wv_op2, value='+22.5deg', uvalue = 'op25',/align_center,xsize=60)
			bt=widget_button(bs_wv_op2, value='JOG(+)', uvalue = 'opjg',/align_center,xsize=60)
			bt=widget_button(bs_wv_op2, value='JOG(-)', uvalue = 'omjg',/align_center,xsize=60)
			bt=widget_button(bs_wv_op2, value='STOP', uvalue = 'ostop',/align_center,xsize=60)
		bs_wv_op3=widget_base(wd.bs_wv_op, /row)
			bt=widget_button(bs_wv_op3, value='ORIGIN', uvalue = 'o0',/align_center,xsize=120)
			bt=widget_button(bs_wv_op3, value='Read Origin Address', uvalue = 'ow0ad',/align_center,xsize=120)
lab_ob = widget_label(main,value='>>> etc. <<<');,font=2)
bs_ex=widget_base(main, /column, /frame)
	bt=widget_button(bs_ex, value='close dstangle.dat', uvalue = 'close1',/align_left,xsize=100)
	bs_ex1=widget_base(bs_ex, /row)
		lab = widget_label(bs_ex1,value=' motor pulse = ');,font=2)
		wd.motorpulse=widget_label(bs_ex1,value=string(wp.motorpulse[0], form='(i8)')+' ～ '+	$
		string(wp.motorpulse[1], form='(i8)')+'  pulse/sec ')
	bs_ex2=widget_base(bs_ex, /row)
		lab = widget_label(bs_ex2,value=' waveplate frequency = ');,font=2)
		wd.rot_freq=widget_text(bs_ex2,value=string(wp.rot_freq, form='(f6.3)'), xsize=6, ysize=1, uvalue='rot_freq',/edit)
		lab = widget_label(bs_ex2,value=' [Hz]  => input pulse = ');,font=2)
		wd.inpulse=widget_label(bs_ex2,value=string(wp.inpulse,form='(i8)')+'  pulse/sec ')
wd.Exit = widget_button(main, value="Exit", uvalue = "EXIT")
widget_control, main, /realize
XMANAGER,'DSTPOL_widget',main,modal=modal

END

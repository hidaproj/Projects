
;PRO hsp_widget
;+
;
;  hsp_widget.pro
;
;Main debug procedure by widget for DST Spectro-Polarimetric observation
;based on DST Poralization script by T. Anan and SMART T3 prosilica observation by T. Kawate
;
;20100908  T.A.
;20101125  T.A.			;etc.,window,preview,profile
;20110827  T.A.			; telescope position, pulse of mortor, waveplate
;20121107  T.A.			; exclude telescope position, r, p, i, change motor pulse to period
;20131031  T.A.			; makee save directory automaticaly
;20140222  T.A.			; add 'LUCEO127#2' to waveplate
;20140729  T.A.			; cdio_init,
;20160524  T.A.			; DSTPOL_widget.pro => hsp_widget.pro, period
;20160721  T.A.
;20161212  T.A.                 ; window size
;
;==========================headder============================;
;PRO hsp_widget_event, ev
;PRO hsp_widget
;
;-

;=========================include=============================;
@DSTPOL_ObsLib
@C:\Projects\IDLPRO\dio8\CDIO_64_lib.pro
@C:\Projects\IDLPRO\orca4\orcalib.pro
@C:\Projects\IDLPRO\hardware\emplib.pro

;=========================main================================;

;**************************************************************
pro hsp_widget_event, ev
;--------------------------------------------------------------
common widgetlib,wp,wd,svwp1,svwp2,svwp3,svwp4,pm,ref_pulse
COMMON bridge,bridge

rdio=1
stprm=1
windex_prev=0
windex_prof=1

widget_control, ev.id, get_uvalue=uvalue,get_value=value

print,'uvalue=',uvalue,',  value=',value

	;r=cdio_init()
	;wp.input=''
	;for i=0,200 do widget_CONTROL,wd.in,set_value=wp.input
	;wp.input=cdio_input()
	;for i=0,200 do widget_CONTROL,wd.in,set_value=wp.input
	;cdio_exit

	if (uvalue eq "camera") then begin
		wp.camera=value
		case wp.camera of
			0:begin
				wp.binx=2
				wp.biny=2
				height0=1200
				width0=1600
			end
			1:begin
				wp.binx=1
				wp.biny=1
				height0=2048
				width0=2048
			end
		endcase
		wp.Height=height0/wp.biny
		wp.Width=width0/wp.binx
		widget_CONTROL,wd.binx,set_value=string(wp.binx, form='(i5)')
		widget_CONTROL,wd.biny,set_value=string(wp.biny, form='(i5)')
		widget_CONTROL,wd.Width,set_value=string(wp.Width, form='(i5)')
		widget_CONTROL,wd.height,set_value=string(wp.Height, form='(i5)')
	endif
	if (uvalue eq "nimg") then begin
		case wp.camera of
			0:begin
				if (fix(value) lt 0) or (fix(value) ge 101) then begin
					;print,'integ must be from 0 to 100'
					MessageBox,'integ must be from 0 to 100'
				endif else begin
					wp.nimg=fix(value)
				endelse
			end
			else:begin
				wp.nimg=fix(value)
			end
		endcase
	endif
	if (uvalue eq "expo") then begin
		expo=float(value)
		if expo le 10000 then begin
			expo=10000.
			print,'exposure should be larger than 10,000 us'
			; to output trigger
		endif
		wp.expo=expo
		widget_CONTROL,wd.expo,set_value=string(wp.expo, form='(i8)')
	endif
	if (uvalue eq "gain") then wp.gain=fix(value)
	if (uvalue eq "binx") then begin
		wp.binx=fix(value)
		case wp.camera of
			0:begin
				height0=1200
				width0=1600
			end
			1:begin
				height0=2048
				width0=2048
			end
		endcase
		wp.Width=width0/wp.binx
		widget_CONTROL,wd.Width,set_value=string(wp.Width, form='(i5)')
	endif
	if (uvalue eq "biny") then begin
		wp.biny=fix(value)
		case wp.camera of
			0:begin
				height0=1200
				width0=1600
			end
			1:begin
				height0=2048
				width0=2048
			end
		endcase
		wp.Height=height0/wp.biny
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

	if (uvalue eq "free") then begin
		wp.free = fix(value[0])
		widget_control,wd.free,set_value=wp.free
		if wp.free eq 1 then print,'Free run' else print,'cadence observation'
	endif
	if (uvalue eq "cadense") then begin
		wp.cds = float(value[0])
		widget_control,wd.cds,set_value=string(wp.cds,format='(f5.1)')
		print,'Cadense (s): '+string(wp.cds)
	endif

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

		widget_CONTROL,wd.expo,set_value=string(wp.expo, form='(i8)')
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
	if (uvalue eq "period") then begin
		empinit,'COM'+wp.com

		if float(value) ge 0.3 then begin
			pm.m1.vm=ref_pulse*3./float(value)	;18000./float(value)
			wp.period=strcompress(string(value),/remove_all)
			if wp.empmove eq 1 then begin
				empstop
				wp.empmove=0
				wait,0.05
			endif
			empset,1,vm=pm.m1.vm
			wait,0.05
			empstart,1,/CW
			wp.empmove=1
		endif else begin
			print,'Minimum period is 0.1 sec'
			widget_CONTROL,wd.period,set_value=wp.period
		endelse
	endif
	if (uvalue eq "empstop") then begin
		if wp.empmove eq 1 then begin
			empstop
			wp.empmove=0
			wait,0.05
			empclose
		endif
	endif
	if (uvalue eq "com") then begin
		if wp.empmove eq 1 then begin
			empstop
			wp.empmove=0
			wait,0.05
		endif
		;empclose
		wp.com=strcompress(string(value),/remove_all)
	endif
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
			caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
			time1=hour*3600.+minu*60.+seco*1.
			print,'===== # '+strcompress(string(i+1),/remove_all)+' ====='
			print,hour,minu,seco
			fn[i]=NormalObs(wp,firstimg=firstimg)

			if 1 then begin;==表示==;
				case wp.camera of
					0:begin
						wx=800	& wy=600
					end
					1:begin
						wx=600	& wy=600
					end
				endcase
				if i eq 0 then window,0,ys=wy,xs=wx
				img=congrid(firstimg,wx,wy)
				tvscl,img
				xyouts,0.05,0.05,strcompress(string(i),/remove_all),/norm,charsize=20
			endif;==表示==;

			if wp.free ne 1 then begin
				caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
				time2=hour*3600.+minu*60.+seco*1.
				if (wp.cds - (time2-time1)) le 0 then begin
					print,'!cadence is less than time of taking images'
					wait,0.5
				endif else begin
					wait,(wp.cds - (time2-time1))
				endelse
			endif 
		endfor
		OrcaOutTriggerDefault							; 20160710 T.A.
;MessageBox,'!!FINISH NORMAL OBSERVATION!!'
print,''&print,'!!FINISH NORMAL OBSERVATION!!'
print,''
print,''
print,''
print,''
print,''
print,''
print,''
print,''

	endif

	if (uvalue eq "svp") then begin
		if wp.empmove ne 1 then begin
			empinit,'COM'+wp.com
			empset,1,vm=pm.m1.vm
			wait,0.05
			empstart,1,/CW
			wp.empmove=1
			wait,3
		endif
		OrcaOutTriggerExposure					; 20160710 T.A.
		fn=strarr(wp.nf)
		for i=0,wp.nf-1 do begin	
			print,'===== # '+strcompress(string(i+1),/remove_all)+' ====='
			caldat,systime(/JULIAN), mon1 , day1 , year1 , hour1 , minu1 , seco1
			fn[i]=PolObs('',wp,firstimg=firstimg)

			if 1 then begin;==表示==;
				case wp.camera of
					0:begin
						wx=800	& wy=600
						imax=2.^12
					end
					1:begin
						wx=600	& wy=600
						imax=2.^12
					end
				endcase
				if i eq 0 then window,0,ys=wy,xs=wx
				img=congrid(firstimg,wx,wy)
				tvscl,img
				xyouts,0.05,0.05,strcompress(string(i),/remove_all),/norm,charsize=20
			endif;==表示==;

			caldat,systime(/JULIAN), mon2 , day2 , year2 , hour2 , minu2 , seco2

			dtime=(hour2*3600.+minu2*60.+seco2*1.)-(hour1*3600.+minu1*60.+seco1*1.)
			if wp.free ne 1 then begin
				if (wp.cds - dtime) le 0 then begin
					print,'!cadence is less than time of taking images'
					print,'take ',dtime,'sec'
				endif else begin
					print,'wait',(wp.cds - dtime),', cadence',wp.cds
					wait,(wp.cds - dtime)
				endelse
			endif else begin
				print,'take ',dtime,'sec'
			endelse
		endfor

		goto,jump
		;==表示==;
		;if (wp.binx ne 1) and (wp.biny ne 1) then begin 
			;wx=wp.Width/4	& wy=wp.Height/4  ;20161212 TA commentout
			case wp.camera of
				0:begin
					wx=800	& wy=600
				end
				1:begin
					wx=600	& wy=600
				end
			endcase
			window,0,ys=wy,xs=wx
			for i=0,wp.nf-1 do begin
				case wp.camera of
					0:mreadfits,fn[i],h,img
					1:begin
						img=uint(rfits(fn[i],head=sh))
						byteorder,img
					end
				endcase
				img=rebin(img,wx,wy,wp.nimg)
				for j=0,wp.nimg-1 do begin
					tvscl,img[*,*,j]
					xyouts,0.05,0.05,string(j)+' binx='+	$
						string(wp.binx,format='(i1)')+	$
						' biny='+string(wp.biny,format='(i1)'),/norm
				endfor
			endfor
		;endif else print,'not display observed images because << binning 1>>'
		jump:
		OrcaOutTriggerDefault							; 20160710 T.A.


;MessageBox,'!!FINISH POLARIMETRIC OBSERVATION!!'
print,''&print,'!!FINISH POLARIMETRIC OBSERVATION!!'
print,''
print,''
print,''
print,''
print,''
print,''
print,''
print,''

	endif

	if (uvalue eq "calib") then begin
		OrcaOutTriggerExposure					; 20160710 T.A.
		fn=CalibObs(wp,wd)
		;==表示==;
		;if (wp.binx ne 1) and (wp.biny ne 1) then begin 
			case wp.camera of
				0:begin
					wx=800	& wy=600
				end
				1:begin
					wx=600	& wy=600
				end
			endcase
			window,0,ys=wy,xs=wx
			for i=0,wp.nf-1 do begin
				case wp.camera of
					0:mreadfits,fn[i],h,img
					1:begin
						img=uint(rfits(fn[i],head=sh))
						byteorder,img
					end
				endcase
				img=congrid(img,wx,wy,wp.nimg)
				for j=0,wp.nimg-1 do begin
					tvscl,img[*,*,j]
					xyouts,0.05,0.05,string(j)+' binx='+	$
						string(wp.binx,format='(i1)')+	$
						' biny='+string(wp.biny,format='(i1)'),/norm
				endfor
			endfor
			;wx=wp.Width/2	& wy=wp.Height/2
			;window,0,ys=wy,xs=wx
			;for i=0,8 do begin
			;	mreadfits,fn[i],h,img
			;	img=rebin(img,wx,wy,wp.nimg)
			;	for j=0,wp.nimg-1 do begin
			;		tvscl,img[*,*,j]
			;		xyouts,0.05,0.05,string(j)+' binx='+	$
			;			string(wp.binx,format='(i1)')+	$
			;			' biny='+string(wp.biny,format='(i1)'),/norm
			;	endfor
			;endfor
			OrcaOutTriggerDefault							; 20160710 T.A.
		;endif else print,'not display observed images because << binning 1>>'
;MessageBox,'!!FINISH GETING CALIBRATION DATA!!'
print,''&print,'!!FINISH GETING CALIBRATION DATA!!'
print,''
print,''
print,''
print,''
print,''
print,''
print,''
print,''

	endif
;=========== preview ==========;


	if (uvalue eq 'prev_st') then begin
		case wp.camera of	
			0:begin
				pro_init
				pro_setparam,wp
				mxmi=[0,2.^12]
				wx=800. ;20161212 TA
				wy=600. ;20161212 TA
			end
			1:begin
				;p=orcainit()	; comment out 20160709 T.A.
				p=OrcaSetParam(expo=wp.expo*1e-6,bin=wp.binx)
				OrcaOutTriggerExposure					; 20160710 T.A.
				mxmi=[0,2.^16]
				wx=600. ;20161212 TA
				wy=600. ;20161212 TA
			end
		endcase
		window,0,xs=wx,ys=wy  ;20161212 TA
		window,1,xs=wx,ys=350;20161212 TA
		!p.multi=[0,2,1]
		xy=[400,300]
		img=PrevObs(wp,xy,mxmi=mxmi)
		print,'Click where you want to look profiles!!'
		cursor,xx,yy,/device	& xy=[xx,yy]	& print,xy
		mxmi=[0,max(img[*,xy[1]])>max(img[xy[0],*])]
		while ev_prevstop.id eq 0 do begin
			img=PrevObs(wp,xy,mxmi=mxmi)
			ev_prevstop=widget_event(wd.bt_pren,/nowait)
			ev_emp=widget_event(/nowait)
		endwhile
		OrcaOutTriggerDefault							; 20160710 T.A.

		case wp.camera of	; comment out 20160709 T.A.
			0:pro_exit
			1:;orcafin
		endcase
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
		case wp.camera of
			0:begin
				pro_init
				pro_setparam,wp
				img=rebin(Get1ImageArray(),wx,wy)
				pro_exit
			end
			1:begin
				p=orcainit()
				p=OrcaSetParam(expo=wp.expo*1e-6,bin=wp.binx)
				img=congrid(OrcaObs(nimg=1),wx,wy)
				orcafin
			end
		endcase
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
;	if (uvalue eq 'close1') then begin	; ref. C:\Projects\IDLPRO\IR_Obs\dstangle.pro
;		close,1
;		pro_exit
;	endif
;	wp.motorpulse=MotorPulse(wp)
;	widget_CONTROL,wd.motorpulse,set_value=string(wp.motorpulse[0], form='(i8)')+' ～ '+	$
;		string(wp.motorpulse[1], form='(i8)')+'  pulse/sec '
;	if (uvalue eq 'rot_freq') then begin
;		wp.rot_freq=float(value)
;		wp.inpulse=Ulong(507904.*wp.rot_freq)
;		widget_CONTROL,wd.inpulse,set_value=string(wp.inpulse,form='(i8)')+'  pulse/sec '
;	endif
;====================================;
if (is_dir(wp.svdir) eq 0) then spawn,'mkdir '+wp.svdir

if (uvalue eq "EXIT") then begin
	WIDGET_CONTROL, /destroy, ev.top
	if wp.empmove eq 1 then empstop
	wait,1
	orcafin;20160709 T.A.
	empclose
	obj_destroy,bridge
endif

end

;************************************************************************
pro hsp_widget
;--------------------------------------------------------------
common widgetlib,wp,wd,svwp1,svwp2,svwp3,svwp4,pm,ref_pulse
COMMON bridge,bridge

;-------INITIALIZE-------;
dmy=widget_base(title='T1obs',TLB_FRAME_ATTR=11,row=1,/align_center,ysize=100,xoff=800,yoff=400)
txt=widget_label(dmy,value='Please wait....',/ALIGN_CENTER,xsize=200)
WIDGET_CONTROL,dmy,/REALIZE


ref_pulse=4000.*5.		; resolution of motor * gear ratio (CRK523PAP-N5)
time0=get_systime(ctime=time)	;time=gettime(), 20140629
p=orcainit();20160709 T.A.

;-----  prepare object array for parallel processing -----------
nCPU=!CPU.HW_NCPU
bridge=objarr(nCPU-1)
for i=0,nCPU-2 do begin
	bridge[i]=obj_new('IDL_IDLBridge');IDL_IDLbridge()
endfor
;----------------------------------------------------------------
tmp='                                                             '
wp={widget_param, $
	wavelength:	'10830',	$		; wave length observed [A]
	expo:		200000l,		$		; exposure time [μsec]
	gain:		0,		$		; gain 0～28
	nimg:		100, 		$		;  # of image
	binx:		1, 		$		; Binning X 1～8
	biny:		1, 		$		; Binning Y 1～1200
	Height:		2048,	 	$		; Height  max=1200 (biny=1)
	Width:		2048,	 	$		; Width  max=1600 (binx=1)
	RegionX:	0, 		$		; start of region read out,pixel,left edge
	RegionY:	0, 		$		; start of region read out,pixel,top edge
	svprm:		'1',		$		; # of saved parameter set
	clock: 		79861111l, 	$		; TimeStanmpFrequency [Hz]
	timelo:		0,		$		; Time stamp, lower 32-bits
	timehi:		0,		$		; Time stamp, upper 32-bits
;	svdir:		'C:/data/dst/'+	strmid(time,0,8)+	$
;				'/',	$		; save directory
;	svdir:		'C:\data\dst\'+	strmid(time,0,8)+	$
;	svdir:		'G:\data\'+	strmid(time,0,8)+	$
;	svdir:		'G:\data\'+	strmid(time,0,8)+	$
	svdir:		'D:\data\'+	strmid(time,0,8)+	$
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
	period:		'4',		$		; period of rotating waveplate [sec]
	com:		'9',		$		; COM
	empmove:	0,		$		; motor move 1 or not 0
	rot_freq:	1.,		$		; waveplate roteting frequency [Hz}
	inpulse:	507904,		$		; motor pulse [pulses/sec]
	position:	'west',		$		; telescope position			;20110827
	mtpulse:	'507904',	$		; motor pulse [pulses/sec]		;20110827
	waveplate:	'LUCEO127#2',	$		; waveplate				;20110827
	camera:		1,		$		; 0: GE1650, 1:ORCA-Flash4.0		;20140629
	free:		1,		$		; free run or fixed cadance		;20160721
	cds:		30.,		$		; cadance (sec)				;20160721
	n_evsample: 	0l 		$		; omake
	}

if wp.camera eq 0 then begin
	wp.binx=2
	wp.biny=2
	wp.Height=600
	wp.Width=800
endif

svwp1=wp
svwp2=wp
svwp3=wp
wp.motorpulse=MotorPulse(wp)
if is_dir(wp.svdir) eq 0 then spawn,'mkdir '+wp.svdir

pm=emp_ctl()
pm.m1.name='M: HSP-Modulator'
pm.m2.name=''
pm.m1.vm=ref_pulse*3./float(wp.period)	;18000./float(wp.period)
pm.m2.vm=0.
pm.dev_exist=1.

widget_control,dmy,/destroy
;--------------------------;


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
	com:		0l,	$		; COM
	rot_freq:	0l,	$		; waveplate roteting frequency [Hz}
	inpulse:	0l,	$		; motor pulse [pulses/sec]
	position:	0l,	$		; telescope position			;20110827
	mtpulse:	0l,	$		; motor pulse [pulses/sec]		;20110827
	waveplate:	0l,	$		; waveplate				;20110827
	free:		0l,	$		; free run or fixed cadance		;20160721
	cds:		0l,	$		; cadance (sec)			;20160721
	Exit:		0l	$
}
main = WIDGET_BASE(title='Horizontal Specto-Polarimeter',/column)

lab= widget_label(main,value='>>> Camera <<<');,font=2)
cw_bg=cw_bgroup(main,['GE1650 (CCD)','ORCA-Flash4.0 (CMOS)'],/row,	$
	uvalue='camera',/no_release,font='1',ypad=0,	$
	set_value=wp.camera,/exclusive,/frame,	$
	xsize=440,ysize=25)

	;== Set Parameter ==;
lab= widget_label(main,value='>>> Set Parameter <<<');,font=2)
bs_sp=widget_base(main, /column, /frame)
	bs_sp2=widget_base(bs_sp, /row)
		lab=widget_label(bs_sp2,value='      expo     : ')
		wd.expo=widget_text(bs_sp2,value=string(wp.expo, form='(i8)'), xsize=8, ysize=1, uvalue='expo',/edit)
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
			wd.waveplate=cw_bselector(bs_sv2,waveplates,label_left='     Waveplate : ', uvalue="waveplate",set_value=0, ysize=1)
		bs_sv35=widget_base(bs_sv, /row)
			lab=widget_label(bs_sv35,value='Period of rotating waveplate :')
			wd.period=widget_text(bs_sv35,value=wp.period, xsize=6, uvalue='period',/edit,	$
					xoffset=100000)
			lab=widget_label(bs_sv35,value=' [sec] ')
			bt=widget_button(bs_sv35, value="STOP", uvalue = "empstop",/align_center,xsize=60)
			lab=widget_label(bs_sv35,value=' COM')
			wd.com=widget_text(bs_sv35,value=strcompress(wp.com,/remove_all), xsize=6, uvalue='com',/edit,	$
					xoffset=100000)

		; 20160721 =>
		bs_sv36=widget_base(bs_sv, /row)
			wd.free=cw_bgroup(bs_sv36,'Free run',/nonexclusive,set_value=string(wp.free,format='(i1.1)'),uvalue='free')
			lab=widget_label(bs_sv36,value='  Cadence : ')
			wd.cds=widget_text(bs_sv36,value=string(wp.cds,format='(i5)'),uvalue='cadense',/edit,xsize=6,ysize=1)
			lab=widget_label(bs_sv36,value=' sec')
		; <=



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
;lab_ob = widget_label(main,value='>>> etc. <<<');,font=2)
;bs_ex=widget_base(main, /column, /frame)
;	bt=widget_button(bs_ex, value='close dstangle.dat', uvalue = 'close1',/align_left,xsize=100)
;	bs_ex1=widget_base(bs_ex, /row)
;		lab = widget_label(bs_ex1,value=' motor pulse = ');,font=2)
;		wd.motorpulse=widget_label(bs_ex1,value=string(wp.motorpulse[0], form='(i8)')+' ～ '+	$
;		string(wp.motorpulse[1], form='(i8)')+'  pulse/sec ')
;	bs_ex2=widget_base(bs_ex, /row)
;		lab = widget_label(bs_ex2,value=' waveplate frequency = ');,font=2)
;		wd.rot_freq=widget_text(bs_ex2,value=string(wp.rot_freq, form='(f6.3)'), xsize=6, ysize=1, uvalue='rot_freq',/edit)
;		lab = widget_label(bs_ex2,value=' [Hz]  => input pulse = ');,font=2)
;		wd.inpulse=widget_label(bs_ex2,value=string(wp.inpulse,form='(i8)')+'  pulse/sec ')
wd.Exit = widget_button(main, value="Exit", uvalue = "EXIT")
widget_control, main, /realize
XMANAGER,'hsp_widget',main,modal=modal

END

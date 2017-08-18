;*****************************************************
;+
;   XEVA640_WGT.PRO
;	based Norikura Project-E observation program
; 2014.10.23 T. Anan & T. Yoneya    cadence, continuous mode 
; 2016.05.26 T. Anan	    	    T-mode, rename T-mode to T-C-mode
; 2016.11.09 T. Anan                Bridge, but fail
;-
;*****************************************************

;=====================================================
FUNCTION data_event, ev, wdd, p
;=====================================================
;handle data
;ev   -- event structure
;wdd  -- Data widgets
;p    -- parameter structure
;-----------------------------------------------------
addhd=0
case (ev.id) of

	wdd.Dir: begin
		widget_control, ev.id, get_value=value, set_value=''
		p.dir = value[0]
		widget_control, ev.id, set_value=p.dir
		print, '-- Dir : '+p.dir
	end

	wdd.Fname: begin
		widget_control, ev.id, get_value=value, set_value=''
		p.fname = value[0]
		widget_control, ev.id, set_value=p.fname
		print, '-- File name : '+p.fname
	end

	wdd.Seqno: begin
		widget_control, ev.id, get_value=value, set_value=''
		p.seqno = fix(value[0])
		widget_control, ev.id, set_value=strtrim(string(p.seqno),2)
		print, '-- # of Sequences : '+string(p.seqno)
	end

;	else:
endcase

RETURN, p
END

;=====================================================
PRO xeva640obs_event, eve
;=====================================================
common xeva640, wd, p, h, img, nx, ny
common xeva640_wgt,b_btn1_2,b_btn3_2



widget_control, eve.id, get_uvalue=value
if (n_elements(value) eq 0) then value = ''
name = strmid(tag_names(eve,/structure_name), 7, 1000)
;---------- Data handle ----------
l = wd.data
wid_data = [l.Dir, l.Fname, l.Seqno]
i = where(eve.id EQ wid_data)
IF i(0) ne -1 THEN BEGIN
	p = data_event(eve, wd.data, p)
	RETURN
ENDIF
;---------- Obs control ----------
print,eve.id
CASE (eve.id) OF

	wd.obs.Exptime: BEGIN
		widget_control, eve.id, get_value=value, set_value=''
		expo = long(value[0])
		h.exptime = float(expo/1000000.)
		widget_control, eve.id, set_value=strtrim(string(expo),2)
		set_exptime, expo=expo
		print, '-- Exposure time : '+string(expo)+' usec'
	RETURN
	END
	wd.obs.Naxis3: BEGIN
		widget_control, eve.id, get_value=value, set_value=''
		h.naxis3=long(value[0])

		;xeva640_startcap, frms=h.naxis3, expo=long(h.exptime*1000000.);, gain=gain
		widget_control, eve.id, set_value=string(h.naxis3, format='(i3)')
		print, '-- naxis3 : '+string(h.naxis3, format='(i3)')
	RETURN
	END
;	wd.obs.FrameRate: begin
;		widget_control, eve.id, get_value=value, set_value=''
;		p.ccd.frate=long(value(0))
;		widget_control, eve.id,set_value=string(p.ccd.frate,format='(i2)')
;		print,'-- FrameRate : ',p.ccd.frate,' Hz'
;	return
;	end

	wd.obs.naxis3: BEGIN
		widget_control, eve.id, get_value=value, set_value=''
		h.naxis3 = fix(value[0])
		set_naxis3, h
		widget_control, eve.id, set_value = string(h.naxis3,format='(i3)')
		print, '-- # of Frames : '+string(h.naxis3)
	RETURN
	END
; 2014.10.23 T. Anan & T. Yoneya & T. Fujita =>
	wd.obs.free: BEGIN
		widget_control, eve.id, get_value=value;, set_value=''
		h.free = fix(value[0])
		widget_control, eve.id, set_value = h.free
		print, '-- free run : '+string(h.free)
	RETURN
	END

	wd.obs.cds: BEGIN
		widget_control, eve.id, get_value=value, set_value=''
		h.cds = fix(value[0])
		widget_control, eve.id, set_value = string(h.cds, format='(i4)')
		print, '-- # cadence : '+ string(h.cds)
	RETURN
	END
;<=


	wd.obs.Preview: BEGIN
		xeva640_prev, img, nx, ny;, expo=h.exptime
	RETURN
	END

	wd.obs.Snap: BEGIN
		xeva640_snap, h=h, img, index=wd.obs.windex;, expo=h.exptime
		xeva640_save_snap, img, h=h, p=p
		print, '-- got snap'
	END

	wd.obs.Cmode: BEGIN
;		print, '-- C-mode starts Sequences.'
		FOR seqn=0,p.seqno-1 DO BEGIN
			obs_cmode, h=h, img, index=wd.obs.windex, seqn=seqn+1, aseqn=p.seqno, nx, ny;   ,word='#'+string(seqn+1)  ;, expo=expo
;			time0=call_external('C:\Projects\cprog\VS2005\Xenics\cam_set5\debug\cam_set5.dll','getime',/all_value,/cdecl)*1d
			xeva640_save, img, h=h, p=p;, seqn
;			print,call_external('C:\Projects\cprog\VS2005\Xenics\cam_set5\debug\cam_set5.dll','getime',/all_value,/cdecl)*1d -time0
		ENDFOR
		print, '-- C-mode finished ALL Sequences.'
	END

; 2014.10.23 T. Anan & T. Yoneya & T. Fujita =>
	wd.obs.Tmode: BEGIN
;		print, '-- T-C-mode starts Sequences.'
		FOR seqn=0,p.seqno-1 DO BEGIN
			time1=yyyymmdd()
			print,time1
			if h.free eq 1 then begin
				obs_tmode, h=h, img, index=wd.obs.windex, seqn=seqn+1, aseqn=p.seqno, nx, ny
				xeva640_save, img, h=h, p=p;, seqn
			endif else begin
				obs_tmode, h=h, img, index=wd.obs.windex, seqn=seqn+1, aseqn=p.seqno, nx, ny
				xeva640_save, img, h=h, p=p;, seqn
				time2=yyyymmdd()
				sec1=strmid(time1,8,2)*60.*60.+strmid(time1,10,2)*60.+strmid(time1,12,2)
				sec2=strmid(time2,8,2)*60.*60.+strmid(time2,10,2)*60.+strmid(time2,12,2)
				if (h.cds - (sec2-sec1)) ne 0 then begin
					print,'!cadence is less than time of taking images'
				endif else begin
					wait,h.cds - (sec2-sec1)
				endelse
			endelse
		ENDFOR
		print, '-- T-C-mode finished ALL Sequences.'
	END
	wd.obs.Tmode2: BEGIN
;		print, '-- T-mode starts Sequences.'
		FOR seqn=0,p.seqno-1 DO BEGIN
			time1=yyyymmdd()
			print,time1
			if h.free eq 1 then begin
				obs_tmode2, h=h, img, index=wd.obs.windex, seqn=seqn+1, aseqn=p.seqno, nx, ny
				xeva640_save, img, h=h, p=p;, seqn
			endif else begin
				obs_tmode2, h=h, img, index=wd.obs.windex, seqn=seqn+1, aseqn=p.seqno, nx, ny
				xeva640_save, img, h=h, p=p;, seqn
				time2=yyyymmdd()
				sec1=strmid(time1,8,2)*60.*60.+strmid(time1,10,2)*60.+strmid(time1,12,2)
				sec2=strmid(time2,8,2)*60.*60.+strmid(time2,10,2)*60.+strmid(time2,12,2)
				;if (h.cds - (sec2-sec1)) ne 0 then begin
				if (h.cds - (sec2-sec1)) le 0 then begin	;20160721 T.A.
					print,'!cadence is less than time of taking images'
				endif else begin
					wait,h.cds - (sec2-sec1)
				endelse
			endelse
		ENDFOR
		print, '-- T-mode finished ALL Sequences.'
	END
	wd.obs.TCmode_con_start: BEGIN
;		print, '-- T-C-mode starts continuous.'
		uvalue1 = 'TCmode_con_start'
		ii=1l
		WHILE (uvalue1 NE "TCmode_con_stop") DO BEGIN
			event = widget_event(b_btn1_2, /nowait)
			IF event.id NE 0 THEN WIDGET_CONTROL, get_uvalue=uvalue1, event.id
			CASE uvalue1 OF
				"TCmode_con_start": BEGIN
					time1=yyyymmdd()
					print,time1,'start'
					wait,0.5
					if h.free eq 1 then begin
						obs_tmode, h=h, img, index=wd.obs.windex, seqn=1, aseqn=p.seqno, nx, ny,  $
							   word=strcompress(string(ii),/remove_all)                       ;2016.11.14 TA      
						xeva640_save, img, h=h, p=p;, seqn
						print,'waiting 5 sec' & wait,5.
					endif else begin
						obs_tmode, h=h, img, index=wd.obs.windex, seqn=1, aseqn=p.seqno, nx, ny,  $
							   word=strcompress(string(ii),/remove_all)                       ;2016.11.14 TA   
						xeva640_save, img, h=h, p=p;, seqn
						time2=yyyymmdd()
						sec1=strmid(time1,8,2)*60.*60.+strmid(time1,10,2)*60.+strmid(time1,12,2)
						sec2=strmid(time2,8,2)*60.*60.+strmid(time2,10,2)*60.+strmid(time2,12,2)
						if (h.cds - (sec2-sec1)) le 0 then begin
							print,'!cadence is less than time of taking images' 	;20160823 T.A.
						endif else begin
							print,'wait',h.cds - (sec2-sec1),', cadence',h.cds
							wait,h.cds - (sec2-sec1)
						endelse
					endelse
				END
				'TCmode_con_stop': GOTO, loopend
			ENDCASE
			ii=ii+1l
		ENDWHILE
		loopend:
		print, '-- T-C-mode finished ALL Continuous.'
	END

;<=
	wd.obs.Tmode_con_start: BEGIN
;		print, '-- T-mode starts continuous.'
		uvalue1 = 'Tmode_con_start'
		WHILE (uvalue1 NE "Tmode_con_stop") DO BEGIN
			event = widget_event(b_btn3_2, /nowait)
			IF event.id NE 0 THEN WIDGET_CONTROL, get_uvalue=uvalue1, event.id
			CASE uvalue1 OF
				"Tmode_con_start": BEGIN
					time1=yyyymmdd()
					print,time1,'start'
					wait,0.5
					if h.free eq 1 then begin
						obs_tmode2, h=h, img, index=wd.obs.windex, seqn=1, aseqn=p.seqno, nx, ny
						xeva640_save, img, h=h, p=p;, seqn
					endif else begin
						obs_tmode2, h=h, img, index=wd.obs.windex, seqn=1, aseqn=p.seqno, nx, ny
						xeva640_save, img, h=h, p=p;, seqn
						time2=yyyymmdd()
						sec1=strmid(time1,8,2)*60.*60.+strmid(time1,10,2)*60.+strmid(time1,12,2)
						sec2=strmid(time2,8,2)*60.*60.+strmid(time2,10,2)*60.+strmid(time2,12,2)
						if (h.cds - (sec2-sec1)) le 0 then begin
							print,'!cadence is less than time of taking images'
						endif else begin
							wait,h.cds - (sec2-sec1)
						endelse
					endelse
				END
				'Tmode_con_stop': GOTO, loopend1
			ENDCASE
		ENDWHILE
		loopend1:
		print, '-- T-C-mode finished ALL Continuous.'
	END

;<=

;	wd.obs.Rdus: begin
;		widget_control, eve.id, get_value=value, set_value=''
;		p.data.rdus=value(0)
;		widget_control, eve.id,set_value=string(p.data.rdus);,format='(i5.5)')
;		print,'-- Radius : ',p.data.rdus
;	return
;	end

;	wd.obs.Pagl: begin
;		widget_control, eve.id, get_value=value, set_value=''
;		p.data.pagl=value(0)
;		widget_control, eve.id,set_value=string(p.data.pagl);,format='(i5.5)')
;		print,'-- Polar angle : ',p.data.pagl
;	return
;	end

;	wd.obs.Incl: begin
;		widget_control, eve.id, get_value=value, set_value=''
;		p.data.Incl=value(0)
;		widget_control, eve.id,set_value=string(p.data.Incl);,format='(i5.5)')
;		print,'-- Inclination : ',p.data.incl
;	return
;	end

	wd.obs.wave: BEGIN
		widget_control, eve.id, get_value=value, set_value=''
		h.wave=value[0]
		widget_control, eve.id, set_value=string(h.wave);,format='(i5.5)')
		print, '-- Wavelength : '+h.wave
	RETURN
	END

	wd.obs.period: BEGIN
		widget_control, eve.id, get_value=value, set_value=''
		h.period=float(value[0])
		widget_control, eve.id, set_value=strtrim(string(fix(h.period)),2)
		print, '-- Rotating period : '+string(h.period)
	RETURN
	END

	wd.obs.wvplate: BEGIN
		widget_control, eve.id, get_value=value;, set_value=''
		CASE value[eve.index] OF
			value[0]: h.wvplate = 'APSAW'
			value[1]: h.wvplate = 'QUARTZ'
			value[2]: h.wvplate = 'LUCEO127#2'
		ENDCASE
;		widget_control, eve.id, set_value=value[eve.index]
		print, '-- Wave plate : '+h.wvplate
	RETURN
	END

	wd.exit: BEGIN
		WIDGET_CONTROL, /destroy, eve.top
		WIDGET_CONTROL, /destroy, wd.obs.wbase
		xeva640_stopcap
		xeva640_stopfan
		xeva640_stop
		;obj_destroy,bridge                      ;20161109 TA
		print, '>>  F I N I S H E D !  <<'
	RETURN
	END

;	else:
endcase

RETURN
END

;=====================================================
FUNCTION widget_obs, base, h=h
;=====================================================
common xeva640_wgt,b_btn1_2,b_btn3_2
;create widget for controling Observation
;return widget ID in wd_obse
;base     ---  base window
;h        ---  header for fits
;-----------------------------------------------------
wd_obse = {wd_obse,			$
			Exptime:	0l,	$
;			FrameRate:	0l,	$
			Preview:	0l,	$
			Snap:		0l,	$
			Cmode:		0l,	$
			Tmode:		0l,	$
			Tmode2:		0l,	$
			Tcmode_con_start:		0l,	$
			Tcmode_con_stop:		0l,	$
			Tmode_con_start:		0l,	$
			Tmode_con_stop:		0l,	$
			Naxis3:		0l,	$
			free:		0l,	$	; 2014.10.23 T. Anan & T. Yoneya
			cds:		0l,	$	; 2014.10.23 T. Anan & T. Yoneya
;			R:			0l,	$
;			P:			0l,	$
;			Il:			0l,	$
			Wave:		0l, $
			Period:		0l,	$
			Wvplate:	0l, $
			windex:		0l,	$	;<-add for winsow
			wbase:		0l	$	;<-add for winsow
		}

obswindow, wbase, windex
wd_obse.windex = windex
wd_obse.wbase = wbase

lab = widget_label(base, value='>> Observation <<', font='Arial')

b_obs = widget_base(base, /row, /frame)

	b_obs1 = widget_base(b_obs, /row)

	b_prm = widget_base(b_obs1, /column, /frame)
	lab = widget_label(b_prm, value=' -- Parameters -- ', font='Arial')

	b_exp = widget_base(b_prm, /row)
		lab = widget_label(b_exp, value=' Exp : ', font='Arial')
		wd_obse.Exptime = widget_text(b_exp, value=strtrim(string(long(h.exptime*1000000)),2) $
			,uvalue='Exptime', xsize=8, /edit, font='Arial')
		lab = widget_label(b_exp, value=' usec ', font='Arial')

	b_note = widget_base(b_prm, /row)
		lab = widget_label(b_note, value='(1 -- 10,000,000 usec)', font='Arial')

;	b_fr=widget_base(b_prm, /row)
;		lab = widget_label(b_fr,value=' FrameRate : ',font='Arial')
;		wd_obse.FrameRate = widget_text(b_fr,value=string(p_ccd.frate,format='(i2)') $
;			,uvalue='FrameRate',xsize=2,/edit,font='Arial')
;		lab = widget_label(b_fr,value=' Hz ',font='Arial')

;	b_gin=widget_base(b_prm, /row)
;		lab = widget_label(b_gin,value=' Gain : ',font='Arial')
;		wd_obse.Gain = cw_bgroup(b_gin,['High','Low'],set_value=0 $
;				,uvalue="Gain",/row,/no_release,/exclusive)

	b_frms = widget_base(b_prm, /row)
		lab = widget_label(b_frms, value='Frames : ', font='Arial')
		wd_obse.Naxis3 = widget_text(b_frms $
			;,value=string(100, format='(i3)'), uvalue='Naxis3', xsize=4,font='Arial')
			,value=string(h.naxis3, format='(i3)'), uvalue='frms', xsize=4, font='Arial',/edit)

; 2014.10.23 T. Anan & T.Yoneda & T. Fujita =>
	b_cds1 = widget_base(b_prm, /row)
		wd_obse.free=cw_bgroup(b_cds1,'Free run',/nonexclusive,set_value=h.free,font='Arial')

	b_cds2 = widget_base(b_prm, /row)
		lab = widget_label(b_cds2, value='Cadence : ', font='Arial')
		wd_obse.cds = widget_text(b_cds2 $
			,value=string(h.cds, format='(i4)'), uvalue='cadence', xsize=4,font='Arial',edit=1)
		lab = widget_label(b_cds2, value=' sec', font='Arial')
; <=


	b_btn = widget_base(b_obs, /column)
		b_btn2 = widget_base(b_btn, /row)
		wd_obse.Preview = widget_button(b_btn2, value="Preview", uvalue="Prev", font='Arial')
		lab = widget_label(b_btn2, value='|', font='Arial')
		wd_obse.Snap  = widget_button(b_btn2, value=" Snap ", uvalue="Snap", font='Arial')
		wd_obse.Cmode = widget_button(b_btn, value="C-mode obs", uvalue="Cmode", font='Arial')
; 2014.10.23 T. Anan & T.Yoneda & T. Fujita =>
		lab = widget_label(b_btn, value='', font='Arial')
		lab = widget_label(b_btn, value='T-C-mode obs', font='Arial')
		b_btn1 = widget_base(b_btn, /column,frame=1)
		wd_obse.Tmode = widget_button(b_btn1, value="Sequence start", uvalue="Tmode", font='Arial')
		b_btn1_2 = widget_base(b_btn1, /row,frame=0)
		lab = widget_label(b_btn1_2, value='Continuous ', font='Arial')
		wd_obse.TCmode_con_start = widget_button(b_btn1_2, value="Start", uvalue="TCmode_con_start", font='Arial')
		wd_obse.TCmode_con_stop = widget_button(b_btn1_2, value="Stop", uvalue="TCmode_con_stop", font='Arial')
;<=
; 2016.05.26 T. Anan =>
		lab = widget_label(b_btn, value='', font='Arial')
		lab = widget_label(b_btn, value='T-mode obs', font='Arial')
		b_btn3 = widget_base(b_btn, /column,frame=1)
		wd_obse.Tmode2 = widget_button(b_btn3, value="Sequence start", uvalue="Tmode2", font='Arial')
		b_btn3_2 = widget_base(b_btn3, /row,frame=0)
		lab = widget_label(b_btn3_2, value='Continuous ', font='Arial')
		wd_obse.Tmode_con_start = widget_button(b_btn3_2, value="Start", uvalue="Tmode_con_start", font='Arial')
		wd_obse.Tmode_con_stop = widget_button(b_btn3_2, value="Stop", uvalue="Tmode_con_stop", font='Arial')
;<=

	lab = widget_label(base, value='>> Header <<', font='Arial')
	b_hdr = widget_base(base, /column, /frame)

		b_WL = widget_base(b_hdr, /row)
		lab = widget_label(b_WL, value=' Wevelength : ', font='Arial')
		wd_obse.wave = widget_text(b_WL, value=string(h.wave) $;,format='(i5.5)') $
			,uvalue='Wave', xsize=6, /edit, font='Arial')
		lab = widget_label(b_WL, value=' A ', font='Arial')

		b_pd = widget_base(b_hdr, /row)
		lab = widget_label(b_pd, value=' Period : ', font='Arial')
		wd_obse.period = widget_text(b_pd, value=strtrim(string(fix(h.period)),2) $;,format='(i5.5)') $
			,uvalue='Period',xsize=3, /edit, font='Arial')
		lab = widget_label(b_pd, value=' sec / rev. (min: 1)', font='Arial')

		b_wvp=widget_base(b_hdr, /row)
		lab = widget_label(b_wvp,value=' Wave plate : ',font='Arial')
		wpname = ['APSAW', 'QUARTZ', 'LUCEO127#2']
  		wd_obse.wvplate = WIDGET_DROPLIST(b_wvp, VALUE=wpname, uvalue='Wvplate', /align_center, font='Arial')

RETURN, wd_obse
END

;=====================================================
FUNCTION widget_data, base, p=p
;=====================================================
;create widget for handling image data
;return widget ID in wd_data
;INPUTS;
;	base	base window
;	p		Data parameter structre
;-----------------------------------------------------
wd_data = {wd_data,			$
			dir:		0l,	$
			fname:		0l,	$
			seqno:		0l	$
			}

lab = widget_label(base, value='>> Output <<', font='Arial')

b_op = widget_base(base, /column, /frame)

	b_dir = widget_base(b_op, /row)

		lab = widget_label(b_dir, value=' Dir : ', font='Arial')
		wd_data.dir = widget_text(b_dir, value=p.dir, uvalue='Dir', xsize=25, font='Arial', /edit)

	b_fnam = widget_base(b_op, /row)

		lab = widget_label(b_fnam, value=' Fname : ', font='Arial')
		wd_data.fname = widget_text(b_fnam, value=p.fname, uvalue='Fname', xsize=10, font='Arial', /edit)

		lab = widget_label(b_fnam, value=' # of Seq : ', font='Arial')
		wd_data.seqno = widget_text(b_fnam, value=strtrim(string(p.seqno),2), uvalue='Seqno', xsize=4, font='Arial', /edit)

RETURN, wd_data
END

;=====================================================
FUNCTION widget_exit, base
;=====================================================
;INPUTS;
;	base	base window
;-----------------------------------------------------
wd_exit = widget_button(base, value="Exit", uvalue = "EXIT", font='Arial')

RETURN, wd_exit
END


;*****************************************************
;+
;
; XEVA640_LIB.PRO
; library for controling XEVA640 "cam_set5.dll"
;
;	'09/06/02	M.H.
;	'10/12/15	A.O.	copy from irlib.pro
;	'11/08/04	A.O.	add Trigger mode
;	'12/11/20	A.O.	renewal
;	'14/10/23	T.A. & T.Y.	cadence, continuous mode
;	'16/05/10	T.A.	set_naxis3, obs_cmode,obs_tmod,xeva640_save, xeva640_save_sna
;	'16/05/26	T.A.	xeva640_gettmode2, obs_tmode2
;	'16/11/09	T.A.	add az and imgrot in mkheader and str2str
;       '16/11/14       T.A.    word keyword in obs_cmode and obs_tmode
;
;	PRO xeva640_init
;	PRO xeva640_startfan, temp=temp
;	PRO xeva640_startcap, frms=frms, expo=expo, gain=gain
;	PRO xeva640_stopcap
;	PRO xeva640_stopfan
;	PRO xeva640_stop
;	PRO set_exptime, expo=expo
;	PRO xeva640_getimg, img
;	PRO xeva640_getcmode, img, tim1;, frms=frms
;	PRO xeva640_gettmode, img, tim0;, frms=frms
;	FUNCTION xeva640_checktemp
;	FUNCTION xeva640_checkpower
;	FUNCTION mkheader
;	FUNCTION mkdatapm
;	FUNCTION yyyymmdd, date=date, t=t, ut=ut
;	FUNCTION str2str, h=h
;	PRO xeva640_snap, h=h, img, index=index;, expo=expo, p=p
;	PRO obs_cmode, h=h, img, index=index, seqn=seqn, nx, ny
;	PRO obs_tmode, h=h, img, index=index, seqn=seqn, nx, ny
;	PRO obswindow, wbase, index
;	PRO xeva640_save_snap, img, h=h, p=p
;	PRO xeva640_save, img, h=h, p=p;, seqn
;
;-
;*****************************************************

;=====================================================
PRO xeva640_init
;=====================================================
; init camera
;-----------------------------------------------------
common xeva640lib, m_hCam, dllfile
dllfile = 'C:\Projects\cprog\VS2005\Xenics\cam_set5\debug\cam_set5.dll'
;dllfile='E:\projects\cprog\VS2005\Xenics\cam_set5\debug\cam_set5.dll'
m_hCam = call_external(dllfile, 'createcam', /all_value, /cdecl)
END

;=====================================================
PRO xeva640_startfan, temp=temp
;=====================================================
; start camera cooling
;-----------------------------------------------------
IF NOT keyword_set(temp) THEN Temp=260	; Cooling Temp. (K)
common xeva640lib, m_hCam, dllfile
stfan = call_external(dllfile, 'startfan', m_hCam, /all_value, /cdecl)
ctemp = call_external(dllfile, 'settemp', m_hCam, Temp, /all_value, /cdecl)
END

;=====================================================
PRO xeva640_startcap, frms=frms, expo=expo, gain=gain
;=====================================================
; start camera capturing
;-----------------------------------------------------
IF NOT keyword_set(frms) THEN frms=100		; # of frames
IF NOT keyword_set(expo) THEN expo=200000l	; unit=us
IF NOT keyword_set(gain) THEN gain=3		; gain=3 means high gain
common xeva640lib, m_hCam, dllfile
m_hCam = call_external(dllfile, 'startcap', m_hCam, expo, gain, /all_value,/cdecl)
rr = call_external(dllfile, 'initboard', /all_value, /cdecl)
rr = call_external(dllfile, 'InitParam', frms, /all_value, /cdecl)
rr = call_external(dllfile, 'AllocMemory', /all_value, /cdecl)
END

;=====================================================
PRO xeva640_stopcap
;=====================================================
; stop capturing
;-----------------------------------------------------
common xeva640lib, m_hCam, dllfile
rr = call_external(dllfile, 'ReleaseMemory', /all_value, /cdecl)
rr = call_external(dllfile, 'closeboard', /all_value, /cdecl)
END

;=====================================================
PRO xeva640_stopfan
;=====================================================
; stop camera cooling
;-----------------------------------------------------
common xeva640lib, m_hCam, dllfile
stfan = call_external(dllfile, 'stopfan', m_hCam, /all_value, /cdecl)
END

;=====================================================
PRO xeva640_stop
;=====================================================
; stop camera
;-----------------------------------------------------
common xeva640lib, m_hCam, dllfile
m_hCam = call_external(dllfile, 'stopcap', m_hCam, /all_value, /cdecl)
END

;=====================================================
PRO set_exptime, expo=expo
;=====================================================
; set exposure time
;-----------------------------------------------------
common xeva640lib, m_hCam, dllfile
eexpo=call_external(dllfile,'set_exptim',expo,/all_value,/cdecl)
END

;=====================================================
PRO xeva640_getimg, img
;=====================================================
; get image
;-----------------------------------------------------
common xeva640lib, m_hCam, dllfile
mm = call_external(dllfile, 'get_img', img, value=[0b], /cdecl)
END

;=====================================================
PRO xeva640_getcmode, img, tim1;, frms=frms
;=====================================================
; get image continuous mode
;-----------------------------------------------------
;if not keyword_set(frms) then frms=1
;if frms gt 100 then frms=100
common xeva640lib, m_hCam, dllfile
;value=bytarr(2) & value[0]=[0b] & value[1]=0
;tim1=call_external(dllfile,'get_cmode',img,frms,value=value,/cdecl)
tim1 = call_external(dllfile, 'get_cmode', img, value=[0b], /cdecl)
END

;=====================================================
PRO xeva640_gettmode, img, tim0;, frms=frms
;=====================================================
; get iamge ExternalTrigger mode
;-----------------------------------------------------
;if not keyword_set(frms) then frms=1
;if frms gt 100 then frms=100
common xeva640lib, m_hCam, dllfile
;value=bytarr(2) & value[0]=[0b] & value[1]=0
;tim0=call_external(dllfile,'get_tmode',img,frms,value=[0b],/cdecl)
tim0 = call_external(dllfile, 'get_tmode', img, value=[0b], /cdecl)
END
;=====================================================
PRO xeva640_gettmode2, img, tim0;, frms=frms
;=====================================================
; get iamge ExternalTrigger mode
;-----------------------------------------------------
;if not keyword_set(frms) then frms=1
;if frms gt 100 then frms=100
common xeva640lib, m_hCam, dllfile
;value=bytarr(2) & value[0]=[0b] & value[1]=0
;tim0=call_external(dllfile,'get_tmode',img,frms,value=[0b],/cdecl)
tim0 = call_external(dllfile, 'get_tmode2', img, value=[0b], /cdecl)
END

;=====================================================
FUNCTION xeva640_checktemp
;=====================================================
; check temperature
;-----------------------------------------------------
common xeva640lib, m_hCam, dllfile
temp = call_external(dllfile, 'checktemp', m_hCam, /all_value, /cdecl)
;print, 'Temp. ='+string(ctemp,form='(i5)')+'K'
RETURN, temp
END

;=====================================================
FUNCTION xeva640_checkpower
;=====================================================
; check fanpower
;-----------------------------------------------------
common xeva640lib, m_hCam, dllfile
pwr = call_external(dllfile, 'checkpower', m_hCam, /all_value, /cdecl)
;print, 'Temp. ='+string(ctemp,form='(i5)')+'K'
RETURN, pwr
END

;========== set frame rate ===========================
pro ircam_setfrate,frate=frate
	if not keyword_set(frate) then frate=1	; unit=Hz
common xeva640lib ,m_hCam, dllfile
sfr=call_external(dllfile,'set_frate',frate,/all_value,/cdecl)
end

;========== check frame rate ===========================
pro ircam_chkfrate
common xeva640lib, m_hCam, dllfile
cfr=call_external(dllfile,'getfrate',/all_value,/cdecl)
print,cfr
end

;=====================================================
FUNCTION mkheader
;=====================================================

h = {header,							$
		simple:		'T',				$
		bitpix:		16,					$
		naxis:		3,					$
		naxis1:		640,				$
		naxis2:		512,				$
		naxis3:		100,				$
		extend:		'F',				$
		bscale:		1.,					$
		bzero:		0.,					$; 32768.,$
		origin:		'HIDA OBSERVATORY',	$
		observat:	'HIDA OBSERVATORY',	$
		telescop:	'DST',				$
		instrume:	'',					$
		program:	'XEVA640_obs',		$
		prog_ver:	1,					$
		obs_type:	'NOM',				$
		polstate:	'',					$
		timesys:	'UTC',				$
		date:		'',					$
		time_obs:	'',					$
		time_end:	'',					$
		wvplate:	'QUARTZ',			$
		period:		4.,					$
		detnam:		'XEVA640',			$
		det_temp:	0l,					$
		det_pwr:	0l,					$
		exptime:	0.2,				$
		camgain:	1,					$
		fgbinx:		1l,					$
		fgbiny:		1l,					$
		x0:			1,					$
		x1:			640,				$
		y0:			1,					$
		y1:			512,				$
		wave:		'10830',			$
		r:			0.d0,				$
		p:			0.d0,				$
		i:			0.d0,				$
		ha:			0.d0,				$
		zd:			0.d0,				$
		az:			0.d0,				$
		imgrot:			0.d0,				$
		free:			1,				$
		cds:			30,				$
		wait:			1				$
	}

RETURN, h
END

;=====================================================
FUNCTION mkdatapm
;=====================================================

p = {data,					$
		dir:	'c:\data\',	$
		fname:	'a',		$
		seqno:	1			$
	}

RETURN, p
END

;=====================================================
FUNCTION yyyymmdd, date=date, t=t, ut=ut
;=====================================================
; return YYYY-MM-DDThh:mm:ss.000 string,
;	get_time  --  get hh:mm:ss string
; 2011/04/02	copy from yymmdd.pro	A.O.
; 2012/11/07	JST -> UT		A.O.
;-----------------------------------------------------
IF keyword_set(ut) then tstr=systime(/utc) else tstr=systime()

yyyy=strmid(tstr,20,4)
CASE strmid(tstr,4,3) OF
          'Jan':        mm='01'
          'Feb':        mm='02'
          'Mar':        mm='03'
          'Apr':        mm='04'
          'May':        mm='05'
          'Jun':        mm='06'
          'Jul':        mm='07'
          'Aug':        mm='08'
          'Sep':        mm='09'
          'Oct':        mm='10'
          'Nov':        mm='11'
          'Dec':        mm='12'
ENDCASE
dd=strmid(tstr,8,2)
IF strmid(dd,0,1) EQ ' ' THEN dd='0'+strmid(dd,1,1)

IF keyword_set(t) THEN yyyymmdd=yyyy+'-'+mm+'-'+dd+'T'+strmid(tstr,11,8)+'.000' $
ELSE yyyymmdd=yyyy+mm+dd+strmid(tstr,11,2)+strmid(tstr,14,2)+strmid(tstr,17,2)

IF keyword_set(date) THEN yyyymmdd=yyyy+mm+dd

RETURN, yyyymmdd
END

;=====================================================
FUNCTION str2str, h=h
;=====================================================
fh = strarr(46) & i=0

fh[i] = string('SIMPLE  = ', h.SIMPLE, '/',		format='(a10,a20,a2)') & ++i
fh[i] = string('BITPIX  = ', h.BITPIX, '/',		format='(a10,a20,a2)') & ++i
fh[i] = string('NAXIS   = ', h.NAXIS,  '/',		format='(a10,a20,a2)') & ++i
fh[i] = string('NAXIS1  = ', h.NAXIS1, '/',		format='(a10,a20,a2)') & ++i
fh[i] = string('NAXIS2  = ', h.NAXIS2, '/',		format='(a10,a20,a2)') & ++i
fh[i] = string('NAXIS3  = ', h.NAXIS3, '/',		format='(a10,a20,a2)') & ++i
fh[i] = string('EXTEND  = ', h.EXTEND, '/',		format='(a10,a20,a2)') & ++i
fh[i] = string('BSCALE  = ', h.BSCALE, '/',		format='(a10,i20,a2)') & ++i
fh[i] = string('BZERO   = ', h.BZERO,  '/',		format='(a10,i20,a2)') & ++i
fh[i] = string('ORIGIN  = ', "'", h.ORIGIN,   "'", '/',	format='(a10,a1,a18,a1,a2)') & ++i
fh[i] = string('OBSERVAT= ', "'", h.OBSERVAT, "'", '/',	format='(a10,a1,a18,a1,a2)') & ++i
fh[i] = string('TELESCOP= ', "'", h.TELESCOP, "'", '/',	format='(a10,a1,a18,a1,a2)') & ++i
fh[i] = string('INSTRUME= ', "'", h.INSTRUME, "'", '/',	format='(a10,a1,a18,a1,a2)') & ++i
fh[i] = string('PROGRAM = ', "'", h.PROGRAM,  "'", '/',	format='(a10,a1,a18,a1,a2)') & ++i
fh[i] = string('PROG_VER= ',      h.PROG_VER,      '/',	format='(a10,a20,a2)') & ++i
fh[i] = string('OBS_TYPE= ', "'", h.OBS_TYPE, "'", '/',	format='(a10,a1,a18,a1,a2)') & ++i
fh[i] = string('POLSTATE= ', "'", h.POLSTATE, "'", '/',	format='(a10,a1,a18,a1,a2)') & ++i
fh[i] = string('TIMESYS = ', "'", h.TIMESYS,  "'", '/',	format='(a10,a1,a18,a1,a2)') & ++i
fh[i] = string('DATE    = ', "'", h.DATE,     "'", '/',	format='(a10,a1,a23,a1,a2)') & ++i
fh[i] = string('DATE_OBS= ', "'", h.TIME_OBS, "'", '/',	format='(a10,a1,a23,a1,a2)') & ++i
fh[i] = string('DATE_END= ', "'", h.TIME_END, "'", '/',	format='(a10,a1,a23,a1,a2)') & ++i
fh[i] = string('WVPLATE = ', "'", h.WVPLATE,  "'", '/',	format='(a10,a1,a18,a1,a2)') & ++i
fh[i] = string('PERIOD  = ',      h.PERIOD,        '/', format='(a10,f20,a2)') & ++i
fh[i] = string('DETNAM  = ', "'", h.DETNAM,   "'", '/',	format='(a10,a1,a18,a1,a2)') & ++i
fh[i] = string('DET_TMP = ', h.DET_TEMP,           '/',	format='(a10,i20,a2)') & ++i
fh[i] = string('DET_PWR = ', h.DET_PWR,            '/',	format='(a10,i20,a2)') & ++i
fh[i] = string('EXPTIME = ', h.EXPTIME,            '/',	format='(a10,f20,a2)') & ++i
fh[i] = string('CAMGAIN = ', h.CAMGAIN,            '/',	format='(a10,i20,a2)') & ++i
fh[i] = string('FGBINX  = ', h.FGBINX,             '/',	format='(a10,i20,a2)') & ++i
fh[i] = string('FGBINY  = ', h.FGBINY,             '/',	format='(a10,i20,a2)') & ++i
fh[i] = string('X0      = ', h.X0,                 '/',	format='(a10,i20,a2)') & ++i
fh[i] = string('X1      = ', h.x1,                 '/',	format='(a10,i20,a2)') & ++i
fh[i] = string('Y0      = ', h.Y0,                 '/',	format='(a10,i20,a2)') & ++i
fh[i] = string('Y1      = ', h.Y1,                 '/',	format='(a10,i20,a2)') & ++i
fh[i] = string('WAVE    = ', "'", h.WAVE,     "'", '/',	format='(a10,a1,a18,a1,a2)') & ++i
fh[i] = string('R       = ', h.R,  '/',			format='(a10,d20,a2)') & ++i
fh[i] = string('P       = ', h.P,  '/',			format='(a10,d20,a2)') & ++i
fh[i] = string('I       = ', h.I,  '/',			format='(a10,d20,a2)') & ++i
fh[i] = string('HA      = ', h.HA, '/',			format='(a10,d20,a2)') & ++i
fh[i] = string('ZD      = ', h.ZD, '/',			format='(a10,d20,a2)') & ++i
fh[i] = string('AZ      = ', h.az, '/',			format='(a10,d20,a2)') & ++i
fh[i] = string('IMGROT  = ', h.imgrot, '/',		format='(a10,d20,a2)') & ++i
fh[i] = string('COMMENT   ', '',			format='(a10,a20)') & ++i
fh[i] = string('COMMENT   ', '',			format='(a10,a20)') & ++i
fh[i] = string('HISTORY   ', '',			format='(a10,a20)')
fh[i] = string('END       ',				format='(a10)')
blnk80 = string(' ',format='(a80)')
fh = strmid(fh+blnk80,0,80)

RETURN, fh
END

;=====================================================
PRO xeva640_prev, img, nx, ny;, expo=expo
;=====================================================
; preview mode
;-----------------------------------------------------
common xeva640lib, m_hCam, dllfile

IF NOT keyword_set(mxmi) THEN mxmi=[0,100]
;---------- Create a base widget ----------
base1 = WIDGET_BASE(title='XEVA640 / Preview', /column, xoffset=512, yoffset=512)
xpdmenu, ['/START/', '/EXIT/'], base1, /frame, title='XEVA640_preview'
draw = WIDGET_DRAW(base1, XSIZE = nx, YSIZE = ny)
base2 = WIDGET_BASE(base1, title='DRAW', /row)
plotx = WIDGET_DRAW(base2, XSIZE=nx/2, YSIZE=ny/2)
ploty = WIDGET_DRAW(base2, XSIZE=nx/2, YSIZE=ny/2)
;---------- Realize the widgets ----------
WIDGET_CONTROL, /REALIZE, base1
WIDGET_CONTROL, plotx, GET_VALUE=index_x
WIDGET_CONTROL, ploty, GET_VALUE=index_y
;---------- Obtain the window index ----------
WIDGET_CONTROL, draw, GET_VALUE=index
WIDGET_CONTROL, plotx, GET_VALUE=index_x
WIDGET_CONTROL, ploty, GET_VALUE=index_y
;---------- Set the new widget to be the current graphics window ----------
uvalue = ''
print, "Click 'START' then image"
;---------- Set the new widget to be the current graphics window ----------
WSET, index
xeva640_getimg, img
tvscl, img[*,*,0]
xyouts, 10, 10, size=2, "Click 'START' then image", /dev
cursor, xx, yy, /dev, /down

uvalue = ''
WHILE (uvalue NE "EXIT") DO BEGIN
	event = widget_event(base1, /nowait)
	IF event.id NE 0 THEN WIDGET_CONTROL, get_uvalue=uvalue, event.id
	CASE uvalue OF
		'START': BEGIN
				WSET, index
				xeva640_getimg, img
				temp = xeva640_checktemp()
				pwr = xeva640_checkpower()
				mxmi = [0,max(img[*,yy,0])>max(img[xx,*,0])>mxmi[1]]
				tvscl, img[*,*,0]
				plots, [0,640-1], [yy,yy], line=1, /dev
				plots, [xx,xx], [0,512-1], line=1, /dev
				xyouts, 10,10, /dev, size=2, 'TMP: '+strtrim(temp,2)+'K('+strtrim(temp-273,2)+'C), PWR: '+strtrim(pwr,2)
				wset, index_x
				plot, img[*,yy,0], /xstyle, yr=mxmi, title='X profile (@y='+strtrim(yy,2)+' pixel)'
				wset, index_y
				plot, img[xx,*,0], /xstyle, yr=mxmi, title='Y profile (@x='+strtrim(xx,2)+' pixel)'
		END
		'EXIT': GOTO, loopend
	ENDCASE
ENDWHILE
loopend:

WIDGET_CONTROL, /destroy, base2
WIDGET_CONTROL, /destroy, base1

END

;=====================================================
PRO xeva640_snap, h=h, img, index=index;, expo=expo, p=p
;=====================================================
common xeva640lib, m_hCam, dllfile

h.obs_type='NOM'

h.time_obs=yyyymmdd(/ut,/T)
xeva640_getimg, img
h.time_end=yyyymmdd(/ut,/T)

h.det_temp=fix(xeva640_checktemp())
h.det_pwr=fix(xeva640_checkpower())

WSET, index
tvscl, img[*,*,0]
xyouts, 10, 512-30, 'EXP: '+strtrim(long(h.exptime*1000000),2)+'us, Frames: 1', /dev, size=2
xyouts, 10, 10, 'TMP: '+strtrim(h.det_temp,2)+'K('+strtrim(h.det_temp-273,2)+'C), PWR: '+strtrim(h.det_pwr,2), /dev, size=2

END

;=====================================================
PRO obs_cmode, h=h, img, index=index, seqn=seqn, aseqn=aseqn, nx, ny,   $
               word=word               ;2016.11.14 TA
;=====================================================
;	h		header
;	img		raw data
;	index	window index
;	seqn	sequence number
;-----------------------------------------------------
common xeva640lib, m_hCam, dllfile

;time0=call_external(dllfile,'getime',/all_value,/cdecl)*1.

h.obs_type='NOM'

h.time_obs = yyyymmdd(/ut,/T)
;print,call_external(dllfile,'getime',/all_value,/cdecl)*1.-time0		; ~40-60ms
xeva640_getcmode, img, tim1
;print,' ',call_external(dllfile,'getime',/all_value,/cdecl)*1.-time0	; ~11140-11160ms
h.time_end = yyyymmdd(/ut,/T)												; 11s

h.det_temp=fix(xeva640_checktemp())
h.det_pwr=fix(xeva640_checkpower())

;IF 0 THEN BEGIN				;20120403, anan
;disp = rebin(img,nx,ny,1)
WSET, index
tvscl, img[*,*,0];disp
;xyouts, 10, 512-30, 'EXP: '+strtrim(long(h.exptime*1000000),2)+'us, Frames: 100, Seq.N: '+strtrim(seqn,2)+'/'+strtrim(aseqn,2), /dev, size=2
xyouts, 10, 512-30, 'EXP: '+strtrim(long(h.exptime*1000000),2)+	$
	'us, Frames: '+strtrim(long(h.naxis3),2)+$	
	', Seq.N: '+strtrim(seqn,2)+'/'+strtrim(aseqn,2), /dev, size=2		;2016.05.10 T.A
xyouts, 10, 10, 'TMP: '+strtrim(h.det_temp,2)+'K('+strtrim(h.det_temp-273,2)+'C), PWR: '+strtrim(h.det_pwr,2), /dev, size=2
;ENDIF
;print,'TMP: '+strtrim(h.det_temp,2)+'K('+strtrim(h.det_temp-273,2)+'C), PWR: '+strtrim(h.det_pwr,2)
;print,' ',call_external(dllfile,'getime',/all_value,/cdecl)*1.-time0	; ~11031-11260ms

if keyword_set(word) then begin                      ;2016.11.14 TA
	xyouts, .1, .4,norm=1,word,charsize=100
endif

END

;=====================================================
PRO obs_tmode, h=h, img, index=index, seqn=seqn, aseqn=aseqn, nx, ny,  $
               word=word     ;2016.11.14 TA
;=====================================================
;	h		header
;	img		raw data
;	index	window index
;	seqn	sequence number
;-----------------------------------------------------
common xeva640lib, m_hCam, dllfile

;time0=call_external(dllfile,'getime',/all_value,/cdecl)*1.
h.obs_type='POL'

h.time_obs=yyyymmdd(/ut,/T)
print,'trigger waiting, ',h.time_obs & wait,0.1
xeva640_gettmode, img, tim1;, frms=1

;xeva640_getcmode, img, tim1;, frms=frms
;print,' ',call_external(dllfile,'getime',/all_value,/cdecl)*1.-time0	; ~
h.time_end=yyyymmdd(/ut,/T)
print,'complete taking images',h.time_end



h.det_temp=fix(xeva640_checktemp())
h.det_pwr=fix(xeva640_checkpower())

;disp = rebin(img,nx,ny,1)
WSET, index
tvscl, img[*,*,0];disp
;xyouts, 10, 512-30, 'EXP: '+strtrim(long(h.exptime*1000000),2)+'us, Frames: 100, Seq.N: '+strtrim(seqn,2)+'/'+strtrim(aseqn,2), /dev, size=2
xyouts, 10, 512-30, 'EXP: '+strtrim(long(h.exptime*1000000),2)+	$
	'us, Frames: '+strtrim(long(h.naxis3),2)+$	
	', Seq.N: '+strtrim(seqn,2)+'/'+strtrim(aseqn,2), /dev, size=2		;2016.05.10 T.A
xyouts, 10, 10, 'TMP: '+strtrim(h.det_temp,2)+'K('+strtrim(h.det_temp-273,2)+'C), PWR: '+strtrim(h.det_pwr,2), /dev, size=2
;print,'TMP: '+strtrim(h.det_temp,2)+'K('+strtrim(h.det_temp-273,2)+'C), PWR: '+strtrim(h.det_pwr,2)
;print,' ',call_external(dllfile,'getime',/all_value,/cdecl)*1.-time0	; ~

if keyword_set(word) then begin                      ;2016.11.14 TA
	xyouts, .1, .4,norm=1,word,charsize=20
endif

END
;=====================================================
PRO obs_tmode2, h=h, img, index=index, seqn=seqn, aseqn=aseqn, nx, ny
;=====================================================
;	h		header
;	img		raw data
;	index	window index
;	seqn	sequence number
;-----------------------------------------------------
common xeva640lib, m_hCam, dllfile

;time0=call_external(dllfile,'getime',/all_value,/cdecl)*1.
h.obs_type='POL'

h.time_obs=yyyymmdd(/ut,/T)
xeva640_gettmode2, img, tim1;, frms=1
;print,call_external(dllfile,'getime',/all_value,/cdecl)*1.-time0		; ~
;xeva640_getcmode, img, tim1;, frms=frms
;print,' ',call_external(dllfile,'getime',/all_value,/cdecl)*1.-time0	; ~
h.time_end=yyyymmdd(/ut,/T)

h.det_temp=fix(xeva640_checktemp())
h.det_pwr=fix(xeva640_checkpower())

;disp = rebin(img,nx,ny,1)
WSET, index
tvscl, img[*,*,0];disp
;xyouts, 10, 512-30, 'EXP: '+strtrim(long(h.exptime*1000000),2)+'us, Frames: 100, Seq.N: '+strtrim(seqn,2)+'/'+strtrim(aseqn,2), /dev, size=2
xyouts, 10, 512-30, 'EXP: '+strtrim(long(h.exptime*1000000),2)+	$
	'us, Frames: '+strtrim(long(h.naxis3),2)+$	
	', Seq.N: '+strtrim(seqn,2)+'/'+strtrim(aseqn,2), /dev, size=2		;2016.05.10 T.A
xyouts, 10, 10, 'TMP: '+strtrim(h.det_temp,2)+'K('+strtrim(h.det_temp-273,2)+'C), PWR: '+strtrim(h.det_pwr,2), /dev, size=2
;print,'TMP: '+strtrim(h.det_temp,2)+'K('+strtrim(h.det_temp-273,2)+'C), PWR: '+strtrim(h.det_pwr,2)
;print,' ',call_external(dllfile,'getime',/all_value,/cdecl)*1.-time0	; ~

END

;=====================================================
PRO obswindow, wbase, index
;=====================================================
common xeva640lib, m_hCam, dllfile

wbase = WIDGET_BASE(title='XEVA640 / Observing', /column, xoffset=256*3, yoffset=512)
draw = WIDGET_DRAW(wbase, XSIZE=640, YSIZE=512)
WIDGET_CONTROL, draw, GET_VALUE=index 
WIDGET_CONTROL, /REALIZE, wbase

END

;=====================================================
PRO xeva640_save_snap, img, h=h, p=p
;=====================================================
h.naxis=fix(2)
h.naxis3=fix(1)
h.date=yyyymmdd(/ut,/T)
sh=str2str(h=h)
fname='snap_'+yyyymmdd()+'.fits'
writefits,p.dir+fname, img[*,*,0], sh
h.naxis=fix(3)
h.naxis3=fix(100)

print,'saved '+p.dir+fname;		;2016.05.10 T.A
END

;=====================================================
PRO xeva640_save, img, h=h, p=p;, seqn
;=====================================================
h.date=yyyymmdd(/ut,/T)
sh=str2str(h=h)
fname=p.fname+'_'+yyyymmdd()+'.fits'
imgs=img[*,*,0:h.naxis3-1]		;2016.05.10 T.A
writefits,p.dir+fname, imgs, sh		;2016.05.10 T.A
print,'saved '+p.dir+fname		;2016.05.10 T.A

END
;=====================================================
PRO set_naxis3, h
;=====================================================
common xeva640lib, m_hCam, dllfile

rr = call_external(dllfile, 'ReleaseMemory', /all_value, /cdecl)
rr = call_external(dllfile, 'closeboard', /all_value, /cdecl)	
rr = call_external(dllfile, 'initboard', /all_value, /cdecl)
rr = call_external(dllfile, 'InitParam', h.naxis3, /all_value, /cdecl)
rr = call_external(dllfile, 'AllocMemory', /all_value, /cdecl)


END


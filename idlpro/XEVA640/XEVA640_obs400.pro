;*****************************************************
;+
;   XEVA640_OBS.PRO
;	based Norikura Project-E observation program
;
;CALLS:
;		XEVA640_LIB.PRO		library for XEVA640
;		XEVA640_WGT.PRO		Widget for XEVA640 Obs.
;		WRITEFITS.PRO (SSW)	save data fits file
;-
;*****************************************************
@XEVA640_lib	; compile XEVA640 library
@XEVA640_wgt	; compile XEVA640 widget
@XMANAGER

;---------- MODIFICATION HISTORY ----------
FUNCTION version
;ver=0		; '96/07/08	k.i. copy from obsa.pro
;ver=1.0	; '96/08/11	k.i. Ver.1.0
;ver='1.1'	; '96/08/15	k.i. bug
;ver='1.2'	; '97/04/19	k.i. version()
;ver='2.0'	; '97/06/15	k.i. prje --> nhk
;ver='2.1'	; '97/07/22	k.i. new cal.file with new nhklib.pro
;ver='3.0'	; '05/09/08	k.i., t.t.y., new cal.file with nhktopa.pro
;ver='3.1'	; '05/09/18	k.s. if p.ccd.exp lt 1500 then wait,1.2, for sp(*,*) x nx
;ver='4.0'	; '09/06/01	M.H. IR Camera
;ver='4.1'	; '10/12/15	A.O. IR Camera at Hida
;ver='5.0'	; '11/08/07 	A.O.
;ver='5.1'	; '12/09/30 	A.O. change header
ver=6		; '12/11/20 	A.O. renewal
ver=7		; '14/10/23	T.A. & T.Y.	cadence, continuous mode 

RETURN,ver
END

;=====================================================
;NAME       : XEVA640_obs (main)
;=====================================================
common xeva640, wd, p, h, img, nx, ny

xeva640_init
xeva640_startfan, temp=260
xeva640_startcap, frms=400, expo=200000l, gain=3

h = mkheader()
p = mkdatapm()
nx=640 & ny=512

h.prog_ver = version()
date = yyyymmdd(/date)
;dir='E:\data\'+date+'\'
p.dir = 'C:\data\'+date+'\'
spawn, 'mkdir '+p.dir
p.fname = 'name'
img = intarr(nx,ny,100)

print,'>>  G O   A H E A D !  <<'

base 	= WIDGET_BASE(title='Infrared Observation: ver. '+strtrim(string(h.prog_ver),2), /column)
wd_obs 	= widget_obs(base, h=h);,nx=nx,ny=ny)
wd_data = widget_data(base, p=p)
wd_exit = widget_exit(base)
wd = {widgete,				$
		obs:	wd_obs,		$
		data:	wd_data,	$
		exit:	wd_exit		$
		}

widget_control, base, /realize
XMANAGER, 'xeva640obs', base

END

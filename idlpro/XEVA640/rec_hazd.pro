
;PRO rec_hazd
;
;	record time(yyyymmddhhmmss), hour angle(sec), and zenith distance(sec)
;       time cadence 1sec
;20110808  T.A.
;20110927  T.A.		save in each 1 minute.
;       2012.11.06     Addition of r,p,i,ga

;==========================headder============================;
;PRO rec_hazd_event, ev
;PRO rec_hazd_
;
;-

;=========================include=============================;
;=========================main================================;
;**************************************************************
pro rec_hazd_event, ev
;--------------------------------------------------------------
common widgetlib,wd

ev_stop=widget_event(wd.bt_en,/nowait)

widget_control, ev.id, get_uvalue=uvalue,get_value=value
print,'uvalue=',uvalue

if (uvalue eq "svdir") then begin
	wd.svdir=string(value)
	print,wd.svdir
endif

if (uvalue eq 'close') then close,1

if (uvalue eq 'start') then begin
key=0
while ev_stop.id eq 0 do begin
	if key eq 0 then begin
		caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
			yyyymmddhhmmss=string(year,form='(i4.4)')+string(mon,form='(i2.2)')+string(day,form='(i2.2)') $
			+string(hour,form='(i2.2)')+string(minu,form='(i2.2)')+string(seco,form='(i2.2)')
		filename=wd.svdir+yyyymmddhhmmss+'.txt'
		openw,lun,filename,/get_lun 
	endif
	caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
	yyyymmddhhmmss=string(year,form='(i4.4)')+string(mon,form='(i2.2)')+string(day,form='(i2.2)') $
	+string(hour,form='(i2.2)')+string(minu,form='(i2.2)')+string(seco,form='(i2.2)')
	;dstangl,hangl,zd
	;str=yyyymmddhhmmss+':'+string(hangl)+';'+string(zd)
	dstangl,hangl,zd,radius,pangl,incli,ga
	str=yyyymmddhhmmss+':'+string(hangl)+';'+string(zd)+';'+string(radius)+';'+string(pangl)+';'+string(incli)+';'+string(ga)
	printf,lun,str; & print,str
	widget_CONTROL,wd.cont,set_value=str
	wait,1. & key=key+1
	if key eq 59 then begin
		free_lun,lun & key=0
	endif
	ev_stop=widget_event(wd.bt_en,/nowait)
	ev_emp=widget_event(/nowait)
endwhile
free_lun,lun
endif

if (uvalue eq "EXIT") then WIDGET_CONTROL, /destroy, ev.top

end

;************************************************************************
pro rec_hazd
;--------------------------------------------------------------
common widgetlib,wd

caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
yyyymmddhhmmss=string(year,form='(i4.4)')+string(mon,form='(i2.2)')+string(day,form='(i2.2)') $
	+string(hour,form='(i2.2)')+string(minu,form='(i2.2)')+string(seco,form='(i2.2)')

wd={widget_param, $
	svdir:		'C:\data\'+strmid(yyyymmddhhmmss,0,8)+'\hazd\',	$; save directory
;	svdir:		'C:/anan/test/',	$; 
	bt_en:		0l,		$
	cont:		0l,		$
	Exit:		0l,		$
	n_evsample: 	0l 		$		; omake
	}
if (is_dir(wd.svdir) eq 0) then spawn,'mkdir '+wd.svdir

main = WIDGET_BASE(title='Record Ha, Zd, and time',/column)

bs_dir=widget_base(main, /row)
	lab=widget_label(bs_dir,value='Save Directory : ')
	text=widget_text(bs_dir,value=wd.svdir, xsize=30, uvalue='svdir',/edit)
bs_con=widget_base(main, /row)
	wd.cont=widget_text(bs_con,value='', xsize=40, ysize=1, uvalue='str',/edit)
bs_con1=widget_base(main, /row)
	bt_st=WIDGET_BUTTON(bs_con1,uvalue='close',value='close dstangle.dat ( IDL> close,1 )'$
	,/align_center,xsize=243)
bs_bt=widget_base(main, /row)
	bt_st=WIDGET_BUTTON(bs_bt,uvalue='start',value='Start',/align_center,xsize=123)
	wd.bt_en=WIDGET_BUTTON(bs_bt,uvalue='stop',value='Stop',/align_center,xsize=120)

wd.Exit = widget_button(main, value="Exit", uvalue = "EXIT")

widget_control, main, /realize
XMANAGER,'rec_hazd',main,modal=modal

END

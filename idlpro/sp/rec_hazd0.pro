
;PRO rec_hazd
;
;	record time(yyyymmddhhmmss), hour angle(sec), and zenith distance(sec)
;       time cadence 1sec
;20110808  T.A.
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

if (uvalue eq 'start') then begin
caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
yyyymmddhhmmss=string(year,form='(i4.4)')+string(mon,form='(i2.2)')+string(day,form='(i2.2)') $
	+string(hour,form='(i2.2)')+string(minu,form='(i2.2)')+string(seco,form='(i2.2)')
filename=wd.svdir+yyyymmddhhmmss+'.txt'
openw,lun,filename,/get_lun
while ev_stop.id eq 0 do begin
	caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
	yyyymmddhhmmss=string(year,form='(i4.4)')+string(mon,form='(i2.2)')+string(day,form='(i2.2)') $
	+string(hour,form='(i2.2)')+string(minu,form='(i2.2)')+string(seco,form='(i2.2)')
	dstangl,hangl,zd
	str=yyyymmddhhmmss+':'+string(hangl)+';'+string(zd)
	printf,lun,str; & print,str
	widget_CONTROL,wd.cont,set_value=str
	wait,1.
	close,1
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
	svdir:		'C:/data/dst/'+strmid(yyyymmddhhmmss,0,8)+'/',	$; save directory
	bt_en:		0l,		$
	cont:		0l,		$
	Exit:		0l,		$
	n_evsample: 	0l 		$		; omake
	}

main = WIDGET_BASE(title='Record Ha, Zd, and time',/column)

bs_dir=widget_base(main, /row)
	lab=widget_label(bs_dir,value='Save Directory : ')
	text=widget_text(bs_dir,value=wd.svdir, xsize=30, uvalue='svdir',/edit)
bs_con=widget_base(main, /row)
	wd.cont=widget_text(bs_con,value='', xsize=40, ysize=1, uvalue='str',/edit)
bs_bt=widget_base(main, /row)
	bt_st=WIDGET_BUTTON(bs_bt,uvalue='start',value='Start',/align_center,xsize=123)
	wd.bt_en=WIDGET_BUTTON(bs_bt,uvalue='stop',value='Stop',/align_center,xsize=120)

wd.Exit = widget_button(main, value="Exit", uvalue = "EXIT")

widget_control, main, /realize
XMANAGER,'rec_hazd',main,modal=modal

END

; sspm.pro
@hr2000lib

;** MODIFICATION HISTORY **
function version
ver='0.1'	; 2016/08/07	k.i.   from hr2000.pro
ver='1.0'	; 2017/04/01	k.i.   ObsStart etc.
ver='1.1'	; 2017/04/04	k.i.   QLmap

return,ver
end


;**************************************************************************
pro spobs_event, ev1
common spobs_com, wd, p, sp, wrange, dev_exist


;--------------- Obs control -----------------------------
case (ev1.id) of
    wd.Prev: begin
	print,'Preview Start'
	ev_stop=widget_event(wd.Stop,/nowait)
	while ev_stop.id eq 0 do begin
		ev_stop=widget_event(wd.Stop,/nowait)
		sp1=hr_getsp1()
		;sp=hr_getsps(p.integ)
		plot,p.wl,sp1,yrange=[0,6.4e4],xrange=wrange,xstyle=1,xtitle='wavelength [nm]'	;;;fixed yrange
	endwhile
	end
    wd.ObsStart: begin
	print,'ObsStart'
	ev_stop=widget_event(wd.Stop,/nowait)
	sp=dblarr(n_elements(p.wl))
	while ev_stop.id eq 0 do begin
		ev_stop=widget_event(wd.Stop,/nowait)
		p.date_obs=get_systime(ctime=ctime)
		sp[*]=0.d
		for i=0,p.integ-1 do sp=sp+hr_getsp1()
		sp=sp/p.integ
		p.date_obs2=get_systime()
		plot,p.wl,sp,yrange=[0,6.4e4],xrange=wrange,xstyle=1,xtitle='wavelength [nm]'	;;;fixed yrange
		p.outfile=p.fnam+ctime+'.sav'
		widget_control,wd.Outfile,set_value=p.outfile
		save,p,sp,file=p.outdir+p.outfile
	endwhile
	end
    wd.Stop: begin
	end
    wd.Get1: begin
	print,'Get1'
	sp1=hr_getsp1()
	sp=dblarr(n_elements(sp1))
	p.date_obs=get_systime(ctime=ctime)
	for i=0,p.integ-1 do begin
		sp1=hr_getsp1()
		sp=sp+sp1
	endfor
	p.date_obs2=get_systime()
	sp=sp/p.integ
	plot,p.wl,sp,yrange=[0,6.4e4],xrange=wrange,xstyle=1,xtitle='wavelength [nm]'	;;;fixed yrange
	p.outfile=p.fnam+ctime+'.sav'
	widget_control,wd.Outfile,set_value=p.outfile
	end
    wd.Gets: begin
	print,'Gets'
	p.date_obs=get_systime(ctime=ctime)
	sp=hr_getsps(p.integ)
	p.date_obs2=get_systime()
	plot,p.wl,sp[*,0],yrange=[0,6.4e4],xrange=wrange,xstyle=1,xtitle='wavelength [nm]'	;;;fixed yrange
	p.outfile=p.fnam+ctime+'.sav'
	widget_control,wd.Outfile,set_value=p.outfile
	end
    wd.Expo: begin
	widget_control, ev1.id, get_value=value
	p.expo=fix(value)
	print,'expo=',p.expo
	hr_setexpo,p.expo
	end
    wd.Integ: begin
	widget_control, ev1.id, get_value=value
	p.integ=fix(value)
	print,'integ=',p.integ
	end
    wd.Outdir: begin
	widget_control, ev1.id, get_value=value
	p.outdir=value
	print,'outdir=',p.outdir
	end
    wd.Outfile: begin
	widget_control, ev1.id, get_value=value
	p.outfile=value
	print,'outfile=',p.outfile
	end
    wd.FNAM: begin
	widget_control, ev1.id, get_value=value
	p.fnam=value
	print,'fnam=',p.fnam
    end
    wd.Save: begin
	wl=p.wl
	save,p,sp,file=p.outdir+p.outfile
	print,'p & sp saved in '+p.outdir+p.outfile
	end
    wd.QLmap: begin
	map=sspm_map(p.outdir)
	ans=''
	qlfile='c:\data\sspm\sspm'+strmid(p.date_obs,0,10)+'.jpeg'
	ans=wdyesno('Do you save this map to '+qlfile+'?', x=500,y=400)
	if ans eq 'y' then win2jpeg,qlfile
	end
    wd.Exit: begin
	if dev_exist then hr_close
	WIDGET_CONTROL, /destroy, ev1.top
	return
	end
    else: 
endcase



end


;**************************************************************************
;  main
common spobs_com, wd, p, sp, wrange, dev_exist

expo=1
integ=400

dev_exist=0
if dev_exist then begin
	hr_init,spname
	wl=hr_getwl()
	hr_setexpo,expo
endif else begin
	wl=findgen(3648)	; 
	spname='dymmy'
endelse
wrange=[300.,(fix(max(wl))/100)*100]
dmy=get_systime(ctime=ctime)
date=strmid(ctime,0,8)


wd = {wd_spobs, $
	Get1:		0l, $
	Gets:		0l, $
	Integ:		0l, $
	Expo:		0l, $
	Prev:		0l, $
	ObsStart:	0l, $
	Stop:		0l, $
	Outdir:		0l, $
	Outfile:	0l, $
	Fnam:		0l, $
	Save:		0l, $
	QLmap:		0l, $
	Exit:		0l $
	}
p = {spobs_prm, $
	spectrograph:	spname, $	; 'MayaPro2000',,
	expo:		expo, $		; exsposure, msec
	integ:		integ, $	; integ.#
	wl:		wl, $		; wavelength array [2048]
	date_obs:  	'', $
	date_obs2:  	'', $
	outdir:  	'd:\data\sspm\'+date+'\', $
	fnam:		'sp', $
	outfile:	'a.sav' $
	}

make_dir,p.outdir
window,0,xs=600,ys=400

;-------  create widgets and get ID structure  ---------------------------
base = WIDGET_BASE(title=spname+' spectrometer (Ver.'+version()+')', /column) 
b1=widget_base(base, /row, /frame )
   	wd.Prev = widget_button(b1, value="Prev", uvalue = "Prev")
   	wd.ObsStart = widget_button(b1, value="ObsStart", uvalue = "ObsStart")
   	wd.Stop = widget_button(b1, value="Stop", uvalue = "Stop")
   	wd.Exit = widget_button(b1, value="Exit", uvalue = "Exit")
b1a=widget_base(base, /row, /frame )
   	wd.Get1 = widget_button(b1a, value="Get1", uvalue = "Get1")
   	wd.Gets = widget_button(b1a, value="Gets", uvalue = "Gets")
   	wd.Save = widget_button(b1a, value="Save", uvalue = "Save")
   	wd.QLmap = widget_button(b1a, value="QLmap", uvalue = "QLmap")
b1b=widget_base(base, /row, /frame )
	wd_label = widget_label(b1b,value='Expo:')
	cexpo=string(p.expo,form='(i5)')
	wd.Expo = widget_text(b1b,value=cexpo,xsize=6, uvalue='Expo', /edit)
	wd_label = widget_label(b1b,value='msec,  Integ')
	cinteg=string(p.integ,form='(i5)')
	wd.Integ = widget_text(b1b,value=cinteg,xsize=6, uvalue='Integ', /edit)
b2=widget_base(base, /column, /frame )
  	b21=widget_base(b2, /row )
	wd_label = widget_label(b21,value='Outdir:')
	wd.Outdir = widget_text(b21,value=p.outdir,uvalue='Outdir', xsize=25, /edit)
  	b22=widget_base(b2, /row )
	wd_label = widget_label(b22,value='Outfile:')
	wd.Outfile = widget_text(b22,value=p.outfile,uvalue='Outfile', xsize=25, /edit)
  	b23=widget_base(b2, /row )
	wd_label = widget_label(b23,value='fnam:')
	wd.Fnam = widget_text(b23,value=p.fnam,uvalue='FNAM', xsize=10, /edit)
;b3=widget_base(base, /row )

widget_control, base, /realize
XMANAGER, 'spobs', base


end

; mmsp.pro
;  Mueller Matrix SpectroPolarimeter
@emplib
@hr2000lib
@ta_mmsplib

;** MODIFICATION HISTORY **
function version
ver='0.1'	; '2010/08/10	k.i. 
ver='0.2'	; '2010/08/10	k.i., t.a.,  motor origin 
ver='0.3'	; '2010/10/19	k.i., t.a.,  Measure 
ver='0.4'	; '2011/03/07	k.i., t.a.,  decomp. 
ver='0.5'	; '2012/03/31	k.i., x-ax 
return,ver
end


;**************************************************************************
pro mmsp_event, ev1
common mmsp_com, wd, p, wl, sp1, drk, dat, clr, outfile0

;yrange=[0,16384.]
yrange=[0,65536.] ;;;fixed yrange
iobit=1

;--------------- Obs control -----------------------------
case (ev1.id) of
    wd.Start: begin
	print,'Start'
	window,0
	ev_stop=widget_event(wd.Stop,/nowait)
	while ev_stop.id eq 0 do begin
		ev_stop=widget_event(wd.Stop,/nowait)
		sp1=hr_getsp1()
		plot,wl,sp1,yrange=yrange,ystyle=1,xtitle='wl [nm]',xrange=[200,1100],xstyle=1
		ev_mstart=widget_event(wd.mStart,/nowait)
		if ev_mstart.id ne 0 then begin
			empset,1,vm=p.m.m1.vm &	wait,0.05 &	empstart,1,/CW &	wait,0.1
			empset,2,vm=p.m.m2.vm & wait,0.05 &	empstart,2,/CW &
		end
		ev_mstop=widget_event(wd.mStop,/nowait)
		if ev_mstop.id ne 0 then begin
			empstop &	wait,0.05 
			empstop,2
		end
	endwhile
	plot,wl,sp1,yrange=yrange,ystyle=1,xtitle='wl [nm]',xrange=[200,1100],xstyle=1
	end
    wd.Stop: begin
	end
    wd.Get1: begin
	print,'Get1'
;	sp=dblarr(2048)
	sp=dblarr(2068)
	for i=0,p.integ-1 do begin
		sp1=hr_getsp1()
		sp=sp+sp1
	endfor
	sp1=sp
	plot,wl,sp1,title=string(total(sp1)/n_elements(sp1)/100.,form='(i5)'),xrange=[200,1100],xstyle=1,xtitle='wl [nm]'
	end
    wd.ShOpn: hr_gpio,iobit,0	; bit-iobit, out-1
    wd.ShCls: hr_gpio,iobit,1	; bit-iobit, out-0
    wd.Dark: begin
	print,'Get Dark'
	hr_gpio,iobit,1	; bit-iobit, out-0
;	sp=dblarr(2048)
	sp=dblarr(2068); 2012.04.04
	for i=0,p.integ-1 do begin
		sp1=hr_getsp1()
		sp=sp+sp1
	endfor
	drk=sp
	hr_gpio,iobit,0	; bit-iobit, out-1
	plot,wl,drk
	end
    wd.Mesure: begin
	print,'Measure'
	window,0
	;-- dark --
	print,'Get Dark'
	hr_gpio,iobit,1	; bit-iobit, out-0
;	sp=dblarr(2048)
	sp=dblarr(2068); 2012.04.04
	for i=0,p.integ-1 do begin
		sp1=hr_getsp1()
		sp=sp+sp1
	endfor
	drk=sp
	hr_gpio,iobit,0	; bit-iobit, out-1
	plot,wl,drk,xtitle='wl [nm]',xrange=[200,1100],xstyle=1
	;-- origin --
	print,'Origin'
	empset,1,vm=3000l & wait,0.05
	emporig,1 &	wait,0.05 
	empset,2,vm=3000l & wait,0.05
	emporig,2 &	wait,0.05 
;	empstatus,busy=busy,nodisp=nodisp
;	while busy eq 1 do empstatus,busy=busy,nodisp=nodisp
	;--
	ans='' &	read,ans
	dmy=yymmdd(get_time=time) & p.time=strmid(time,0,2)+strmid(time,3,2)+strmid(time,6,2)
;	dat=dblarr(2048,p.nth)
	dat=dblarr(2068,p.nth) ;2012.04.04
	for j=0,p.nth-1 do begin
		pn1=j*p.m.m1.revpn/360.*p.dth1
		pn2=pn1/5
		;emppos2,pn1,pn2,/gwait
		emppos,2,pn2 &	wait,0.01
		emppos,1,pn1,/gwait
		;pn1=p.m.m1.revpn/360.*p.dth1
		;empmove,1,pn1,/gwait
		;empmove,2,pn1/5,/gwait
;		sp=dblarr(2048)
		sp=dblarr(2068) ;2012.04.04
		for i=0,p.integ-1 do begin
			sp1=hr_getsp1()
			sp=sp+sp1
		endfor
		dat[*,j]=sp-drk
		plot,wl,sp1,yrange=yrange,ystyle=1,title=string(j,form='(i3)'), $
			xtitle='wl [nm]',xrange=[200,1100],xstyle=1
	endfor
	p.outfile='m'+p.date+'_'+p.time+'.sav'
	widget_control, wd.Outfile, set_value=p.outfile
	outfile0=p.outfile
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
    wd.mStart: begin
	empset,1,vm=p.m.m1.vm &	wait,0.05 &	empstart,1,/CW &	wait,0.1
	empset,2,vm=p.m.m2.vm & wait,0.05 &	empstart,2,/CW &
	end
    wd.mStop: begin
	empstop &	wait,0.05 
	empstop,2
	end
    wd.mOrigin: begin
	empset,1,vm=3000l & wait,0.05
	emporig,1 &	wait,0.05 
	empset,2,vm=3000l & wait,0.05
	emporig,2
	end
    wd.Dth1: begin
	widget_control, ev1.id, get_value=value
	p.integ=float(value)
	print,'dth1=',p.dth1
	end
    wd.Nth: begin
	widget_control, ev1.id, get_value=value
	p.integ=fix(value)
	print,'nth=',p.nth
	end
    wd.Optics: begin
	widget_control, ev1.id, get_value=value
	p.optics=value
	p.outfile=string(p.optics)+'_T'+string(p.temp,format='(i2.2)')+'_I'+$
			string(p.incang)+'_'+string(p.seqn,format='(i2.2)')+'.sav'
	widget_CONTROL,wd.outfile,set_value=p.outfile
	end
    wd.Temp: begin
	widget_control, ev1.id, get_value=value
	p.temp=value
	p.outfile=string(p.optics)+'_T'+string(p.temp,format='(i2.2)')+'_I'+$
			string(p.incang)+'_'+string(p.seqn,format='(i2.2)')+'.sav'
	widget_CONTROL,wd.outfile,set_value=p.outfile
	end
    wd.Incang: begin
	widget_control, ev1.id, get_value=value
	p.incang=value
	p.outfile=string(p.optics)+'_T'+string(p.temp,format='(i2.2)')+'_I'+$
			string(p.incang)+'_'+string(p.seqn,format='(i2.2)')+'.sav'
	widget_CONTROL,wd.outfile,set_value=p.outfile
	end
    wd.Seqn: begin
	widget_control, ev1.id, get_value=value
	p.seqn=value
	p.outfile=string(p.optics)+'_T'+string(p.temp,format='(i2.2)')+'_I'+$
			string(p.incang)+'_'+string(p.seqn,format='(i2.2)')+'.sav'
	widget_CONTROL,wd.outfile,set_value=p.outfile
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
    wd.Clear: begin
	clr=dat
	p.outfile='c'+p.date+'_'+p.time+'.sav'
	widget_control, wd.Outfile, set_value=p.outfile
	end
    wd.Reset: begin
	p.outfile=outfile0
	widget_CONTROL,wd.outfile,set_value=p.outfile
	end
    wd.Save: begin
	;save,p,sp1,file=p.outdir+p.outfile
	save,p,wl,dat,file=p.outdir+p.outfile
	print,'save '+p.outdir+p.outfile
	end
    wd.Anarysis: begin
	res=ta_mmsp_cal(dat=dat,clr,p,wl,dMM=dMM)
	end
    wd.Savefig: begin
	wset,0
	write_png,p.outdir+strmid(p.outfile,0,strpos(p.outfile,'.sav'))+'_MM.png',tvrd()
	wset,3
	write_png,p.outdir+strmid(p.outfile,0,strpos(p.outfile,'.sav'))+'_trdd.png',tvrd()
	end
    wd.Exit: begin
	WIDGET_CONTROL, /destroy, ev1.top
	return
	end
    else: 
endcase



end


;**************************************************************************
;  main
common mmsp_com, wd, p, wl, sp1, drk, dat, clr, outfile0

hr_init

outfile0='a.sav'	;anan
expo=1
integ=100
dth1=22.5
nth=360./dth1*5
device,decom=0

wl=hr_getwl()	; wavelength array [2048]
hr_setexpo,expo
pm=emp_ctl()
pm.m1.name='M1: P-Analyzer'
pm.m2.name='M2: P-Generator'
;pm.m1.vm = 54000l
pm.m1.vm = 54000l/5
pm.m2.vm = pm.m1.vm/5
pm.dev_exist=1
; WP motor -- 18000pulse/rot (0.02deg/pulse)


wd = {wd_mmsp, $
	Get1:		0l, $
	Integ:		0l, $
	Expo:		0l, $
	Start:		0l, $
	Stop:		0l, $
	Dark:		0l, $
	Mesure:		0l, $
	ShOpn:		0l, $
	ShCls:		0l, $
	mStart:		0l, $
	mStop:		0l, $
	mOrigin:	0l, $
	Dth1:		0l, $
	Nth:		0l, $
	Optics:		0l, $
	Temp:		0l, $
	Incang:		0l, $
	Seqn:		0l, $
	Outdir:		0l, $
	Outfile:	0l, $
	Clear:		0l, $
	Reset:		0l, $
	Save:		0l, $
	Anarysis:	0l, $
	Savefig:	0l, $
	Exit:		0l $
	}

date='20'+yymmdd(get_time=time)
time=strmid(time,0,2)+strmid(time,3,2)+strmid(time,6,2)
p = {mmsp_prm, $
	m:		pm, $ ; 
	expo:		expo, $	; exsposure, msec
	integ:		integ, $	; exsposure, msec
	dth1:		dth1, $	; angle step of WP
	nth:		nth, $	; # of sampling position
	wbin:		1, $	; wavelength binning
	date:		date, $
	time:		time,$	; hhmmss
	optics:  	'', $
	temp:	  	'20', $
	incang:  	'p00', $
	seqn: 	 	'00', $
	outdir:  	'c:\data\MMSP\'+date+'\', $
	outfile:	'a.sav' $
	}

empinit,'COM8';,p=pm




;-------  create widgets and get ID structure  ---------------------------
base = WIDGET_BASE(title='MMSP (Ver.'+version()+')', /column) 
b1=widget_base(base, /column, /frame )
b10=widget_base(b1, /row )
	wd_label = widget_label(b10,value='HR2000:')
   	wd.Start = widget_button(b10, value="Start", uvalue = "Start")
   	wd.Stop = widget_button(b10, value="Stop", uvalue = "Stop")
	wd.ShOpn = widget_button(b10, value="ShOpn", uvalue = "ShOpn")
	wd.ShCls = widget_button(b10, value="ShCls", uvalue = "ShCls")
b1a=widget_base(b1, /row )
   	wd.Get1 = widget_button(b1a, value="Get1", uvalue = "Get1")
   	wd.Dark = widget_button(b1a, value="Dark", uvalue = "Dark")
   	wd.Mesure = widget_button(b1a, value="Mesure", uvalue = "Mesure")
b1b=widget_base(b1, /row )
	wd_label = widget_label(b1b,value='Expo:')
	cexpo=string(p.expo,form='(i5)')
	wd.Expo = widget_text(b1b,value=cexpo,xsize=6, uvalue='Expo', /edit)
	wd_label = widget_label(b1b,value='msec,  Integ')
	cinteg=string(p.integ,form='(i5)')
	wd.Integ = widget_text(b1b,value=cinteg,xsize=6, uvalue='Integ', /edit)
b1c=widget_base(b1, /row )
	wd_label = widget_label(b1c,value='dth1:')
	cdth1=string(p.dth1,form='(f7.1)')
	wd.Dth1 = widget_text(b1c,value=cdth1,xsize=6, uvalue='Dth1', /edit)
	wd_label = widget_label(b1c,value='deg,  # pos:')
	cnth=string(p.nth,form='(i5)')
	wd.Nth = widget_text(b1c,value=cnth,xsize=6, uvalue='Nth', /edit)
b2=widget_base(base, /column, /frame )
b2a=widget_base(base, /row, /frame )
	wd_label = widget_label(b2a,value='Motor:')
   	wd.mStart = widget_button(b2a, value="Start", uvalue = "mStart")
   	wd.mStop = widget_button(b2a, value="Stop", uvalue = "mStop")
   	wd.mOrigin = widget_button(b2a, value="Origin", uvalue = "mOrigin")
b3=widget_base(base, /column, /frame )
b3y=widget_base(b3, /row )
	wd_label = widget_label(b3y,value='Optics:')
	wd.Optics= widget_text(b3y,value=p.optics,uvalue='Optics',xsize=5,/edit)
	wd_label = widget_label(b3y,value='  Temp:')
	wd.Temp  = widget_text(b3y,value=p.temp,uvalue='Temp',xsize=5,/edit)
	wd_label = widget_label(b3y,value='��')
b3z=widget_base(b3, /row )
	wd_label = widget_label(b3z,value='Inc.Ang.:')
	wd.Incang= widget_text(b3z,value=p.incang,uvalue='Incang',xsize=5,/edit)
	wd_label = widget_label(b3z,value='deg  Seqn#:')
	wd.Seqn	 = widget_text(b3z,value=p.seqn,uvalue='Seqn',xsize=5,/edit)
b3a=widget_base(b3, /row )
	wd_label = widget_label(b3a,value='Outdir:')
	wd.Outdir = widget_text(b3a,value=p.outdir,uvalue='Outdir',xsize=25,/edit)
b3b=widget_base(b3, /row )
	wd_label = widget_label(b3b,value='Outfile:')
	wd.Outfile = widget_text(b3b,value=p.outfile,uvalue='Outfile',xsize=25, /edit)
b3c=widget_base(b3, /row )
   	wd.Clear = widget_button(b3c, value="Set Clear", uvalue = "Clear")
   	wd.Reset = widget_button(b3c, value="Reset", uvalue = "Reset")
   	wd.Save = widget_button(b3c, value="Save", uvalue = "Save")
b3d=widget_base(b3, /row )
   	wd.Anarysis = widget_button(b3d, value="Analysis", uvalue = "Analysis")
   	wd.Savefig  = widget_button(b3d, value="Save Fig.", uvalue = "Savefig")
   	wd.Exit = widget_button(b3d, value="Exit", uvalue = "Exit")

widget_control, base, /realize
XMANAGER, 'mmsp', base

empclose
hr_close

end

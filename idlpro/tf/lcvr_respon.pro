; aiousb.pro
@caiolib2
@mllclib			; Meadowlark  driver

;  2009.5.4   copy from nkrprj\idlpro\hardware\caio.pro
;  2012.5.12   copy from adi12_8.pro
;  2012.5.13   k.i., M.H., TNE On/Off


;**************************************************************
function widget_lc_caio,base,p	; display widget status
;--------------------------------------------------------------

wd_caio={wd_caio,	$
	Status:		0l,	$
	Count:		0l,	$
	Ndat:		0l,	$
	Memsld:		0l,	$
	AdStart:	0l,	$
	AdStop:		0l,	$
	RecStart:	0l,	$
	RecStop:	0l,	$
	Rec1:		0l,	$
	NRec1:		0l,	$
	Getstat:	0l,	$
	DO1:		0l,	$
	Rate:		0l,	$
	Trange:		0l,	$
	Vrange:		0l,	$
	Nch:		0l,	$
	DataDir:	0l,	$
	Save:		0l,	$
	TNE:		0l,	$
	Exit:		0l	$
	}

st=caio_status(BUSY=busy,START_TRG=START_TRG,DATA_NUM=data_num,OFERR=oferr)
count=caio_count()
sldmax=p.c.maxn/p.c.nch

lab = widget_label(base,value='>>> CAIO: '+p.c.card+'   <<<');,font=2)

	base1=widget_base(base, /column, /frame)
	b_1=widget_base(base1, /row)
	lab = widget_label(b_1,value='Status:')
	;wd_caio.Status = widget_text(b_1,value=string(st,form='(z8.8)')+'H',xsize=9)
	wd_caio.Status = widget_label(b_1,value=string(st,form='(z8.8)')+'H',xsize=70)
	lab = widget_label(b_1,value='Mamory:')
	;wd_caio.Count = widget_text(b_2,value=string(count,form='(i6.6)'),xsize=6)
	wd_caio.Count = widget_label(b_1,value=string(count,form='(i6.6)'),xsize=50)
;	wd_caio.Memsld  = widget_slider(b_2, value=0, uvalue='Csld', $
;		minimum=0, maximum=sldmax, xsize=150,suppress=1, vertical=0, frame=40)
	lab = widget_label(b_1,value=' data#:')
	;wd_caio.Ndat = widget_text(b_1,value=string(0,form='(i6.6)'),xsize=6)
	wd_caio.Ndat = widget_label(b_1,value=string(0,form='(i6.6)'),xsize=40)

	b_2=widget_base(base1, /row)
	b_3=widget_base(base1, /row)
	lab = widget_label(b_3,value='AD:')
	wd_caio.AdStart = widget_button(b_3, value="Start", uvalue = "AdStart")
	wd_caio.AdStop = widget_button(b_3, value="Stop", uvalue = "AdStop")
	lab = widget_label(b_3,value='  Rec:')
	wd_caio.RecStart = widget_button(b_3, value="Start", uvalue = "RecStart")
	wd_caio.RecStop = widget_button(b_3, value="Stop", uvalue = "RecStop")
	lab = widget_label(b_3,value=' ')
	wd_caio.Rec1 = widget_button(b_3, value="Rec1", uvalue = "Rec1")

	b_31=widget_base(base1, /row, ysize=25)
	lab = widget_label(b_31,value='DO1:')
	wd_caio.DO1 = cw_bgroup(b_31,['0','1'],/row, uvalue="DO1", $
		/no_release,set_value=0,/exclusive,/frame)
	wd_caio.GetStat = widget_button(b_31, value="Status", uvalue = "GetStat")
	wd_caio.Save = widget_button(b_31, value="Save", uvalue = "Save")

	b_4=widget_base(base1, /row)
	lab = widget_label(b_4,value='Rate:')
	wd_caio.Rate=widget_text(b_4,value=string(p.rate, form='(i5)'), xsize=6, uvalue='Rate',/edit)
	lab = widget_label(b_4,value='Hz,  Nch:')
	wd_caio.Nch=widget_text(b_4,value=string(p.nch, form='(i2)'), xsize=3, uvalue='Nch',/edit)
	lab = widget_label(b_4,value=',  NRec:')
	wd_caio.NRec1=widget_text(b_4,value=string(p.n_rec1, form='(i8)'), xsize=8, uvalue='NRec1',/edit)
	lab = widget_label(b_4,value='')
	b_5=widget_base(base1, /row)
	lab = widget_label(b_5,value='tmax:')
	wd_caio.Trange=widget_text(b_5,value=string(p.trange(1), form='(i3)'), xsize=3, uvalue='Tragge',/edit)
	lab = widget_label(b_5,value='s')
	lab = widget_label(b_5,value=',  Vmax:')
	wd_caio.Vrange=widget_text(b_5,value=string(p.yrange(1), form='(i3)'), xsize=3, uvalue='Tragge',/edit)
	lab = widget_label(b_5,value='V')
	lab = widget_label(b_5,value='TNE:')
	wd_caio.TNE = cw_bgroup(b_5,['Off','On'],/row, uvalue="TNE", $
		/no_release,set_value=0,/exclusive,/frame)


;	wd_caio.Exit = widget_button(b_4, value="Exit", uvalue = "Exit")
;	b_5=widget_base(base1, /row)
;	wd_caio.DatDir=widget_button(b_5, value="Data dir:", uvalue = "DatDir")
;	lab = widget_label(b_5,value=p.datadir,font=2)

	return,wd_caio

end

;**************************************************************
function lc_caio_event, ev, wd, p, dat=dat, $
	wd_stop=wd_stop2, wd_rec2=wd_rec2, n_rec=n_rec
;--------------------------------------------------------------
;  wd  -- widget
;  p   -- control parameters
;  dat -- read data
;  wd_stop2 -- wd-id for other stop
;  wd_rec2 -- wd-id for other rec
;  n_rec -- # of data for rec2
;  return new p

lcv=[0,2,8,2,0]	; V

widget_control, ev.id, get_uvalue=value

case (ev.id) of
  wd.AdStart: begin ;----------------------------------- monitoring -----------
	p=caio_pSet(p)
	ev_adstop=widget_event(wd.AdStop,/nowait)
	ev_recstart=widget_event(wd.RecStart,/nowait)
	ev_recstop=widget_event(wd.RecStop,/nowait)
	ev_rec1=widget_event(wd.Rec1,/nowait)
	if ev_recstart.id ne 0 then p.rec=2
	t=findgen((p.trange(1)-p.trange(0))*p.rate)/p.rate
	nch=p.c.nch &	dn1=p.c.n_evsample
	plot,t,t,xtitle='time [sec]',yrange=p.yrange,ytitle='V',/nodata,ystyle=1
	is=0l	; if 0, initial plot
	ndat=0l	; # of recorded data in dat(*,*)
	caio_start
	while ev_adstop.id eq 0 do begin
		caio_wd_status,wd,st=st,count=count,ndat=ndat,DATA_NUM=dnev
		if p.rec eq 0 then begin
			ev_recstart=widget_event(wd.RecStart,/nowait)
			ev_rec1=widget_event(wd.Rec1,/nowait)
			ev_nrec1=widget_event(wd.NRec1,/nowait)
		endif else ev_recstop=widget_event(wd.RecStop,/nowait)
		if ev_nrec1.id ne 0 then begin
			p.n_rec1=long(gt_wdtxt(wd.NRec1))
			print,p.n_rec1
		endif
		if ev_recstart.id ne 0 then begin
			p.rec=2
			ndat=0l
			ev_recstart.id=0
			print,'REC start...'
		endif
		if ev_rec1.id ne 0 then begin
			p.rec=1
			ndat=0l
			ev_rec1.id=0
			lcvoltm,[lcv[0],lcv[0],0,0]
			print,'REC1 start...'
		endif
		if ev_recstop.id ne 0 then begin
			p.rec=0
			ev_recstop.id=0
			print,'REC stop...'
		endif
		if dnev eq 1 then begin
			dat1=caio_read(nch,dn1,ret=ret)	; <--- read data
			if ret ne 0 then print,'Caio_read: ',ret
			if p.rec ne 0 then begin
				if ndat eq 0 then dat=dat1 else dat=[[dat],[dat1]]
				ndat=ndat+dn1
				if ndat eq 20000l then lcvoltm,[lcv[1],lcv[1],0,0]
				if ndat eq 40000l then lcvoltm,[lcv[2],lcv[2],0,0]
				if ndat eq 60000l then lcvoltm,[lcv[3],lcv[3],0,0]
				if ndat eq 80000l then lcvoltm,[lcv[4],lcv[4],0,0]
				if p.rec eq 1 and ndat ge p.n_rec1 then begin
					p.rec=0
					print,'REC1 finish..'
					help,dat
				endif
			endif
			im=is*dn1
			if im+dn1 gt n_elements(t) then begin
				is=0l & im=0l
				plot,t,t,xtitle='time [sec]',yrange=p.yrange,ytitle='V',/nodata,ystyle=1
			endif
			if im gt 0 then begin
				t1=t(im-1:im+dn1-1)
				datp=[[dat_1],[dat1]]
			endif else begin
				t1=t(im:im+dn1-1)
				datp=dat1
			endelse
			for j=0,nch-1 do oplot,t1,datp(j,*),color=254-j
			is=is+1
			dat_1=dat1(*,dn1-1)
		endif
		ev_adstop=widget_event(wd.AdStop,/nowait)
		;if keyword_set(wd_stop2) then ev_stop2=widget_event(wd_stop2,/nowait)
		;----- branch
		ev_emp=widget_event(/nowait)
		;if ev_emp.id ne 0 then print,ev_emp.id
		;-----
	endwhile
	caio_stop
	p.rec=0
	help,dat
	caio_wd_status,wd,st=st,count=count,ndat=ndat,DATA_NUM=dnev
	end
  wd.GetStat: begin
	caio_wd_status,wd,st=st,count=count,DATA_NUM=dnev
	p.c=caio_getinfo()
	help,p.c,/st
	end
  wd.AdStop: begin
	caio_stop
	end
  wd.RecStart: begin
	end
  wd.RecStop: begin
	end
  wd.Rec1: begin
	end
  wd.NRec1: begin
	end
  wd.DO1: begin
        case ev.value of
            0: begin
                caio_dout,0,0
                p.do1=0
                end
            1: begin
                caio_dout,0,1
                p.do1=1
               end
        endcase
	end
  wd.TNE: begin
        case ev.value of
            0: begin	; TNE - Off
                lccom,'tne:1,0,65535'
                lccom,'tne:2,0,65535'
                lccom,'tne:3,0,65535'
                lccom,'tne:4,0,65535'
		print,'TNE Off'
                end
            1: begin	; TNE - On
                lccom,'tne:1,10,65535'
                lccom,'tne:2,10,65535'
                lccom,'tne:3,10,65535'
                lccom,'tne:4,10,65535'
		print,'TNE On'
               end
        endcase
	end
  wd.Rate: begin
	p.rate=fix(gt_wdtxt(ev.id))
	end
  wd.Trange: begin	; time range for monitor
	p.trange(1)=float(gt_wdtxt(ev.id))
	print,p.trange(1)
	end
  wd.Vrange: begin	; V range for monitor
	p.yrange(1)=float(gt_wdtxt(ev.id))
	end
  wd.Nch: begin
	p.nch=float(gt_wdtxt(ev.id))
	end
  wd.Save: begin
	file=dialog_pickfile(path=p.datadir,filter='*.isv',title='CAIO data',get_path=path)
	p.datadir=path
	save,p,dat,file=file
	print,'p, dat saved in '+file
	end
  wd.Exit: begin
	WIDGET_CONTROL, /destroy, ev.top
	;caio_exit
	return,p
	end
  else:
endcase

return,p

end


;**************************************************************
pro caio_main_event, ev
;--------------------------------------------------------------
common caio, wd, p, dat

widget_control, ev.id, get_uvalue=value
p=lc_caio_event(ev,wd,p,dat=dat)
end

;************************************************************************
common caio, wd, p, dat
; wd	- widget structure
; p	- control parameters

p=caio_ctl()
caio_init,p=p,st=st
help,p.c,/st
p.yrange=[-10,10]
p.trange=[0,5]
p.nch=2
p.rate=20000l
p.n_rec1=100000l
p.datadir='C:\data\TF\20120513_lcres\'
lcdrv_open,'COM7'		; Meadowlark LC control


window,0,xsize=600,ysize=400
@set_color
device,decom=0
t=findgen((p.trange(1)-p.trange(0))*p.rate)/p.rate
plot,t,t,xtitle='time [sec]',yrange=p.yrange,ytitle='V',/nodata,ystyle=1


base = WIDGET_BASE(title='CAIO:  ', /column)
wd = widget_lc_caio(base,p)
wd.Exit=widget_button(base, value="Exit", uvalue = "Exit")
widget_control, base, /realize
XMANAGER, 'caio_main', base
lcdrv_close


end


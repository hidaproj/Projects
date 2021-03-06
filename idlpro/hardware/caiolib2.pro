; caiolib2.pro
;  handle Contec ADI12-8 (USB)
;   2009.5.4  k.i.   copy from nkrprj/idlpro/hardware/caiolib.pro
;   2009.10.4  T.A.   caio_trig
;   2011.3.6  K.I.   from ADI12-8lib
;   2012.4.10  K.I.   bug
;   2014.5.19  K.I.   misc.
;   2014.5.19  K.I.   misc.
;   2015.7.26  K.I.   bug fix

;**************************************************************
function caio_cardst	; return CAIO current setting structurel

c={caio_card, $
	DevName:	'', $		; device name, ex. 'AIO000'
	card:		'', $		; Crad name, ex. 'AD12-8(PM)'
	maxch:		0, $		; max ch number
	nbit:		0, $		; A/D bit
	maxn:		0l, $		; max storage data, mamory capacity
	range:		[0,0], $	; V range
	ClockType:	0, $		; 0: Int, 1: Ext
	clock: 		0., $		; sampling clock speed [micro sec]
	nch:		0, $		; # of AD channels
	memory:		'', $		; memory type, 'FIFO' or 'Ring'
	start_trig: 	0, $		; 0: software
	stop_trig: 	0, $		; 0: 設定回数変換終了, 4: AioStopAi
	n_sample: 	0l, $		; # of sampling given to CAIO
	n_evsample: 	0l $		; # of event sampling
	}
return,c
end

;**************************************************************
function caio_ctl	; return CAIO control parameter structure

c=caio_cardst()

p={caio_ctl, $	; CAIO control params.
	dev_exist:	1, $	; device exist(1) not not(0)
	rate:		200, $	; sampling rate [Hz]
	nch:		3, $	; # of sampling channels
	dt_read1: 	0.05, $	; time of every reading for continuous sampling, [sec]
	n_sample: 	100, $	; # of data for one read
	n_rec1: 	10800l, $	; # of data for one record
	trange:		[0.,1.], $	; time interval for monitor [sec]
	yrange:		[0,10.], $	; data display range
	datadir:	'', $	; output data dir
	rec:		0, $	; Rec(1) or through (0)
	do1:		0, $	; Digital IO ch1
	c:		c $
	}
return,p
end

;**************************************************************
function caio_getinfo
;--------------------------------------------------------------
common Contec_aiow,dllfile,c,Dev_exist

maxch=0 &	nbit=0 &	nrange=0 &	ClockType=0 &	nch=0 &	memType=0
StartTrig=0 &	StopTrig=0 &	n_sample=0l &	n_evsample=0l
clock=0l
if Dev_exist then $
	clock = call_external(dllfile,'Caio_getinfo', $
	maxch,nbit,nrange,ClockType,nch,memType,StartTrig,StopTrig, $
	n_sample,n_evsample,/F_VALUE)

c.maxch=maxch
c.nbit=nbit
c.ClockType=ClockType
c.clock=clock			; sampling clock speed [micro sec]
c.nch=nch			; # of AD channels
c.start_trig=StartTrig		; Start trig. 0: software
c.stop_trig=StopTrig		; Stop trig. 0: software
c.n_sample=n_sample		; # of sampling
c.n_evsample=n_evsample		; # of event sampling

case nrange of
   0: c.range=[-10,10]
   50: c.range=[0,10]
endcase
case memType of
   0: c.memory='FIFO'
   1: c.memory='Ring'
endcase
case c.card of
  'AD12-8(PM)': 	c.maxn=256l*1024
  'ADA16-8/2(LPCI)L':	c.maxn=256l*1024
  'AI-1608AY-USB':	c.maxn=1024l
  else:			c.maxn=0l
endcase

return,c

end

;**************************************************************
pro caio_Set,nchan=nchan,rate=rate,n_sample=n_sample,n_evsample=n_evsample, $
	stop_trig=stop_trig,ret=rslt,chseq=chseq
;  nchan  --  # of using channels
;  rate  --  sampling clock speed (Hz)
;  stop_trig  --  0: # n_sample,  4: stop command
;--------------------------------------------------------------
common Contec_aiow,dllfile,c,Dev_exist

if not Dev_exist then return

if n_elements(stop_trig) ne 0 then $
	rslt = call_external(dllfile,'Caio_SetStopTrigger',fix(stop_trig));

if keyword_set(nchan) then $
	rslt = call_external(dllfile,'Caio_SetChannels',nchan);

if keyword_set(chseq) then begin
	for i=0,n_elements(chseq)-1 do $
		rslt = call_external(dllfile,'Caio_SetChannelSeq',i,chseq(i));
endif

if keyword_set(rate) then begin
	clock=1e6/rate
	rslt = call_external(dllfile,'Caio_SetClock',clock);
endif
if keyword_set(n_sample) then $
	rslt = call_external(dllfile,'Caio_SetStopTimes',long(n_sample));
; AD12-8(PM),ADA16-8/2(LPCI)L	 設定可能なAiStopTimesは1〜4294967295です。

if keyword_set(n_evsample) then $
	rslt = call_external(dllfile,'Caio_SetEvSamplingTimes',long(n_evsample));
; AD12-8(PM),ADA16-8/2(LPCI)L
;	 ≦n_evsample×使用チャネル数≦256*1024の範囲内で任意に設定可能です。

;print,'Caio_Set: ret=',rslt

end

;**************************************************************
function caio_pSet,p
;--------------------------------------------------------------

caio_Set,nchan=p.nch,rate=p.rate,n_sample=p.n_sample,n_evsample=p.dt_read1*p.rate, $
	ret=rslt
p.c=caio_getinfo()
return,p
end

;**************************************************************
pro caio_init,p=p,st=st,dllfile=dll
;--------------------------------------------------------------
common Contec_aiow,dllfile,c,Dev_exist

if not keyword_set(dll) then begin
	;dllfile='C:\nkrprj\CPROG\Contec_AIOW\Debug\Contec_AIOW.dll'
	dllfile='C:\Projects\cprog\VS2005\ADI12_8\Debug\ADI12_8.dll'
	dllfile='C:\Projects\cprog\VS2010\AIO_64\x64\Debug\AIO_64.dll'
endif else begin
	dllfile=dll
endelse

Dev_exist=1
if keyword_set(p) then Dev_exist=p.dev_exist

Devstr='Null,Null'
if Dev_exist then begin
	Devstr = call_external(dllfile,'Caio_query',/S_VALUE);
;	Devstr1 = call_external(dllfile,'Caio_query1',/S_VALUE);
endif
Devs=strsep(Devstr,sep=',')
DevName=Devs[0]	; ex. 'AIO000'
Card=Devs[1]	; ex. 'AD12-8(PM)'
;DevName='AIO000' &	Card='AI-1608AY-USB'
DevName='AIO000' &	Card='AIO-160802AY-USB'

c=caio_cardst()
c.DevName=DevName &	c.card=Card


if Card eq 'Null' then begin
	if keyword_set(p) then p.c=c
	st=-1
	Dev_exist=0
	return
endif

st=0
if Dev_exist then st = call_external(dllfile,'Caio_init',DevName);
print,'caio_init: st=',st	&	if st ne 0 then stop
c=caio_getinfo()
if keyword_set(p) then begin
	p.c=c
	p=caio_pSet(p)
endif

end


;**************************************************************
pro caio_exit
;--------------------------------------------------------------
common Contec_aiow,dllfile,c,Dev_exist

if Dev_exist then rslt = call_external(dllfile,'Caio_exit');
end

;**************************************************************
pro caio_start,n_sample=n,ret=ret	; start sampling
; n - # of sampling, if 0 endless
;--------------------------------------------------------------
common Contec_aiow,dllfile,c,Dev_exist

if keyword_set(n) then caio_set,stop_trig=0,n_sample=n $
else caio_set,stop_trig=4

if Dev_exist then ret = call_external(dllfile,'Caio_start') else ret=-1
;print,'AD start, Ret=',ret

end

;**************************************************************
pro caio_stop	; stop sampling  ** NOT available for ADI12-8 **
;--------------------------------------------------------------
common Contec_aiow,dllfile,c,Dev_exist
if Dev_exist then rslt = call_external(dllfile,'Caio_stop') else rslt=-1
print,'AD stop, Ret=',rslt

end

;**************************************************************
pro caio_dout,bit,dat	; digital out
;--------------------------------------------------------------
; bit=0/1/2/3  for AD12-8(PM)
; dat=0/1
common Contec_aiow,dllfile,c,Dev_exist

if n_elements(bit) eq 1 then begin
	if Dev_exist then rslt = call_external(dllfile,'Caio_dout',bit,dat);
endif else begin
	port=0
	bdat=0
	for i=0,n_elements(bit)-1 do bdat=bdat+dat(i)*2^i
;	if Dev_exist then rslt = call_external(dllfile,'Caio_Dobyte',port,bdat);
endelse

end

;**************************************************************
function caio_status,BUSY=BUSY,START_TRG=START_TRG,DATA_NUM=DATA_NUM, $
	OFERR=OFERR,SCERR=SCERR,AIERR=AIERR
; return Caio atatus
;--------------------------------------------------------------
common Contec_aiow,dllfile,c,Dev_exist
st=0l
if Dev_exist then ret = call_external(dllfile,'Caio_GetStatus',st);

;  デバイス動作中 		00000001H	AIS_BUSY
;  開始トリガ待ち		00000002H	AIS_START_TRG
;  指定個数以上データ格納 	00000010H	AIS_DATA_NUM
;  オーバーフロー	 	00010000H	AIS_OFERR
;  サンプリングクロックエラー	00020000H	AIS_SCERR
;  AD変換エラー			00040000H	AIS_AIERR
if (st mod 16) eq 1 then BUSY=1 else BUSY=0
if (st mod 16) eq 2 then START_TRG=1 else START_TRG=0
if (ishft(st,-4) mod 16) eq 1 then DATA_NUM=1 else DATA_NUM=0
if (ishft(st,-16) mod 16) eq 1 then OFERR=1 else OFERR=0

return,st
end

;**************************************************************
function caio_count,ret=ret	; return Caio sampling count
;--------------------------------------------------------------
common Contec_aiow,dllfile,c,Dev_exist
count=0l
if Dev_exist then ret = call_external(dllfile,'Caio_GetCount',count);
return,count
end

;**************************************************************
function caio_read,nch,ndat,ret=ret,raw=raw
;--------------------------------------------------------------
common Contec_aiow,dllfile,c,Dev_exist


dat=lonarr(nch,long(ndat))
if Dev_exist then ret = call_external(dllfile,'Caio_read',long(ndat),dat) $
else for j=0,nch-1 do dat(j,*)=sin(findgen(ndat)/0.1)	; dummy data

if not keyword_set(raw) then begin	; return V
	dat=float(c.range(1)-c.range(0))/(2.^c.nbit)*dat+c.range(0)
endif else begin
	dat=fix(dat)
endelse

;plot,transpose(dat)
;stop


return,dat

end


;**************************************************************
function caio_get1,nch=nch,nsample=nsampl	; get for nhk
;--------------------------------------------------------------
common Contec_aiow,dllfile,c,Dev_exist


caio_start,n_sample=nsampl
ndat=0

while ndat lt nsampl do begin
	ndat=caio_count()
endwhile
intens=caio_read(2,nsampl)

return,reform(rebin(intens,nch,1),nch)

end

;**************************************************************
pro caio_wd_status,wd,st=st,count=count,ndat=ndat,DATA_NUM=data_num
;--------------------------------------------------------------

st=caio_status(BUSY=busy,START_TRG=START_TRG,DATA_NUM=data_num,OFERR=oferr)
count=caio_count()

	widget_control,wd.Status,set_value=string(st,form='(z8.8)')+'H'
	widget_control,wd.Count,set_value=string(count,form='(i6.6)')
;	widget_control,wd.Memsld,set_value=count
	if n_elements(ndat) then $
	widget_control,wd.Ndat,set_value=string(ndat,form='(i6.6)')
end


;**************************************************************
function caio_event, ev, wd, p, dat=dat, $
	wd_stop=wd_stop2, wd_rec2=wd_rec2, n_rec=n_rec
;--------------------------------------------------------------
;  wd  -- widget
;  p   -- control parameters
;  dat -- read data
;  wd_stop2 -- wd-id for other stop
;  wd_rec2 -- wd-id for other rec
;  n_rec -- # of data for rec2
;  return new p

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
function widget_caio,base,p	; display widget status
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

;	wd_caio.Exit = widget_button(b_4, value="Exit", uvalue = "Exit")
;	b_5=widget_base(base1, /row)
;	wd_caio.DatDir=widget_button(b_5, value="Data dir:", uvalue = "DatDir")
;	lab = widget_label(b_5,value=p.datadir,font=2)

	return,wd_caio

end
;**************************************************************
pro caio_trig,nch
;--------------------------------------------------------------
common Contec_aiow,dllfile,c,Dev_exist

;threshold=fix(float(threshold_V-c.range(0))*float(2.^c.nbit)/float(c.range(1)-c.range(0)))

ndat=1
dat=lonarr(nch,long(ndat))
ret = call_external(dllfile,'Caio_trig',long(ndat),dat)
;ret = call_external(dllfile,'Caio_trig',long(ndat),long(threshold))

end
;**************************************************************
function caio_trig1,nch
;--------------------------------------------------------------
common Contec_aiow,dllfile,c,Dev_exist

;threshold=fix(float(threshold_V-c.range(0))*float(2.^c.nbit)/float(c.range(1)-c.range(0)))

ndat=1
dat=lonarr(nch,long(ndat))
dt=caio_read(nch,1)

while (dt ge 2.) do begin
	dt=caio_read(nch,1)
	print,dt
endwhile
;i=1
;while (dt le 2.) do begin
;	dt=caio_read(nch,1)
;	i=i+1
;endwhile
j=1
;	caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
;	yyyymmdd_hhmmss=string(year,form='(i4.4)')+string(mon,form='(i2.2)')+string(day,form='(i2.2)') $
;		+'_'+string(hour,form='(i2.2)')+string(minu,form='(i2.2)')+string(seco,form='(i2.2)')
;	print,'while_start',yyyymmdd_hhmmss
;while (dt ge 2.) do begin
;	dt=caio_read(nch,1)
;	j=j+1
;endwhile
;	caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
;	yyyymmdd_hhmmss=string(year,form='(i4.4)')+string(mon,form='(i2.2)')+string(day,form='(i2.2)') $
;		+'_'+string(hour,form='(i2.2)')+string(minu,form='(i2.2)')+string(seco,form='(i2.2)')
;	print,'while_end',yyyymmdd_hhmmss

return,j
;ret = call_external(dllfile,'Caio_trig',long(ndat),long(threshold))

end


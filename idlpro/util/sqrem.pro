;*********************************************************
; sqrem.pro
; plot temperatures from SQREM
; LINKAGE:
;	c:\system\sqrem			SQREM softwares
;	   "           \datfiles	SQREM data ('nkr.*', sqnkr.bin)
;	   "           \  "     \orig	original data (nkr_####.*)
;	c:\usr\gs261			gswin software
;	c:\nkrprj\idlpro\util\*.pro	timestmp, movefile, psout
;		 \cprog\oscomdll.dll	timestmp, movefile, excom
;	c:\home\idlpro\util\psyoko.pro	
;
; ver.1.0	98/01/24	k.i.
; ver.1.1	98/01/27	k.i.	file handling
; ver.1.2	99/02/15	k.i.	sqremlib.pro
; ver.2.0	99/04/07	k.i.	file ->'sqyymmdd.bin', gif output
@sqremlib
;********************************************************
function wd_sqrem,base,tt	;  define widget for sqrem

wd = {wd_sqrem, $
	S_yy:	0l,	$
	S_mon:	0l,	$
	S_dd:	0l,	$
	S_hh:	0l,	$
	S_mm:	0l,	$
	S_ss:	0l,	$
	E_yy:	0l,	$
	E_mon:	0l,	$
	E_dd:	0l,	$
	E_hh:	0l,	$
	E_mm:	0l,	$
	E_ss:	0l,	$
	Sqrem:	0l,	$
	Reset:	0l,	$
	Show:	0l,	$
	Print:	0l,	$
	Exit:	0l,	$
	ts_s:	tstruct(),	$
	ts_e:	tstruct()	$
	}
;wd.ts_s=tstruct(year(0),ltime(0))
;nt=n_elements(ltime)
;wd.ts_e=tstruct(year(nt-1),ltime(nt-1))
wd.ts_s=tstruct(tt.y_s,tt.lt_s)
wd.ts_e=tstruct(tt.y_e,tt.lt_e)
b00=widget_base(base, /row)
	wd.Sqrem = widget_button(b00, value="SQREM: Connect to the Logger", uvalue = "Sqrem")
b0_2=widget_base(base, /column, /frame)
b0=widget_base(b0_2, /row)
	lab = widget_label(b0,value='Interval :       date          time',font=1)
b1=widget_base(b0_2, /row)
	lab = widget_label(b1,value='Start : ',font=1)
	wd.S_yy = widget_text(b1,value=string(wd.ts_s.yy,format='(i4.4)'), $
		uvalue='Txt_set', xsize=4, /edit)
	lab = widget_label(b1,value='/',font=0)
	wd.S_mon = widget_text(b1,value=string(wd.ts_s.mon,format='(i2.2)'), $
		uvalue='Txt_set', xsize=2, /edit)
	lab = widget_label(b1,value='/',font=0)
	wd.S_dd = widget_text(b1,value=string(wd.ts_s.dd,format='(i2.2)'), $
		uvalue='Txt_set', xsize=2, /edit)
	lab = widget_label(b1,value='   ',font=0)
	wd.S_hh = widget_text(b1,value=string(wd.ts_s.hh,format='(i2.2)'), $
		uvalue='Txt_set', xsize=2, /edit)
	lab = widget_label(b1,value=':',font=0)
	wd.S_mm = widget_text(b1,value=string(wd.ts_s.mm,format='(i2.2)'), $
		uvalue='Txt_set', xsize=2, /edit)
	lab = widget_label(b1,value=':',font=0)
	wd.S_ss = widget_text(b1,value=string(wd.ts_s.ss,format='(i2.2)'), $
		uvalue='Txt_set', xsize=2, /edit)
	lab = widget_label(b1,value=' JST',font=0)
b2=widget_base(b0_2, /row)
	lab = widget_label(b2,value='End  : ',font=1)
	wd.E_yy = widget_text(b2,value=string(wd.ts_e.yy,format='(i4.4)'), $
		uvalue='Txt_set', xsize=4, /edit)
	lab = widget_label(b2,value='/',font=0)
	wd.E_mon = widget_text(b2,value=string(wd.ts_e.mon,format='(i2.2)'), $
		uvalue='Txt_set', xsize=2, /edit)
	lab = widget_label(b2,value='/',font=0)
	wd.E_dd = widget_text(b2,value=string(wd.ts_e.dd,format='(i2.2)'), $
		uvalue='Txt_set', xsize=2, /edit)
	lab = widget_label(b2,value='   ',font=0)
	wd.E_hh = widget_text(b2,value=string(wd.ts_e.hh,format='(i2.2)'), $
		uvalue='Txt_set', xsize=2, /edit)
	lab = widget_label(b2,value=':',font=0)
	wd.E_mm = widget_text(b2,value=string(wd.ts_e.mm,format='(i2.2)'), $
		uvalue='Txt_set', xsize=2, /edit)
	lab = widget_label(b2,value=':',font=0)
	wd.E_ss = widget_text(b2,value=string(wd.ts_e.ss,format='(i2.2)'), $
		uvalue='Txt_set', xsize=2, /edit)
	lab = widget_label(b2,value=' JST',font=0)
b3=widget_base(base, /row)
	wd.Show = widget_button(b3, value="Show", uvalue = "Show")
	wd.Print = widget_button(b3, value="to Gif", uvalue = "Print")
	wd.Reset = widget_button(b3, value="Reset", uvalue = "Reset")
	wd.Exit = widget_button(b3, value="Exit", uvalue = "Exit")

return,wd
end

;********************************************************
function inpwdtxt,ev,yy=yy	; input number from text widget
	widget_control, ev.id, get_value=value, set_value=''
	val=fix(value(0))
	if keyword_set(yy) then $
		widget_control, ev.id, set_value=string(val,format='(i4.4)') $
	else 	widget_control, ev.id, set_value=string(val,format='(i2.2)') 
return,val
end

;********************************************************
function ts2ltime,ts	
; time structure --> long time from beginning of the year

dom=[31,28,31,30,31,30,31,31,30,31,30,31]
if ts.yy mod 4 eq 0 then dom(1)=29
doy=fix(total(dom(0:ts.mon-1)))-dom(ts.mon-1)+ts.dd
ltime=long(ts.hh)*3600l+long(ts.mm)*60l+long(ts.ss) + (doy-1)*3600l*24l
return,ltime
end

;********************************************************
function ttstruct,wd	; wd --> tt-struct

tt={time_int, $
	y_s:	1998, $
	lt_s:	3000000l, $
	y_e:	1999, $
	lt_e:	10000l $
	}
if not keyword_set(wd) then return,tt
tt.y_s=wd.ts_s.yy
tt.lt_s=ts2ltime(wd.ts_s)
tt.y_e=wd.ts_e.yy
tt.lt_e=ts2ltime(wd.ts_e)

return,tt
end

;********************************************************
pro setintvl,wd,year,ltime,tend=tend	; set time interval widgets

	if keyword_set(ltime) then begin
		if not keyword_set(tend) then wd.ts_s=tstruct(year(0),ltime(0))
		nt=n_elements(ltime)
		wd.ts_e=tstruct(year(nt-1),ltime(nt-1))
	endif
	widget_control,wd.S_yy, set_value=string(wd.ts_s.yy,format='(i4.4)')
	widget_control,wd.S_mon, set_value=string(wd.ts_s.mon,format='(i2.2)')
	widget_control,wd.S_dd, set_value=string(wd.ts_s.dd,format='(i2.2)')
	widget_control,wd.S_hh, set_value=string(wd.ts_s.hh,format='(i2.2)')
	widget_control,wd.S_mm, set_value=string(wd.ts_s.mm,format='(i2.2)')
	widget_control,wd.S_ss, set_value=string(wd.ts_s.ss,format='(i2.2)')
	widget_control,wd.E_yy, set_value=string(wd.ts_e.yy,format='(i4.4)')
	widget_control,wd.E_mon, set_value=string(wd.ts_e.mon,format='(i2.2)')
	widget_control,wd.E_dd, set_value=string(wd.ts_e.dd,format='(i2.2)')
	widget_control,wd.E_hh, set_value=string(wd.ts_e.hh,format='(i2.2)')
	widget_control,wd.E_mm, set_value=string(wd.ts_e.mm,format='(i2.2)')
	widget_control,wd.E_ss, set_value=string(wd.ts_e.ss,format='(i2.2)')

end


;********************************************************
pro sqrem_event, ev	; sqrem event handler
common sqrem,year,ltime,temp,wd,sqrdir,datadir,binfile,psfile

ts_rst=0
case ev.id of 
    wd.Sqrem: begin
	fnam='nkr'	; downloaded filename
	cd,sqrdir,current=old_dir
	;spawn,'sqrem.exe /direct'
	spawn,'sqrem.exe'
	cd,datadir,current=old_dir
	ascfile=findfile(fnam+'.l01',count=count)
	if count eq 0 then return	;  no download
	;--- read new data ---
	print,'New data downloaded..'
	rdascdat,ascfile(0),year2,doy2,ltim2,temp2,str1=str1
	if year2(0) eq -1 then return
	ltime2=long(doy2-1)*24l*3600+ltim2
	;--- write bin file file ---
	outfnam='sq'+strmid(str1,1,2)+strmid(str1,4,2)+strmid(str1,7,2)
	print,'output to ',datadir+outfnam+'.bin'
	wtbindat,datadir+outfnam+'.bin',year2,doy2,ltim2,temp2
	movefile,fnam+'.dat','orig\'+outfnam+'.dat'
	movefile,fnam+'.txt','orig\'+outfnam+'.txt'
	movefile,fnam+'.l00','orig\'+outfnam+'.l00'
	movefile,fnam+'.l01','orig\'+outfnam+'.l01'

	;---  append to arrays  ---
	nn=n_elements(ltime)
	t9_old=ltime(nn-1)
	t1_new=(year2(0)-year(nn-1))*365l*3600l*24l+ltime2(0)
	if t1_new le t9_old then begin	; overwrapping
		ii=where(year2 eq year(nn-1) and ltime2 eq ltime(nn-1))
		i0=ii(0)+1
		nn2=n_elements(ltime2)
		if i0 eq nn2 then begin
			print,'data not updated'
			return
		endif
		year2=year2(i0:nn2-1)
		doy2=doy2(i0:nn2-1)
		ltim2=ltim2(i0:nn2-1)
		ltime2=ltime2(i0:nn2-1)
		temp2=temp2(*,i0:nn2-1)
	endif
	year=[year,year2]
	ltime=[ltime,ltime2]
	temp=[[temp],[temp2]]

	setintvl,wd,year,ltime,/tend
	tt=ttstruct(wd)
	end
    wd.S_yy: begin & wd.ts_s.yy=inpwdtxt(ev,/yy) & ts_rst=1 & end
    wd.S_mon: begin & wd.ts_s.mon=inpwdtxt(ev) & ts_rst=1 & end
    wd.S_dd: begin & wd.ts_s.dd=inpwdtxt(ev) & ts_rst=1 & end
    wd.S_hh: wd.ts_s.hh=inpwdtxt(ev)
    wd.S_mm: wd.ts_s.mm=inpwdtxt(ev)
    wd.S_ss: wd.ts_s.ss=inpwdtxt(ev)
    wd.E_yy: wd.ts_e.yy=inpwdtxt(ev,/yy)
    wd.E_mon: wd.ts_e.mon=inpwdtxt(ev)
    wd.E_dd: wd.ts_e.dd=inpwdtxt(ev)
    wd.E_hh: wd.ts_e.hh=inpwdtxt(ev)
    wd.E_mm: wd.ts_e.mm=inpwdtxt(ev)
    wd.E_ss: wd.ts_e.ss=inpwdtxt(ev)
    wd.Reset: setintvl,wd,year,ltime
    wd.Show: begin
	ds=fday1998(wd.ts_s.yy,ts2ltime(wd.ts_s))
	de=fday1998(wd.ts_e.yy,ts2ltime(wd.ts_e))
	dd=fday1998(year,ltime)
	if dd(0) gt ds then begin
		tt=ttstruct(wd)
		get_bindat,datadir,tt,year,ltime,temp
		dd=fday1998(year,ltime)
	endif
	ii=where(dd ge ds and dd le de, count)
	if count eq 0 then begin
		print,'Time interval is inadequate!'
		return
	endif
	pltsqrem1,year(ii),ltime(ii),temp(*,ii)
	end
    wd.Print: begin
	tvlct,r,g,b,/get
	ii=tvrd()
	write_gif,datadir+'nkrsqrm.gif',ii,r,g,b
	print,datadir+'nkrsqrm.gif is created...'
;	ds=fday1998(wd.ts_s.yy,ts2ltime(wd.ts_s))
;	de=fday1998(wd.ts_e.yy,ts2ltime(wd.ts_e))
;	dd=fday1998(year,ltime)
;	ii=where(dd ge ds and dd le de, count)
;	if count eq 0 then begin
;		print,'Time interval is inadequate!'
;		return
;	endif
;	psyoko,ysize=17
;	device,filename=datadir+psfile
;	pltsqrem1,year(ii),ltime(ii),temp(*,ii),/mono
;	device,/close
;	set_plot,'win'
;	psout,datadir+psfile,/printout
	end
    wd.Exit: begin
	WIDGET_CONTROL, /destroy, ev.top
	end
endcase

if ts_rst then begin
	wd.ts_s.hh=0
	wd.ts_s.mm=0
	wd.ts_s.ss=0
	setintvl,wd
endif

end

;********************************************************
;pro getsqorig,datadir,year,ltime,temp
;  read SQREM *.l01 files
;cd,datadir,current=old_dir
;ascfiles=findfile('nkr*.l01')
;cd,old_dir
;nf=n_elements(ascfiles)
;files=strarr(nf)
;for i=0,nf-1 do files(i)=strmid(ascfiles(i),0,strpos(ascfiles(i),'.'))
;ltime=0
;for i=0,nf-1 do begin
;	getsqrem,datadir+files(i),year,ltime,temp
;endfor
;end

;********************************************************
pro get_bindat,datadir,tt,year,ltime,temp,binfiless=binfiless

cd,datadir,current=old_dir
binfiles=findfile('sq9*.bin',count=count)
cd,old_dir
dates=long(strmid(binfiles,2,6))
ii=sort(dates)
binfiles=binfiles(ii)
dates=dates(ii)
timestrs,tt.y_s,tt.lt_s,cdate_s,ctime_s,/yymmdd
date_s=long(cdate_s)
ii=where(dates gt date_s, count)
if count eq 0 then begin
	ii=n_elements(binfiles)-1
endif else begin
	if ii(0) gt 0 then ii=[ii(0)-1,ii]
endelse
binfiless=binfiles(ii)

rdbindat,datadir+binfiless,year,doy,ltim,temp
ltime=long(doy-1)*24l*3600l+ltim
nn=n_elements(year)
tt.y_e=year(nn-1)
tt.lt_e=ltime(nn-1)
end

;********************************************************
common sqrem,year,ltime,temp,wd,sqdir,datadir,binfile,psfile	; main

;----  read data --------------------------------
sqdir='c:\system\sqrem'
ff=findfile(sqdir,count=count)
if count eq 0 then sqdir='c:\usr\sqrem'
datadir	=sqdir+'\datfiles\'
psfile	='sqplot.ps'
binfile	='sqnkr.bin'
logfile	='sqnkr.log'

tt=ttstruct()
restore,datadir+logfile  ; --> tt
get_bindat,datadir,tt,year,ltime,temp

;----  control widget  -------------------------
base = WIDGET_BASE(title='Norikura SQREM', /column) 
wd=wd_sqrem(base,tt)
widget_control, base, /realize
XMANAGER, 'sqrem', base

tt=ttstruct(wd)
save,tt,file=datadir+logfile
;stop
;&&&&&& special operation &&&&&
;cd,datadir,current=old_dir
;ffs=['nkr1','nkr2','nkr']+'.bin'
;rdbindat,ffs(0),year,doy,ltim,temp
;for i=1,n_elements(ffs)-1 do begin
;	rdbindat,ffs(i),year2,doy2,ltim2,temp2
;	year=[year,year2]
;	doy=[doy,doy2]
;	ltim=[ltim,ltim2]
;	temp=[[temp],[temp2]]
;endfor
;wtbindat,binfile,year,doy,ltim,temp
;cd,old_dir


end

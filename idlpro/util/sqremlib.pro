; sqremlib.pro
;	'99/02/16	k.i. from sqrem.pro, bug fix
;	'99/04/07	k.i. sqremplt1, modify doy
;	'99/08/31	k.i. fig.background, mean

;*********************************************************
pro str2num,str,year,doy,ltime,temp	; covert ascii data to numbers
;  year	 - eg. 1998
;  doy	 - day of year
;  ltime - long second from the beginning the year
;  temp	 - temperatures (6,*)

form='yy/mm/dd'
dom=[31,28,31,30,31,30,31,31,30,31,30,31]
case form of
   'dd-mon-yy': begin
	date=strmid(str,1,9)
	dd=fix(strmid(date,0,2))
	mon=strmid(date,3,3)
	yy=fix(strmid(date,7,2))
	case mon of
	  'Jan': m=1
	  'Feb': m=2
	  'Mar': m=3
	  'Apr': m=4
	  'May': m=5
	  'Jun': m=6
	  'Jul': m=7
	  'Aug': m=8
	  'Sep': m=9
	  'Oct': m=10
	  'Nov': m=11
	  'Dec': m=12
	endcase
	ctime=strmid(str,13,8)
	strt=strmid(str,22,strlen(str)-1)
	end
    'yy/mm/dd': begin
	date=strmid(str,1,8)
	dd=fix(strmid(date,6,2))
	m=fix(strmid(date,3,2))
	yy=fix(strmid(date,0,2))
	ctime=strmid(str,12,8)
	strt=strmid(str,21,strlen(str)-1)
	end
endcase

if yy gt 90 then year=1900+yy $
else year=2000+yy
if yy mod 4 eq 0 then dom(1)=29
doy=fix(total(dom(0:m-1)))-dom(m-1)+dd
ltime=long(strmid(ctime,0,2))*3600l $
	+long(strmid(ctime,3,2))*60l+long(strmid(ctime,6,2))
count=0
while strpos(strt,' ') ne -1 do begin
	while strpos(strt,' ') eq 0 do  $
		strt=strmid(strt,1,strlen(strt)-1)
	ib=strpos(strt,' ')
	if ib eq -1 then ib=strlen(strt)
	t1=float(strmid(strt,0,ib))
	if count eq 0 then temp=t1 else temp=[temp,t1]
	strt=strmid(strt,ib,strlen(strt)-1)
	count=count+1
endwhile

end

;********************************************************
function tstruct,year,ltime

ts={time_st, $
	yy:	0, 	$ ;
	mon:	0,	$ ;
	dd:	0,	$
	hh:	0,	$
	mm:	0,	$
	ss:	0	$
	}
if not keyword_set(ltime) then ltime=0l
if not keyword_set(year) then year=0
yr=year; mod 100

dom=[31,28,31,30,31,30,31,31,30,31,30,31]
if yr mod 4 eq 0 then dom(1)=29
doy=ltime/24/3600+1 &	tim=ltime-(doy-1)*24*3600
tdoy=total(dom)
if doy gt tdoy then begin
	year=year+1
	yr=yr+1
	doy=doy-tdoy
	ltime=ltime-tdoy*24l*3600
endif
m=0
while doy ge 1 do begin
	doy=doy-dom(m)
	m=m+1
endwhile
dd=doy+dom(m-1)
ts.yy=yr &	ts.mon=m &	ts.dd=dd
ts.hh=tim/3600 &	ts.mm=(tim/60) mod 60 &	ts.ss=tim mod 60

return,ts
end

;*********************************************************
pro timestrs,year,ltime,cdate,ctime,yymmdd=yymmdd
;  make 'yy/mm/dd' and 'hh:mm:ss' string from yr and ltime

yr=year mod 100
ts=tstruct(yr,ltime)
if keyword_set(yymmdd) then sep='' else sep='/'
cdate=string(ts.yy,format='(i2.2)')+sep $
	+string(ts.mon,format='(i2.2)') $
	+sep+string(ts.dd,format='(i2.2)')
ctime=string(ts.hh,format='(i2.2)')+':'+string(ts.mm,format='(i2.2)')+ $
	':'+string(ts.ss,format='(i2.2)')
end

;*********************************************************
pro pltsqrem,year,ltime,temp,mono=mono	; draw graph

td=float(ltime)/24./3600.
s=size(temp) &	nn=s(1) & nt=s(2)

timestrs,year(0),ltime(0),cdate0,ctime0
timestrs,year(nt-1),ltime(nt-1),cdate9,ctime9

td1=(min(td)) &	td2=(max(td)+1)
if td2-td1 lt 2. then begin
	td=td *24 - fix(td)*24
	xtitle='time (hr)'
endif else begin
	xtitle='Day of Year'
endelse

title='Norikura Monitor: '+cdate0+'('+ctime0+') - '+cdate9+'('+ctime9+')'
plot,td,temp(0,*),/nodata,title=title, $
	xtitle=xtitle,xminor=6, $
	xstyle=1,xrange=[fix(min(td)),fix(max(td)+1)], $
	ystyle=1,ytitle='Temp. (C)',yrange=[-30,30]
tt=[-20,-10,0,10,20]
for i=0,n_elements(tt)-1 do oplot,[0,9999],[tt(i),tt(i)],line=1

if not keyword_set(mono) then begin
	red=[255,  0,255,  0,  0,255]
	grn=[255,255,  0,  0,255,255]
	blu=[255,255,  0,255,  0,  0]
	tvlct,red,grn,blu,100
endif
com=['outside','25cm dome','25cm dark room','10cm dome', $
	'engine room','musen room']
for i=0,nn-1 do begin
	;oplot,td,temp(i,*),psym=i,symsize=0.5
	tmp1=smooth(temp(i,*),1)
	if keyword_set(mono) then begin
		oplot,td,tmp1
		xyouts,td(0),temp(i,0)+1.,com(i),/data
	endif else begin
		oplot,td,tmp1,color=i+100
		xyouts,td(0),temp(i,0)+1.,com(i),/data,color=i+100
	endelse
endfor

end

;********************************************************
pro wtbindat,binfile,year,doy,ltim,temp	;  write binnay data
	;---------  create bin file *.bin  -----
	s=size(temp) &	n_sens=s(1) & n_time=s(2)
	openw,1,binfile
	writeu,1,fix(n_sens),long(n_time)
	writeu,1,fix(year)
	writeu,1,fix(doy)
	writeu,1,long(ltim)
	writeu,1,fix(temp*100)
	close,1
end

;********************************************************
pro rdbindat,binfile,year,doy,ltim,temp	;  read binnay data

   nf=n_elements(binfile)
   if nf eq 1 then begin
	print,'getting from binnary file. '+binfile+' ...'
	yr=0 &	n_sens=0 &	n_time=0l
	openr,1,binfile
	readu,1,n_sens,n_time
	year=intarr(n_time)
	doy=intarr(n_time)
	ltim=lonarr(n_time)
	temp=intarr(n_sens,n_time)
	readu,1,year
	readu,1,doy
	readu,1,ltim
	readu,1,temp
	close,1
	temp=float(temp)/100.
   endif else begin
	rdbindat,binfile(0),year,doy,ltim,temp
	for i=1,nf-1 do begin
		rdbindat,binfile(i),year1,doy1,ltim1,temp1
		year=[year,year1]
		doy=[doy,doy1]
		ltim=[ltim,ltim1]
		temp=[[temp],[temp1]]
	endfor
   endelse
end

;********************************************************
pro rdascdat,ascfile,year,doy,ltim,temp,head=head,str1=str1
;  read ascii data
;  head	-- return only first line
;  str1 -- return first line

	print,'getting from ascii file. '+ascfile+' ...'
	;---------  get from ascii file *.l01  ------
	str=''
	openr,1,ascfile
	readf,1,str
	readf,1,str
	if eof(1) then begin
		print,'no data in '+ascfile
		close,1
		year=-1
		return
	endif
	if strmid(str,23,5) eq 'Dummy' then readf,1,str
	str2num,str,year,doy,ltim,temp
	if keyword_set(head) then begin
		close,1
		return
	endif
	str1=str
	while not eof(1) do begin
		readf,1,str
		if strmid(str,23,5) eq 'Dummy' then readf,1,str
		;print,str
		str2num,str,year1,doy1,ltim1,t1
		year=[year,year1]
		doy=[doy,doy1]
		ltim=[ltim,ltim1]
		temp=[[temp],[t1]]
	endwhile
	close,1

end

;********************************************************
pro getsqrem,file,years,ltimes,temps	; get SQREM array data
;  read data from ascii or bin file
;  create bin file if it is older than ascii file
;  years(*)    --  year (eg. 1998)
;  ltimes(*)   --  long second from the begining of the year
;  temp(6,*)   --   temperature

ascfile=file+'.l01'
ff=findfile(ascfile,count=counta)
binfile=file+'.bin'
ff=findfile(binfile,count=countb)
if counta eq 0 and countb eq 0 then begin
	print,'Log data '+file+' not found'
	return
endif
if counta ne 0 then ltsa=timestmp(ascfile,/l) else ltsa=-1e7
if countb ne 0 then ltsb=timestmp(binfile,/l) else ltsb=-1e7
if ltsa gt ltsb then begin
	rdascdat,ascfile,year,doy,ltim,temp
	;---------  create bin file *.bin  -----
	wtbindat,binfile,year,doy,ltim,temp
endif else begin
	rdbindat,binfile,year,doy,ltim,temp
endelse

ltim=long(doy)*24l*3600l+ltim
if not keyword_set(ltimes) then begin
	years=year
	ltimes=ltim
	temps=temp
endif else begin
	years=[years,year]
	ltimes=[ltimes,ltim]
	temps=[[temps],[temp]]
endelse

end

;********************************************************
function fday1998,year,ltime
;  return float day from 1998/01/01 00:00:00

y0=1998	; origin of time
fd=(year-y0)*365.+ltime/24./3600.
ii=where(year ge 2001, count)	; include 2000 (uruu)
if count ne 0 then fd(ii)=fd(ii)+1

return,fd
end

;********************************************************
pro fd2yl,fday,year,ltime
;  float day from 1998/01/01 00:00:00 -> year, ltime

y0=1998	; origin of time
year=y0
doy=365
fday1=fday
while fday1-doy gt 0 do begin
	fday1=fday1-doy
	year=year+1
	if year mod 4 eq 0 then doy=366 else doy=365
endwhile
ltime=long(fday1*24*3600)

return
end

;*********************************************************
pro pltsqrem1,year,ltime,temp,mono=mono,giffile=giffile	; draw graph

td=fday1998(year,ltime)
s=size(temp) &	nn=s(1) & nt=s(2)

timestrs,year(0),ltime(0),cdate0,ctime0
timestrs,year(nt-1),ltime(nt-1),cdate9,ctime9
td0=fix(td(0))
td9=fix(td(nt-1))+1
dtd=max([1,fix((td9-td0)/8.)])	; tick interval (day)
ntick=(td9-td0)/dtd+1
tickv=td0+findgen(ntick)*dtd
tickn=strarr(ntick)
for i=0,ntick-1 do begin
	fd2yl,tickv(i),yr1,ltime1
	timestrs,yr1,ltime1,cdate1,ctime1
	cmm=strcompress(string(fix(strmid(cdate1,3,2))),/remove_all)
	cdd=strcompress(string(fix(strmid(cdate1,6,2))),/remove_all)
	tickn(i)=cmm+'/'+cdd
endfor

title='Norikura Monitor: '+cdate0+'('+ctime0+') - '+cdate9+'('+ctime9+')'
white=replicate(150,!d.x_size,!d.y_size)
tv,white
plot,td,temp(0,*),/nodata,title=title,/noerase, $
	xtitle='Date',xminor=6,xstyle=1,xrange=[td0,td9], $
	xticks=ntick,xtickv=tickv,xtickname=tickn, $
	ystyle=1,ytitle='Temp. (C)',yrange=[-30,30],color=0
tt=[-20,-10,0,10,20]
for i=0,n_elements(tt)-1 do oplot,[0,9999],[tt(i),tt(i)],line=1,color=0
oplot,td(0)*[1,1],[0,1],line=1,color=0

if not keyword_set(mono) then begin
	red=[0,  0,255,  0,  0,255]
	grn=[0,255,  0,  0,255,255]
	blu=[0,255,  0,255,  0,  0]
endif
com=['outside','25cm dome','25cm dark room','10cm dome', $
	'engine room','25-dome ground']
for i=0,nn-1 do begin
	;oplot,td,temp(i,*),psym=i,symsize=0.5
	;tmp1=smooth(temp(i,*),96)	; <== 1day mean
	tmp1=temp(i,*)
	if keyword_set(mono) then begin
		oplot,td,tmp1
		;xyouts,td(0),temp(i,0)+1.,com(i),/data
	endif else begin
		oplot,td,tmp1,color=i+100
		;xyouts,td(0),temp(i,0)+1.,com(i),/data,color=i+100
	endelse
endfor
xc=td0+dtd*3
for i=0,nn-1 do begin
	yc=27.-i*2
	if keyword_set(mono) then begin
		oplot,[xc,xc+dtd],yc*[1,1]
		xyouts,xc+dtd*1.1,yc-0.5,com(i),/data
	endif else begin
		oplot,[xc,xc+dtd],yc*[1,1],color=i+100
		xyouts,xc+dtd*1.1,yc-0.5,com(i),/data,color=i+100
	endelse
endfor
if keyword_set(giffile) then begin
	tvlct,r,g,b,/get
	ii=tvrd()
	write_gif,giffile,ii,r,g,b
endif

end


; get_systime.pro
;  return system time with msec using timeGeTime()
;  in 'yyyy-mm-ddThh:mm:ss.sss'
;  ctime = 'yyyymmdd_hhmmss.sss'
;   2014.5.19	k.i.
;   2016.2.11	k.i.	bug fix
;   2016.2.24	k.o.	yyyymmdd_hhmmss->ctime
;   2017.4.1	k.i.	dllfile=file_search()

function get_systime,msec=msec,ctime=ctime,init=init

common get_systime,msec0,cmon0,cday0,cyear0,lsec0

dfile='\Projects\cprog\VS2010\oscom64\x64\Debug\oscom64.dll'
dllfile=file_search('C:'+dfile) 
if dllfile eq '' then dllfile=file_search('D:'+dfile) 
dllfile=dllfile[0]
if n_elements(msec0) eq 0 or keyword_set(init) then begin
	sec0=strmid(systime(),17,2)
	sec1=strmid(systime(),17,2)
	while (sec1 eq sec0) do begin
		;print,sec1
		sec1=strmid(systime(),17,2)
	endwhile
	msec0=call_external(dllfile,'Dgettime',/all_value,/cdecl)
	caldat,systime(/JULIAN), mon0 , day0 , year0 , hour0 , minu0 , seco0
	lsec0=hour0*3600+minu0*60+seco0
	cyear0=string(year0,form='(i4.4)')
	cmon0=string(mon0,form='(i2.2)')
	cday0=string(day0,form='(i2.2)')
	print,'msec0 time ref. was set in get_systime()'
endif

;caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
msec=call_external(dllfile,'Dgettime',/all_value,/cdecl)

dmsec=msec-msec0
lsec=long(lsec0+dmsec/1000)
hour=lsec/3600l &	minu=(lsec-hour*3600l)/60l &	seco=lsec mod 60l

if hour ge 24 then begin
	dmy=get_systime(/init)
	ccsds=get_systime(ctime=ctime)
	return,ccsds
endif
chour=string(hour,form='(i2.2)')
cminu=string(minu,form='(i2.2)')
cseco=+string(seco,form='(i2.2)')
cmsec=string(dmsec mod 1000l, form='(i3.3)')

ccsds=cyear0+'-'+cmon0+'-'+cday0+'T'+chour+':'+cminu+':'+cseco+'.'+cmsec
;yyyymmdd_hhmmss=cyear0+cmon0+cday0+'_'+chour+cminu+cseco+'.'+cmsec
ctime=cyear0+cmon0+cday0+'_'+chour+cminu+cseco+'.'+cmsec

return,ccsds

end

;	sartorius.pro

;	2016.01.28	t.a.
;       2016.02.20 TTI
;       2016.07.14 K. Otsuji for new PC (C:\->D:\)
;       2016.07.25 K. Otsuji sav -> fits


;=============================================
; include


pro sartorius_select_flare,date=date

if n_elements(date) eq 0 then begin
caldat,systime(/JULIAN),mon,day,year,hour,minu,seco
date=string(year,format='(i4.4)')+string(mon,format='(i2.2)')+string(day,format='(i2.2)')
endif


savedir='D:\data\'+date
	
eventsdir='D:\data\events\'+date

file=file_search(savedir+'\fits\*cen.fits',count=nf)
normfile=file
time=strmid(file,26,2)*60.*60.+strmid(file,28,2)*60.+strmid(file,30,2)*1.
nflare=0
read,'number of flare? : ',nflare
		
	pos_events=-1
		

if nflare ge 1 then begin
	for i=0,nflare-1 do begin
	time1=''
	read,'Start time of flare#'+		$
		strcompress(string(i+1,format='(i)'),/remove_all)+$
		', hh:mm?[UT] : ',time1
	time2=''
	read,'Finish time of flare#'+		$
		strcompress(string(i+1,format='(i)'),/remove_all)+$
		', hh:mm?[UT] : ',time2

	tmp=strpos(time1,':')
	ntime1=float(strmid(time1,0,tmp))*60.*60.+$	
		float(strmid(time1,tmp+1,strlen(time1)-tmp+1)-1.)*60.
	ntime2=float(strmid(time2,0,tmp))*60.*60.+$
		(float(strmid(time2,tmp+1,strlen(time2)-tmp+1))+1.)*60.
	pos_events=[pos_events,where(time ge ntime1 and time le ntime2)]
	endfor
endif

		

nf=n_elements(pos_events)
if nf ge 1 then begin
	spawn,'mkdir '+eventsdir
	for i=1,nf-1 do begin
		spawn,'copy '+file[pos_events[i]]+' '+eventsdir
	endfor
endif



end


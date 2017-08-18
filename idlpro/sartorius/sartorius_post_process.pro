;	sartorius.pro

;	2016.01.28	t.a.
;       2016.02.20 TTI


;=============================================
; include


pro sartorius_post_process,date=date

	savedir='C:\data\'+date
	selecteddir='C:\data\selected\'+date 
	eventsdir='C:\data\events\'+date

		file=file_search(savedir+'\save\*cen.sav',count=nf)
		normfile=file
		time=strmid(file,26,2)*60.*60.+strmid(file,28,2)*60.+strmid(file,30,2)*1.
		nflare=0
		read,'number of flare? : ',nflare
		
		pos_events=-1
		pos_selected=0

		if nflare ge 1 then begin
			for i=0,nflare-1 do begin
				time1=''
				read,'Start time of flare#'+		$
					strcompress(string(i+1,format='(i)'),/remove_all)+$
					', hh:mm? : ',time1
				time2=''
				read,'Finish time of flare#'+		$
					strcompress(string(i+1,format='(i)'),/remove_all)+$
					', hh:mm? : ',time2
				tmp=strpos(time1,':')
				ntime1=float(strmid(time1,0,tmp))*60.*60.+$
					float(strmid(time1,tmp+1,strlen(time1)-tmp+1)-1.)*60.
				ntime2=float(strmid(time2,0,tmp))*60.*60.+$
					(float(strmid(time2,tmp+1,strlen(time2)-tmp+1))+1.)*60.
				pos_events=[pos_events,where(time ge ntime1 and time le ntime2)]
			endfor
		endif

		
		st=time[0]
		for i=1,nf-1 do begin
		ck=time[i]
		if ck-st ge 120 then begin 
		pos_selected=[pos_selected,i]
		st=ck
		endif
		endfor

			nf=n_elements(pos_selected)
			print,nf
			spawn,'mkdir '+selecteddir
			for i=0,nf-1 do begin
			spawn,'copy '+file[pos_selected[i]]+' '+selecteddir		
			endfor

		nf=n_elements(pos_events)
		if nf ge 1 then begin
		spawn,'mkdir '+eventsdir
		for i=1,nf-1 do begin
		spawn,'copy '+file[pos_events[i]]+' '+eventsdir
		endfor
		endif



END

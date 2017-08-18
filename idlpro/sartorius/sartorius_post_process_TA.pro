;	sartorius.pro

;	2016.01.28	t.a.


;=============================================
; include


;=============================================
pro sartorius_post_process_event, ev
;---------------------------------------------
common widget,wp,wd,rmfile,file

widget_control,ev.id,get_uvalue=uvalue,get_value=value

case uvalue of
	'select':begin
		;====================================================;
		; SELECT FILES
		;====================================================;

		file=file_search(wp.savedir+'save\*cen.sav',count=nf)
		normfile=file
		time=strmid(file,26,2)*60.*60.+strmid(file,28,2)*60.+strmid(file,30,2)*1.
		nflare=0
		read,'number of flare? : ',nflare
		
		pos=findgen(nf)
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
					float(strmid(time1,tmp+1,strlen(time1)-tmp+1))*60.
				ntime2=float(strmid(time2,0,tmp))*60.*60.+$
					(float(strmid(time2,tmp+1,strlen(time2)-tmp+1))+1.)*60.
				pos=pos[where(time[pos] le ntime1 or time[pos] ge ntime2)]
			endfor
		endif

		pos1=where((time[pos] mod (2*60)) le 5, npos1)
		if npos1 ge 2 then begin
			pos2=where(time[pos[pos1[1:npos1-1]]] - time[pos[pos1[0:npos1-2]]] le 60,npos2)
			if npos2 ge 1 then begin
				rmfile=file[pos[pos1[pos2]]]
				pos1=where((time[pos] mod (2*60)) gt 5, npos1)
				rmfile=[rmfile,file[pos[pos1]]]
			endif else begin
				pos1=where((time[pos] mod (2*60)) gt 5, npos1)
				rmfile=file[pos[pos1]]
			endelse
		endif else rmfile=file[pos]

		print,'finish to select flare file'
		print,'****************************'
		;====================================================;
	end
	'remove':begin
		;====================================================;
		; REMOVE? FILES
		;====================================================;
		tmp=''
		read,'remove OK?, y or n :',tmp
		if tmp eq 'y' then begin
			nf=n_elements(file)
			for i=0,nf-1 do begin
				pos=where(rmfile eq file[i],npos)
				if npos eq 1 then spawn,'del '+file[i]
			endfor
		endif
		print,'finish to remove non-flare file'
		print,'****************************'
	end
	'exit':begin
		widget_control,/destroy, ev.top
	end
	else:print,'no uvalue'
endcase

END
;=============================================
pro sartorius_post_process
;---------------------------------------------
common widget,wp,wd,rmfile,file

files=''
caldat,systime(/JULIAN),mon,day,year,hour,minu,seco
wp={widget_param,		$
	savedir:	'C:\data\'+string(year,format='(i4.4)')+	$
			string(mon,format='(i2.2)')+			$
			string(day,format='(i2.2)')+'\'			$
	}
wd={wd_cdio,			$
	savedir:	0l,	$
	select:		0l,	$
	remove:		0l,	$
	ffftp:		0l,	$
	exit:		0l	$
	}
if is_dir(wp.savedir) eq 0 then	print,'no directory'

xsize=300
ysize=30
fnt='HGënâpäpÉSÉVÉbÉNUB*BOLD*20'
fntsmall='HGënâpäpÉSÉVÉbÉNUB*BOLD*10'

main=widget_base(title='SARTORIUS Ha observation',/column,xsize=xsize)

 base1=widget_base(main,/row)
  lab=widget_label(base1,value='save dir ',	$
	units=0,xsize=xsize/3.,ysize=ysize,FONT=fnt,xoffset=0,yoffset=0)
  wd.savedir=widget_text(base1,value=wp.savedir,edit=1,	$
	units=0,xsize=xsize,ysize=ysize/80.,uvalue='savedir',FONT=fnt)

 wd.select=widget_button(main,uvalue='select',value='SELECT FLARE',/align_center,FONT=fnt,xsize=xsize)

 wd.remove=widget_button(main,uvalue='remove',value='REMOVE NON- FLARE FILES',/align_center,FONT=fnt,xsize=xsize)

; wd.ffftp=widget_button(main,uvalue='ffftp',value='FFFTP',/align_center,FONT=fnt,xsize=xsize)

wd.exit=widget_button(main,value='EXIT',uvalue='exit',FONT=fnt)
widget_control,main,/realize
xmanager,'sartorius_post_process',main,modal=modal

END
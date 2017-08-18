	pos_selected=0
		st=time[0]
		for i=1,nf-1 do begin
		ck=time[i]
		if ck-st ge 120 then begin 
		pos_selected=[pos_selected,i]
		st=ck
		endif 
		endfor

end

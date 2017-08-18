pro wdmenu_event, ev

common wdmenu,base,wdm,menu

  widget_control, ev.id, get_uvalue=value
  if (n_elements(value) eq 0) then value = ''
  name = strmid(tag_names(eva, /structure_name), 7, 1000)
 print,'tag=',name,'  value=',value
 print,ev

  if (value eq "EXIT") then begin
  	WIDGET_CONTROL, /destroy, eva.top
  	return
  endif
  if value eq 'BUTTON' and name eq 'Button' then begin
	widget_control,base,/hourglass
	for j=0,200l  do begin
	    ev1=widget_event(wd_stop,/nowait)
	    if ev1.id ne 0 then begin
		print,'stopped'
		return
	    endif
	    print,ev1
	    ;widget_control, wd_stop, get_uvalue=value
		;print,value
	    ;if value eq 'STOP' then return
	    if eva.id eq wd_stop then begin
		print,'Stoped'
		return
		end
	    rr=get_kbrd(0)	; above not work!
	    if rr eq 's' then return
		print,'a',format='($,A1)'
	end
  endif
  return

end

;--------------------------------------------------------------
function wdmenu,menu,title=title
common wdmenu,base,wdm,menu

if not keyword_set(title) then title=''
base = WIDGET_BASE(title=title, /row) 	; base window

nn=n_elements(menu)
wdm = lonarr(nn)
for i=0,nn-1 do begin
	wd_button = widget_button(base, value=menu(i), uvalue = menu(i))
endfor
widget_control, base, /realize

XMANAGER, 'wdmenu', base, /modal	; <= modal is impotant


end

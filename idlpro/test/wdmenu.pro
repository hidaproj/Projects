;+
;  sel=wdmenu(menu_strings, title=title
;	'96/08/09 	k.i.
;-

pro wdmenu_event, ev

common wdmenu,base,wdm,menus,sel

  widget_control, ev.id, get_uvalue=value
  if (n_elements(value) eq 0) then value = ''
;print,wdm &	print,ev.id
sel=where(ev.id eq wdm)
sel=sel(0)

WIDGET_CONTROL, /destroy, ev.top

end

;--------------------------------------------------------------
function wdmenu,menu,title=title
common wdmenu,base,wdm,menus,sel

menus=menu

if not keyword_set(title) then title=''
base = WIDGET_BASE(title=title, /row) 	; base window

nn=n_elements(menus)
wdm = lonarr(nn)
for i=0,nn-1 do begin
	wdm(i) = widget_button(base, value=menus(i), uvalue = menus(i))
endfor
widget_control, base, /realize

XMANAGER, 'wdmenu', base, /modal	; <= modal is impotant

return,sel

end

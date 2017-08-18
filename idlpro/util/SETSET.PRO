;+
;  setset.pro (function)
;-
pro setset_event,ev
common setset,bOK,bMenu,value
if ev.id eq bOK then begin
	widget_control, bMenu, get_value=value, set_value=''
	WIDGET_CONTROL, /destroy, ev.top
endif
end

function setset,menu,title=title,init=init
common setset,bOK,bMenu,value

if not keyword_set(title) then title=''
if not keyword_set(init) then init=intarr(n_elements(menu))
base = WIDGET_BASE(title=title, /column ) 
bMenu = cw_bgroup(	$
	base,menu,/column, uvalue="SetSet", $
	no_release=0,set_value=init,/nonexclusive, /frame)
bOK = widget_button(base, value="OK", uvalue="TAKE1")
widget_control, base, /realize
XMANAGER, 'SetSet', base, /modal	; <= modal is impotant

return,value
end

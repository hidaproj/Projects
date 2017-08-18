;+
;  smenu.pro (function)
;-
pro smenu_event,ev
common smenu,bOK,bMenu,value

widget_control, bMenu, get_value=value, set_value=''
WIDGET_CONTROL, /destroy, ev.top
end

;--------------------------------------------------------
;function smenu,menu,title=title,pd=pd,base=base0
common smenu,bOK,bMenu,value

menu=['a','b','sayuri']
if not keyword_set(title) then title=''
if not keyword_set(base0) then $
	base = WIDGET_BASE(title=title, /column) $
else 	base = WIDGET_BASE(base0,title=title, /column) 

if keyword_set(pd) then begion
	xpdmenu,['/Table/ {', '/-- Corona --/','/-- Magnetic --/'],base
endif else begin
	bMenu = cw_bgroup(	$
		base,menu,/column, uvalue="SetSet", $
		no_release=0,set_value=init,/nonexclusive, /frame)
endelse

widget_control, base, /realize
XMANAGER, 'smenu', base, /modal	; <= modal is impotant

;return,value
end



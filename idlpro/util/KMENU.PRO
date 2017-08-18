;+
;  setset.pro (function)
;-
pro wpdmenu_event,ev
common wpdmenu,bOK,bMenu,value
if ev.id eq bOK then begin
	widget_control, bMenu, get_value=value, set_value=''
	WIDGET_CONTROL, /destroy, ev.top
endif
end

;--------------------------------------------------------
;function wpdmenu,menu,title=title
common wpdmenu,bOK,bMenu,value

if not keyword_set(title) then title=''

base = WIDGET_BASE(title=title, /column) 
xpdmenu,['/Table/ {', '/-- Corona --/','/-- Magnetic --/'],base
widget_control, base, /realize
XMANAGER, 'SetSet', base, /modal	; <= modal is impotant

;return,value
end



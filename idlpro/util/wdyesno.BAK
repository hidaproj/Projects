;+
;  wdyesno.pro (function)
;	wdyesno
;	'97/10/01	k.i.
;-
pro wdyesno_event,ev
common wdyesno,value
value=ev.value
WIDGET_CONTROL, /destroy, ev.top
end

;--------------------------------------------------------
function wdyesno,str,xpos=xpos,ypos=ypos,title=title
common wdyesno,value

if not keyword_set(xpos) then xpos=0
if not keyword_set(ypos) then ypos=0
if not keyword_set(title) then title=''
base = WIDGET_BASE(title=title, /column) 

wdid = cw_bgroup(base,['Yes','No'],/row,label_top=str, $
		uvalue="Wdyesno")
widget_control, base, /realize, tlb_set_xoffset=xpos,tlb_set_yoffset=ypos
XMANAGER, 'wdyesno', base, /modal	; <= modal is impotant

return,1-value
end

;+
;  wdgetstr.pro (function)
;	wdgetstr
;	'97/10/01	k.i.
;-
pro wdgetstr_event,ev
common wdgetstr,value
widget_control, ev.id, get_value=value, set_value=''
WIDGET_CONTROL, /destroy, ev.top
end

;--------------------------------------------------------
function wdgetstr,str,xpos=xpos,ypos=ypos,title=title
common wdgetstr,value

if not keyword_set(xpos) then xpos=0
if not keyword_set(ypos) then ypos=0
if not keyword_set(title) then title=''
base = WIDGET_BASE(title=title, /row) 

lab = widget_label(base,value=str,font=2)
wdid = widget_text(base,value='', uvalue='Wdgetstr',/edit)
widget_control, base, /realize, tlb_set_xoffset=xpos,tlb_set_yoffset=ypos
XMANAGER, 'wdgetstr', base, /modal	; <= modal is impotant

return,value(0)
end

; pro_obs1.pro
@pro_obslib

;**************************************************************
pro probs_main_event, ev
;--------------------------------------------------------------
common probs, wd, pp

pp=probs_event(ev)
end

;************************************************************************
common probs, wd, pp
; wd	- widget structure
; pp	- control parameters

main = WIDGET_BASE(title='Prosilica Observation',/column)
wd = widget_probs(main)
wd.Exit=widget_button(main, value="Exit", uvalue = "Exit")
widget_control, main, /realize
;XMANAGER, 'probs_main', main, modal=modal
XMANAGER, 'probs_main', main


end


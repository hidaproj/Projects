;***************************************
pro wdtext,wdid,str
;  display string in widget, '96/11/20 k.i.

  if keyword_set(wdid) then $
	widget_control, wdid, set_value=str


end

pro test_event, eva

  widget_control, eva.id, get_uvalue=value
  if (n_elements(value) eq 0) then value = ''
  name = strmid(tag_names(eva, /structure_name), 7, 1000)
  print,'tag=',name,'  value=',value,eva

  if (value eq "EXIT") then begin
  	WIDGET_CONTROL, /destroy, eva.top
  	return
  endif
  return

end
;--------------------------------------------------------------
; widgets.pro

strings=['Kon-nichiwa','Hellow','Buenos','Ni-hao','Annyon-haseyo','Guten']

base = WIDGET_BASE(title='widget samples', /column) 	; base window

wd_utton = widget_button(base, value="Button", uvalue = "Button")
cw_bg = cw_bgroup(  base,strings,/row,label_top='cw_bgroup', $
	uvalue="Cw_bgroup",/no_release,font=3,ypad=0, $
	set_value=0,/exclusive,/frame)

wd_label = widget_label(base,value='Exposure')
wd_list = widget_list( base, value=strings, uvalue='Widget_list',/frame, $
	xsize=5,ysize=5,kill_notify='')
cw_bsel = cw_bselector( base,strings,label_left='cw_bselector', $
		uvalue="Cw_bselector",set_value=1)

wd_slider = widget_slider(base, minimum=0,maximum=20, $
	xsize=200,value=1,scroll=300, title='wd_slider', $
	/frame, suppress_value=1)
cw_fslid = cw_fslider(base, minimum=0.,maximum=20., $
	xsize=200,value=1, uvalue='Cw_slider', /drag, /edit, $
	title='cw_fslider', /frame, /suppress_value)
xmenu,strings,base,/row,/frame,uvalue=strings,	$
		title='xmenu',buttons=buttons,/exclusive
stringpd=['/Test/ {','/'+strings+'/','}','/AAA/']
xpdmenu,stringpd,base,/frame,title='xpdmenu'
st={pdst, flags:0, name:''}
ns=n_elements(strings)
desc=replicate(st,ns)
desc.name=strings &	desc(0).flags=1 &	desc(ns-1).flags=2
cw_pd = cw_pdmenu(base,desc,/return_name,uvalue='Cw_pdmenu')

Exita = widget_button(base, value="Exit", uvalue = "EXIT")

widget_control, base, /realize

XMANAGER, 'test', base


end

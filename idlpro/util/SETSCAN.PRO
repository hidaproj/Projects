;+
;  setscan.pro (function)
;-
pro setscan_event,ev
common setscan,wd_scan,pobs2

case ev.id of
    wd_scan.bOK:  begin
	WIDGET_CONTROL, /destroy, ev.top
	end
    wd_scan.Step: begin
	widget_control, ev.id, get_value=value, set_value=''
	pobs2.scan_dx=fix(value(0))
	widget_control, ev.id,  $
		set_value=string(pobs2.scan_dx,format='(i4)')
	print,'Step was set ',pobs2.scan_dx
	end
    wd_scan.Npoint: begin
	widget_control, ev.id, get_value=value, set_value=''
	pobs2.scan_nx=fix(value(0))
	widget_control, ev.id,  $
		set_value=string(pobs2.scan_nx,format='(i4)')
	print,'# of point was set ',pobs2.scan_nx
	end
endcase

end

function setscan,pobs
common setscan,wd_scan,pobs2

wd_scan={wd_scan,	$
	Step:	0l,	$
	Npoint:	0l,	$
	bOK:	0l	$
	}
pobs2=pobs

title='SetScan'
base = WIDGET_BASE(title=title, /column) 
	b_ss1=widget_base(base, /row )
	    lab = widget_label(b_ss1,value='Step       :',font=2)
	    wd_scan.Step = widget_text(b_ss1,	$
		value=string(pobs.scan_dx,format='(i3)'),	$
		 uvalue='Dx_set',xsize=5,/edit)
	    lab = widget_label(b_ss1,value='arcsec',font=2)
	b_ss2=widget_base(base, /row )
	    lab = widget_label(b_ss2,value='# of point:',font=2)
	    wd_scan.Npoint = widget_text(b_ss2,	$
		value=string(pobs.scan_nx,format='(i4)'), $
		 uvalue='Dx_set',xsize=5,/edit)
	    ;lab = widget_label(b_ss2,value='',font=2)
	wd_scan.bOK = widget_button(base, value="OK", uvalue="TAKE1")

widget_control, base, /realize
XMANAGER, 'SetScan', base, /modal	; <= modal is impotant

return,pobs2
end

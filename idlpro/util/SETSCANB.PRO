;+
;  setscanb.pro (function)
;-
pro setscanb_event,ev
common setscanb,wd_scan,pobs2

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
    wd_scan.Nrepeat: begin
	widget_control, ev.id, get_value=value, set_value=''
	pobs2.nrepeat=fix(value(0))
	widget_control, ev.id,  $
		set_value=string(pobs2.nrepeat,format='(i4)')
	print,'Nrepeat was set ',pobs2.nrepeat
	end
    wd_scan.dt: begin
	widget_control, ev.id, get_value=value, set_value=''
	pobs2.dt=fix(value(0))
	widget_control, ev.id,  $
		set_value=string(pobs2.dt,format='(i4)')
	print,'dt was set ',pobs2.dt
	end
    wd_scan.Mode: begin
	case ev.value of
	    0:	pobs2.mode=0
	    1:	pobs2.mode=1
	endcase
	;return
	end
endcase

end

function setscanb,pobs
common setscanb,wd_scan,pobs2

wd_scan={wd_scanb,	$
	Step:		0l,	$
	Npoint:		0l,	$
	Nrepeat:	0l,	$
	Dt:		0l,	$
	Mode:		0l,	$	; for sp(*,*,*)x 1 file
	bOK:		0l	$
	}
pobs2=pobs

title='SetScan'
base = WIDGET_BASE(title=title, /column) 
	b_ss1=widget_base(base, /row )
	    lab = widget_label(b_ss1,value='dx (step)         :',font=2)
	    wd_scan.Step = widget_text(b_ss1,	$
		value=string(pobs.scan_dx,format='(i3)'),	$
		 uvalue='Dx_set',xsize=5,/edit)
	    lab = widget_label(b_ss1,value='arcsec',font=2)
	b_ss2=widget_base(base, /row )
	    lab = widget_label(b_ss2,value='nx (# of pos.)  :',font=2)
	    wd_scan.Npoint = widget_text(b_ss2,	$
		value=string(pobs.scan_nx,format='(i4)'), $
		 uvalue='Dx_set',xsize=5,/edit)
	    ;lab = widget_label(b_ss2,value='',font=2)
	b_ss21=widget_base(base, /row )
	    lab = widget_label(b_ss21,value='# of repeat    :',font=2)
	    wd_scan.Nrepeat = widget_text(b_ss21,	$
		value=string(pobs.nrepeat,format='(i4)'), $
		 uvalue='Nrepeat',xsize=5,/edit)
	    ;lab = widget_label(b_ss2,value='',font=2)
	b_ss22=widget_base(base, /row )
	    lab = widget_label(b_ss22,value='waiting time  :',font=2)
	    wd_scan.Dt = widget_text(b_ss22,	$
		value=string(pobs.dt,format='(i4)'), $
		 uvalue='Dt',xsize=5,/edit)
	    lab = widget_label(b_ss22,value='sec',font=2)
	b_ss3=widget_base(base, /row )
	    lab = widget_label(b_ss3,value='Mode  :',font=1)
	    if pobs.mode eq 0 then value = 0 else value=1
    	    wd_scan.Mode = cw_bgroup(	$
		b_ss3,['sp(*,*,nx) x timeseq','sp(*,*) x nx'],/column, uvalue="Mode", $
		/no_release,set_value=value,/exclusive)
	wd_scan.bOK = widget_button(base, value="OK", uvalue="TAKE1")

widget_control, base, /realize
XMANAGER, 'SetScanb', base, /modal	; <= modal is impotant

return,pobs2
end

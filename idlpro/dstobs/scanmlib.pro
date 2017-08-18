; scanmlib.pro
; library for controling scan mirror via PAT-001 and "Comm32.dll"
;   '09/05/06  k.i.   from gblibh.pro
@patlib

;*************************************************************************
pro sminit,ComInit,Nomk5=Nomk5
;-------------------------------------------------------------------------
common scanmlib,SwAmp,Pos,aspp,Speeds,Trig

; aspp -- arcsec/pulse

if not keyword_set(ComInit) then begin
	;mkinit,'COM1',100,10000,150,bps=9600	; for 25-cm MK-5
	patinit,'COM1',100,3500,150,bps=9600	; for DST PAT-001
	ComInit=1
endif

scl=128./20.		; image scale, arcsec/mm
D_dst=600.		; DST aperture
D_pup=13.		; pupil size
pt=0.0025d/180.*!dpi	; radian/step for scan mirror (half step)
aspp=pt*D_pup/D_dst/!dpi*180.*3600
print,aspp,' arcsec/pulse'

SwAmp=200	; Swing amplitude (")
if keyword_set(Nomk5) then begin
	Pos=99
	return
endif
;wait,0.5
patstat,/nodisp
patstat,pn,st
Pos=aspp*pn
print,'SM-Pos=',Pos
patcom,'O:1'

end

;*************************************************************************
function smpos,next_pos,gwait=gwait	; move GB to next_pos
;-------------------------------------------------------------------------
common scanmlib,SwAmp,Pos,aspp,Speeds,Trig

if not keyword_set(gwait) then gwait=0
if n_elements(next_pos) then begin
	patpos,next_pos/aspp,gwait=gwait
	Pos=next_pos
endif 
return,Pos
end

;*************************************************************************
function widget_gb,base
;-------------------------------------------------------------------------
;   	create widget for controling Glass Block
;	return widget ID in wd_gb
;  base     ---  base window
common scanmlib,SwAmp,Pos,aspp,Speeds,Trig

Speeds=[3500,100]
SwAmp=250
Trig=0
pmin=-150 &	pmax=150 &	dpos=1	; arcsec
Poss=pmin+findgen((pmax-pmin)/dpos+1)*dpos
wd_gb={wd_gb,	$
	Pos_txt:	0l,	$
	Pos_set:	0l,	$
	Swing_Start:	0l,	$
	Swing_Stop:	0l,	$
	Swing_Ampl:	0l,	$
	Swing_Speed1:	0l,	$
	Swing_Speed2:	0l,	$
	Trig:		0l,	$
	MkStat:		0l,	$
	Origin:		0l,	$
	Exit:		0l,	$
	Poss:		Poss	$
	}

;--------------  MK-5 ---------------
lab = widget_label(base,value='>> Scan Mirror <<');,font='Bold')
base2 = widget_base(base, /column, /frame )	
b_gb0 = widget_base(base2, /row )	
	lab = widget_label(b_gb0,value='Speed: ');,font=1)
	wd_gb.Swing_Speed1 = widget_text(	$
		b_gb0,value=string(Speeds[0],format='(i5)'), $
		uvalue='Swing_Speed1', xsize=5, /edit)
	wd_gb.Swing_Speed2 = widget_text(	$
		b_gb0,value=string(Speeds[1],format='(i5)'), $
		uvalue='Swing_Speed2', xsize=5, /edit)
	lab = widget_label(b_gb0,value=' pps,  Trig:');,font=0)
	wd_gb.Trig = cw_bgroup(b_gb0,['off','on'],/exclusive,/row,/no_release,set_value=0)
b_gb = widget_base(base2, /row )	
    b_gb1=widget_base(b_gb, /column,/frame)
	b_gb1a=widget_base(b_gb1, /row)
	    lab = widget_label(b_gb1a,value='Pos.:  ');,font=1)
	    wd_gb.Pos_txt = widget_text(	$
		b_gb1a,value=string(Pos,format='(i4)'), $
		uvalue='Pos_set', xsize=5, /edit)
	    lab = widget_label(b_gb1a,value=' arcsec');,font=0)
	b_gb1b=widget_base(b_gb1, /row)
	    dmy=min(abs(wd_gb.Poss-pos),is)
	    wd_gb.Pos_set  = widget_slider( 	$
		b_gb1b, value=is, uvalue='Pos_set', $
		minimum=0, maximum=n_elements(Poss)-1, xsize=180, $
		suppress=1, vertical=0, frame=50, /drag )
    b_gb2=widget_base(b_gb, /column,/frame)
	b_gb2a=widget_base(b_gb2, /row)
	    lab = widget_label(b_gb2a,value='Swing:  ');,font=1)
	    wd_gb.Swing_Ampl = widget_text(	$
		b_gb2a,value=string(SwAmp,format='(i3)'), $
		uvalue='Swing_Ampl', xsize=3, /edit)
	    lab = widget_label(b_gb2a,value=' "');,font=0)
	b_gb2b=widget_base(b_gb2, /row)
    	wd_gb.Swing_Start = widget_button(b_gb2b, value="Start", uvalue="SSTART")
    	wd_gb.Swing_Stop = widget_button(b_gb2b, value="Stop", uvalue="SStop")
b_gbb = widget_base(base2, /row )	
   	wd_gb.MkStat = widget_button(b_gbb, value="MkStat", uvalue="MkStat")
  	wd_gb.Origin = widget_button(b_gbb, value="Origin", uvalue="Origin")
  	wd_gb.Exit = widget_button(b_gbb, value="Exit", uvalue="Exit")
return,wd_gb

end

;*************************************************************************
pro wdg_set, wdg, pos=pos0, SwAmp=Amp, Speeds=Speeds0
;-------------------------------------------------------------------------
;  set Scan Mirror position widget
;	wdg  -- Scan Mirror Block widgets
common scanmlib,SwAmp,Pos,aspp,Speeds,Trig

if keyword_set(Amp) then SwAmp=Amp
if n_elements(pos0) ne 0 then Pos=pos0
if n_elements(Speeds0) ne 0 then Speeds=Speeds0
widget_control, wdg.Pos_txt, set_value=string(Pos,format='(i4)')
dmy=min(abs(wdg.poss-Pos),il1)
widget_control, wdg.Pos_set, set_value=il1
widget_control, wdg.Swing_Ampl, set_value=string(SwAmp,format='(i3)')
widget_control, wdg.Swing_Speed1, set_value=string(Speeds[0],format='(i5)')
widget_control, wdg.Swing_Speed2, set_value=string(Speeds[1],format='(i5)')

end

;*************************************************************************
function sm_event, ev, wdg
;-------------------------------------------------------------------------
;  handle Glass Block
;	ev   -- event structure
;	wdg  -- Glass Block widgets
common scanmlib,SwAmp,Pos,aspp,Speeds,Trig

MK_s=100 &	MK_r=150	; initial speed,  acc. time (ms)
MK_sc=strcompress(string(MK_s),/remove_all)
MK_rc=strcompress(string(MK_r),/remove_all)
case ev.id of
    wdg.Origin: begin
	patorig
	Pos=0
	wdg_set,wdg
	end
    wdg.MkStat: begin
	patstat,pns,st
	print,'PN=',pns,',  st=',st
	end
    wdg.Trig: begin
	Trig=ev.value
	print,'Trigger set ',Trig
	end
    wdg.Swing_Stop: begin
	end
    wdg.Swing_Start: begin
	MK_fc1=strcompress(string(Speeds[0]),/remove_all)
	com1='D:1S'+MK_sc+'F'+MK_fc1+'R'+MK_rc;+',S'+MK_sc+',F'+MK_fc1+',R'+MK_rc	; max.
	MK_fc2=strcompress(string(Speeds[1]),/remove_all)
	com2='D:1S'+MK_sc+'F'+MK_fc2+'R'+MK_rc;+',S'+MK_sc+',F'+MK_fc2+',R'+MK_rc	; obs.
	while 1 do begin
	    ev1=widget_event(wdg.Swing_Stop,/nowait)
	    if ev1.id ne 0 then begin
		patstat,PN1,ask,/nodisp
		if strmid(ask,2,1) ne 'B' then begin
			patcom,com1
			if Trig then begin
			    patcom,'O:1' ;&	wait,0.1 &	patcom,'O:00'
			endif
			Pos=smpos(0,/gwait)
			wdg_set,wdg
			patstat
			return,Pos
		endif
	    endif
	    patcom,com1	; max
	    Pos=smpos(SwAmp/2,/gwait)
	    wdg_set,wdg
	    patstat,/nodisp ;& wait,0.5 & patstat & wait,0.5
	    patcom,com2	; obs.
	    if Trig then begin
		print,'Trig!'
		patcom,'O:0' ;&	wait,0.1 &	patcom,'O:00'
	    endif
	    Pos=smpos(-SwAmp/2,/gwait)
	    if Trig then begin
		patcom,'O:1' ;&	wait,0.1 &	patcom,'O:00'
	    endif
	    wdg_set,wdg
	    patstat,/nodisp ;& wait,0.5 & patstat & wait,0.5
	endwhile
	end
    wdg.Swing_Ampl: begin
	widget_control, ev.id, get_value=value, set_value=''
	SwAmp=fix(value(0))
	widget_control, ev.id, set_value=string(SwAmp,format='(i3)')
	print,'Swing Ampl. was set ',SwAmp
	end
    wdg.Swing_Speed1: begin
	widget_control, ev.id, get_value=value, set_value=''
	Speeds[0]=fix(value(0))
	widget_control, ev.id, set_value=string(Speeds[0],format='(i5)')
	print,'Swing Speed1 was set ',Speeds[0]
	MK_fc=strcompress(string(Speeds[0]),/remove_all)
	com1='D:1S'+MK_sc+'F'+MK_fc+'R'+MK_rc;+',S'+MK_sc+',F'+MK_fc+',R'+MK_rc
	patcom,com1
	end
    wdg.Swing_Speed2: begin
	widget_control, ev.id, get_value=value, set_value=''
	Speeds[1]=fix(value(0))
	widget_control, ev.id, set_value=string(Speeds[1],format='(i5)')
	print,'Swing Speed2 was set ',Speeds[1]
	end
    wdg.Pos_txt: begin
	widget_control, ev.id, get_value=value, set_value=''
	next_pos=fix(value(0))
	if next_pos gt 299 then next_pos=299
	if next_pos lt -299 then next_pos=-299
	Pos=smpos(next_pos,/gwait)
	wdg_set,wdg
	end
    wdg.Pos_set: begin
	next_pos=fix(wdg.Poss(ev.value)) ; for SLIDER
	Pos=smpos(next_pos,/gwait)
	wdg_set,wdg
	end
    wdg.Exit: begin
	WIDGET_CONTROL, /destroy, ev.top
	end
endcase

return,Pos
end


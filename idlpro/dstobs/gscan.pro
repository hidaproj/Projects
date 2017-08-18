;  gscan.pro
@gblibh
@as2pn_dst

;** MODIFICATION HISTORY **
function version
ver='1.0'	; '08/07/12	k.i. 
; MK5不安定
; RS232C bps  9600 --> 1200 にすると改善するもじきにハングアップ
;	RS232C ケーブル交換で解消

return,ver
end

;*************************************************************************
;NAME       : gscan event handler (procedure)
;FUNCTION   : DST observation
;PROGRAMMER : k.i. 
; ========================================================================
pro gscan_event, eve

common gscan_com, p, wd, h, img

widget_control, eve.id, get_uvalue=value
if (n_elements(value) eq 0) then value = ''
name = strmid(tag_names(eve, /structure_name), 7, 1000)
;print,'tag=',name,'  value=',value
;help,eve,/st


;--------------- MK-5 control -----------------------------
l=wd.gb
wid_gb=[l.Pos_txt,l.Pos_set,l.MkStat,l.Origin,	$
	l.Swing_Start,l.Swing_Stop,l.Swing_Ampl, $
	l.Swing_Speed1,l.Swing_Speed2,l.Trig,l.Exit]
i = where(eve.id eq wid_gb)
if i(0) ne -1 then begin
	dmy = gb_event( eve, wd.gb )
	return
endif


;--------------- Obs control -----------------------------
case (eve.id) of
    wd.obs.Start: begin
	print,'Obs. Start..'
	return
	end
    wd.obs.Stop: begin
	print,'Obs. Stop..'
	return
	end
    wd.obs.Exit: begin
	WIDGET_CONTROL, /destroy, eve.top
	return
	end
    else: 
endcase

return

Term:	; terminate scan
	print,'Scan terminated...'
    	gbp=gbpos(0,/gwait)
	wdg_set,wd.gb,pos=gbp
	return


end

;*************************************************************************
function widget_obs,base
;-------------------------------------------------------------------------
;   	create widget for controling Observation
;	return widget ID in wd_obs
;  base     ---  base window

wd_obs={wd_obs,	$
	Start:		0l,	$
	Stop:		0l,	$
	Exit:		0l	$
	}
;--------------  Obs setting ---------------
lab = widget_label(base,value='>> Observation <<',font=2)
b_obs = widget_base(base, /row, /frame )	
   	wd_obs.Start = widget_button(b_obs, value="Start", uvalue = "START")
   	wd_obs.Stop = widget_button(b_obs, value="Stop", uvalue = "STOP")
   	wd_obs.Exit = widget_button(b_obs, value="Exit", uvalue = "EXIT")

return,wd_obs
end

;*************************************************************************
;NAME       : gscan (main)
;FUNCTION   : DST observation
; ========================================================================
;pro gscan

common gscan_com, p, wd, h, img
common gblib,SwAmp,Pos,as2pnp,Speeds,Trig


;-------  get setting parameters and initilize system  ------------
gbinit,ComInit	; after execution, ComInit is set to 1
dpn=as2pnp[1]-as2pnp[0]
print,'1arcsec=',dpn,'pulse'

;-------  create widgets and get ID structure  ---------------------------
base = WIDGET_BASE(title='Hida DST Obs. (Ver.'+version()+')', /column) 
;wd_obs = widget_obs(base)
wd_gb  = widget_gb(base)
wdg_set,wd_gb,SwAmp=128
wd = {widgets,	$	;---- widgets
	gb:	wd_gb	$
;	obs:	wd_obs	$
	}
widget_control, base, /realize
XMANAGER, 'gscan', base

;-------  end procedures  ------------------------------------------------

end

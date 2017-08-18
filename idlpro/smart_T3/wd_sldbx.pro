;+
; FUNCTION:
;	create slide box with text window
; KEYWORDS
;	values	-- value string array
;	init	-- initial value
; RETURN:
;	wd.Txt & wd.Sld
; HISTORY:
;	98/05/05  k.i.	row keyword
;	99/12/03  k.i.	ht_sldbx
;	04/03/12  k.i.  if n_elements(base) eq 0 then return,wd
;	07/07/10  k.i.  if not keyword_set(name), bug

;-
;*******************************************************************
function ht_sldbx,ev,wd,valarr,setv,format=form,hit=hit
;  handle the event on wd_sldbx
;	ev	- event
;	wd	- widget struct made by wd_sldbx
;	valarr	- array of variable
;	setv	- set value
;	hit	- set 1 if ev hits
;	return 1 if ev hits the wd, 0 if not
;   USAGE:
;	if ht_sldbx(ev,wd,valarr,setv,format=format) then val=setv

hit=0
if not keyword_set(form) then form=''
if ev.id eq wd.sld then begin
	setv=valarr(ev.value) ; for SLIDER
	widget_control, wd.txt,set_value=string(setv,format=form)
	hit=1
endif
if ev.id eq wd.txt then begin
	widget_control, ev.id, get_value=value, set_value=''
	s=size(valarr) &	type=s(s(0)+1)
	case type of
	    1: setv=byte(value)
	    2: setv=fix(value)
	    3: setv=long(value)
	    4: setv=float(value)
	    5: setv=double(value)
	    6: setv=complex(value)
	endcase
	setv=setv[0]
	dmy=min(abs(valarr-setv),i1)
	widget_control, wd.sld, set_value=i1
	widget_control, wd.txt,set_value=string(setv,format=form)
	hit=1
endif
return,hit

end

;*******************************************************************
function wd_sldbx,base,name=name,values=values,unit=unit,size=size, $
	edit=edit,frame=frame,init=init,row=row,format=form,iinit=iinit,nobase=nobase
;	init	- initial string value
;	iinit	- initial i for values(*)
wd={wd_sldbox,	Txt:	0l,	Sld:	0l	}
if n_elements(base) eq 0 then return,wd

if not keyword_set(frame) then frame=0
if not keyword_set(edit) then edit=0
if not keyword_set(init) then init=values(0)
if not keyword_set(size) then size=100
if not keyword_set(format) then form=''
if not keyword_set(name) then name=''
nlen=max(strlen(values))


cinit=string(init,form=form)
if keyword_set(row) then b_0=widget_base(base, /row, frame=frame) $
else	b_0=widget_base(base, /column, frame=frame)
if keyword_set(nobase) then b_0=base

	b_1=widget_base(b_0, /row)
	    lab = widget_label(b_1,value=name,/align_left);,xsize=strlen(name));,font=1)
	    if keyword_set(iinit) then init=values(iinit)
	    wd.Txt = widget_text(b_1,value=init,uvalue='Txt_set', $
		xsize=nlen, edit=edit)
	    if keyword_set(unit) then $
		lab = widget_label(b_1,value=unit);,font=0)
	b_2=widget_base(b_0, /row)
	    if n_elements(iinit) ne 0 then i=iinit $
	    else begin
		ii=where(init eq values) &	i=max([ii(0),0])
	    endelse
	    wd.Sld  = widget_slider(b_2, value=i, uvalue='Sld_set', $
		minimum=0, maximum=n_elements(values)-1, xsize=size, $
		suppress=1, vertical=0, frame=7, /drag )

return,wd
end

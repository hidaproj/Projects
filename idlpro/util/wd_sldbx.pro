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
;-
function wd_sldbx,base,name=name,values=values,unit=unit,size=size, $
	edit=edit,frame=frame,init=init,row=row
if not keyword_set(frame) then frame=0
if not keyword_set(edit) then edit=0
if not keyword_set(init) then init=values(0)
nlen=max(strlen(values))


wd={wd_sldbox,	Txt:	0l,	Sld:	0l	}

if keyword_set(row) then b_0=widget_base(base, /row, frame=frame) $
else	b_0=widget_base(base, /column, frame=frame)
	b_1=widget_base(b_0, /row)
	    lab = widget_label(b_1,value=name,font=1)
	    wd.Txt = widget_text(b_1,value=init,uvalue='Txt_set', $
		xsize=nlen, edit=edit)
	    if keyword_set(unit) then $
		lab = widget_label(b_1,value=unit,font=0)
	b_2=widget_base(b_0, /row)
	    ii=where(init eq values) &	i=max([ii(0),0])
	    wd.Sld  = widget_slider(b_2, value=i, uvalue='Sld_set', $
		minimum=0, maximum=n_elements(values)-1, xsize=size, $
		suppress=1, vertical=0, frame=50, /drag )

return,wd
end

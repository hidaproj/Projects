;+
;   imghndl.pro
;	image handling routin for IDL obs
;	'96/09/21	k.i.
;	'97/04/18	k.i.	NewWin
;	'98/01/22	k.i.	Zoom
;-

;*************************************************************************
function data_event, ev, wdd, pd, h=h, img=img
;  handle data
;	ev   -- event structure
;	wdd  -- Data widgets
;	pd   -- parameter structure, 

case (ev.id) of

    wdd.Loadct: begin
	xloadct
	end
    wdd.Profil: begin
	profiles,img
	end
    wdd.Erase: begin
	erase
	end
    wdd.Zoom: begin
	zoom,fact=6,/conti
	end
    wdd.Wshow: begin
	wshow
	end
    wdd.NewWin: begin
	i_win=!d.window
	;print,i_win
	if i_win eq 0 then begin
		wx=!d.x_size
		wy=!d.y_size
		window,1,xsize=wx,ysize=wy
	endif
	if i_win eq 1 then begin
		wset,0
		wshow,0
	endif
	end
    wdd.Dir: begin
	widget_control, ev.id, get_value=value, set_value=''
	pd.Dir=value(0)
	widget_control, ev.id, set_value=pd.dir
	print,'Dir was set ',pd.dir
	end
    wdd.File1: begin
	widget_control, ev.id, get_value=value, set_value=''
	pd.file1=value(0)
	widget_control, ev.id, set_value=pd.file1
	print,'File1 was set ',pd.file1
	end
    wdd.Fnam: begin
	widget_control, ev.id, get_value=value, set_value=''
	pd.fnam=value(0)
	widget_control, ev.id, set_value=pd.fnam
	print,'fnam was set ',pd.fnam
	end
    wdd.Seqno: begin
	widget_control, ev.id, get_value=value, set_value=''
	pd.seqno=fix(value(0))
	widget_control, ev.id, set_value=string(pd.seqno,format='(i4.4)')
	print,'seqno was set ',pd.seqno
	end
    wdd.Ext: begin
	widget_control, ev.id, get_value=value, set_value=''
	pd.Ext=value(0)
	widget_control, ev.id, set_value=pd.ext
	print,'ext was set ',pd.ext
	end
    wdd.Save1: begin
	nimg=n_elements(h)
	if nimg eq 1 then begin
		nkrsave,pd.dir+'\'+pd.file1,h,img
	endif else begin
		h.nset=nimg
		nkrsave,pd.dir+'\'+pd.file1,h(0),img(*,*,0) 
		for i=1,nimg-1 do $
			nkrsave,pd.dir+'\'+pd.file1,h(i),img(*,*,i),/append 
	endelse
	print,'image was saved to ',pd.dir+'\'+pd.file1
	end
    wdd.SaveSeq: begin
	file=pd.dir+'\'+pd.fnam+'_'+string(pd.seqno,format='(i4.4)')+'.'+pd.Ext
	nkrsave,file,h,img
	print,'image was saved to ',file
	pd.seqno=pd.seqno+1
	widget_control, wdd.Seqno, set_value=string(pd.seqno,format='(i4.4)')
	end
    else: 
endcase

return,pd
end

;*************************************************************************
function widget_data,base,p_data
;-------------------------------------------------------------------------
;   	create widget for handling image data
;	return widget ID in wd_data
;  base     ---  base window
;  p_data   ---  give initial set

wd_data={wd_data,	$
	Loadct:		0l,	$
	Profil:		0l,	$
	Wshow:		0l,	$
	Erase:		0l,	$
	Zoom:		0l,	$
	NewWin:		0l,	$
	Dir:		0l,	$
	File1:		0l,	$
	Fnam:		0l,	$
	Seqno:		0l,	$
	Ext:		0l,	$
	Save1:		0l,	$
	SaveSeq:	0l	$
	}
;--------------  image handling ---------------
lab = widget_label(base,value='>> Image Handling <<',font=2)
b_data=widget_base(base, /column, /frame )
    bx=62
    ;bx=50
    b_data1=widget_base(b_data, /row )
	wd_data.Loadct = widget_button(	$
		b_data1, value="Color", uvalue = "Loadct", xsize=bx)
	wd_data.Profil = widget_button(	$
		b_data1, value="Profile", uvalue = "Profile", xsize=bx)
	wd_data.Zoom = widget_button(	$
		b_data1, value="Zoom", uvalue = "Zoom", xsize=bx)
	wd_data.Wshow = widget_button(	$
		b_data1, value="Show", uvalue = "Wshow", xsize=bx)
	wd_data.Erase = widget_button(	$
		b_data1, value="Erase", uvalue = "Erase", xsize=bx)
	wd_data.NewWin = widget_button(	$
		b_data1, value="NewWin", uvalue = "NewWin", xsize=bx)
    b_data15=widget_base(b_data, /row, /frame )
    b_data2=widget_base(b_data15, /column, /frame )
	    xsize=15
	b_data20=widget_base(b_data2, /row )
	    lab = widget_label(b_data20,value='Dir      :',font=2)
	    wd_data.Dir = widget_text(b_data20,	$
		value=p_data.dir, uvalue='Dir_set',xsize=24,/edit)
	b_data21=widget_base(b_data2, /row )
	    lab = widget_label(b_data21,value='File1   :',font=2)
	    wd_data.File1 = widget_text(b_data21,	$
		value=p_data.file1, uvalue='File1',xsize=xsize,/edit )
	b_data22=widget_base(b_data2, /row )
	    lab = widget_label(b_data22,value='Fnam :',font=2)
	    wd_data.Fnam = widget_text(b_data22,	$
		value=p_data.fnam, uvalue='Fnam_set',xsize=4,/edit)
	    lab = widget_label(b_data22,value='Seq # :',font=2)
	    wd_data.Seqno = widget_text(b_data22,	$
		value=string(p_data.seqno,format='(i4.4)'),	$
		uvalue='Seq_set',xsize=5,/edit)
	    lab = widget_label(b_data22,value='   Ext :',font=2)
	    wd_data.Ext = widget_text(b_data22,	$
		value=p_data.ext, uvalue='Ext_set',xsize=3,/edit)
    b_data3=widget_base(b_data15, /column )
	    wd_data.Save1 = widget_button(	$
		b_data3, value="Save1", uvalue = "Save1",	$
		xsize=60, yoffset=-20)
	    wd_data.SaveSeq = widget_button(	$
		b_data3, value="SaveS", uvalue = "SaveSeq",	$
		xsize=60)
return,wd_data
end
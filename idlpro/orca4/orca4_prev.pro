@orcalib
pro orca4_prev_event,ev
common orca4_prev,draw,plotx,ploty,base_img,mxmi,nx,ny,p,dark,img0

widget_control,ev.id,get_value=value,get_uvalue=uvalue
case uvalue of
	;----- preview -----
	'prev': begin

		;---------- Obtain the window index ----------
		WIDGET_CONTROL, draw, GET_VALUE = index
		WIDGET_CONTROL, plotx, GET_VALUE = index_x
		WIDGET_CONTROL, ploty, GET_VALUE = index_y
		;---------- Set the new widget to be the current graphics window ----------
		uvalue=''
		;print,'START -> Click !!'
		;---------- Set the new widget to be the current graphics window ----------
		WSET, index
		img=OrcaObs(nimg=nimg)
		img=float(img)-float(dark)
		tvscl,rebin(img,nx,ny)
		xyouts,10,10,size=2,'START -> Click !!',/dev
		cursor,xx,yy,/dev,/down
		uvalue=''
		while (uvalue ne "STOP") do begin
			event = widget_event(base_img,/nowait)
			if event.id ne 0 then WIDGET_CONTROL, get_uvalue=uvalue, event.id
					tt=get_therm_temp(ch=0,time=time)
					WSET, index
					img=OrcaObs(nimg=nimg)
					img=float(img)-float(dark)
					mxmi=[0,max(img[*,yy,0])>max(img[xx,*,0])>mxmi[1]]
					tvscl,rebin(img,nx,ny)
					plots,[0,nx-1],[yy,yy],line=1,/dev
					plots,[xx,xx],[0,ny-1],line=1,/dev
					xyouts,10,10,/dev,size=1.8,time+'   TMP: '+string(tt,form='(f6.2)')+'C'
					wset,index_x
					plot,(rebin(img,nx,ny))[*,yy],/xstyle,yr=mxmi,title='X profile (@y='+strtrim(yy,2)+' pixel)'
					wset,index_y
					plot,(rebin(img,nx,ny))[xx,*],/xstyle,yr=mxmi,title='Y profile (@x='+strtrim(xx,2)+' pixel)'
		endwhile
		   end

	;----- get image -----
	'gimg': begin
		WIDGET_CONTROL, plotx, GET_VALUE = index_x
		WIDGET_CONTROL, ploty, GET_VALUE = index_y
		;---------- Obtain the window index ----------
		WIDGET_CONTROL, draw, GET_VALUE = index
		WIDGET_CONTROL, plotx, GET_VALUE = index_x
		WIDGET_CONTROL, ploty, GET_VALUE = index_y
		;---------- Set the new widget to be the current graphics window ----------
		uvalue=''
		WSET, index
					img0=OrcaObs(nimg=nimg)
					img=float(img0)-float(dark)
					xx=nx/2.
					yy=ny/2.
					tt=get_therm_temp(ch=0,time=time)
					mxmi=[0,max(img[*,yy,0])>max(img[xx,*,0])>mxmi[1]]
					tvscl,rebin(img,nx,ny)
					xyouts,10,10,/dev,size=1.8,time+'   TMP: '+string(tt,form='(f6.2)')+'C'
					wset,index_x
					plot,(rebin(img,nx,ny))[*,yy],/xstyle,yr=mxmi,title='X profile peak='$
										+string(max((rebin(img,nx,ny))[*,yy],mp),form='(f5.1)')+', '+strtrim(mp,2)
					wset,index_y
					plot,(rebin(img,nx,ny))[xx,*],/xstyle,yr=mxmi,title='Y profile (@x='+strtrim(xx,2)+' pixel)'
		   end
	;----- save -----
	 'SAVE': begin
			;save_data,f,g,img,dark,g.sdir
		 end

	;----- EXIT -----
	 'EXIT': begin
		 widget_control,/destroy,ev.top
			OrcaFin
		 end

end

;pro orca4_prev


common orca4_prev,draw,plotx,ploty,base_img,mxmi,nx,ny,p,dark,img0


p=orcainit()
expo= 0.01
bin=2
nimg=1
p=OrcaSetParam(expo=expo,bin=bin)


base=widget_base(title='tf_ctl1',/row)
	base_ctl=widget_base(base,/column)

	ibase=widget_base(base_ctl,/row,/align_center)
	prev=widget_button(ibase,value="preview",uvalue='prev',font='Arial')
	img=widget_button(ibase,value="GET img",uvalue='gimg',font='Arial')

	;----- save -----
;	sfile = widget_text(base_ctl,value=string(sldv7,form='(f5.1)'),uvalue='Txt_set7',$
;				xsize=7, /edit,font='Arial')
;	sv=widget_button(base_ctl,value="SAVE",uvalue='SAVE',font='Arial')

	;----- EXIT -----
	bt=widget_button(base_ctl,value="EXIT",uvalue='EXIT',font='Arial')


nx=1024/2 & ny=1024/2
if not keyword_set(mxmi) then mxmi=[0,100]
	base_img = WIDGET_BASE(base,/column,xoffset=512,yoffset=512)
	xpdmenu,['/START/','/STOP/'],base_img,/frame,title='irprev2'
	draw = WIDGET_DRAW(base_img, XSIZE = nx, YSIZE = ny)
	base_img2 = WIDGET_BASE(base_img,title='DRAW',/row)
	plotx = WIDGET_DRAW(base_img2, XSIZE = nx/2, YSIZE = ny/2)
	ploty = WIDGET_DRAW(base_img2, XSIZE = nx/2, YSIZE = ny/2)

	widget_control,base,/realize
	xmanager,'orca4_prev',base
end
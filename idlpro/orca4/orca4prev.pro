@orcalib
@tf_lib

pro orca4prev_event,ev
common orca4prev,draw,plotx,ploty,base_img,nx,ny,mxmi,sdir,img0

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
		img=float(img)
		tvscl,rebin(img,nx,ny)
		xyouts,10,10,size=2,'START -> Click !!',/dev
		cursor,xx,yy,/dev,/down
		uvalue=''
		while (uvalue ne "STOP") do begin
			event = widget_event(base_img,/nowait)
			if event.id ne 0 then WIDGET_CONTROL, get_uvalue=uvalue, event.id
					WSET, index
					img=OrcaObs(nimg=nimg)
					img=float(img)
					mxmi=[0,max(img[*,yy,0])>max(img[xx,*,0])>mxmi[1]]
					tvscl,rebin(img,nx,ny)
					plots,[0,nx-1],[yy,yy],line=1,/dev
					plots,[xx,xx],[0,ny-1],line=1,/dev
					;xyouts,10,10,/dev,size=1.8,time
					wset,index_x
					plot,(rebin(img,nx,ny))[*,yy],/xstyle,yr=mxmi,title='X profile (@y='+strtrim(yy,2)+' pixel)'
					wset,index_y
					plot,(rebin(img,nx,ny))[xx,*],/xstyle,yr=mxmi,title='Y profile (@x='+strtrim(xx,2)+' pixel)'
		endwhile
		   end

	;----- get image -----
	'gimg': begin
		;---------- Obtain the window index ----------
		WIDGET_CONTROL, draw, GET_VALUE = index
		WIDGET_CONTROL, plotx, GET_VALUE = index_x
		WIDGET_CONTROL, ploty, GET_VALUE = index_y
		;---------- Set the new widget to be the current graphics window ----------
		uvalue=''
		WSET, index
					img0=OrcaObs(nimg=nimg)
	help,img0
					img=float(img0)
					xx=nx/2.
					yy=ny/2.
					mxmi=[0,max(img[*,yy,0])>max(img[xx,*,0])>mxmi[1]]
					tvscl,rebin(img,nx,ny)
					wset,index_x
					plot,(rebin(img,nx,ny))[*,yy],/xstyle,yr=mxmi,title='X profile peak='$
							+string(max((rebin(img,nx,ny))[*,yy],mp),form='(f5.1)')+', '+strtrim(mp,2)
					wset,index_y
					plot,(rebin(img,nx,ny))[xx,*],/xstyle,yr=mxmi,title='Y profile (@x='+strtrim(xx,2)+' pixel)'
		end

	;----- save -----
	 'SAVE': begin
			time=get_tim()
			stim=strjoin(str_sep((str_sep(time,' '))[0],'/'))+'_'+strjoin(str_sep((str_sep(time,' '))[1],':'))
			ofile=sdir+'orca_'+stim+'.dat'
help,img0
			save,filename=ofile,img0
		 end


	;----- EXIT -----
	 'EXIT': begin
		 widget_control,/destroy,ev.top
			OrcaFin
		 end
endcase
end

;pro orca4prev
common orca4prev,draw,plotx,ploty,base_img,nx,ny,mxmi,sdir,img0



sdir='C:\Projects\data\orca\'+today()+'\'
spawn,'mkdir '+sdir

p=orcainit()
expo= 0.1
bin=1
nimg=1
p=OrcaSetParam(expo=expo,bin=bin)

base=widget_base(title='orca4_prev',/row)
	base_ctl=widget_base(base,/column)

	ibase=widget_base(base_ctl,/row,/align_center)

	prev=widget_button(ibase,value="preview",uvalue='prev',font='Arial')
	img=widget_button(ibase,value="GET img",uvalue='gimg',font='Arial')

	;----- save -----
;	sfile = widget_text(base_ctl,value=string(sldv7,form='(f5.1)'),uvalue='Txt_set7',$
;				xsize=7, /edit,font='Arial')
	sv=widget_button(base_ctl,value="SAVE",uvalue='SAVE',font='Arial')

	;----- EXIT -----
	bt=widget_button(base_ctl,value="EXIT",uvalue='EXIT',font='Arial')

nx=1024/2 & ny=1024/2
if not keyword_set(mxmi) then mxmi=[0,100]
	base_img = WIDGET_BASE(base,/column,xoffset=1024/2,yoffset=1024/2)
	xpdmenu,['/START/','/STOP/'],base_img,/frame,title='irprev2'
	draw = WIDGET_DRAW(base_img, XSIZE = nx, YSIZE = ny)
	base_img2 = WIDGET_BASE(base_img,title='DRAW',/row)
	plotx = WIDGET_DRAW(base_img2, XSIZE = nx/2, YSIZE = ny/2)
	ploty = WIDGET_DRAW(base_img2, XSIZE = nx/2, YSIZE = ny/2)

	widget_control,base,/realize
	xmanager,'orca4prev',base
end
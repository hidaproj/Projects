@tf_lib

pro tf_show,time
common tf_ctl2,wd,f,g,p,dark,img0, tmp,dat_dir
		;---------- Obtain the window index ----------
		WIDGET_CONTROL, wd.draw, GET_VALUE = index
		WIDGET_CONTROL, wd.plotx, GET_VALUE = index_x
		WIDGET_CONTROL, wd.ploty, GET_VALUE = index_y
		;---------- Set the new widget to be the current graphics window ----------
		uvalue=''
		WSET, index
			nx=g.imgsize[0]/4 & ny=g.imgsize[1]/4
			if keyword_set(dark) then img=abs(float(img0)-float(dark)) else img=abs(float(img0))
			xx=nx/2.
			yy=ny/2.
			;mxmi=[0,max(img[*,yy,0])>max(img[xx,*,0])]
			mxmi=[0,max(img)]
			tvscl,sqrt(rebin(img,nx,ny))
			xyouts,10,10,/dev,size=1.8,time+'   TMP: '+string(tmp,form='(f6.2)')+'C'
			wset,index_x
			plot,(rebin(img,nx,ny))[*,yy],/xstyle,yr=mxmi,title='X profile peak='$
				+string(max((rebin(img,nx,ny))[*,yy],mp),form='(f6.1)')+', '+strtrim(mp,2)
			wset,index_y
			plot,(rebin(img,nx,ny))[xx,*],/xstyle,yr=mxmi,title='Y profile (@x='+strtrim(xx,2)+' pixel)'
end
;*************************************************************************************
pro set_sld,hsld,htxt,hvolt,value
common tf_ctl2,wd,f,g,p,dark,img0, tmp,dat_dir
			widget_control,htxt,set_value=string(value,format='(F5.1)')
			widget_control,hsld,get_uvalue=uv
				bi=fix(strmid(uv,3,1))-1
				bl=f.b[bi].bname
				 block7,bl,fsr,elem,cthick,lc,ch,usb
				  f.b[bi].fsr=fsr
				  f.b[bi].elem=elem
				  f.b[bi].cthick=cthick
				ret=value/360.
				;;;tmp=get_therm_temp(ch=0,time=time)
				v=lc_volt_fit(LC,f.wl0,ret,tmp)
;				;;;lcvolt,ch,v,USBx=usb
				;;;wait,0.1
					f.b[bi].temp=tmp
					f.b[bi].ret_offset=ret
					f.b[bi].volt=v
			widget_control,hvolt,set_value=string(v[0],form='(f5.3)')
end
;*************************************************************************************
pro set_txt,hsld,htxt,hvolt,value
common tf_ctl2,wd,f,g,p,dark,img0, tmp,dat_dir
			widget_control,hsld,set_value=fix(value)
			widget_control,htxt,get_uvalue=uv
				bi=fix(strmid(uv,3,1))-1
				bl=f.b[bi].bname
				 block7,bl,fsr,elem,cthick,lc,ch,usb
				  f.b[bi].fsr=fsr
				  f.b[bi].elem=elem
				  f.b[bi].cthick=cthick
					ret=value/360.
				;;;tmp=get_therm_temp(ch=0,time=time)
				v=lc_volt_fit(LC,f.wl0,ret,tmp)
				;;;lcvolt,ch,v,USBx=usb
				;;;wait,0.1
					f.b[bi].temp=tmp
					f.b[bi].ret_offset=ret
					f.b[bi].volt=v
			widget_control,hvolt,set_value=string(v[0],form='(f5.3)')
end

;*************************************************************************************
pro pal_shift,sh
common tf_ctl2,wd,f,g,p,dark,img0, tmp,dat_dir
			bls=['block6','block5','block7','block4','block3','block2','block1']
			bunbo=[1.,2.,-4.,-8.,-16.,-32.,64.]
			bli=[5,4,6,3,2,1,0]
			s=sort(bls) & bls=bls[s] & bunbo=bunbo[s] & bli=bli[s]
			for iii=0,7-1 do begin
				block7,bls[iii],fsr,elem,cthick,lc,ch,usb
				dret=(sh/360.)/bunbo[iii]
				if abs(dret) gt 1.0 then dret=dret-float(round(dret))
				ret1=f.b[bli[iii]].ret_offset/360.+dret
				;;;tmp=get_therm_temp(ch=0,time=time)
				v=lc_volt_fit(LC,f.wl0,ret1,tmp)
				;;;lcvolt,ch,v,USBx=usb
					f.b[iii].fsr=fsr
					f.b[iii].elem=elem
					f.b[iii].cthick=cthick
					f.b[iii].temp=tmp
					f.b[iii].ret=ret1
					f.b[iii].volt=v
				widget_control,wd.hsld[iii],set_value=string(f.b[iii].ret*360.,form='(f5.1)')
				widget_control,wd.htxt[iii],set_value=string(f.b[iii].ret*360.,form='(f5.1)')
				widget_control,wd.hvolt[iii],set_value=string(f.b[iii].volt,form='(f5.3)')
			endfor
				;;;wait,0.1
end

;*************************************************************************************
pro tf_ctl2_event,ev
common tf_ctl2,wd,f,g,p,dark,img0, tmp,dat_dir
widget_control,ev.id,get_value=value,get_uvalue=uvalue

case uvalue of
;----- Wavelen -----
	 'wl': begin
			wl=value
			f.wl=value
			case (str_sep(strtrim(wl,2),'.'))[0] of
				'656' :  g.gpos='18 31'
				'517' :  g.gpos='13 23'
				'854' :  g.gpos='26 12'
				'1083':  g.gpos='35 58'
				else  :  g.gpos=''
			endcase
		 end
;----- TEMP -----
	 'temp': begin
				;;;tmp=get_therm_temp(ch=0,time=time)
				widget_control,wd.htemp,set_value=string(tmp,form='(f5.1)')
		 end
;----- cam set param -----
	'cam_set':begin
			widget_control,wd.hcam_e,get_value=expo
			g.exp=expo
			widget_control,wd.hcam_bin,get_value=bin
			g.binx=bin & g.biny=bin
			g.imgsize=[2048,2048]/bin
			;print,g.exp,g.binx
			;;;p=OrcaSetParam(expo=g.exp,bin=g.binx)
		  end
;----- dark -----
	'dark':begin
		print,'Dark!!'
		;;;dark=OrcaObs(nimg=1)
		;;;save_data,f,g,dark,g.sdir,/dark
	       end

;----- load table -----
	'load':begin
			
			readtxt,dat_dir+'offset_table.txt',ll,nn
			ll2=strarr(10,nn)
			for l=0,nn-1 do ll2[*,l]=str_sep(ll[l],',')
			l=(where(transpose(ll2[0,*]) eq string(f.wl0,form='(f5.1)')))[0]
			oset=ll2[1:7,l]
			for ii=0,7-1 do begin
				f.b[ii].ret_offset=oset[ii]
				widget_control,wd.hsld[ii],set_value=string(f.b[ii].ret_offset,form='(f5.1)')
				widget_control,wd.htxt[ii],set_value=string(f.b[ii].ret_offset,form='(f5.1)')
				bl=f.b[ii].bname
				 block7,bl,fsr,elem,cthick,lc,ch,usb
					ret=float(f.b[ii].ret_offset)/360.
				f.b[ii].ret=ret
				;;;tmp=get_therm_temp(ch=0,time=time)
;help,LC,f.wl0,ret,tmp
				v=lc_volt_fit(LC,f.wl0,ret,tmp)
				;;;lcvolt,ch,v,USBx=usb
				;;;wait,0.1
				f.b[ii].volt=v
				widget_control,wd.hvolt[ii],set_value=string(f.b[ii].volt,form='(f5.3)')
			endfor
		end

;----- sld -----

;for j=0,7-1 do begin
;	wd.usld[j]: set_sld,wd.hsld[j],wd.htxt[j],wd.hvolt[j],value
;	wd.utxt[j]: set_txt,wd.hsld[j],wd.htxt[j],wd.hvolt[j],value
;endfor

	 'sld1': set_sld,wd.hsld[0],wd.htxt[0],wd.hvolt[0],value
	 'sld2': set_sld,wd.hsld[1],wd.htxt[1],wd.hvolt[1],value
	 'sld3': set_sld,wd.hsld[2],wd.htxt[2],wd.hvolt[2],value
	 'sld4': set_sld,wd.hsld[3],wd.htxt[3],wd.hvolt[3],value
	 'sld5': set_sld,wd.hsld[4],wd.htxt[4],wd.hvolt[4],value
	 'sld6': set_sld,wd.hsld[5],wd.htxt[5],wd.hvolt[5],value
	 'sld7': set_sld,wd.hsld[6],wd.htxt[6],wd.hvolt[6],value
;----- txt -----
	 'txt1': set_txt,wd.hsld[0],wd.htxt[0],wd.hvolt[0],value
	 'txt2': set_txt,wd.hsld[1],wd.htxt[1],wd.hvolt[1],value
	 'txt3': set_txt,wd.hsld[2],wd.htxt[2],wd.hvolt[2],value
	 'txt4': set_txt,wd.hsld[3],wd.htxt[3],wd.hvolt[3],value
	 'txt5': set_txt,wd.hsld[4],wd.htxt[4],wd.hvolt[4],value
	 'txt6': set_txt,wd.hsld[5],wd.htxt[5],wd.hvolt[5],value
	 'txt7': set_txt,wd.hsld[6],wd.htxt[6],wd.hvolt[6],value

;----- shft -----
	'sh_set': begin
			sh=value
			pal_shift,sh
			;;;tmp=get_therm_temp(ch=0,time=time)
			;;;img0=OrcaObs(nimg=nimg)
			time=get_tim()
			tf_show,time
		   end
;----- scan -----
	'sh_sc':begin
		nsh=32*4
		unit0=360./4
			for jj=0,nsh do begin
				sh=(jj*unit0)-(nsh*unit0)
				widget_control,wd.hshtxt,set_value=string(sh,format='(F9.1)')
				pal_shift,sh
				;;;tmp=get_therm_temp(ch=0,time=time)
				;;;img0=OrcaObs(nimg=nimg)
				time=get_tim()
				tf_show,time
				;;;	save_data,f,g,img0,g.sdir
			endfor
;				;;;	save_data,f,g,img0,g.sdir,dark=dark
				print,'scan finished !!'
	end
;----- preview -----
	'prev': begin
		;---------- Obtain the window index ----------
		WIDGET_CONTROL, wd.draw, GET_VALUE = index
		WIDGET_CONTROL, wd.plotx, GET_VALUE = index_x
		WIDGET_CONTROL, wd.ploty, GET_VALUE = index_y
		;---------- Set the new widget to be the current graphics window ----------
		uvalue=''
		print,'START -> Click !!'
		;---------- Set the new widget to be the current graphics window ----------
		WSET, index
		;;;img=OrcaObs(nimg=nimg)
		img=img0
		img=float(img);-float(dark)
		nx=g.imgsize[0]/4 & ny=g.imgsize[1]/4
		tvscl,rebin(img,nx,ny)
		xyouts,10,10,size=2,'START -> Click !!',/dev
		cursor,xx,yy,/dev,/down
		uvalue=''
		while (uvalue ne "STOP") do begin
			event = widget_event(wd.base_img,/nowait)
			if event.id ne 0 then WIDGET_CONTROL, get_uvalue=uvalue, event.id
					;;;tmp=get_therm_temp(ch=0,time=time)
			WSET, index
					;;;img=OrcaObs(nimg=nimg)
					img=float(img);-float(dark)
					mxmi=[0,max(img)]
					tvscl,rebin(img,nx,ny)
					plots,[0,nx-1],[yy,yy],line=1,/dev
					plots,[xx,xx],[0,ny-1],line=1,/dev
					time=get_tim()
					xyouts,10,10,/dev,size=1.8,time+'   TMP: '+string(tmp,form='(f6.2)')+'C'
					wset,index_x
					plot,(rebin(img,nx,ny))[*,yy],/xstyle,yr=mxmi,title='X profile (@y='+strtrim(yy,2)+' pixel)'
					wset,index_y
					plot,(rebin(img,nx,ny))[xx,*],/xstyle,yr=mxmi,title='Y profile (@x='+strtrim(xx,2)+' pixel)'
		endwhile
		end
	;----- get image -----
	'gimg': begin
		;;;tmp=get_therm_temp(ch=0,time=time)
		;;;img0=OrcaObs(nimg=nimg)
		time=get_tim()
		tf_show,time
		;;;save_data,f,g,img0,g.sdir
		   end
	;----- EXIT -----
	 'EXIT': begin
		 widget_control,/destroy,ev.top
			;;;lcdrv_zero
			;;;lcdrv_close,usbx='USB1'
			;;;lcdrv_close,usbx='USB2'
			;;;OrcaFin
		end
	endcase
end

;pro tf_ctl2

@tf_lib
;;;@orcalib

;;;@mllclib_usb
;;;@caiolib2

common tf_ctl2,wd,f,g,p,dark,img0, tmp,dat_dir

;----- mk header and init TF ctl -----
mk_tf_head,f,g
;ht=lonarr(7) & ht[*]=1L
hs=lonarr(7) & hs[*]=1L
htx=lonarr(7) & htx[*]=1L
hv=lonarr(7) & hv[*]=1L
wd={hwl:1L,$
    htemp:1L,$
    hcam_e:1L,$
    hcam_bin:1L,$
    hdark:1L,$
    hload:1L,$
    hsld:hs,$
    htxt:htx,$
    hvolt:hv,$
    utemp:['temp1','temp2','temp3','temp4','temp5','temp6','temp7'],$
    usld:['sld1','sld2','sld3','sld4','sld5','sld6','sld7'],$
    utxt:['txt1','txt2','txt3','txt4','txt5','txt6','txt7'],$
    vtxt:['volt1','volt2','volt3','volt4','volt5','volt6','volt7'],$
    hshift:1L,$
    hshtxt:1L,$
    hshb:1L,$
    base_img:1L,$
    draw:1L,$
    plotx:1L,$
    ploty:1L}

bl='block'+string(indgen(7)+1,form='(i1)')
sdir='C:\Projects\data\block\block7\'+today()+'\'
dat_dir='C:\projects\data\'

;;; spawn,'mkdir '+sdir
 g.sdir=sdir
 wl=f.wl0
tmp=20.
img0=findgen(2048,2048)
dark=img0 & dark[*,*]=0.
;;; tmp=get_therm_temp(ch=0,/init,time=time)
;;;lcdrv_Open,retn,usbx='USB1'
;;;lcdrv_Open,retn,usbx='USB2'
;;;p=orcainit()
;;; p=OrcaSetParam(expo=g.exp,bin=g.binx)

ww=500
base=widget_base(title='tf_ctl2',/row)
	base_ctl=widget_base(base,/column,xsize=ww+10)

	label = widget_label(base_ctl, value='TF Contol', font='Arial*28', /align_center)

;----- wavelength and temp-----
	wlbase=widget_base(base_ctl,/row,/align_center)
		label = widget_label(wlbase, value='Wavelength', font='Arial', /align_center)
		wd.hwl = widget_text(wlbase,value=string(f.wl0,form='(f8.1)'),uvalue='wl',$
					xsize=7, /edit,font='Arial')
		label = widget_label(wlbase, value='nm', font='Arial', /align_center)
	tbase=widget_base(base_ctl,/row,/align_center)
		temp=widget_button(tbase,value="GET temp",uvalue='temp',font='Arial')
		wd.htemp = widget_text(tbase,value=string(tmp,form='(f5.1)'),uvalue='temp',$
					xsize=7, /edit,font='Arial')
		label = widget_label(tbase, value='C', font='Arial', /align_center)
;----- camera info input-----
	base_cam=widget_base(base_ctl,/column ,/align_center,/frame,xsize=ww)
		label = widget_label(base_cam, value='Camera', font='Arial*28', /align_center)
		base_expo=widget_base(base_cam,/row ,/align_center)
			C_label = widget_label(base_expo, value='exp : ', font='Arial')
			wd.hcam_e = widget_text(base_expo,value=string(g.exp,form='(f7.3)'), uvalue='cam_set', xsize=7, /edit,font='Arial',scr_xsize=70)
			C_label = widget_label(base_expo, value='sec', font='Arial')
			C_label = widget_label(base_expo, value='     bin : ', font='Arial')
			wd.hcam_bin = widget_text(base_expo,value=string(g.binx,form='(i2)'), uvalue='cam_set', xsize=7, /edit,font='Arial',scr_xsize=50)

;----- LC ctrl -----
	sldv1=0.
	volt1=0.
	base_LC=widget_base(base_ctl,/column ,/align_center,/frame,xsize=ww)
	label = widget_label(base_LC, value='LC ctl', font='Arial*28', /align_center)

	wd.hload=widget_button(base_LC,value="Load offset table",uvalue='load',font='Arial')

	for i=0,7-1 do begin
		f.b[i].bname=bl[i]
		block7,bl[i],fsr,elem,cthick,lc,ch,usb
		base1=widget_base(base_LC,/row ,/align_center)
		if fsr ge 1 then aa=string(fsr,form='(i4)') else aa=string(fsr,form='(f4.2)')
		label = widget_label(base1, value=bl[i]+'('+aa+'A)', font='Arial')
		wd.hSLD[i] = widget_slider(base1, value=sldv1, uvalue=wd.usld[i], minimum=0, maximum=360, xsize=150, suppress=1, vertical=0, frame=7, /drag)
		wd.htxt[i] = widget_text(base1,value=string(sldv1,form='(f5.1)'),uvalue=wd.utxt[i],xsize=7, /edit,font='Arial')
		label = widget_label(base1, value='deg', font='Arial')
		wd.hvolt[i] = widget_text(base1,value=string(sldv1,form='(f5.3)'),uvalue=wd.vtxt[i],xsize=7,font='Arial')
		label = widget_label(base1, value='V', font='Arial')
	endfor
;----- shift -----
	sh=0
	wd.hshift=widget_base(base_lc,/row ,/align_center)
	wd.hshtxt = widget_text(wd.hshift,value=string(sh,form='(f5.1)'),uvalue='sh_set',xsize=7, /edit,font='Arial')
	wd.hshb=widget_button(wd.hshift,value='scan',uvalue='sh_sc',font='Arial')
;----- image -----
	ibase=widget_base(base_ctl,/row,/align_center,/frame,xsize=ww)
	prev=widget_button(ibase,value="preview",uvalue='prev',font='Arial')
	img=widget_button(ibase,value="GET img",uvalue='gimg',font='Arial')
;----- dark -----
	dk=widget_button(ibase,value="Dark",uvalue='dark',font='Arial')
;----- prev window -----
	nx=g.imgsize[0]/4 & ny=g.imgsize[1]/4
	wd.base_img = WIDGET_BASE(base,/column,xoffset=nx,yoffset=ny)
	xpdmenu,['/START/','/STOP/'],wd.base_img,/frame
	wd.draw = WIDGET_DRAW(wd.base_img, XSIZE = nx, YSIZE = ny)
	base_img2 = WIDGET_BASE(wd.base_img,/row)
	wd.plotx = WIDGET_DRAW(base_img2, XSIZE = nx/2, YSIZE = ny/2)
	wd.ploty = WIDGET_DRAW(base_img2, XSIZE = nx/2, YSIZE = ny/2)
;----- EXIT -----
	bt=widget_button(base_ctl,value="EXIT",uvalue='EXIT',font='Arial')

widget_control,base,/realize
xmanager,'tf_ctl2',base

end
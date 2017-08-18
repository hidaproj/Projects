;******************************************
function get_tim
	tim=str_sep(systime(),' ')
	time=tim[4]+'/'+mmm2mm(tim[1])+'/'+tim[2]+' '+tim[3]
	return,time
end
;******************************************
pro read_tbl,fa,menu,ll3
			readtxt,'C:\Projects\data\UTF32\offset_table.txt',ll,nn
			ln=n_elements(str_sep(ll[0],','))
			ll2=strarr(ln,nn-1)
			for l=1,nn-1 do ll2[*,l-1]=str_sep(ll[l],',')
			l=(where(string(transpose(ll2[0,*]),form='(f5.1)') eq string(fa.wl0,form='(f5.1)')))
			if l[0] ne -1 then begin
			menu=strarr(n_elements(l))
			for li=0,n_elements(l)-1 do $
				menu[li]=strjoin(strtrim([ll2[0,l[li]],ll2[8,l[li]],ll2[9,l[li]]],2),', ') ; wl,temp,time
			endif
			ll3=ll2[*,l]
end

;*************************************************************************************
pro LC_sld,hsld,htxt,hvolt,fa
	widget_control,hsld,get_value=value,get_uvalue=uvalue
	widget_control,htxt,set_value=string(value,format='(F5.1)')
	widget_control,hsld,get_uvalue=uv
	bi=fix(strmid(uv,3,1))-1
	b1=fa.b[bi]
	lc_ch=str_sep(b1.lc_ch,'-')
	usb='USB'+strcompress(lc_ch[0],/remove_all)
	ch=lc_ch[1]
	ret=value/360.
	b1.temp=get_therm_temp(ch=0,time=time)
	v=get_lc_volt_fit(fa.wl0, b1.ck, ret, b1.temp)
	lcvolt,ch,v,USBx=usb
	fa.b[bi].ret_offset=ret
	fa.b[bi].volt=v
	fa.b[bi].temp=b1.temp
	widget_control,hvolt,set_value=string(v[0],form='(f5.3)')
end
;*************************************************************************************
pro LC_txt,hsld,htxt,hvolt,fa
	widget_control,htxt,get_value=value,get_uvalue=uvalue
	widget_control,hsld,set_value=fix(value)
	widget_control,htxt,get_uvalue=uv
	bi=fix(strmid(uv,3,1))-1
	b1=fa.b[bi]
	lc_ch=str_sep(b1.lc_ch,'-')
	usb='USB'+strcompress(lc_ch[0],/remove_all)
	ch=lc_ch[1]
	ret=value/360.
	b1.temp=get_therm_temp(ch=0,time=time)
	v=get_lc_volt_fit(fa.wl0, b1.ck, ret, b1.temp)
	lcvolt,ch,v,USBx=usb
	fa.b[bi].ret_offset=ret
	fa.b[bi].volt=v
	fa.b[bi].temp=b1.temp
	widget_control,hvolt,set_value=string(v[0],form='(f5.3)')
end


;******************************************

pro LC_scan,hsld,htxt,hvolt,hscan
common utf_adj,wd,fa

step=10
nstep=360/step+1
ret=findgen(nstep)*step/360.

widget_control,hscan,get_uvalue=uv
bi=fix(strmid(uv,4,1))-1
b1=fa.b[bi]
lc_ch=str_sep(b1.lc_ch,'-')
usb='USB'+strcompress(lc_ch[0],/remove_all)
ch=lc_ch[1]
for jjj=0,nstep-1 do begin
			widget_control,htxt,set_value=string(ret[jjj]*360,format='(F5.1)')
			widget_control,hsld,set_value=fix(ret[jjj]*360)
			b1.temp=get_therm_temp(ch=0,time=time)
			v=get_lc_volt_fit(fa.wl0, b1.ck, ret[jjj], b1.temp)
			lcvolt,ch,v,USBx=usb
			wait,0.1
			widget_control,hvolt,set_value=string(v[0],form='(f5.3)')
endfor

end


;******************************************
function wd_create,base
common utf_adj,wd,fa

nb=n_elements(fa.b)
hs=lonarr(nb)
htx=lonarr(nb)
hv=lonarr(nb)
hsc=lonarr(nb)

wd={htemp:0L,$
    ttemp:0L,$
    hload:0L,$
    hload_list:0L,$
    hsave:0L,$
    hsld:hs,$
    htxt:htx,$
    hvolt:hv,$
    hscan:hsc,$
    usld:'sld'+string(indgen(nb)+1,form='(i1)'),$
    utxt:'txt'+string(indgen(nb)+1,form='(i1)'),$
    vtxt:'volt'+string(indgen(nb)+1,form='(i1)'),$
    usca:'scan'+string(indgen(nb)+1,form='(i1)'),$
    hwlsh:0L,$
    done:0L}

ww=700

	base_ctl=widget_base(base,/column,xsize=ww+10)

;----- wavelength and temp-----
	tbase=widget_base(base_ctl,/row,/align_center)
		wd.htemp=widget_button(tbase,value="GET temp",uvalue='temp',font='Arial')
		tmp=get_therm_temp(ch=0,time=time)
		wd.ttemp = widget_text(tbase,value=string(tmp,form='(f6.2)'),uvalue='temp',$
					xsize=7, /edit,font='Arial')
		label = widget_label(tbase, value='C', font='Arial', /align_center)

;----- LC ctrl -----
	sldv1=0.
	volt1=0.
	base_LC=widget_base(base_ctl,/column ,/align_center,/frame,xsize=ww)
	label = widget_label(base_LC, value='LC ctl', font='Arial*28', /align_center)

	base_t=widget_base(base_LC,/row ,/align_center)
	wd.hload=widget_button(base_t,value="Load offset table",uvalue='load',font='Arial')
	wd.hload_list=widget_droplist(base_t, value="Load offset table",uvalue='load',font='Arial',xsize=350)

	wd.hsave=widget_button(base_t,value="Save offset table",uvalue='stbl',font='Arial')

	base_label=widget_base(base_LC,/row)
		label = widget_label(base_label, value='                    ', font='Arial')
		label = widget_label(base_label, value='nm', font='Arial')
		label = widget_label(base_label, value='                              ', font='Arial')
		label = widget_label(base_label, value='deg', font='Arial')
		label = widget_label(base_label, value='                              ', font='Arial')
		label = widget_label(base_label, value='V', font='Arial')

		lret=where(fa.b.ret gt 360.)
		if lret[0] ne -1 then fa.b[lret].ret=fa.b[lret].ret-360.

	for i=0,nb-1 do begin
		base1=widget_base(base_LC,/row ,/align_center)
		aa=string(fa.b[i].fsr,form='(f5.3)')
		label = widget_label(base1, value=string(i+1,form='(i1)')+'('+aa+')', font='Arial')
		wd.hsld[i] = widget_slider(base1, value=fa.b[i].ret, uvalue=wd.usld[i], minimum=0, maximum=360, xsize=150, suppress=1, frame=7, /drag)
		wd.htxt[i] = widget_text(base1,value=string(fa.b[i].ret,form='(f5.1)'),uvalue=wd.utxt[i],xsize=7, /edit,font='Arial')
		wd.hvolt[i] = widget_text(base1,value=string(fa.b[i].volt,form='(f5.3)'),uvalue=wd.vtxt[i],xsize=7,font='Arial')
		wd.hscan[i]=widget_button(base1,value="scan",uvalue=wd.usca[i],font='Arial')
	endfor

	ran=[-1.,1.]
	step=0.005
	n=(ran[1]-ran[0])/step+1
	i=(fa.dwl-ran[0])/step
	wd.hwlsh = widget_slider(base_ctl, value=i, uvalue=0, minimum=0, maximum=n, suppress=1, frame=7, /drag)

;----- Done -----
	wd.done=widget_button(base_ctl,value="Done",uvalue='Done',font='Arial')

return,wd

end

;*************************************************************************************
pro utf_adj_event,ev
common utf_adj,wd,fa

case ev.id of

;----- TEMP -----
	 wd.htemp: begin
				tmp=get_therm_temp(ch=0,time=time)
				widget_control,wd.ttemp,set_value=string(tmp,form='(f6.2)')
		 end

;----- load table -----
	wd.hload:begin
			read_tbl,fa,menu
			widget_control,wd.hload_list,set_value=menu,set_uvalue=menu
		end

	wd.hload_list:begin
			l = widget_info(wd.hload_list, /DropList_Select)
			widget_control, wd.hload_list, Get_UValue=menu
			Print, 'Current Selection: ', menu[l]
			read_tbl,fa,menu,ll
			nb=n_elements(fa.b)
			fa.b.ret_offset=float(ll[1:nb,l])
			fa.b[*].temp0=float(ll[nb+1,l])

			nblk=n_elements(fa.b)
			for i=0,nblk-1 do begin
				b1=fa.b[i]
				widget_control,wd.hsld[i],set_value=string(b1.ret_offset,form='(f5.1)')
				widget_control,wd.htxt[i],set_value=string(b1.ret_offset,form='(f5.1)')
				ret1=b1.ret_offset/360.
				b1.ret=ret1*360
				b1.volt=get_lc_volt_fit(fa.wl0, b1.ck, ret1, b1.temp)
				lc_ch=str_sep(b1.lc_ch,'-')
				lcvolt,lc_ch[1],b1.volt,USBx='USB'+strcompress(lc_ch[0],/remove_all)
				fa.b[i]=b1
				widget_control,wd.hvolt[i],set_value=string(b1.volt,form='(f5.3)')
			endfor
			wait,0.1
		end

;----- save table -----
	wd.hsave:begin
			tfile='C:\Projects\data\UTF32\offset_table.txt'
			readtxt,tfile,ll,nn
			lret=where(fa.b.ret gt 360.)
			if lret[0] ne -1 then fa.b[lret].ret=fa.b[lret].ret-360.
			openw,1,tfile
			for jj=0,nn-1 do printf,1,ll[jj]
			tbl=string(fa.wl0,form='(f8.2)')+', '$
				+strjoin(string((fa.b.ret),form='(f10.3)'),', ')+', '$
				+string((rebin(fa.b.temp,1))[0],form='(f10.3)')+', '+get_tim()
			printf,1,tbl
			close,1
			print,'saved '+tfile+' as '+tbl
		end
	;----- wl shift -----
	wd.hwlsh: begin
		j=ev.value
		ran=[-1.,1.]
		step=0.005
		fa.dwl=ran[0]+step*j
		nb=n_elements(fa.b)

		utf_set_wl,fa


		lret=where(fa.b.ret gt 360.)
		if lret[0] ne -1 then fa.b[lret].ret=fa.b[lret].ret-360.

		print,fa.dwl,fa.b.ret

		for ii=0,nb-1 do begin
			widget_control,wd.htxt[ii],set_value=string(fa.b[ii].ret,format='(F5.1)')
			widget_control,wd.hsld[ii],set_value=fix(fa.b[ii].ret)
			widget_control,wd.hvolt[ii],set_value=string(fa.b[ii].volt,form='(f5.3)')
		endfor
		end

	;----- done -----
	  wd.done: begin
		widget_control,/destroy,ev.top
		f=fa
		save,f,file=f.tbldir+'\f.sav'
		end
	else:
	endcase

;----- sld and txt-----
for j=0,7-1 do begin
	if ev.id eq wd.hsld[j] then LC_sld,wd.hsld[j],wd.htxt[j],wd.hvolt[j],fa
	if ev.id eq wd.htxt[j] then LC_txt,wd.hsld[j],wd.htxt[j],wd.hvolt[j],fa
	if ev.id eq wd.hscan[j] then LC_scan,wd.hsld[j],wd.htxt[j],wd.hvolt[j],wd.hscan[j]
endfor

end

;****************************************** main
pro utf_adj,f,wd=wdr
common utf_adj,wd,fa

fa=f
nodev=0

base=widget_base(title='utf_adj',/row)
wd=wd_create(base)

wdr=wd

widget_control,base,/realize

xmanager,'utf_adj',base


end

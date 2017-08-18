
;PRO rec_hazd
;
;20110808  T.A.
;==========================headder============================;
;PRO ta_vsdst_event, ev
;PRO ta_vsdst
;
;-

;=========================include=============================;
@./ta_vsdstlib.pro
;=========================main================================;
;**************************************************************
pro plotres,wl,mm
;--------------------------------------------------------------
common widgetlib,wd,wp,wx,wy

posx=[.1,.96]

wls=findgen(12000)	; A
mms=indgen(8)
plot,wls,wl2ga(wls,mms[0]),xr=[0,12000],yr=[0,80],/xstyle,/ystyle,$
	xtitle='wave length [A]',ytitle='grating angle [deg]',$
	/norm,pos=[posx[0],.7,posx[1],.98]
for i=1,7 do oplot,wls,wl2ga(wls,mms[i])
oplot,[wl,wl],!y.crange
oplot,!x.crange,replicate(wl2ga(wl,mm),2)

ai=ta_readatlas(wl=awl)
wmin=min(awl) &	wmax=max(awl) &	dw=awl(1)-awl(0)
if (wl le wmin) or (wl ge wmax) then print,'no atlas' else begin
	expand=3.
	wrange=widthspectrum(wl,mm)*expand
	w1=wl-wrange/2
	w2=wl+wrange/2
	i1=long((wl-wmin)/dw-wrange/2/dw)
	i2=long((wl-wmin)/dw+wrange/2/dw)
	plot,awl(i1:i2),ai(i1:i2),pos=[posx[0],0.4,posx[1],0.64],$
	 /noerase,xstyle=1,xrange=[w1,w2],ystyle=1,yrange=[0,300],$
	 xtitle='wavelength [A]'
	w10=wl-wrange/2/expand
	w20=wl+wrange/2/expand
	oplot,[w10,w10],!y.crange
	oplot,[w20,w20],!y.crange

	ny=(i2-i1+1)/3.
	spe=fltarr(i2-i1+1,ny)
	for i=0,ny-1 do spe[*,i]=ai(i1:i2)
	spe[0,0]=300 & spe[0,1]=0
	tvscl,congrid(spe,wx*(posx[1]-posx[0]),wy/4),wx*posx[0],wy*0.1
endelse
xyouts,0.1,0.05,'d lambda = '+string(resolutionpower(wl,mm))+' [A]',size=1.5,color=255,/norm
xyouts,0.1,0.01,strcompress(string(resolution(wl,mm)),/remove_all)+' [mm/A]',size=1.5,color=255,/norm

END
;**************************************************************
pro ta_vsdst_event, ev
;--------------------------------------------------------------
common widgetlib,wd,wp,wx,wy

widget_control, ev.id, get_uvalue=uvalue,get_value=value
;print,'uvalue,value=',uvalue,value
if (uvalue eq "ba") then begin
	case value of
		0:wp.ba=15.
		1:wp.ba=56.
	endcase
endif
if (uvalue eq "ga") then begin
	wp.ga=float(strmid(value,strpos(value,',')-2,2))+$
		float(strmid(value,strpos(value,',')+1,2))/60.
	wp.wl=ga2wl(wp.ga,wp.order)
	widget_CONTROL,wd.wl,set_value=string(wp.wl,format='(f8.2)')
	wp.lpdl=wp.wl/resolutionpower(wp.wl,wp.order)
	widget_CONTROL,wd.lpdl,set_value=string(wp.lpdl)
endif
if (uvalue eq "wl") then begin
	wp.wl=float(value)
	wp.ga=wl2ga(wp.wl,wp.order)
	widget_CONTROL,wd.ga,set_value=string(wp.ga,format='(i2)')+$
	','+string((wp.ga mod 1)*60.,format='(i2)')
	wp.lpdl=wp.wl/resolutionpower(wp.wl,wp.order)
	widget_CONTROL,wd.lpdl,set_value=string(wp.lpdl)
endif
if (uvalue eq "order") then wp.order=fix(value) 

if (uvalue eq "EXIT") then WIDGET_CONTROL, /destroy, ev.top

plotres,wp.wl,wp.order

END

;************************************************************************
pro ta_vsdst
;--------------------------------------------------------------
common widgetlib,wd,wp,wx,wy

wx=600 & wy=600
tmp=value_vsdst()
wl=5380.
mm=5
window,0,xs=wx,ys=wy
plotres,wl,mm

wd={widget_param, $
	ba:		0l,		$	; blaze angle
	ga:		0l,		$	; grating angle
	wl:		0l,		$	; wave length
	order:		0l,		$	; order
	lpdl:		0l,		$	; resolution power
	Exit:		0l,		$
	n_evsample: 	0l 		$	; omake
	}
wp={param, $
	ba:		15.,		$; blaze angle [deg], 15 or 56
	ga:		wl2ga(wl,mm),	$	; grating angle [deg]
	wl:		wl,		$	; wave length [A]
	order:		mm,		$	; order
	lpdl:	double(wl/resolutionpower(wl,mm)),	$; resolution power
	Exit:		0l,		$
	n_evsample: 	0l 		$	; omake
	}

main = WIDGET_BASE(title='VS/DST',/column)
bs_ba=widget_base(main, /row)
	bas=['15','56']
	wd.ba=cw_bselector(bs_ba,bas,label_left='Blaze angle [deg]        : ', $
		uvalue='ba',set_value=0, ysize=1)
bs_ga=widget_base(main, /row)
	lab=widget_label(bs_ga,value='Grating angle [deg,mm] : ')
	wd.ga=widget_text(bs_ga,value=string(wp.ga,format='(i2)')+$
	','+string((wp.ga mod 1)*60.,format='(i2)'),$
	xsize=8, uvalue='ga',/edit)
bs_wl=widget_base(main, /row)
	lab=widget_label(bs_wl,value='Wavelength [A]           : ')
	wd.wl=widget_text(bs_wl,value=string(wp.wl,format='(f8.2)'),$
	xsize=8, uvalue='wl',/edit)
bs_or=widget_base(main, /row)
	lab=widget_label(bs_or,value='order                        : ')
	wd.order=widget_text(bs_or,value=string(wp.order,format='(i1)'),$
	xsize=8, uvalue='order',/edit)
bs_lpdl=widget_base(main, /row)
	lab=widget_label(bs_lpdl,value='Lambda / d Lambda     : ')
	wd.lpdl=widget_label(bs_lpdl,value=$
		string(wp.wl/resolutionpower(wp.wl,wp.order)))
wd.Exit = widget_button(main, value="Exit", uvalue = "EXIT")

widget_control, main, /realize
XMANAGER,'ta_vsdst',main,modal=modal

END

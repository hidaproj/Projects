;+
; NAME       : xmovie.pro (procedure)
; PURPOSE :
;    animate sequence of images
; CATEGORY :
;	idlpro/util
; CALLING SEQUENCE :
;	xmovie,imgs,/fit,/modal,title=title
; INPUTS :
;      	imgs(*,*,nn)  : image array
; OUTPUT :
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;	'98/03/16	k.i.  from vttshow.pro
;	'98/05/07	k.i.  modal keyword
;	'98/05/16	k.i.  bug fix
;	'98/07/05	k.i.  delay method
;	'98/10/09	k.i.  fit keyword
;	'99/09/11	k.i.  x0,y0
;	'04/12/21	k.i.  irange keyword
;	'06/10/27	k.i.  info keyword
;	'06/12/24	k.i.  overlay keyword
;	'07/02/07	k.i.  title keyword
; '12/02/07 TTI   for T3PC
;-
;*************************************************
pro xmovie_event,ev
common xmovie_com,wd,imgs,i,nbin,imin,imax,delay,info,xy

s=size(imgs) &	nx=s(1) &	ny=s(2) &	nn=s(3)
type=s(4)
x0=!d.x_size/2-nx/2 &	y0=!d.y_size/2-ny/2
pinfo=50
case ev.id of 
    wd.Slide: begin
	widget_control, wd.Slide, get_value=i
	widget_control, wd.STxt, set_value=string(i,format='(i4.4)')
	tvscl,imgs(*,*,i)>imin<imax,x0,y0
	if n_elements(xy) gt 1 then draw,xy(*,0),xy(*,1)
	if keyword_set(info) then xyouts,pinfo,5,info(i),/dev
	xyouts,5,5,string(i,format='(i4.4)'),/dev
	xyouts,5,5,string(i,format='(i4.4)'),/dev
	end
    wd.Forward: begin 
	Forbegin:
	stp=0
        while stp eq 0 do begin 
	    i=i+1
	    if i eq nn then i=0
	    if type eq 2 then tvscl,imgs(*,*,i)>imin<imax,x0,y0 $
	    else tv,imgs(*,*,i),x0,y0
	    xyouts,5,5,string(i,format='(i4.4)'),/dev
	    if n_elements(xy) gt 1 then draw,xy(*,0),xy(*,1)
	    if keyword_set(info) then xyouts,pinfo,5,info(i),/dev
	    widget_control, wd.Slide, set_value=i
	    widget_control, wd.STxt, set_value=string(i,format='(i4.4)')
            ev1=widget_event(wd.Stop,/nowait) 
            if ev1.id ne 0 then begin
		stp=1
		;print,'stop movie...'
	    endif
            ev2=widget_event(wd.Reverse,/nowait) 
	    if ev2.id ne 0 then goto,Revbegin
            ev3=widget_event(wd.SPSld,/nowait) 
	    if ev3.id ne 0 then begin
		widget_control, wd.SPSld, get_value=j
		delay=wd.Delay(j)
	    endif
	    if delay ne 0 then begin
		;for j=0,delay*1e5 do dmy=1.^2+1.^2
		wait,delay
	    endif
	endwhile 
        end 
    wd.Reverse: begin 
	Revbegin:
	stp=0
        while stp eq 0 do begin 
	    i=i-1
	    if i eq -1 then i=nn-1
	    if type eq 2 then tvscl,imgs(*,*,i)>imin<imax,x0,y0 $
	    else tv,imgs(*,*,i),x0,y0
	    xyouts,5,5,string(i,format='(i4.4)'),/dev
	    if n_elements(xy) gt 1 then draw,xy(*,0),xy(*,1)
	    if keyword_set(info) then xyouts,pinfo,5,info(i),/dev
	    widget_control, wd.Slide, set_value=i
	    widget_control, wd.STxt, set_value=string(i,format='(i4.4)')
            ev1=widget_event(wd.Stop,/nowait) 
            if ev1.id ne 0 then begin
		stp=1
		;print,'stop movie...'
	    endif
            ev2=widget_event(wd.Forward,/nowait) 
	    if ev2.id ne 0 then goto,Forbegin
            ev3=widget_event(wd.SPSld,/nowait) 
	    if ev3.id ne 0 then begin
		widget_control, wd.SPSld, get_value=j
		delay=wd.Delay(j)
	    endif
	    if delay ne 0 then begin
		;for j=0,delay*1e5 do dmy=1.^2+1.^2
		wait,delay
	    endif
	endwhile 
        end 
    wd.SPSld: begin 
	widget_control, wd.SPSld, get_value=j
	delay=wd.Delay(j)
	end
    wd.Loadct: begin 
	xloadct
	end
    wd.Stop: begin 
	end
    wd.Select: begin 
	end
    wd.Exit: begin
    wdelete
	WIDGET_CONTROL, /destroy, ev.top
	return
	end
    endcase
end

;***********************************************************
pro xmovie,img0,modal=modal,fit=fit,irange=irange,info=info0, $
	overlay=overlay,title=title,group_leader=gl
common xmovie_com,wd,imgs,i,nbin,imin,imax,delay,info,xy

;----------------------------------------------
nbin=1				; binning factor
if not keyword_set(irange) then begin
	imin=0 &	imax=3000	; display scale
endif else begin
	imin=irange(0)
	imax=irange(1)
endelse
if not keyword_set(info0) then info='' else info=info0
if keyword_set(overlay) then xy=overlay else xy=0
;----------------------------------------------
if keyword_set(modal) then modal=1 else modal=0
if not keyword_set(title) then title=''

imgs=temporary(img0)
; help,img0,imgs
s=size(imgs) &	nx=s(1) &	ny=s(2) &	nn=s(3)
i=0

delays=(findgen(11)*0.05)^2 &	delays=rotate(delays,2)

wd={wd_xmovie, $
	Slide:	0l,	$
	STxt:	0l,	$
	Forward: 0l,	$
	Reverse: 0l,	$
	Stop:	0l,	$
	SPSld:	0l,	$	; speed slide
	SPTxt:	0l,	$
	Loadct:	0l,	$
	Select:	0l,	$
	Delay:	delays,	$	; delay second
	Exit:	0l	$
	}
;base = WIDGET_BASE(title='Xmovie', /column, /modal, /floating, group_leader=gl ) 
base = WIDGET_BASE(title='Xmovie', /column )
b1 = WIDGET_BASE(base, /row) 
	value=string(indgen(nn),format='(i4.4)')
	unit='/'+string(nn,format='(i4.4)')
	wds=wd_sldbx(b1,name='Count: ',values=value, $
		size=250,/edit,/frame,init='0000',unit=unit)
	wd.Slide=wds.Sld &	wd.STxt=wds.Txt
b2 = WIDGET_BASE(base, /row) 
	wd.Reverse=widget_button(b2, value=" < ", uvalue='Reverse') 
	wd.Stop=widget_button(b2, value="Stop", uvalue='Stop') 
	wd.Forward=widget_button(b2, value=" > ", uvalue='Forward') 
	;wd.Select=widget_button(base, value="Select", uvalue='Select') 
	wd.Loadct=widget_button(b2, value="Color", uvalue='Loadct') 
b3 = WIDGET_BASE(base, /row) 
	lab = widget_label(b3,value='Speed:');,font=1)
	nd=n_elements(wd.Delay)
 delay=delays[nd-5]
	wd.SPSld  = widget_slider(b3, value=nd-5, uvalue='SPSld', $
		minimum=0, maximum=nd-1, xsize=100, $
		suppress=1, vertical=0, /drag )
	wd.Exit=widget_button(b3, value="Exit", uvalue='Exit') 

widget_control, base, /realize

if keyword_set(fit) then window,xsize=nx,ysize=ny,title=title
x0=!d.x_size/2-nx/2 &	y0=!d.y_size/2-ny/2
tvscl,imgs(*,*,i)>imin<imax,x0,y0
help,xy
if n_elements(xy) gt 1 then draw,xy(*,0),xy(*,1)
xyouts,5,5,string(i,format='(i4.4)'),/dev
xyouts,5,5,string(i,format='(i4.4)'),/dev
wshow
XMANAGER, 'xmovie', base, modal=modal

img0=temporary(imgs)

end

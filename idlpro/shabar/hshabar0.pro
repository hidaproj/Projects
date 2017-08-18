; ai1608.pro
@caiolib2

;  2011.3.6   copy from nkrprj\idlpro\hardware\caio.pro

;**************************************************************
pro caio_main_event, ev
;--------------------------------------------------------------
common caio, wd, p, dat

widget_control, ev.id, get_uvalue=value
p=caio_event(ev,wd,p,dat=dat)
end

;************************************************************************
common caio, wd, p, dat
; wd	- widget structure
; p	- control parameters
caiodll='C:\Projects\cprog\VS2005\ContecAIOW2\Debug\ContecAIOW2.dll'

p=caio_ctl()
caio_init,p=p,st=st,dllfile=caiodll
help,p.c,/st
p.yrange=[-1,10]
p.trange=[0,5]
p.rate=5000
p.nch=1
p.datadir='c:\data\shabar\'


window,0,xsize=600,ysize=400
@set_color
device,decom=0
t=findgen((p.trange(1)-p.trange(0))*p.rate)/p.rate
plot,t,t,xtitle='time [sec]',yrange=p.yrange,ytitle='V',/nodata,ystyle=1


base = WIDGET_BASE(title='CAIO:  ', /column)
wd = widget_caio(base,p)
wd.Exit=widget_button(base, value="Exit", uvalue = "Exit")
widget_control, base, /realize
XMANAGER, 'caio_main', base


end


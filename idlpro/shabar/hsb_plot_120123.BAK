;  hsb_plot.pro
;  plot hida shabar data
;	2011.9.6	k.i.
;	2011.9.14	k.i.	pro
;	2011.9.30	k.i.	interval,  ['10:29:00','10:30:00']  'hhmmss'
pro hsb_plot_120123,file,wid=wid,interval=interval


if not keyword_set(file) then begin
	file=dialog_pickfile(path='C:\data\shabar\log\')
endif
;file='C:\data\shabar\log\hsb_110914.csv'
if n_elements(wid) eq 0 then wid=0
dat=rdcsv(file)
ctim=transpose(dat[0,*])
inten=float(transpose(dat[1,*]))
scint=float(transpose(dat[2,*]))
ftim=flttime(ctim)

wx=1200 &	wy=700
x0=100 &	y0=70
x1=wx-50 &	y1=wy/2 &	y2=wy-50
window,wid,xs=wx,ys=wy
stretch,255,0
;tmin=fix(min(ftim)) &	tmax=fix(max(ftim))+1
;itt=tmax-tmin
;x1=x0+itt*dhr
if keyword_set(interval) then begin
	tmin=flttime(interval[0])
	tmax=flttime(interval[1])
endif else begin
	tmin=5. &	tmax=18.
endelse
dhr=(x1-x0)/(tmax-tmin)
pos1=[x0,y0,x1,y1]
pos2=[x0,y1,x1,y2]
filename_sep,file,dir,fnam,ext
ymd=strmid(fnam,4,strlen(fnam-4))
title='Hida SHABAR,  20'+strmid(ymd,0,2)+'.'+strmid(ymd,2,2)+'.'+strmid(ymd,4,2)
plot,ftim,inten,psym=2,symsize=0.2,pos=pos1,/dev, $
	xrange=[tmin,tmax],xstyle=1,xtitle='time [JST]', $
	yrange=[0,10],ystyle=1,ytitle='Brightness',yticklen=1,ygrid=1,charsize=1.3
plot_io,ftim,scint*100,psym=2,symsize=0.2,pos=pos2,/dev,/noerase, $
	xrange=[tmin,tmax],xstyle=1,xtickname=replicate(' ',20), $
	yrange=[0.00005,0.02]*100,ystyle=1,ytitle='Scintillation [%, >10Hz, rms]',yticklen=1,ygrid=1, $
	title=title,charsize=1.3

;win2gif,dir+'\'+fnam+'.gif'


end

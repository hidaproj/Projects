; hsb_repro.pro
;	2011.09.24	k.i.
dir='C:\data\shabar\20110925\'
;hsbfiles=file_search(dir+'*.sav')
hsbfiles=findfile(dir+'*.sav')
nf=n_elements(hsbfiles)
frange=[10,200]

;goto,plt

;ip=strpos(hsbfiles[0],'.sav',/reverse_search)
ip=strpos(hsbfiles[0],'.sav')
ctim=strmid(hsbfiles,ip-6,6)
ftim=float(strmid(ctim,0,2))+float(strmid(ctim,2,2))/60.+float(strmid(ctim,4,2))/3600.

restore,hsbfiles[0]
window,1
inten=fltarr(nf)
scint=fltarr(nf)
i0=1000l
;for i=i0,i0 do begin
for i=0l,nf-1 do begin
	print,i,'  ',hsbfiles[i]
	restore,hsbfiles[i]	; --> p, dat
	tt=findgen(p.n_rec1)/p.rate	; sec
	df=float(p.rate)/p.n_rec1
	datq=transpose(dat[0,*])-p.darklevel
	av=mean(datq)
	calcpsd,tt,datq-av,f,pw,intval=2500,over=0.5
	plot_oo,f,pw,xtitle='freq. [Hz]',xrange=[1,p.rate/2],ytitle='PSD', $
		yrange=[1e-12,1e-5],ystyle=1
	ii=where(f ge frange[0] and f lt frange[1])
	ipw=total(pw[ii])*df
	irms=sqrt(ipw)
	inten[i]=av
	scint[i]=irms/p.intens
	;--
	if 0 then begin
		nbin=10
		n_rec1r=p.n_rec1/nbin &	rater=p.rate/nbin
		ttr=findgen(n_rec1r)/rater	; sec
		df=float(rater)/n_rec1r
		datr=rebin(datq,n_rec1r)
		calcpsd,ttr,datr-av,fr,pwr,intval=2500/nbin,over=0.5
		oplot,fr,pwr,line=2,thick=3
	endif
endfor
stop

plt:
wx=1200 &	wy=700
x0=100 &	y0=70
x1=wx-50 &	y1=wy/2 &	y2=wy-50
window,0,xs=wx,ys=wy
stretch,255,0
tmin=5. &	tmax=18.&	dhr=(x1-x0)/(tmax-tmin)
pos1=[x0,y0,x1,y1]
pos2=[x0,y1,x1,y2]
title='Hida SHABAR,  '+dir
plot,ftim,inten,psym=2,symsize=0.2,pos=pos1,/dev, $
	xrange=[tmin,tmax],xstyle=1,xtitle='time [JST]', $
	yrange=[0,10],ystyle=1,ytitle='Brightness',yticklen=1,ygrid=1,charsize=1.3
plot_io,ftim,scint*100,psym=2,symsize=0.2,pos=pos2,/dev,/noerase, $
	xrange=[tmin,tmax],xstyle=1,xtickname=replicate(' ',20), $
	yrange=[0.00005,0.02]*100,ystyle=1,ytitle='Scintillation [%, >10Hz, rms]',yticklen=1,ygrid=1, $
	title=title,charsize=1.3

end

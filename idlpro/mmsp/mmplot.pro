pro mmplot,wl,mm,xtitle=xtitle,xrange=xr,yrange=yr,title=title,chs=chs, $
	norm=norm,psym=psym
;  2011.03.10	ki.
;  2011.07.08   norm keyword
;  2013.06.26   psym keyword

if not keyword_set(xtitle) then xtitle='wavelength [nm]'
if not keyword_set(yr) then yr=[-1.,1.]
if not keyword_set(chs) then chs=1.
;window,0,xs=600,ys=600
dx0=0.1 &	dx1=0.05
dy0=0.1 &	dy1=0.05
dd=0.005

wx1=(1.-dx0-dx1)/4.
wy1=(1.-dy0-dy1)/4.
blank=replicate(' ',10)
xticks=4
erase
for j=0,3 do begin
for i=0,3 do begin
	x0=dx0+i*wx1 &	y0=dy0+(3-j)*wy1
	pos=[x0,y0,x0+wx1-dd,y0+wy1-dd]
	if i eq 0 then ytickname='' else ytickname=blank
	if j eq 3 and i eq 0 then begin
		xtickname='' 
		xtit=xtitle
	endif else begin
		xtickname=blank
		xtit=''
	endelse
	if keyword_set(norm) then y=mm[i,j,*]/mm[0,0,*] else y=mm[i,j,*]
	if keyword_set(xr) then begin
	    plot,wl,y,/noerase,pos=pos,xtickname=xtickname,ytickname=ytickname, $
		yr=yr,ystyle=1,xr=xr,xstyle=1,chars=chs,xtitle=xtit,xticks=xticks, $
		thick=1
	endif else begin
	    plot,wl,y,/noerase,pos=pos,xtickname=xtickname,ytickname=ytickname, $
		yr=yr,ystyle=1,chars=chs,xtitle=xtit,xticks=xticks,thick=1
	endelse
	if keyword_set(psym) then oplot,wl,y,psym=psym
	oplot,!x.crange,[0,0],line=1
endfor
endfor

if keyword_set(title) then $
	xyouts,dx0,1.-dy1+0.01,title,/norm,chars=chs*1.2

end
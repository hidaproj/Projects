pro mmplot_ip,mm,xtitle=xtitle,yrange=yr,title=title,chs=chs, $
	norm=norm,psym=psym,iiv=iiv
;  2011.03.10	ki.
;  2011.07.08   norm keyword
;  2013.06.26   psym keyword
;  2015.04.23   t.a.	mmplot => mmplot_ip

if not keyword_set(xtitle) then xtitle='X [pix]'
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
;loadct,0
;!p.background=255
;!p.color=0
for j=0,3 do begin
for i=0,3 do begin
	x0=dx0+i*wx1 &	y0=dy0+(3-j)*wy1
	pos=[x0,y0,x0+wx1-dd,y0+wy1-dd]
	if i eq 0 then ytickname='' else ytickname=blank
	if j eq 3 and i eq 0 then begin
		xtickname='' 
		ytickname='' 
		xtit=xtitle
		ytit='Y [pix]'
	endif else begin
		xtickname=blank
		ytickname=blank
		xtit=''
		ytit=''
	endelse
	if keyword_set(norm) then y=mm[i,j,*,*]/mm[0,0,*,*] else y=mm[i,j,*,*]
	y=reform(y)
	nx=(size(y))[1]
	y[0,1]=yr
	plot_image,y,min=yr[0],max=yr[1],/noerase,pos=pos,xtickname=xtickname,ytickname=ytickname, $
		ystyle=1,xstyle=1,chars=chs,xtitle=xtit,ytitle=ytit, $
		thick=1;,color=0,background=255
	xyouts,norm=1,	$
		;color=255,	$
		pos[0]+0.01,pos[1]+wy1-0.035,	$
		charsize=1.5,	$
		string(mean(y[iiv mod nx,iiv/nx]),format='(f6.3)')
	;if keyword_set(psym) then oplot,wl,y,psym=psym
	;oplot,!x.crange,[0,0],line=1
endfor
endfor

if keyword_set(title) then $
	xyouts,dx0,1.-dy1+0.01,title,/norm,chars=chs*1.2,color=0

end

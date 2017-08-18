; modulation test plot
;  2013.04.13	k.i.

pro restmm,file,p,wl,dat
restore,file	; --> p,wl,dat
return
end

stretch,255,0

dir='C:\data\MMSP\20130415\'
mfile='m20130415_110737.sav' &	name='L-fiber, bright'
mfile='m20130415_114825.sav' &	name='L-fiber, dark'
mfile='m20130415_163610.sav' &	name='U-fiber, bright'
mfile='m20130415_174448.sav' &	name='U-fiber, dark'
mfile='m20130415_181813.sav' &	name='ref.mirror'

restmm,dir+mfile,mp,wl,mdat
imgsize,mdat,nw,nth
th=findgen(nth)*p.dth1/5
wbin=4
tfct=4
nw=nw/wbin
mdat=rebin(mdat,nw,nth)
wl=rebin(wl,nw)
window,xs=nw*2,ys=100
erase

ii=where(wl ge 400 and wl le 1100)
mdat=mdat[ii,*]
wl=wl[ii]
nw=n_elements(ii)

for i=0,nw-1 do begin
	mdat[i,*]=mdat[i,*]/mean(mdat[i,*])
endfor
dx=100 &	dy=70
;window,0,xs=73*4*2+2*dx+50,ys=nw+2*dy+20
window,0,xs=nth*tfct+dx+30,ys=nw+dy+40
mm=rotate(rebin(mdat,nw,nth*tfct),4)
tvscl,mm,dx,dy,/dev
;plot,th,wl,pos=[dx*2+nth*4,dy,dx*2+nth*8,dy+nw],xtitle='wp-angle (deg.)', $
plot,th,wl,pos=[dx,dy,dx+nth*4,dy+nw],xtitle='wp-angle (deg.)', $
	xticks=4,xrange=[0,360],xstyle=1,yrange=[400,1100],ystyle=1, $
	ytitle='wl (nm)',/dev,/nodata,chars=1.3,/noerase;,title=name
filename_sep,mfile,di,fnam,ext
xyouts,dx,dy+nw+7,fnam+': '+name,chars=1.5,/dev
win2gif,'c:\tmp\a1.gif'


window,2,xs=600,ys=600
wlp=[525.,854.,1083.]
ns=n_elements(wlp)
plot,th,mdat[100,*],xtitle='wp-angle (deg.)', $
	xticks=4,xrange=[0,360],xstyle=1, $
	ytitle='intensity (DN)',/nodata,chars=1.3,/noeras,title=name
for i=0,ns-1 do begin
	dmy=min(abs(wlp[i]-wl),i0)
	oplot,th,mdat[i0,*],line=i
	oplot,th,mdat[i0,*],psym=1+i,symsize=0.5
endfor
xyouts,130,520,'525, 854, 1083nm',/dev,chars=1.2

win2gif,'c:\tmp\a2.gif'

end

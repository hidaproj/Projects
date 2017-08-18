;  as2pn_dst.pro
;  calculate pn[*], * = 0-300 = displacement of DST image in arcsec
;	2008.7.24	k.i.
function as2pn_dst

d=60.			; thickness of the glass block (GB)
ndx=1.516		; refraction index of BK7
scl=128./20.		; image scale, arcsec/mm
pt=0.005d/180.*!dpi	; radian/step for GB (full step)
pt=pt*128./300.		; unknown correction

pn0=findgen(2000)*10	; pulse number
th=pn0*pt
th2=asin(sin(th)/ndx)
as0=d*sin(th-th2)*scl	;
as=findgen(151)	; arcsec
pn=spline(as0,pn0,as)

if 0 then begin
	plot,as,pn,xtitle='image shift (arcsec)',ytitle='pulse #'
	as2pnp=pn
	save,as2pnp,file='C:\home\idlpro\hida\obs\as2pn_dst.dat'
	stop
endif

return,pn

end

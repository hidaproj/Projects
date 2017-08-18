; hsb_corr
;  correlation between I, rms, C
;	2012.12.30	k.i.

file='C:\data\shabar\log\hsb_120726.csv'
if not keyword_set(file) then begin
	file=dialog_pickfile(path='C:\data\shabar\log\')
endif
dat=rdcsv(file,skip=1)
dat=transpose(dat)
; dat[*,j]	j=  0	  1    2    3    4    5
;		hh:mm:dd, I1, rms1, I2, rms2, C12
ftim=flttime(dat[*,0])
i1=float(dat[*,1])
sc1=float(dat[*,2])
i2=float(dat[*,3])
sc2=float(dat[*,4])
cc=float(dat[*,5])


di=abs(shift(i1,1)-i1)
ii=where(di lt 0.0004)

!p.multi=0
;plot,di,cc,psym=3,xrange=[0,0.05],xtitle='abs(dI/dt)',ytitle='CC',chars=1.3
;plot_oi,sc1[ii],cc[ii],psym=3,xrange=[0.0001,0.0004],xtitle='sc1',ytitle='CC',chars=1.3
plot_oo,sc1[ii],cc[ii],psym=3,xrange=[0.0001,0.0004],yrange=[0.00001,1],xtitle='sc1',ytitle='CC',chars=1.3
oplot,[0.0001,1],[0,0]

stop
!p.multi=[0,1,3]
plot,ftim[ii],cc[ii],psym=3,chars=2,ytitle='CC',ymargin=[2,2]
oplot,[5,20],[0,0]
plot_io,ftim[ii],sc1[ii],chars=2,ytitle='sc1',ymargin=[2,2]
plot,ftim[ii],i1[ii],chars=2,xtitle='time',ytitle='i1',ymargin=[4,2]

!p.multi=0

stop


calcpsd,ftim*3600.,i1,f,pw,intval=10000,over=0.5
plot_io,f,pw,xrange=[0,0.01]


end

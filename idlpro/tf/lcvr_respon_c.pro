; lcvr_respon.pro
;   LCVR response measurement
;	2011.3.25	k.i.
dir='C:\data\TF\20110325_lcres\'
lcv=[0,2,8,2,0]	; V
file='LC1_02.isv'
restore,dir+file	; p, dat
imgsize,dat,nc,nt
t=1./p.rate*findgen(nt)
window,xs=1200,ys=700
!p.multi=[0,1,2]
plot,t,dat[0,*],xstyle=0,xrange=[1,6],pos=[0.1,0.5,0.95,0.9], $
	xtickname=replicate(' ',10),ytitle='Ch-0 (Iout)',charsize=1.5,title=file
plot,t,dat[1,*],xstyle=0,xrange=[1,6],pos=[0.1,0.1,0.95,0.5], $
	xtitle='time [sec]',ytitle='Ch-1 (Vout)',charsize=1.5

stop
;box_cur1,x0, y0, nx, ny,/data
cursor,x0,y0,/data
print,x0
dt=0.2
ii=where(t gt x0 and t lt x0+dt)
tt=(t[ii]-t[ii[0]])*1000.
iout=smooth(dat[0,ii],40)
iout=dat[0,ii]
vout=dat[1,ii]
plot,tt,iout,xstyle=0,xrange=[0,dt*1000],pos=[0.1,0.5,0.95,0.9], $
	xtickname=replicate(' ',10),ytitle='Ch-0 (Iout)',charsize=1.5,title=file
plot,tt,vout,xstyle=0,xrange=[0,dt*1000],pos=[0.1,0.1,0.95,0.5], $
	xtitle='time [msec]',ytitle='Ch-1 (Vout)',charsize=1.5

end

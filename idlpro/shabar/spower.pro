; spower.pro

dir='c:\data\shabar\20120427\'
dir='c:\data\shabar\20140401\'
dir='c:\data\shabar\20140813\'
file=dialog_pickfile(path=dir,filter='*.*',title='data file')
;darkf=dialog_pickfile(path=dir,filter='*.*',title='dark file')

;restore,darkf	; --> p, dat[n,*]
;drk=mean(dat)
drk=0
restore,file	; --> p, dat[n,*]
dat=dat-drk

dat0=transpose(dat[0,*])
nn=n_elements(dat0)
;;dat0=rebin(dat0,nn/4) &	p.rate=p.rate/4

avr=mean(dat0)
dat1=(dat0-avr)/avr
nt=n_elements(dat1)
t=findgen(nt)/p.rate	; sec
calcpsd,t,dat1,f,pw,intval=2000,over=0.5
df=float(p.rate)/nt



lcut=10.	; lower cutoff, Hz
ii=where(f gt lcut)
ii=where(f gt 10 and f lt 200)
ipw=total(pw[ii])*df
rms=sqrt(ipw)

plot_oo,f,pw,xtitle='freq. [Hz]',xrange=[1,p.rate/2],ytitle='PSD', $
	charsize=1.2,title=file+',  dI/I='+string(rms,form='(f7.5)')+' rms' $
	;,yrange=[1e-12,1e-7],ystyle=1
	,yrange=[1e-8,1e-4],ystyle=1
print,total(pw*df),avr




end

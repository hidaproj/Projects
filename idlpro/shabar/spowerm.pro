; spowerm.pro
;  plot of power spectra from Hida SHABAR
;	2012.5.2	k.i.
;	2014.4.3	"


;******************************************************************
pro calc_sbpsd,files,f,avpw,lcut=lcut,verbose=verbose,drk=drk,mov=mov,rms=rms


if not keyword_set(drk) then drk=0

nf=n_elements(files)

mov=bytarr(!d.x_size,!d.y_size,nf)


restore,files[0]	; --> p, dat[n,*]
dat=dat-drk
dat0=transpose(dat[1,*])
nn=n_elements(dat0)
avr=mean(dat0)
dat1=(dat0-avr)/avr
nt=n_elements(dat1)
t=findgen(nt)/p.rate	; sec
df=float(p.rate)/nt
calcpsd,t,dat1,f,pw,intval=p.rate/2,over=0.5
npw=n_elements(pw)
avpw=dblarr(npw)
ii=where(f gt lcut[0] and f lt lcut[1])	; 

for i=0,nf-1 do begin
	restore,files[i]	; --> p, dat[n,*]
	dat=dat-drk
	dat0=transpose(dat[1,*])

	avr=mean(dat0)
	;dat1=(dat0-avr)/avr
	dat1=(dat0-avr)
	;calcpsd,t,dat1,f,pw,intval=2000,over=0.5
	calcpsd,t,dat1,f,pw,intval=p.rate/2,over=0.5
	avpw=avpw+pw

	if keyword_set(verbose) then begin
	    print,i,'  ',files[i]
	    plot_oo,f,pw,xtitle='freq. [Hz]',xrange=[1,p.rate/2],ytitle='PSD', $
		charsize=1.5,title=files[i],ystyle=1;,yrange=[1e-11,1e-7]
	    print,total(pw*df),avr
	    oplot,[3,3000],[1e-7,1e-11],line=1
	    mov[*,*,i]=tvrd()
	    win2jpeg,'c:\tmp\m\m'+string(i,form='(i3.3)')+'.jpg'
	endif
endfor

avpw=avpw/nf

ipw=total(avpw[ii])*df
rms=sqrt(ipw)


end

;******************************************************************
dir0='c:\data\shabar\'

date='20140401' &	date2='20120427'
;date='20140403d' &	date2='20140403'
;date='20140403d' &	date2='20140324'
date='20140403' &	date2='20140401' ;&	date3='20140403d'
date='20140813' &	date2='20140401' 

;date='20110924'

files=findfile(dir0+date+'\*.sav')	; & files=files[0:60]
if keyword_set(date2) then $
files2=findfile(dir0+date2+'\*.sav')	; & files2=files2[0:60]
if keyword_set(date3) then $
files3=findfile(dir0+date3+'\*.sav')	; & files3=files3[0:60]


;file=dialog_pickfile(path=dir,filter='*.*',title='data file')
;darkf=dialog_pickfile(path=dir,filter='*.*',title='dark file')

;restore,darkf	; --> p, dat[n,*]
;drk=mean(dat)
drk=0
lcut=[10.,200]	; lower & higher cutoff, Hz
lcut=[0.,10000]	; lower & higher cutoff, Hz


xs=900 &	ys=650
xs=800 &	ys=600
window,xs=xs,ys=ys
@set_color

calc_sbpsd,files,f,avpw,lcut=lcut,verbose=0,drk=drk,mov=mov,rms=rms
cdate=date
crms='dV='+string(rms,form='(f6.4)')
title=''
plot_oo,f,avpw,xtitle='freq. [Hz]',xrange=[1,p.rate/2],ytitle='PSD (V!u2!n/Hz)', $
	charsize=1.5,title=title,ystyle=1,/nodata,yrange=[1e-9,1e-4]
oplot,f,avpw,line=0,col=red


if n_elements(files2) gt 0 then begin
	calc_sbpsd,files2,f2,avpw2,lcut=lcut,verbose=0,drk=drk,mov=mov,rms=rms
	oplot,f2,avpw2,line=0,col=blue
	cdate=cdate+'/'+date2
	crms=crms+'/ '+string(rms,form='(f6.4)')
endif

if n_elements(files3) gt 0 then begin
	calc_sbpsd,files3,f3,avpw3,lcut=lcut,verbose=0,drk=drk,mov=mov,rms=rms
	oplot,f3,avpw3,line=0,col=black
	cdate=cdate+'/'+date3
	crms=crms+'/ '+string(rms,form='(f6.4)')
endif

crms=crms+' rms'
title=cdate+', '+crms
xyouts,0.1,0.95,title,/norm,chars=1.5


delvar,date2,date3
delvar,files2,files3


end

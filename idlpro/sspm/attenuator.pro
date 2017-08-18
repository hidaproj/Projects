;  attenuator design
;*************************************************
function gt_sp,file,p=p

restore,file
sp1=double(sp1)
return,sp1
end

;*************************************************

;-- design by “ú–{^‹ó ---
tfile='C:\home\doc\projects\”ò‘Ë\SMART\sp_monitor\attenuator.csv'
strs=rdcsv(tfile,skip=2)
wl=float(strs[0,*])
tr=float(strs[1,*])

;-- solar spectrum
dir='c:\data\flame\20160808\'
sun=gt_sp(dir+'sun4000_20160808_115405.997.sav',p=p)
drk=gt_sp(dir+'drk.sav',p=p)
imgsize,sun,nw,nn
drk1=congrid(drk,nw,1)
for i=0,nn-1 do begin
	sun[*,i]=sun[*,i]-drk1
endfor
avsun=congrid(sun,nw,1)
wlo=p.wl
ssp=interpol(avsun,wlo,wl)

window,xs=800,ys=600
plot,wl,ssp,xtitle='wavelength [nm]',chars=1.5,line=1,yrange=[0,1e5]
oplot,wl,ssp*tr/100.,thick=2
oplot,wl,tr*1e3




end

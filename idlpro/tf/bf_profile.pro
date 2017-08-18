; bf_profile.pro

function rd_hr2000sav,file,p=p

restore,file

return,sp1

end


;-----------------------------------------------
dir='C:\data\TF\BF\20130615_bf32mm\'

cwl0='1083'

clr=rd_hr2000sav(dir+'clear.sav',p=pc)
drk=rd_hr2000sav(dir+'dark.sav',p=pd)
wl=pc.wl

clr=clr-drk

sp0=rd_hr2000sav(dir+'bf'+cwl0+'.sav',p=p)

sp=(sp0-drk)/(clr>0)

wl0=float(cwl0)

window,xs=1000,ys=600
stretch,255,0
wrange=wl0*[0.992,1.008]
wrange=wl0*[0.98,1.02]

;asp=atlas(wrange[0]*10,wrange[1]*10,0.1,wl=awl,/liege)
;asp=atlas(wrange[0]*10,wrange[1]*10,wl=awl)
asp=atlas(wl=awl)

plot,wl,sp,xrange=wrange,yrange=[0,1],xtitle='wavelength [nm]',title='bf'+cwl0, $
	chars=1.5,ytitle='transmission'
;oplot,awl/10.,asp/10000.,line=2	; Liege
oplot,awl/10.,asp/255.,line=1






end

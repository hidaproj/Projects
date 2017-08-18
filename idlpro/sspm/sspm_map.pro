;  sspm_map.pro
;	make sspm (wl,t) map
;	2017.4.4	k.i.
function sspm_map,dir

;dir0='d:\data\sspm\' &	date='20170404' &	dir=dir0+date+'\'

wrange=[300,850]

drkfile='D:\data\flame\dark\spdrk1ms_20170402_114602.sav'
restore,drkfile,/RELAXED_STRUCTURE_ASSIGNMENT
drk=sp

files=file_search(dir+'*.sav')

nn=n_elements(files)
if nn eq 0 then begin
	print,'no files'
	stop
endif
restore,files[0],/RELAXED_STRUCTURE_ASSIGNMENT	; -> p, sp

nwl0=n_elements(p.wl)
ii=where(p.wl ge wrange[0] and p.wl lt wrange[1])
iib=[indgen(100),nwl0-indgen(100)-1]
nwl=n_elements(ii)
sps=dblarr(nwl,nn)
date_obs=strarr(nn)
ftim=fltarr(nn)
bias=dblarr(nn)
drkbias=mean(drk[iib])

for i=0,nn-1 do begin
	print,files[i]
	restore,files[i],/RELAXED_STRUCTURE_ASSIGNMENT
	date_obs[i]=p.date_obs
	bias[i]=mean(sp[iib])
	sps[*,i]=sp[ii]-bias[i]-(drk[ii]-drkbias)
	ftim[i]=flttime(strmid(date_obs[i],11,8))
endfor
wl=p.wl[ii]

spav=rebin(sps,nwl,1)
spavc=smooth(spav,100)	; continuum
spm=mean(spav)
spsd=sps
for i=0,nn-1 do begin
	;spsd[*,i]=sps[*,i]*spm/mean(sps[*,i])-spav
	spsd[*,i]=sps[*,i]-spavc*mean(sps[*,i])/spm
endfor

tmin=6 &	tmax=17 &	dpt=60.
wt0=(tmax-tmin)*dpt

ww=900 
map=dblarr(ww,wt0)
for i=0,wt0-1 do begin
	jj=where(ftim ge tmin+float(i)/dpt and ftim lt tmin+float(i+1)/dpt, count)
	;print,i,count
	if count ne 0 then map[*,i]=congrid(sps[*,jj],ww,1)
endfor

x0=70 &	y0=70
window,xs=ww+100,ys=wt0+120
wt=(max(ftim)-min(ftim))*dpt
;tvscl,congrid(sps,ww,wt),x0,y0+(ftim[0]-tmin)*dpt
tvscl,map,x0,y0
plot,wl,tmin+findgen(n_elements(wl))*wt0,/nodata,/noerase, $
	xtitle='wavelength (nm)',xrange=wrange,xstyle=1, $
	ytitle='time (JST)',yrange=[tmin,tmax],ystyle=1, $
	pos=[x0,y0,x0+ww,y0+wt0],chars=1.5,/dev, $
	title='SSPM/SMART; '+strmid(date_obs[0],0,10)


return,map
stop
plot,wl,spav,xrange=wrange,xstyle=1,xtitle='wavelength (nm)'
i=1
plot,wl,(sps[*,i+1]-sps[*,i])/(sps[*,i+1]+sps[*,i])>(-0.01)<0.01 & i=16

end

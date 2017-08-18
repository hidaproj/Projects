; test_flame.pro

;*************************************************
function gt_sp,file,p=p

restore,file
sp1=double(sp1)
return,sp1
end

;*************************************************
dir='c:\data\flame\20160807\'
dfiles=findfile(dir+'d*ms*.sav')

goto,l1

;---  dark invest ----------------------------------

nf=n_elements(dfiles)

sp1=gt_sp(dfiles[0],p=p)
wl=p.wl
imgsize,sp1,nw,nn

expo=fltarr(nf)
avi=dblarr(nf)
disp1=dblarr(nf)

for i=0,nf-1 do begin
	sp1=gt_sp(dfiles[i],p=p)
	imgsize,sp1,nw,nn
	dsp1=dblarr(nw,nn)
	expo[i]=p.expo
	avi[i]=mean(sp1)
	avsp1=rebin(sp1,nw,1)
	for j=0,nn-1 do dsp1[*,j]=sp1[*,j]-avsp1
	disp1[i]=(mean(dsp1^2))
	;if expo[i] eq 200 then stop
endfor

coef=poly_fit(expo,avi,1)
exp1=findgen(1000)
fit=poly(exp1,coef)
print,'Bias=',fit[0]

stretch,255,0
window,xs=700,ys=500
plot_oo,expo,avi,psym=2,xtitle='expo [ms]',ytitle='count', $
	title='mean dark level / frame',chars=1.5
oplot,exp1,fit
xyouts,2,8000,'y='+string(coef[0],form='(f8.2)')+' + '+string(coef[1],form='(f5.3)')+'x expo',/data,chars=1.3
stop
plot_oo,expo,disp1,psym=2,xtitle='expo [ms]',ytitle='dispersion', $
	title='dark rms',chars=1.5
oplot,[1,1000],min(disp1)*[1,1]

stop
l1:
;-------------------------------------
;  solar spectrum and ND

sun=gt_sp(dir+'sun20160807_125345.757.sav',p=p)
nd2sun=gt_sp(dir+'nd2sun20160807_125354.609.sav',p=p)
drk=gt_sp(dir+'drk20160807_125422.603.sav',p=p)
drk1=congrid(drk,nw,1)
imgsize,sun,nw,nn
for i=0,nn-1 do begin
	sun[*,i]=sun[*,i]-drk1
	nd2sun[*,i]=nd2sun[*,i]-drk1
endfor
avsun=congrid(sun,nw,1)
avnd2=congrid(nd2sun,nw,1)

window,xs=1500,ys=700
plot,wl,avsun,xtitle='wavelength [nm]',ytitle='I',xrange=[300,900],xstyle=1, $
	title='',chars=1.5,thick=1
oplot,wl,avnd2


l2:
;--------------------------------------------------------
dir='c:\data\flame\20160808\'

sun=gt_sp(dir+'sun4000_20160808_115405.997.sav',p=p)
drk=gt_sp(dir+'drk.sav',p=p)
drk1=congrid(drk,nw,1)
imgsize,sun,nw,nn
for i=0,nn-1 do begin
	sun[*,i]=sun[*,i]-drk1
endfor
avsun=congrid(sun,nw,1)

wrs=[[370,400],[500,550],[630,670]]
imgsize,wrs,n2,nr

nintegs=[1,2,5,10,25,50,100,200,500,1000]
ni=n_elements(nintegs)
disps=fltarr(ni)
eps=fltarr(ni)
iiodd=indgen(nn/2)*2+1
iievn=indgen(nn/2)*2
window,xs=800,ys=600
plot_oo,nintegs,eps,psym=2,yrange=[1e-4,1e-1],xtitle='n_integ', $
	chars=1.5,ytitle='!7e!3',xrange=[0.8,2000],xstyle=1
for i=0,2 do oplot,[0.1,2000],1e-3*10^i*[1,1],line=2

for iw=0,nr-1 do begin
	w1=wrs[0,iw] &	w2=wrs[1,iw]
	iiw=where(wl ge w1 and wl lt w2)
	for k=0,ni-1 do begin
		nset=nn/nintegs[k]/2
		spodd=rebin(sun[*,iiodd],nw,nset)
		spevn=rebin(sun[*,iievn],nw,nset)
		disp=fltarr(nset)
		for j=0,nset-1 do begin
			diff=spodd[iiw,j]-spevn[iiw,j]
			disp[j]=mean(diff^2)
		endfor
		if nset gt 1 then disp1=mean(disp) else disp1=disp[0]
		print,disp1
		disps[k]=disp1
		iav=mean(sun[iiw,*])
		eps[k]=sqrt(disp1)/iav
	endfor
	oplot,nintegs,eps,psym=iw+4

	x0=1.5 &	y0=2e-4*1.5^iw
	oplot,[x0],[y0],psym=iw+4
	xyouts,x0*1.2,y0,string(w1,form='(i4)')+'-'+string(w2,form='(i4)')+'nm',chars=1.2

endfor

stop


ss=smooth(avsun,400,/edge_trun)
ss=ss/max(ss)*2
ii=where(ss gt 1) &	ss[ii]=(ss[ii])^0.5
ii=where(ss lt 1) &	ss[ii]=(ss[ii])^4
ss=ss/max(ss)
mx=1.5
tr=(mx-ss)/mx
plot,wl,tr,xtitle='wavelength [nm]',ytitle='transmission', $
	chars=1.5,yrange=[0,1.1],xrange=[200,900],thick=3
oplot,wl,avsun/max(avsun)*tr
oplot,wl,avsun/max(avsun),line=2

stop

sunm=gt_sp(dir+'sunmax_20160808_115312.234.sav',p=p)
sunm=sunm-drk1
plot,wl,sunm,xtitle='wavelength [nm]',ytitle='count',chars=1.5,xrange=[200,900],thick=1



stop


;save,p,sun,file=dir+'caled_sun.sav'

nd=avnd2/avsun
plot,wl,nd,xtitle='wavelength [nm]',ytitle='ND2 transmission',xrange=[300,900],xstyle=1, $
	title='',chars=1.5,thick=1,yrange=[0,1]


end

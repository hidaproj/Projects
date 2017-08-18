;+
;	spemirh
;	Speckle Masking Image Reconstruction of Hida

;  Calling Seq.
;	p=spemir_st()
;	p.pix1=0.215		; arcsec/pix
;	p.w=64			; segment size, pix
;	p.D=250.		; Telescope diameter, mm
;	p.wl=647.1		; nm
;	p.wap=p.w/5		; appodization width
;
;	rimg=spemirh(imgs,p=p,avimg=avimg,saimg=saimg,consis=consis, /deconv )
;	
;	return recovered image, rimg[*,*]
;	avimg	- return average image[*,*]
;	saimg	- return shift and add image[*,*]
;	consis	- return consistency
;	deconv	- if set, apply max_likelihood deconvolution to the result
;
;	for DST (@Hida) data
;	rimg=spemirh(imgs, /dst, wl=wl, smpl=pix1, ws=ws, /deconv, /trim)
;	  ws	- window size, pix, default = 128

;
;  REFERENCE
;   Lohmann, Weigelt, and Wirnitzer, 1983, App. Opt., 22, 4028
;   Pehlemann and von der Luhe, 1989, AA, 216, 337
;   O. von del Luhe, 1984, J.Opt.Soc.Am.,1, 5	--> r0
;   Ricort and Aime, 1979, AA, 76, 324	--> STF formula


;** MODIFICATION HISTORY **
function version
ver='0.1'	; 2013.05.24	K.Ichimoto, T.Kawate	new, phase recovery
ver='0.2'	; 2013.06.04	K.Ichimoto, T.Kawate	amplitude recovery
ver='0.3'	; 2013.06.09	K.Ichimoto, 		bug fix, new struct., central obsc.
ver='0.31'	; 2013.06.12	K.Ichimoto, 		No_align option in p, minimum display
ver='0.32'	; 2013.06.14	K.Ichimoto, T.Kawate	the alignment with image shift [mean->median]
ver='0.33'	; 2013.06.15	K.Ichimoto, 		align_imgs new, r0 bug fix
ver='0.34'	; 2013.06.19	K.Ichimoto, 		align_imgs pix keyword
ver='0.4'	; 2013.06.23	K.Ichimoto, T.Kawate	revise p.*, improve phase recov. 
ver='0.41'	; 2013.06.25	K.Ichimoto		bug in phase recovery fix
ver='0.5'	; 2013.06.27	K.Ichimoto, T.Kawate	p.c_trun, subtract fit1, p.k_trun -> p.ph_recov
ver='0.51'	; 2013.07.06	K.Ichimoto, T.Kawate	deconv keyword
ver='0.6'	; 2013.07.10	K.Ichimoto		plane fit by average imgps, deconv for 2 imgs
ver='0.61'	; 2013.09.28	K.Ichimoto		fix error in under sampling,
ver='0.62'	; 2014.04.06	K.Ichimoto		fix error x1=wx2-ps[l] & x2=wx2+ps[l]
ver='0.7'	; 2014.04.28	K.Ichimoto		dst, fisch keyword
ver='0.71'	; 2014.04.29	K.Ichimoto		ws, smpl, wl keywords
ver='0.72'	; 2014.05.28	K.Ichimoto		silent keywords, if deconv ge 5,,,
ver='0.73'	; 2014.08.12	K.Ichimoto		spemir_init, r0  wx1 -> wx1/2, sobel(img1/max(img1)*10000.)
ver='0.8'	; 2015.09.27	K.Ichimoto		extend operation to edges of FOV
ver='0.81'	; 2016.08.16	K.Ichimoto		sddi keyword
ver='0.9'	; 2017.01.31	K.Otsuji		imgsize->size
ver='0.99'	; 2017.02.12	K.Otsuji		enable parallel processing
ver='1.0'	; 2017.02.12	K.Ichimoto		rimg-rmin in deconv
ver='1.1'	; 2017.02.21	K.Otsuji		enable to run on >IDL6.4 (64bit)
ver='1.11'	; 2017.02.28	K.Otsuji		include bridge procedures (build_,barriar_,burn_brudge)

return,ver
end

;-


;**************************************************
function spemir_st

p={spemir_st,		$ ;
	D:		250., $		; Telescope diameter, mm
	Dco:		0., $		; central obscuration diameter, mm
	pix1:		0.215, $	; arcsec/pix
	wl:		647.1, $	; wavelength, nm
	w:		64, $		; segment size, pix
	wap:		12, $		; appodization width
	img_align:	1, $		; if 1, make initial image alignment
	seg_align:	1, $		; if 1, make segment alignment
	seg_select:	1., $		; use only segments whose displacement < w/2*select
	amp_recov:	0.5, $		; amplitude supression index,  amp*istf^amp_recov, if 0, no amp recov.
	ph_recov:	1., $		; truncation factor of k in bispectrum phase recovery, if 0,  average phase
	ph_consis:	0.9, $		; minimum consistency to accept the average phase
	c_trun:		50, $		; truncation of # of pair in phase recovery, select consistent one
	verbose:	1., $		; 1. display image, 2. display consis
	nframe:		0., $		; # of processedframes
	r0:		0., $		; Fried param, mm
	deconv:		0, $		; if deconv keyword is set, set 1 (for record)
	ver: version() $		; version of spemirh.pro
	}
return,p

end

;***************************************************
pro pp,c,title=title
;  display phasor[*] array

if not keyword_set(title) then title=''
mx=max(abs(c))
plot,mx*[-1,1],mx*[-1,1],/nodata,title=title
fl=float(c)
im=imaginary(c)
for i=0,n_elements(c)-1 do oplot,[0,fl[i]],[0,im[i]]

end

;**************************************************
function avphase,c,consis=consis
;   return average phasor

	a=total(c)
	aa=abs(a)
	consis=aa/total(abs(c))
	if aa eq 0. then return,0. else return,a/aa
end

;**************************************************
function bisp,i,j,k,l,ff
;   return bispectrum(i,j,k,l) of ff[i,j]

	bsp=ff[i,j,*]*ff[k,l,*]*conj(ff[i+k,j+l,*])

	return,bsp

end

;**************************************************
function rmap,wx1,wy1
;  2D array of distance from the center

x1=(findgen(wx1)-wx1/2)
y1=(findgen(wy1)-wy1/2)
x=x1 # transpose(replicate(1,wy1))
y=replicate(1,wx1) # transpose(y1)
r=sqrt(x^2+y^2)

return,r
end


;**************************************************
pro align_imgs,imgps,dxy=dxy,box=box,pix=pix,select=select
;----------  align imgs[*,*,nn] --------
;   box[x1,x2,y1,y2] for determine dxy
;   pix	- if set, no sub-pixel shift

	imgsize=size(imgps,/dim)
        wx1=imgsize[0]
        wy1=imgsize[1]
        nn=imgsize[2]
	if n_elements(box) eq 4 then begin
		x1=box[0] &	x2=box[1]
		y1=box[2] &	y2=box[3]
	endif else begin
		x1=0 &	x2=wx1-1
		y1=0 &	y2=wy1-1
	endelse
	var=dblarr(nn)
	npix=long(x2-x1+1)*long(y2-y1+1)
	for i=0,nn-1 do begin
		av=mean(imgps[x1:x2,y1:y2,i])
		var[i]=total((imgps[x1:x2,y1:y2,i]-av)^2)/npix
	endfor
	dmy=max(var,im)
	imgp0=imgps[x1:x2,y1:y2,im]

	nx=x2-x1+1 &	ny=y2-y1+1
	dxy=dblarr(2,nn)
	f0=fft(imgp0,1)
	for i=0,nn-1 do begin
		f1=fft(imgps[x1:x2,y1:y2,i],1)
		ff=fft(f0*conj(f1),-1)
		pp=abs(ff*conj(ff))
		pp=shift(pp,nx/2,ny/2)
		maxval=max(pp,index)
		xp0=index mod nx
		yp0=index/nx
		if keyword_set(pix) then begin
			dxy[*,i]=[xp0,yp0]
		endif else begin
		    if xp0 eq 0 or yp0 eq 0 or xp0 eq nx-1 or yp0 eq ny-1 then begin
			dxy[*,i]=[xp0,yp0]
		    endif else begin
			xp=xp0+(pp[xp0+1,yp0]-pp[xp0-1,yp0])/(pp[xp0+1,yp0]+pp[xp0-1,yp0])
			yp=yp0+(pp[xp0,yp0+1]-pp[xp0,yp0-1])/(pp[xp0,yp0+1]+pp[xp0,yp0-1])
			dxy[*,i]=[xp,yp]
		    endelse
		;tvscl,pp
		endelse
	endfor
	;;dxy[0,*]=dxy[0,*]-nx/2 &	dxy[1,*]=dxy[1,*]-ny/2
	dxy[0,*]=dxy[0,*]-median(dxy[0,*])
	dxy[1,*]=dxy[1,*]-median(dxy[1,*])
	
	if not keyword_set(select) then select=0.
	if select gt 0. and select lt 1. then begin
		rr=sqrt(dxy[0,*]^2+dxy[1,*])
		ii=where(rr lt wx1/2*select)
		imgps=imgps[*,*,ii]
		nn=n_elements(ii)
		print,nn
	endif

	for  i=0,nn-1 do imgps[*,*,i]=imgshift(imgps[*,*,i],dxy[0,i],dxy[1,i])

end

;**************************************************
pro dispcc,cc,r=r,fact=fct,win=win,cntr=cntr
;  display consis[wx1,wy1]

win0=!d.window
if not keyword_set(fct) then fct=4
if n_elements(win) then wset,win
imgsize=size(cc,/dim)
nx=imgsize[0]
ny=imgsize[1]
if !d.x_size lt nx*fct+100 or !d.y_size lt ny*fct+100 then $
	window,!d.window,xs=nx*fct+100,ys=ny*fct+100
pos=[50,50,50+nx*fct,50+ny*fct]
tvscl,rebin(cc,nx*fct,ny*fct,/sampl),pos[0],pos[1]
plot,findgen(nx)-nx/2,findgen(ny)-ny/2,pos=pos, $
	xstyle=1,ystyle=1,/noerase,/dev,/nodata
if keyword_set(r) then circle,nx/2*fct+pos[0]+fct/2,ny/2*fct+pos[1]+fct/2,r*fct,line=2
if keyword_set(cntr) then begin
	contour,rebin(cc,nx*fct,ny*fct,/sampl),pos=pos,level=[cntr],col=0, $
		/dev,/noerase,xstyle=1+4,ystyle=1+4
endif

wset,win0


end

;**************************************************
function spemir1,imgps,imgsa1=imgsa1,p=p,iwx=iwx,iwy=iwy

common spemir, wx1,wy1,nn,dxy,fs,amp,phs,op,consis,q,q1,istf,appo,ov,mmax,im,jm,dd

	imgsize=size(imgps,/dim)
        wx1=imgsize[0]
        wy1=imgsize[1]
        nn=imgsize[2]

	;----------  align a segment series --------
	if p.seg_align then begin
		align_imgs,imgps,dxy=dxy,select=p.seg_select
                imgsize=size(imgps,/dim)
                wx1=imgsize[0]
                wy1=imgsize[1]
                nn=imgsize[2]
	endif

	;-- shift & add --
	imgsa1=rebin(imgps,wx1,wy1,1)

	;----- Foulier transform -----
	fs=complexarr(wx1,wy1,nn)
	avimgp1=rebin(float(imgps),wx1,wy1,1)
	fit1=sfit(avimgp1,1)
	for i=0,nn-1 do begin
		imgp1=imgps[*,*,i]
		imgp1=(imgp1-fit1)*appo
		f=fft(imgp1,-1)
		fs[*,*,i]=f
	endfor
	avp=float(total(fs*conj(fs),3)/nn)
	avf=total(fs,3)/nn
	avf2=float(avf*conj(avf))

	;--------  amplitude recovery  -------
	amp=sqrt(avp)
	ddd=2
	bkg=mean(amp[wx1/2-ddd:wx1/2+ddd,wy1/2-ddd:wy1/2+ddd])
	amp=amp-bkg	; <-- noise remove..

	if p.amp_recov then begin	;---  STF from r0 ---
		amp=amp*istf
	endif
	
	;--------  phase recovery  -------
	; fs[*,*,nn]
	phs=fs/abs(fs)
	;phs=fs	; weight with amplitude, it works but no significant diff.
	op=complexarr(wx1,wy1)
	consis=fltarr(wx1,wy1)
	cc=intarr(wx1,wy1)	; 

	wx2=wx1/2 &	wy2=wy1/2
	;-- phase average --
	phs=shift(phs,wx2,wy2,0)
	a=total(phs,3)
	aa=abs(a)
	consis=aa/total(abs(phs),3)
	ii=where(aa eq 0, count)
	if count ne 0 then aa[ii]=1.
	op=a/aa
	if p.verbose ge 2 then dispcc,consis,fact=4,r=mmax,win=3;,cntr=p.ph_consis

	if 0 then begin	; for check
	;if iwx ge 2 and iwy ge 2 then begin
	;if iwx eq 6 and iwy eq 4 then begin
		print,iwx,iwy
		dispcc,consis,fact=4,r=mmax,win=3
		;wset,3 &	win2gif,'c:\tmp\a.gif' &	wset,0
		stop
		ijp=[1,1] 
		pp,phs[wx2+ijp[0],wy2+ijp[1],*],title='consis='+string(consis[wx2+ijp[0],wy2+ijp[1]],form='(f6.3)')
	end

	if p.ph_recov ne 0. then begin	; phase recovery using bispectrum
		iic=where(consis gt p.ph_consis and dd lt mmax, ic)
		if ic le 1 then begin
			print,'No consistent phase. skip phase recov...'
			goto,l1
		endif
		cc[iic]=2
		ij0=where(cc eq 0 and dd lt mmax*p.ph_recov and jm ge 0, nm)
		ij0=ij0(sort(dd[ij0]))
		ps=im[ij0] &	qs=jm[ij0]	; pixels [i,j] to be recovered
		cc[wx2,wy2]=0	; not use (0,0) elem.
		bn=complexarr(long(wx1)*long(wy2),nn)
		;cc2=intarr(nm)
		for l=0,nm-1 do begin
			ij=where((cc gt 0) and $
				(ps[l]-im ge -wx2) and (ps[l]-im lt wx2) and $
				(qs[l]-jm ge -wy2) and (qs[l]-jm lt wy2), count)
			;print,'count=',count
			if count gt 0 then begin
				;wset,3	
				;plot,im[ij],jm[ij],psym=4,symsize=2
				;oplot,im[ij[0:count/2]],jm[ij[0:count/2]],psym=1
				;oplot,ps[l]-im[ij[0:count/2]],qs[l]-jm[ij[0:count/2]],psym=2
				;wset,0
				i2=ps[l]-im[ij]
				j2=qs[l]-jm[ij]
				ij2=(wy2+j2)*wx1+wx2+i2
				ij3=where(cc[ij2] gt 0, count2)
				count2=count2/2
				ij3=ij3[0:count2]
				;cc2[l]=count2
				if count2 gt 0 then begin
					if count2 gt p.c_trun then begin
						cons=consis[ij2[ij3]]*consis[ij[ij3]]
						ss=reverse(sort(cons))
						ij3=ij3[ss]
						count2=min([count2,p.c_trun])
					endif
					ia=im[ij[ij3]] &	ja=jm[ij[ij3]]
					ib=im[ij2[ij3]] &	jb=jm[ij2[ij3]]
					; plot,consis[wx2+ia,wy2+ja]*consis[wx2+ib,wy2+jb],xtitle='pix',ytitle='consistency!u2!n',chars=1.5	; for check
					for i=0,count2 do begin
						bn[i,*]=phs[wx2+ia[i],wy2+ja[i],*] $
							*phs[wx2+ib[i],wy2+jb[i],*] $
							*conj(phs[wx2+ps[l],wy2+qs[l],*])
					endfor
					b=op[wx2+ia,wy2+ja]*op[wx2+ib,wy2+jb]*conj(total(bn[0:count2/2,*],2))
					ab=abs(b)
					iii=where(ab gt 0., countb)
					if countb ge 1 then b[iii]=b[iii]/ab[iii]
					x1=wx2-ps[l] &	x2=wx2+ps[l] &	x1=x1>0<(p.w-1) &  x2=x2>0<(p.w-1)
					y1=wy2-qs[l] &	y2=wy2+qs[l] &	y1=y1>0<(p.w-1) &  y2=y2>0<(p.w-1)
					op[x2,y2]=avphase(b,con=a)
					op[x1,y1]=conj(op[x2,y2])
					consis[x2,y2]=a
					consis[x1,y1]=a
					cc[x2,y2]=1
					cc[x1,y1]=1
				endif ; else print,'count2=0'
			endif else begin
				print,ps[l],qs[l],'  count=0'
			endelse
			if p.verbose ge 2 then dispcc,cc,fact=4,win=3

		;	stop
		endfor
	endif

	l1:
	op=shift(op,-wx2,-wy2)
	phs=shift(phs,-wx2,-wy2,0)

	;------  image reconstruct  -----------------
	ft=amp*op
	imgr1=float(fft(ft,1))
	imgr1=imgr1+fit1

	;if iwx eq 6 and iwy eq 4 then stop	;- for test


return,imgr1

end

;**************************************************
function spemir1_last,imgps,imgsa1=imgsa1,p=p,iwx=iwx,iwy=iwy

common spemir, wx1,wy1,nn,dxy,fs,amp,phs,op,consis,q,q1,istf,appo,ov,mmax,im,jm,dd

;  amp and op[*,*] using difference from average in last process

	;----------  align a segment series --------
	if p.seg_align then begin
		for  i=0,nn-1 do imgps[*,*,i]=imgshift(imgps[*,*,i],dxy[0,i],dxy[1,i])
	endif

	;-- shift & add --
	imgsa1=rebin(imgps,wx1,wy1,1)

	;----- Foulier transform -----
	fs=complexarr(wx1,wy1,nn)
	avimgp1=rebin(float(imgps),wx1,wy1,1)
	fit1=sfit(avimgp1,1)
	for i=0,nn-1 do begin
		imgp1=imgps[*,*,i]
		imgp1=(imgp1-fit1)*appo
		f=fft(imgp1,-1)
		fs[*,*,i]=f
	endfor
	avp=float(total(fs*conj(fs),3)/nn)
	avf=total(fs,3)/nn
	avf2=float(avf*conj(avf))

	;--------  amplitude recovery  -------
	amp=sqrt(avp)
	ddd=2
	bkg=mean(amp[wx1/2-ddd:wx1/2+ddd,wy1/2-ddd:wy1/2+ddd])
	amp=amp-bkg	; <-- noise remove..

	if p.amp_recov then begin	;---  STF from r0 ---
		amp=amp*istf
	endif
	
	;--------  phase recovery  -------
	; fs[*,*,nn]
	mph0=total(phs,3)	; last phase
	ii=where(abs(mph0) ne 0.)
	mph0[ii]=mph0[ii]/abs(mph0[ii])
	ii=where(abs(fs) ne 0.)
	phs=fs
	phs[ii]=fs[ii]/abs(fs[ii])
	mph=total(phs,3)
	ii=where(abs(mph) ne 0.) 
	mph[ii]=mph[ii]/abs(mph[ii])
	op=(op-mph0)+mph
	ii=where(abs(op) ne 0.)
	op[ii]=op[ii]/abs(op[ii])

	;------  image reconstruct  -----------------
	ft=amp*op
	imgr1=float(fft(ft,1))
	imgr1=imgr1+fit1

	;if iwx eq 1 and iwy eq 0 then stop	;- for test


return,imgr1

end


;**************************************************
pro spemir_init,imgs,p=p

common spemir, wx1,wy1,nn,dxy,fs,amp,phs,op,consis,q,q1,istf,appo,ov,mmax,im,jm,dd

imgsize=size(imgs,/dim)
nx=imgsize[0]
ny=imgsize[1]
nn=imgsize[2]


p.nframe=nn

wx1=p.w &	wy1=p.w

;-----  appodization filter for a segment  --------
if p.wap eq 0 then 	appo=replicate(1.,wx1,wy1) $
else 			appo=edge_appod_filt(wx1,wy1,wap=p.wap)

;-----  wave number vector  ------------
reso=p.wl*1e-6/p.D/!pi*3600.*180.
k_limit=1./reso	; wave number of diff. limit,  #/arcsec
dkx=1./(p.pix1*wx1)
dky=1./(p.pix1*wy1)
dk=1./(p.pix1*wx1)
kx=[findgen(wx1/2)*dkx,(wx1/2-findgen(wx1/2))*dkx]
ky=[findgen(wy1/2)*dky,(wy1/2-findgen(wy1/2))*dky]
q=findgen(wx1,wy1)
for j=0,wy1-1 do begin
	q[*,j]=sqrt(ky[j]^2+kx^2)/k_limit
endfor
dq=min([dkx,dky])/k_limit
q1=findgen(max(q)/dq)*dq
nq=n_elements(q1)
kk=q*k_limit
k=q1*k_limit


;------  theoretical MTF -----
ra=reso*1.22	; radius of Airy disk
mtf1=mtf(q1,eps=p.Dco/p.D)
ii=where(q1 ge 1, count) &	if count gt 0 then mtf1[ii]=0.
mtf2=mtf(q,eps=p.Dco/p.D)
ii=where(q ge 1, count) &	if count gt 0 then mtf2[ii]=0.
mmax=fix(1./dq)+1


;-----  r0 from <S>^2/<S^2>  -----
;img1=float(imgs[*,*,0])
img1=float(imgs[wx1/2:nx-wx1/2-1,wy1/2:ny-wy1/2-1,0])	; exclude edge
img1=img1*sobel(img1/max(img1)*10000.)
dmy=max(img1,ic)
;x0=ic mod nx &	x0=max([x0,wx1]) &	x0=min([x0,nx-wx1-1]) &	x0=x0+wx1
;y0=ic/nx &	y0=max([y0,wy1]) &	y0=min([y0,ny-wy1-1]) &	y0=y0+wy1
x0=ic mod (nx-wx1) &	x0=x0+wx1/2
y0=ic/(nx-wy1) &	y0=y0+wy1/2
imgps=imgs[x0-wx1/2:x0+wx1/2-1,y0-wy1/2:y0+wy1/2-1,*]

align_imgs,imgps,dxy=dxy

avp=fltarr(wx1,wy1)
avf=complexarr(wx1,wy1)
for i=0,nn-1 do begin
	img1=imgps[*,*,i]
	img1=(img1-mean(img1))*appo
	f=fft(img1,-1)
	avf=avf+f
	avp=avp+float(f*conj(f))
endfor
avp=avp/nn
avf=avf/nn
ddd=2
bkg=min(avp[wx1/2-ddd:wx1/2+ddd,wy1/2-ddd:wy1/2+ddd])
avp=avp-bkg
avf2=float(avf*conj(avf))
eps=avf2/avp

avp1=dblarr(nq)
avf1=dblarr(nq)
eps1=dblarr(nq)
for i=0,nq-2 do begin
	ii=where(q gt q1[i] and q le q1[i+1])
	avp1[i]=mean(avp[ii])
	avf1[i]=mean(avf2[ii])
	eps1[i]=mean(eps[ii])
endfor

de=0.02
;;e0=0.5 & A=5.01 &	B=1.27	; for eps=0.5,  long exp. von del Luhe, 1984, J.Opt.Soc.Am.,1, 5
;;e0=0.7 & A=8.92 &	B=1.13	; for eps=0.7,  long exp.
e0=0.5 & A=0.99 &	B=1.07	; for eps=0.5,  short exp.
eps1=smooth(eps1,2)
i0=min(where(eps1 lt e0))
i3=[i0-1,i0,i0+1]
qm=interpol(q1[i3],eps1[i3],e0) &	qm=qm[0]
alp=A*qm^B
r0=p.D*alp	; Fried param.

;--- another way to obtain r0 ---
;Qmx=0.7
;ii=where(q1 le Qmx)
;A=-total(6.88*5./6*5./3*(1./0.44*(1-eps1[ii])/eps1[ii]*mtf1[ii])^(-5./6)*q1[ii]^(2./3))*dq
;r0b=D*(-6.88*5/6./alog(A+1))^(3./5.)*Qmx

print,'r0=',r0,' mm'
p.r0=r0


;---  Inverse Speckle Transfer Func. STF from r0 -----------
;---  amp -> amp*istf
istf1=float(sqrt( mtf1/(mtf1*exp(-6.88*(q1/alp)^(5./3.))+0.44*alp^2) ))
istf=float(sqrt( mtf2/(mtf2*exp(-6.88*(q/alp)^(5./3.))+0.44*alp^2) ))
istf=istf^p.amp_recov


;------  overlap func. ----------
ov=fltarr(wx1,wy1)
cx=(-cos(findgen(wx1)/wx1*2*!pi)+1)/2.
cy=(transpose(-cos(findgen(wy1)/wy1*2*!pi))+1)/2.
for j=0,wy1-1 do ov[*,j]=cx
for i=0,wx1-1 do ov[i,*]=ov[i,*]*cy


;------  ij-map ----------
im=(indgen(wx1)-wx1/2)#transpose(replicate(1,wy1))
jm=replicate(1,wx1)#transpose(indgen(wy1)-wy1/2)
dd=sqrt(im^2+jm^2)

if 0 then begin	;-- for demo --
	window,2,xs=450,ys=512
	xtit='!8q!3 (= !8u/u!3!llimit!n)'
	;xrange=[0,max(q1)]
	xrange=[0,1.2]
	plot_io,q1,sqrt(avp1/max(avp1)),xtitle=xtit,xrange=xrange,xstyle=1,yrange=[1e-4,1],chars=1.5
	oplot,q1,sqrt(avf1/max(avp1)),line=2
	oplot,q1,mtf1>1e-4,line=1
	stop
	plot,q1,eps1,xtitle=xtit,xrange=xrange,xstyle=1,ytitle='eps',chars=1.5
	xyouts,0.4,0.6,'!8r!3!l0!n ='+string(p.r0,form='(f5.1)')+'mm',chars=1.5,/data
	stop
	plot,q1,istf1,xtitle=xtit,xrange=xrange,xstyle=1,ytitle='!8R!3(!8q!3)',chars=1.5
	oplot,q1,istf1^0.5,line=2
	stop
endif

end

;***********************************************************
function spemirh_build_bridges, ncpus, nthreads
  if n_elements(ncpus) eq 0 then $
     ncpus = !cpu.hw_ncpu
  if n_elements(nthreads) eq 0 then $
     nthreads = 1
  bridges = objarr(ncpus)
  cd, current=pwd
  for cpu=0,ncpus-1 do begin
     ; create bridge
     bridges[cpu] = obj_new('IDL_IDLBridge')
     ; execute startup
     ;(bridges[cpu])->execute, '@' + pref_get('IDL_STARTUP')
     ; set path to include PWD
     (bridges[cpu])->execute, "cd, '" + pwd + "'"
     ; set thread pool params
     (bridges[cpu])->execute, "cpu, tpool_nthreads=" + string(nthreads)
  endfor
  return, bridges
end

;***********************************************************
pro spemirh_barrier_bridges, bridges
  ncpus = n_elements(bridges)
  idle = bytarr(ncpus)
  widle = where(idle eq 0, nw)
  repeat begin
     for i=0,nw-1 do begin
        case (bridges[widle[i]])->status(error=errstr) of
           0: idle[widle[i]] = 1b
           2: idle[widle[i]] = 1b
           3: begin
              print, 'Error encountered: '+errstr
              stop
           end
           4: begin
              print, 'Aborted execution: '+errstr
              stop
           end
           else:                ; do nothing
        endcase
     endfor
     widle = where(idle eq 0, nw)
     if nw gt 0 then wait, 1    ; idle loop
  endrep until nw eq 0
end

;***********************************************************
pro spemirh_burn_bridges, bridges
  ncpus = n_elements(bridges)
  for cpu=0,ncpus-1 do $
     obj_destroy, bridges[cpu]
end

;***********************************************************
pro spemirh_callback, status, error, bridge, userdata
  
  common spemir, wx1,wy1

  imgr1 = bridge->getvar('imgr1')
  imgsa = bridge->getvar('imgsa')
  consis = bridge->getvar('consis')

  (*(userdata.primg))[userdata.x1:userdata.x1+wx1-1,userdata.y1:userdata.y1+wy1-1]+=imgr1*userdata.ov
  (*(userdata.psaimg))[userdata.x1:userdata.x1+wx1-1,userdata.y1:userdata.y1+wy1-1]+=imgsa*userdata.ov

  if userdata.verbose ge 1 then begin
     wset,0
     tv,bytscl((*(userdata.primg))[userdata.x1:userdata.x1+wx1-1,userdata.y1:userdata.y1+wy1-1],min=userdata.min1,max=userdata.max1),userdata.x1,userdata.y1
     wait,0.01
  endif

  (*(userdata.pconsisr))+=consis

  ;win2jpeg,'c:\tmp\m\a_'+string(count+nn+1,form='(i3.3)')+'.jpg'
  ;count=count+1

  ;---  2nd image  ----
  if userdata.IM2 then begin
     imgr2 = bridge->getvar('imgr2')
     imgsa2 = bridge->getvar('imgsa2')
     
     (*(userdata.primg2))[userdata.x1:userdata.x1+wx1-1,userdata.y1:userdata.y1+wy1-1]+=imgr2*userdata.ov
     (*(userdata.psaimg2))[userdata.x1:userdata.x1+wx1-1,userdata.y1:userdata.y1+wy1-1]+=imgsa2*userdata.ov

     if userdata.verbose ge 1 then begin
        wset,2
        tv,bytscl((*(userdata.primg))[userdata.x1:userdata.x1+wx1-1,userdata.y1:userdata.y1+wy1-1],min=userdata.min2,max=userdata.max2),userdata.x1,userdata.y1
     endif
  endif

end

;***********************************************************
pro spemirh_worker, p, iwx, iwy, imgps, imgr1, imgsa, imgps2, imgr2, imgsa2
  
  common spemir, wx1,wy1,nn,dxy,fs,amp,phs,op,consis,q,q1,istf,appo,ov,mmax,im,jm,dd
  
  imgr1=spemir1(imgps,imgsa1=imgsa,p=p,iwx=iwx,iwy=iwy)

  if n_elements(imgps2) ne 0 then imgr2=spemir1_last(imgps2,imgsa1=imgsa2,p=p,iwx=iwx,iwy=iwy)

end

;***********************************************************
function spemirh,imgs,imgs2,rimg=rimg,p=p, $
	avimg=avimg,saimg=saimg,istf=istfr,consis=consisr,deconv=deconv, $
	DST=dst,FISCH=fisch,SDDI=sddi,smpl=pix1,ws=ws,wl=wl,trim=trim,silent=silent

common spemir, wx1,wy1,nn,dxy,fs,amp,phs,op,consis,q,q1,istf,appo,ov,mmax,im,jm,dd

if keyword_set(dst) then begin
	p=spemir_st()
	p.D=600.		; mm
	p.DCO=210.		; mm
	p.wl=wl			; nm
	if keyword_set(ws) then p.w=ws	else p.w=128		; pix
	p.pix1=pix1
endif
if keyword_set(fisch) then begin
	p=spemir_st()
	p.D=250.		; mm
	p.DCO=0.		; mm
	p.wl=wl			; nm
	if keyword_set(ws) then p.w=ws	else p.w=64		; pix
	p.pix1=pix1
endif
if keyword_set(sddi) then begin
	p=spemir_st()
	p.D=200.		; mm
	p.DCO=0.		; mm
	p.wl=656.3		; nm
	if keyword_set(ws) then p.w=ws	else p.w=64		; pix
	p.pix1=1.23
endif
if keyword_set(silent) then p.verbose=0

if p.img_align then begin
	align_imgs,imgs,dxy=dxy,/pix;,box=[nx/3,nx/3*2-1,ny/3,ny/3*2-1]
endif

spemir_init,imgs,p=p

if n_elements(imgs2) ne 0 then IM2=1 else IM2=0

imgsize=size(imgs,/dim)
nx=imgsize[0]
ny=imgsize[1]
nn=imgsize[2]
imin=0
wx1=p.w &	wy1=p.w

avimg=rebin(float(imgs),nx,ny,1)
if p.verbose ge 1 then begin
	window,0,xs=nx,ys=ny
	tvscl,avimg>imin
endif
timg=avimg
;min1=min(avimg)*0.95 &	max1=max(avimg)*1.05
min1=min(avimg)*0.88 &	max1=max(avimg)*1.12

;win2jpeg,'c:\tmp\m\a_'+string(nn,form='(i3.3)')+'.jpg'

;----------  segmentation  -------------------
rimg=fltarr(nx,ny)	; reconstructed image
saimg=fltarr(nx,ny)	; shift & add image

primg=ptr_new(fltarr(nx,ny),/no_copy)	; reconstructed image pointer
psaimg=ptr_new(fltarr(nx,ny),/no_copy)	; shift & add image pointer
if IM2 then begin
   avimg2=rebin(float(imgs2),nx,ny,1)
   timg2=avimg2
   rimg2=fltarr(nx,ny)          ; reconstructed image -2
   saimg2=fltarr(nx,ny)         ; shift & add image
   
   primg2=ptr_new(fltarr(nx,ny),/no_copy) ; reconstructed image -2 pointer
   psaimg2=ptr_new(fltarr(nx,ny),/no_copy) ; shift & add image pointer
  
   if p.verbose ge 1 then begin
      window,2,xs=nx,ys=ny
      tvscl,avimg2
   endif
   min2=min(avimg2)*0.95 &	max2=max(avimg2)*1.05
endif

nwx=nx/(wx1/2)-1 &	nwy=ny/(wy1/2)-1
if nx mod wx1/2 gt 10 then nwx=nwx+1
if ny mod wy1/2 gt 10 then nwy=nwy+1

consisr=fltarr(wx1,wy1)
pconsisr=ptr_new(fltarr(wx1,wy1),/no_copy)

;parallel bridge
if n_elements(bridges) eq 0 then bridges = spemirh_build_bridges()
ncpus = n_elements(bridges)

for i=0,ncpus-1 do begin
   (bridges[i])->execute, '.r spemirh'

   (bridges[i])->setproperty, callback='spemirh_callback'
   
   (bridges[i])->execute, 'common spemir, wx1,wy1,nn,dxy,fs,amp,phs,op,consis,q,q1,istf,appo,ov,mmax,im,jm,dd'

   (bridges[i])->setvar, 'wx1', wx1
   (bridges[i])->setvar, 'wy1', wy1
   (bridges[i])->setvar, 'nn', nn
   (bridges[i])->setvar, 'dxy', dxy
   ;(bridges[i])->setvar, 'fs', fs
   ;(bridges[i])->setvar, 'amp', amp
   ;(bridges[i])->setvar, 'phs', phs
   ;(bridges[i])->setvar, 'op', op
   ;(bridges[i])->setvar, 'consis', consis
   (bridges[i])->setvar, 'q', q
   (bridges[i])->setvar, 'q1', q1
   (bridges[i])->setvar, 'istf', istf
   (bridges[i])->setvar, 'appo', appo
   (bridges[i])->setvar, 'ov', ov
   (bridges[i])->setvar, 'mmax', mmax
   (bridges[i])->setvar, 'im', im
   (bridges[i])->setvar, 'jm',jm 
   (bridges[i])->setvar, 'dd', dd

   help,bridges[i],/st,output=tmp
   savname=n_elements(tmp) ge 2?filepath('.',/tmp)+'/spemirh_bridge_'+ $
           (strsplit(tmp[where(strmatch(tmp,'*IDL_COOKIE*'))],' ',/ex))[2]+'.sav': $
           filepath('.',/tmp)+'/spemirh_bridge_'+(strsplit(tmp,'<(',/ex))[1]+'.sav'
   save,file=savname,p
   (bridges[i])->execute, 'restore,"'+savname+'"'
   file_delete,savname,/allow_non

endfor

ov0=ov
;count=0
for iwy=0,nwy-1 do begin
for iwx=0,nwx-1 do begin

   ov=ov0
   x1=iwx*wx1/2
   if x1+wx1 ge nx then begin
      x1=nx-wx1
      dox=iwx*wx1/2-x1
      ov=shift(ov0,dox,0)
      ov[0:dox,*]=0
      for i=wx1/2+dox,wx1-1 do ov[i,*]=ov[wx1/2+dox,*]
   endif
   y1=iwy*wy1/2
   if y1+wy1 ge ny then begin
      y1=ny-wy1
      doy=iwy*wy1/2-y1
      ov=shift(ov,0,doy)
      ov[*,0:doy]=0
      for j=wy1/2+doy,wy1-1 do ov[*,j]=ov[*,wy1/2+doy]
   endif
   if iwx eq 0 then for i=0,wx1/2-1 do ov[i,*]=ov[wx1/2,*]
   if iwy eq 0 then for j=0,wy1/2-1 do ov[*,j]=ov[*,wy1/2]
   print,iwx,iwy

   ;bridge
   bridge = get_idle_bridge(bridges)
   userdata={x1:x1,y1:y1,ov:ov,verbose:p.verbose,min1:min1,max1:max1,IM2:IM2,primg:primg,psaimg:psaimg,pconsisr:pconsisr}
   if IM2 then userdata=create_struct(userdata,{min2:min2,max2:max2,primg2:primg2,psaimg2:psaimg2})
   bridge->setproperty, userdata=userdata
   bridge->setvar, 'iwx', iwx
   bridge->setvar, 'iwy', iwy

   ;---  set image  ----
   imgps=float(imgs[x1:x1+wx1-1,y1:y1+wy1-1,*])
   bridge->setvar, 'imgps', imgps
   if IM2 then begin
      imgps2=float(imgs2[x1:x1+wx1-1,y1:y1+wy1-1,*])
      bridge->setvar, 'imgps2', imgps2
   endif

   ;paralell execution
   bridge->execute, /nowait, 'spemirh_worker, p, iwx, iwy, imgps, imgr1, imgsa, imgps2, imgr2, imgsa2'

endfor
endfor

spemirh_barrier_bridges, bridges
spemirh_burn_bridges, bridges

rimg=(*primg)
saimg=(*psaimg)

if IM2 then begin
	avimg=[[[avimg]],[[avimg2]]]
	saimg=[[[saimg]],[[(*psaimg2)]]]
	rimg=[[[rimg]],[[(*primg2)]]]
endif

istfr=istf
consisr=(*pconsisr)/nwx/nwy

if keyword_set(deconv) then begin
	psf=psfd(p.D,obsc=p.Dco/p.D,wl=p.wl,pix1=p.pix1,npix=50)
	psf=psf/total(psf)
	if deconv ge 5 then Niter=deconv else Niter=12
	rimg1=rimg[*,*,0]
	rmin=min(rimg1)
	if rmin lt 0 then rimg1=rimg1-rmin
	deconv=rimg1
	for j=1,Niter do Max_Likelihood, rimg1, psf, deconv, FT_PSF=psf_ft
	if rmin lt 0 then deconv=deconv+rmin
	p.deconv=1
	if IM2 then begin
		rimg2=rimg[*,*,1]
		rmin2=min(rimg2)
		if rmin2 lt 0 then rimg2=rimg2-rmin2
		deconv2=rimg2
		for j=1,Niter do Max_Likelihood, rimg2, psf, deconv2, FT_PSF=psf_ft
		if rmin2 lt 0 then deconv2=deconv2+rmin2
		deconv=[[[deconv]],[[deconv2]]]
	endif
	tv,((deconv-min1)/(max1-min1)*255)>0<255
	;win2jpeg,'c:\tmp\m\a_'+string(count+nn+1,form='(i3.3)')+'.jpg'
	;if keyword_set(trim) then deconv=deconv[p.w/2:p.w/2*nwx-1,p.w/2:p.w/2*nwy-1,0:IM2]
	if keyword_set(trim) then deconv=deconv[trim:nx-1-trim,trim:ny-1-trim,0:IM2]
	return,deconv
endif

;if keyword_set(trim) then rimg=rimg[p.w/2:p.w/2*nwx-1,p.w/2:p.w/2*nwy-1,0:IM2]
if keyword_set(trim) then rimg=rimg[trim:nx-1-trim,trim:ny-1-trim,0:IM2]

return,rimg

end

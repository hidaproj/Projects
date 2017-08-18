;*****************************************************************
; psfD.pro
; return PSF for diameter D
;	2013.7.3	k.i.	from sotpsf.pro
;	2013.7.6	k.i.	obsc keyword
function psfD,D,obsc=obsc,wl=wl,pix1=pix1,npix=npix,wfe=wfe,rmswfe=rmswfe, $
	mtf=mtf,df=df,w0=w0,measure=measure, $
	a20=a20,a31=a31,b31=b31,a22=a22,b22=b22,a33=a33,b33=b33, $
	a40=a40,a42=a42,b42=b42,a51=a51,b51=b51,a60=a60

; D   -- diameter, mm
; obsc - central obsculation (ratio to D)
; wl  --- in nm
; pix1  - arcsec
; a20,,,,a60 -- Zernike, lambda @wl, single path
; measure - use measured wfe
; rmswfe  - retuen nm rms
; mtf
; df	  - freqency resolution of mtf,  /arcsec

;----  pupil params  ----
rad=!pi/180./60./60.
ds=10.		; width of spar (mm) at -30.,90.,210.deg.
nD=64		; # of mesh for pupil diameter
if not keyword_set(obsc) then obsc=0.	; central obscuration,  (ratio to D)
if not keyword_set(wl) then wl=500.	; wavelength (A)
if not keyword_set(npix) then npix=100	; size of returning psf
if not keyword_set(pix1) then pix1=0.01	; size of image pixel (arcsec)
if not keyword_set(a20) then a20=0.	; lambda @wl, single path
if not keyword_set(a31) then a31=0.	; 
if not keyword_set(b31) then b31=0.	; 
if not keyword_set(a22) then a22=0.	; 
if not keyword_set(b22) then b22=0.	; 
if not keyword_set(a33) then a33=0.	; 
if not keyword_set(b33) then b33=0.	; 
if not keyword_set(a40) then a40=0.	; 
if not keyword_set(a42) then a42=0.	; 
if not keyword_set(b42) then b42=0.	; 
if not keyword_set(a51) then a51=0.	; 
if not keyword_set(b51) then b51=0.	; 
if not keyword_set(a60) then a60=0.	; 

f_cut=D/(wl/1e6)*rad	; cut-off freq. by optics, #/arcsec
res=1./f_cut		; optical resolution (arcsec)
wfact=res/pix1
d1=D/nD		; size of 1 mesh of pupil (mm)

;--- get OTA OPD --
if keyword_set(measure) then begin
	opddir='C:\data\SOT optics\WFE\'
	opdfile=opddir+'usd_sl_av-tfacmd6b33.Opd'
	opdfile=opddir+'usd_sl_av-tfac.Opd'
	pro_read_opd,opdfile,h,opd,mask
	imgsize,opd,nx1,ny1
	im=replicate(1,nx1,ny1)
	ii=where(opd eq min(opd))
	im(ii)=0.
	im=congrid(im,nD,nD)
	opd=congrid(opd,nD,nD)	; double pth lambda @632.8
	wfe=opd/2.*632.8/wl*2.*!pi
	baseW0=im*complex(cos(wfe),sin(wfe))	; pupil function
endif else baseW0=0


w=pupilfunc(nD=nD,obsc=obsc,spider=ds/d1,wfact=wfact,w0=w0,baseW0=baseW0, $
	a20=a20,a31=a31,b31=b31,a22=a22,b22=b22,a33=a33,b33=b33,a40=a40, $
	a42=a42,b42=b42,a51=a51,b51=b51,a60=a60,wfe=wfe)
psf2=psfpupil(w,mtf=mtf)
fmax=D*wfact/(wl/1e6)*rad/2
imgsize,mtf,nxm,nym
;f=findgen(nxm)*2*fmax/nxm-fmax
df=fmax*2/nxm

ii=where(w0 ne 0.)
rmswfe=sqrt(total(wfe(ii)^2)/n_elements(ii))/!pi/2.*wl

imgsize,w,nw,nw
p0=nw/2-npix/2
psf2=psf2(p0:p0+npix-1,p0:p0+npix-1)

;print,rmswfe,max(psf2)
;print,total(psf2)/n_elements(psf2)
;ax=findgen(npix)*pix1
;surface,psf2,ax,ax,charsize=2,zrange=[0,1]

return,psf2

end

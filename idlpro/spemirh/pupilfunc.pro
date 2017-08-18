;*****************************************************************
;+
; NAME       : pupilfunc.pro (function)
; PURPOSE :
; 	return pupil function W(*,*)
; CATEGORY :
;        idlpro/optic/resolut
; CALLING SEQUENCE :
;       p=pupilfunc(n,m,nn=nn)
; INPUTS :
; OUTPUT :
;	2D image of pupil func. 
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
;   nD		- # of mesh for diameter
;   obsc 	- central obsculation (ratio to dia.)
;   spider 	- width of spider (in unit of mesh)
;   wfact 	- factor for extension of W
;   a20,,,,	- zernike WFE (lambda)
;   basewfe(*,*) - use measured WFE, in rad
;   iiobsc	- position of obsclation in basewfe
;   z(*)	- WFE (lambda) for sequencial Zernike coeff.
; REFERENCE:
;	Otsubo text, 2001.04.02
; MODIFICATION HISTORY :
;   2003.7.12 	k.i.
;   2004.6.1 	k.i.
;   2005.5.13 	k.i.	z(*) keyword
;-
function pupilfunc,nD=nD,obsc=obsc,spider=spider,wfact=wfact,w0=w0, $
	baseW0=baseW0, $
	a20=a20,a31=a31,b31=b31,a22=a22,b22=b22,a33=a33,b33=b33,a40=a40, $
	a42=a42,b42=b42,a51=a51,b51=b51,a60=a60,wfe=wfe,z=z

if not keyword_set(obsc) then obsc=0

;----------  pupil function  -----------
nR=float(nD-1)/2.
nRin=nR*obsc
W0=complexarr(nD,nD)	; pupil function
ra=fltarr(nD,nD)
x0=nR &	y0=nR
x=(findgen(nD)-x0)#replicate(1,nD)
y=replicate(1,nD)#(findgen(nD)-y0)
ra=sqrt(x^2+y^2)
ii=where(ra gt nR or ra lt nRin)

wfe=fltarr(nD,nD)
if keyword_set(A20) then wfe=wfe+zernike(2,0,nn=nD)*A20*2*!pi
if keyword_set(A31) then wfe=wfe+zernike(3,1,nn=nD)*A31*2*!pi
if keyword_set(B31) then wfe=wfe+zernike(3,-1,nn=nD)*B31*2*!pi
if keyword_set(A22) then wfe=wfe+zernike(2,2,nn=nD)*A22*2*!pi
if keyword_set(B22) then wfe=wfe+zernike(2,-2,nn=nD)*B22*2*!pi
if keyword_set(A33) then wfe=wfe+zernike(3,3,nn=nD)*A33*2*!pi
if keyword_set(B33) then wfe=wfe+zernike(3,-3,nn=nD)*B33*2*!pi
if keyword_set(A40) then wfe=wfe+zernike(4,0,nn=nD)*A40*2*!pi
if keyword_set(A60) then wfe=wfe+zernike(6,0,nn=nD)*A60*2*!pi
if keyword_set(A42) then wfe=wfe+zernike(4,2,nn=nD)*A42*2*!pi
if keyword_set(B42) then wfe=wfe+zernike(4,-2,nn=nD)*B42*2*!pi
if keyword_set(A51) then wfe=wfe+zernike(5,1,nn=nD)*A51*2*!pi
if keyword_set(B51) then wfe=wfe+zernike(5,-1,nn=nD)*B51*2*!pi
if keyword_set(z) then begin
	nz=n_elements(z)
	for i=0,nz-1 do begin
		wfe=wfe+zernike(i,nn=nD)*z[i]*2*!pi
	endfor
endif
if keyword_set(baseW0) then begin
	bW0=congrid(baseW0,nD,nD)
	iiobsc=where(abs(bW0) eq 0.)
	wfe=wfe+atan(imaginary(bW0),float(bW0))
endif
W0=1.*complex(cos(wfe),sin(wfe))
W0(ii)=0.
if keyword_set(baseW0) then W0(iiobsc)=0.

if keyword_set(spider) then begin
	th=(-30.+[0,1,2]*120.)*!pi/180.
	for i=0,n_elements(th)-1 do begin
		th1=th(i)
		r=abs(sin(th1)*x-cos(th1)*y)
		jj=where(r lt spider/2. and (cos(th1)*x+sin(th1)*y) gt 0., count)
		if count gt 0 then begin
			W0(jj)=0.
			ii=[ii,jj]
		endif
	endfor
endif
if keyword_set(iiobsc) then begin
	W0(iiobsc)=0.
	ii=[ii,iiobsc]
endif
if keyword_set(wfact) then begin
	nw=nD*wfact
	;print,wfact,nw
	W=complexarr(nw,nw)
	dd=(nw-nD)/2
	W(dd:dd+nD-1,dd:dd+nD-1)=W0
endif else W=W0


return,W
end

;+ *********************************************************************
; NAME :	mtf (function)
; PURPOSE :
;		calculate Modulation Transfer Function
; CATEGORY :
;		idlpro/optic
; CALLING SEQUENCE :
;		mtf = mtf(nu, eps=eps,ripple=ripple,jitter=jitter,npix=npix, $
;			Tr=Tr,Tp=Tp,Ta=Ta)
; INPUTS :
;		nu : normalized spatial frequency, nu/nu(crit), 
;			nu(crit) = D/wl
; KEYWORD PARAMETERS :
;		eps : central obscuration (scale ratio)
;		ripple=[l,dwl] : wavefront random error
;			l --  size of ripple in unit of pupil diameter,
;				 if 0, --> microroughness
;			dwl -- rms wavefront error in unit of wavelength
;		jitter : standard deviation of image motion
;			in unit of resolution, wl/D
;		npix : number of pixels per wl* F (~radius of Airy disk)
;		Tr : MTF by random wavefront error (returned)
;		Tp : MTF by image motion (")
;		Ta : MTF by detector pixel (")
; OUTPUTS :
;		MTF
; PROCEDURE :
;		"ASTRONOMICAL OPTICS", D.J.Schroeder, Sect.11-1
;		axi-symmetric optics is assumed
; MODIFICATION HISTORY :
;		'96/01/08
;		'98/09/22	k.i.  bug fix
;- ---------------------------------------------------------------------
function mtf,nu,eps=eps,ripple=ripple,jitter=jitter,npix=npix, $
	Tr=Tr,Tp=Tp,Ta=Ta

if not keyword_set(eps) then eps=0.
mode='ideal'

case mode of
 'ideal': begin	; ideal case Table 11.1 (from Wetherell)
	A = acos(nu) - nu*sqrt(1.-nu^2)
	if eps ne 0. then begin
		nnu=n_elements(nu)
		B = fltarr(nnu)
		ii=where(nu le eps, count)
		if count ne 0 then B(ii) = eps^2 * ( acos(nu(ii)/eps)  $
				- (nu(ii)/eps)*sqrt(1.-(nu(ii)/eps)^2) )
		C = dblarr(nnu)
		ii=where(2.*nu le 1.-eps, count)
		if count ne 0 then C(ii) = -!pi*eps^2
		ii=where( (2.*nu gt 1.-eps) and (2.*nu lt 1.+eps), count )
		if count ne 0 then begin
		    phi=acos( ((1.+eps^2 - 4.*nu(ii)^2)/2./eps)>(-1.)<1. )
		    C(ii) = -!pi*eps^2 $
			+ eps*sin(phi) + phi/2.*(1.+eps^2) $
			- (1.-eps^2)*atan( (1+eps)/(1.-eps)*tan(phi/2.) )
		endif
	endif else begin
		B=0.d
		C=0.d
	endelse
	T = 2./!pi * (A+B+C)/(1.-eps^2)
	end
endcase

;---  degradation by rippple (Wetherell)
if keyword_set(ripple) then begin
	l=ripple(0) &	dwl=ripple(1)
	if l eq 0. then cnu=replicate(0.,n_elements(nu)) $
	else cnu=exp(-4.d*(nu/l)^2)
	Tr = exp(-(2.d*!pi*dwl)^2 *(1.-cnu) )
	T=T*Tr
endif
;---  degradation by image motion (Mahajan & Wetherell)
if keyword_set(jitter) then begin
	Tp = exp(-2.*(!pi*jitter*nu)^2 )
	T=T*Tp
endif
;---  pixel MTF
if keyword_set(npix) then begin
	aa = !pi*nu/npix
	Ta = sin(aa)
	Ta(where(nu eq 0.))=1.
	ii=where(nu ne 0.)
	Ta(ii)=Ta(ii)/aa(ii)
	T=T*Ta
endif

return,T

end

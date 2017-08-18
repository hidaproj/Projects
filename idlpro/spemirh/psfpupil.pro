;+ *********************************************************************
; NAME :	psfpupil (function)
; PURPOSE :
;		calculate Point Spread Function from a pupil func.
; CATEGORY :
;		idlpro/optic/lib
; CALLING SEQUENCE :
;		psf = psfpupil(w)
; INPUTS :
;		w  : pupil function
; KEYWORD PARAMETERS :
;		mtf : return MTF
; OUTPUTS :
;		PSF 
; PROCEDURE :
; MODIFICATION HISTORY :
;	'98/11/19	k.i.
;- ---------------------------------------------------------------------
function psfpupil,w,mtf=mtf2

;---  W --> PSF, MTF ----------
psi=fft(w,-1)*n_elements(w)/n_elements(where(abs(w) ne 0.)) 
psf=float(psi*conj(psi))
otf=fft(psf,1)
mtf=sqrt(float(otf*conj(otf)))

s=size(w) &	nx=s(1) &	ny=s(2)
psf2=shift(psf,nx/2,ny/2)
mtf2=shift(mtf,nx/2,ny/2)

return,psf2
end

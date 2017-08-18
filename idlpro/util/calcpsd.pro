;+
; NAME       : calcpsd.pro (procedure)
; PURPOSE :
;    calculate PSD from x(t)
; CATEGORY :
;	idlpro/util
; CALLING SEQUENCE :
;	calcpsd,t,x,f,psd
; INPUTS :
;       x(t)  : time seriese array
; OUTPUT :
;	psd(f): f in Hz if t in sec
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;	K.I. '01/11/11	new
;	K.I. '02/05/08	intval,overlap keywords
;	K.I. '02/08/13	fbin keywords
;	K.I. '03/05/31	ph keywords
;	K.I. '06/10/30	double precesion
;-
pro calcpsd,t,x,f,psd,smooth=smooth,intval=intval,overlap=overlap,fbin=fbin,ph=ph

	nn=n_elements(t)
	tmin=min(t) &	tmax=max(t)
	if keyword_set(intval) then begin
		ii=lindgen(intval)
		count=0
		while max(ii) lt nn do begin
			t1=t(ii) &	x1=x(ii)
			if count eq 0 then begin
				calcpsd,t1,x1,f,psd
			endif else begin
				calcpsd,t1,x1,f1,psd1
				psd=psd+psd1
			endelse
			count=count+1
			ii=ii+intval*overlap
		endwhile
		psd=psd/count
	endif else begin
		df=1./(tmax-tmin)
		f=dindgen(nn/2)*df
		wind=0.5+0.5*cos(dindgen(nn)/nn*2.*!dpi-!dpi)
		xx=x*wind *1.6	; <=== artificial
		;;xx=x
		ft=fft(xx,-1)
		pw=abs(ft*conj(ft))
		psd=pw(0:nn/2-1)/df*2
		ph=-atan(imaginary(ft),float(ft))
		ph=ph(0:nn/2-1)
	endelse
	if keyword_set(smooth) then psd=smooth(psd,smooth)
	if keyword_set(fbin) then begin
		psd=rebin(psd,n_elements(psd)/fbin)
		f=rebin(f,n_elements(f)/fbin)
	endif

end

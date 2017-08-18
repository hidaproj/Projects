
;+
; Project: 
;	SDAC
;                   
; NAME: 
;	PULSE_SPREAD
;
; PURPOSE:
;	This procedure generates a matrix of gaussian pulse-shapes which can
;	then multiply a matrix of energy-losses to form a full pulse-height
;	matrix.
;
; CATEGORY:
;	MATH, CALIBRATION, INSTRUMENT, DETECTORS, RESPONSE, SPECTROSCOPY
;
; CALLING SEQUENCE:
;	PULSE_SPREAD, INPUT, PSM, INMATRIX, OUTMATRIX
; EXAMPLES:
;	pulse_spread, input_psm, pulse_shape, eloss_mat.eloss_mat, drm
;
; CALLS:
;	EDGE_PRODUCTS, CHKARG, F_DIV
;
; INPUTS:
;       INPUT- An anonymous structure with these tags:
;		EIN- 2 x Ninput energy array with low and high energy edges, nominally keV
;		but any consistent units for EIN, EOUT, and FUNC_PAR are acceptable
;		EOUT- 2 X Noutput energy array with low and high energy edges, units of EIN
;		FUNC- A string with the name of the function of Full Width at Half Maximum
;		vs energy, i.e. FWHM = call_function( FUNC, avg(EIN,0), FUNC_PAR)
;		FUNC_PAR- A vector of parameters used to control FUNC
;
; OPTIONAL INPUTS:
;	INMATRIX- To complete the expression OUTMATRIX = PSM # INMATRIX, Ninput X Nflux
;
; OUTPUTS:
;       PSM - resultant PULSE-SPREAD-MATRIX
;
; OPTIONAL OUTPUTS:
;	OUMATRIX- Pulse-spread broadened product of INMATRIX, Noutput x Nflux
;
; KEYWORDS:
;	none
; COMMON BLOCKS:
;	none
;
; SIDE EFFECTS:
;	none
;
; RESTRICTIONS:
;	none
;
; PROCEDURE:
;	The GAUSSINT function is used to construct the point-spread function.  GAUSSINT is
;	the integral over the normally used GAUSSIAN function and is the correct function
;	where the Gaussian is a valid approximation only when the output channels are
;	narrow wrt the resolution.  Also, if INMATRIX is given, an efficient matrix
;	multiplication is performed on large matrices, multiplying only over the
;	non-zero elements of INMATRIX, useful when INMATRIX is mainly the photoefficiency
;	without a Compton tail.
;
; MODIFICATION HISTORY:
;	Version 1, RAS, 19-NOVEMBER-1996
;	Version 2, RAS, 20-NOVEMBER-1996, multiply submatrix of all non-zero elements
;	Version 3, RAS, 15-May-1997, fixed problem which occurred when input bins were larger than
;	a resolution element.  Now, instead of integrating gaussian response over output
;	bin evaluated at centroid of input bin, then input bins are subdivided into
;	at least 4 resolution elements when evaluating the integral.
;	Version 4, richard.schwartz@gsfc.nasa.gov, 7-sep-1997, more documentation
;-


pro pulse_spread, input, psm, inmatrix, outmatrix

on_error,2
error=0
if datatype(input) ne 'STC' then error=1
if not error then begin
	tags = tag_names(input)
	test = ['EIN','EOUT','FUNC','FUNC_PAR']
	ltest = where_arr( tags, test, count)
	error = count ne n_elements(test)
endif

if error then begin
	message,/continue, 'Error in input arguments!!!'
	chkarg,'pulse_spread',proc
	more, proc
	return
endif

ninput = n_elements( input.ein(0,*))
noutput= n_elements( input.eout(0,*))	 
edge_products, input.ein, width=wein, mean=emin 
sigmax = call_function(input.func, emin, input.func_par)/2.36

psm = fltarr(noutput,ninput)

res_elem = f_div( sigmax, wein )
w1 = where( res_elem ge 2.0, nw1)
w2 = where( res_elem lt 2.0, nw2)
if nw1 ge 1 then for i=0,nw1 - 1 do $
        psm(*,w1(i))= (gaussint((input.eout(1,*)-emin(w1(i)))/sigmax(w1(i))) $
             -   gaussint( (input.eout(0,*)-emin(w1(i)))/sigmax(w1(i)) ))(*)
if nw2 ge 1 then for i=0,nw2 -1 do begin
	nbins = ceil(1./res_elem(w2(i))*4.0)
	enew = interpol( input.ein(*,w2(i)), nbins+1)
	edge_products, enew, mean=emnew
	emnew=rebin(reform(emnew,1,nbins),noutput,nbins)
	e1 = rebin((input.eout(1,*))(*),noutput,nbins)
	e0 = rebin((input.eout(0,*))(*),noutput,nbins)
	psm(*,w2(i))= rebin( gaussint( (e1-emnew)/sigmax(w2(i))) $
			  -  gaussint( (e0-emnew)/sigmax(w2(i))), noutput)
	endfor

if n_elements(inmatrix) gt 0 then begin
	nflux = n_elements(inmatrix(0,*))
	if 1.0*ninput*ninput*noutput*nflux le 1e8 then $
	outmatrix = psm # inmatrix $
	else begin
	outmatrix = fltarr( noutput, nflux )
	for i=0, nflux-1 do begin
	wp = where( inmatrix(*,i) ne 0.0, np)
	if np ge 1 then outmatrix(0,i) = psm(*,wp) # inmatrix(wp,i)
	endfor
	endelse
endif


end

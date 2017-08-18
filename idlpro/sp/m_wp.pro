;+
; NAME       : m_wp.pro (function)
; PURPOSE :
; 	return Mueller matrix of linear retarder
; CATEGORY :
;        idlpro/optic/raylib
; CALLING SEQUENCE :
;        mat = m_wp(del,phai,/jones,ref=ref,thick=thick,wv=wv)
; INPUTS :
; 	del  --  retardance (rad.)
; 	phai --  angle of the axis (rad., counter clockwise)
;	jones -  return Jones vector
;       ref  --  reflective index
;       thick -  thickness of wave plate (mm)
;       wv    -  wave length (nm)
; OUTPUT :
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;        k.i. '95/10/26  from lwp.pro
;        k.i. '96/02/17		jones keyword
;        T.A. '10/03/16		ref,thick,wv keyword
;        T.A. '10/04/07		modificate L61 and keywords(thck,wvl)
;-
;*************************************************************************
function m_wp,del,phai,jones=jones,ref=ref,thick=thck,wv=wvl

if not keyword_set(jones) then begin	; Mueller matrix
    c2 = cos(2.*phai)                                                      
    s2 = sin(2.*phai)  
    cd = cos(del)
    sd = sin(del)

    if not keyword_set(ref) then begin	; no reflection
 	mat=[ [	1.,	0.,		0.,		0.	],	$
      	      [ 0.,	c2^2+s2^2*cd,	s2*c2*(1.-cd),	-s2*sd	],	$
              [ 0.,	s2*c2*(1.-cd),	s2^2+c2^2*cd,	c2*sd],	$
              [	0.,	s2*sd,		-c2*sd,		cd	] ]
    endif else begin	; reflection
    	if (not keyword_set(thck)) or (not keyword_set(wvl)) then begin
            print,'you must input two keywords "thick [mm]" and "wv [nm]"'
            mat = -1
        endif else begin
            ref=ref*1.
            thick=thck*10.^(-3)	;[m]		
            wv=wvl*10.^(-9)		;[m]
            rr  = 2.*((1.-ref)/(1.+ref))^2
            clm = cos(4.*!pi*thick*ref/wv)
            slm = sin(4.*!pi*thick*ref/wv)
            c2d = cos(2.*del)	
            s2d = sin(2.*del)	
            f11 = rr*clm*cd+1.
            f12 = -1.*rr*slm*sd
            f33 = rr*clm*c2d+cd
            f43 = rr*clm*s2d+sd

            mat0=[ [	f11,	f12*c2,			f12*s2,			0.	],	$
                  [ f12*c2,	f11*c2^2+f33*s2^2,	s2*c2*(f11-f33),	-1.*f43*s2],	$
                  [ f12*s2,	s2*c2*(f11-f33),	f11*s2^2+f33*c2^2,	f43*c2	],	$
                  [	0.,	f43*s2,			-1.*f43*c2,		f33	] ]
            mat = (1.-(1.-ref)^2/(1.+ref)^2)^2 *mat0
        endelse
    endelse

endif else begin	; Jones matrix
    c1=cos(phai)
    s1=sin(phai)
    i=complex(0,1)

    if not keyword_set(ref) then begin	; no reflection
    	edel=exp(-i*del)	; sign ok?
    	m11=c1^2+edel*s1^2
    	m22=s1^2+edel*c1^2
    	m12=(1.-edel)*c1*s1
    	mat=[ [ 	m11,	m12 ],	$
		[	m12,	m22 ] ]
    ;mat=mat*conj(m11)/abs(m11)
    endif else begin	; reflection
    	if (not keyword_set(thck)) or (not keyword_set(wvl)) then begin
            print,'you must input two keywords "thick [mm]" and "wv [nm]"'
            mat = -1
        endif else begin
            ref  = ref*1.
            thick= thck*10.^(-3)	;[m]
            wv   = wvl*10.^(-9)		;[m]
            lx   = (4.*!pi*thick*ref/wv+del)*0.5    ; x is fast axis
            ly   = (4.*!pi*thick*ref/wv-del)*0.5
            rr   = (1.-ref)^2/(1.+ref)^2
            fx   = rr*exp(3.*i*lx)+exp(i*lx)
            fy   = rr*exp(3.*i*ly)+exp(i*ly)
            m11  = fx*c1^2+fy*s1^2
            m22  = fx*s1^2+fy*c1^2
            m12  = fx*s1*c1-fy*c1*s1
            
            mat0  = [ [ m11,	m12 ],	$
		     [	m12,	m22 ] ]
            mat = (1-(1.-ref)^2/(1.+ref)^2) *mat0
        endelse
    endelse
endelse

return,mat
end

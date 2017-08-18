;+
; NAME       : m_dst.pro (function)
; PURPOSE :
; 	return Mueller matrix for DST model
; CALLING SEQUENCE :
;        res=dst(ro_N,tau_N,ro_C,tau_C,ha,zd)
; INPUTS :
; OUTPUT :
;	ro_N	ro of Newton mirror
;	tau_N	tau of Newton mirror
;	ro_C	ro of Coude mirror
;	tau_N	tau of Coude mirror
;	ha	hour angle
;	zd	zenith distance
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;      T.A. '09/08/23
;*******************************************************************
function m_dst,position,ro_N,tau_N,ro_C,tau_C,ha,zd,incli,   $
               th_en,del_en,th_ex,del_ex

lat=36.252/!radeg		;Hidaten
za=asin(cos(lat)*sin(ha)/sin(zd))

;==== Kiyohara form ====;
if 0 then begin
	if position eq 'west' then begin
	    phi_N=za
	    phi_C=zd
	    phi_v=+zd+za-incli
	endif else begin
	    phi_N=za
	    phi_C=-zd
	    phi_v=-zd+za-incli
	endelse
endif
;==== anan form ====;
if 1 then begin
	if position eq 'west' then begin
	    phi_N=za
	    phi_C=-zd
	    phi_v=+zd-za-incli
	endif else begin
	    phi_N=za
	    phi_C=+zd
	    phi_v=-zd-za-incli
	endelse
endif
;==================;


M_p=[$
	[1.,0.,0.,0.],	$
	[0.,1.,0.,0.],	$
	[0.,0.,-1.,0.],	$
	[0.,0.,0.,-1.]	$
	]
M_G=M_p
M_N=m_mirr1(tau_N,ro_N)
M_C=m_mirr1(tau_C,ro_C)
D_en=M_WP(del_en,th_en)
D_ex=M_WP(del_ex,th_ex)
R_N=m_rot(phi_N)
R_C=m_rot(phi_C)
R_pl=m_rot(phi_v)

;print,R_pl,'',D_ex,'',M_C,'',M_G,'',R_C,'',M_N,'',M_P,'',D_en,'',R_N

mat=R_pl##D_ex##M_C##M_G##R_C##M_N##M_P##D_en##R_N

return,mat

end

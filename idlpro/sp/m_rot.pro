;+
; NAME       : m_rot.pro (finction)
; PURPOSE :
; 	return Mueller matrix for axis rotation
; CALLING SEQUENCE :
;        mat = m_rotaxis(phai,/jones)
; INPUTS :
; 	phai --  angle of axis rotation 
;		(rad., counterclockwise when we view towards the sun)
; OUTPUT :
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;      T.A. '09/08/23
;*******************************************************************
function m_rot,phai

c2=cos(2.*phai)
s2=sin(2.*phai)
mat=[$
      [1.,	0.,	0.,	0.],	$
      [0.,	c2,	s2,	0.],	$
      [0.,	-s2,	c2,	0.],	$
      [0.,      0.,	0.,	1.] ]

return,mat

end

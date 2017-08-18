;+
; NAME       : offset.pro (function)
; PURPOSE :
; 	return offset of rotating waveplate 
; CATEGORY :
;        idlpro/polobs/
; CALLING SEQUENCE :
;        res=offset(expo,perio)
; INPUTS :
;       res  --  offset of rotating waveplate
;       expo --  exposure (usec)
;       perio--  period of rotating waveplate (s)
;       width--  width (pix)
;       height-  height (pix)
; OUTPUT :
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;        T.A. '10/06/20		
;        T.A. '10/06/24   real model		
;-
;*************************************************************************
function offset,expo,perio,width,height

perio = perio*1d
expo  = expo*1d
nx    = width*1d
ny    = height*1d

;inc   = expo

res   = 0.29720456d + 2d*!pi*(1.5979223d*1e-7 * expo + 2.1586980d*1e-5 * nx + 0.028633225d)/perio 
   ; ry = [70,149]          ;bin=1
   ; rx = [130,199]         ;bin=1


return,res
end 

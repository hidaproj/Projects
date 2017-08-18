;+
; Project     : EIS
;
; Name        : EIS_STARTUP
;
; Purpose     : Load EIS software and databases
;
; Category    : system
;                   
; Inputs      : None
;
; Outputs     : None
;
; Keywords    : QUIET = set for no output
;
; History     : 31-Jan-2003,  D.M. Zarro (EER/GSFC)  Written
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

pro eis_startup, _ref_extra=extra


load_path,'$SSW/solarb/eis/idl','eis_holder',_extra=extra,/append

;load_path,'$SSW/soho/cds/idl','list_main',_extra=extra,/append

eis_dbase,_extra=extra

return & end








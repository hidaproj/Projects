;+
; Project     : HESSI
;
; Name        : GOES_LTC__DEFINE
;
; Purpose     : Define a GOES_LTC  object
;
; Category    : synoptic objects
;
; Explanation : Object wrapper around GOES
;
; Syntax      : IDL> new=obj_new('goes_ltc')
;
; History     : Written 30 Jan 2004, D. Zarro (L-3/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;               
;-

;---------------------------------------------------------------------------

function goes_ltc::init,_ref_extra=extra

return,self->goes::init(_extra=extra)

end

;--------------------------------------------------------------------------

pro goes_ltc::cleanup

message,'cleaning up...',/cont

self->goes::cleanup

return
end

;---------------------------------------------------------------------------

pro goes_ltc__define

goes_struct={goes_ltc,inherits goes}

return & end



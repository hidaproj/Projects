;+
; Project     : SOHO-CDS
;
; Name        : GET_ARR_CENTER
;
; Purpose     : compute center of input coordinates array
;
; Category    : imaging
;
; Syntax      : center=get_arr_center(array)
;
; Inputs      : ARRAY = 1 or 2d coordinate array
;
; Outputs     : CENTER = center coordinate value
;
; Keywords    : ERR = error string
;               DX, DY = mean pixel spacing in x and y directions
;
; History     : Written 16 Feb 1998, D. Zarro, SAC/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

function get_arr_center,array,err=err,dx=dx,dy=dy

on_error,1
center=-9999.
dx=0. & dy=0.

sz=size(array)
if sz(0) eq 2 then begin
 nx=sz(1) & ny=sz(2)
endif else begin
 nx=sz(1) & ny=1
endelse

if not exist(array) then begin
 pr_syntax,'center=get_arr_center(array)'
 return,center
endif
amin=min(array,max=amax)

dx=(amax-amin)/(nx-1.)
dy=dx
if (ny gt 1) then dy=(amax-amin)/(ny-1.)

center=(amin+amax)/2.

return,center & end


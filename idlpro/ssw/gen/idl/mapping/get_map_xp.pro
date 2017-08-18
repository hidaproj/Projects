;+
; Project     : SOHO-CDS
;
; Name        : GET_MAP_XP
;
; Purpose     : extract X-coordinate arrays of map
;
; Category    : imaging
;
; Syntax      : xp=get_map_xp(map)
;
; Inputs      : MAP = image map
;
; Opt. Inputs : None
;
; Outputs     : XP = 2d X-coordinate array
;
; Opt. Outputs: None
;
; Keywords    : ERR = error string
;               DX  = X-pixel spacing
;               XC  = X-pixel center coordinate
;               ONED = extract coordinate as 1-D
;
; History     : Written 16 Feb 1998, D. Zarro, SAC/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

function get_map_xp,map,dx=dx,xc=xc,err=err,oned=oned,nx=nx

err=''
if not valid_map(map,err=err) then return,-1
if n_elements(map) ne 1 then begin
 err='cannot handle more than one map'
 message,err,/cont
 return,-1
endif

sz=size(map.data)
nx=sz(1) & ny=sz(2)
dx=map.dx & xc=map.xc
if keyword_set(oned) then ny=1
xp=mk_map_xp(xc,dx,nx,ny)

return,xp

end

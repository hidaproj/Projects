;+
; Project     : SOHO-CDS
;
; Name        : INTER_MAP
;
; Purpose     : interpolate an image map onto a new coordinate system
;
; Category    : imaging
;
; Explanation : 
;
; Syntax      : imap=inter_map(map,rmap)
;
; Examples    :
;
; Inputs      : MAP = image map structure
;               RMAP = reference map with coordinates 
;
; Opt. Inputs : None
;
; Outputs     : IMAP = interpolated map
;
; Opt. Outputs: None
;
; Keywords    : ERR = error strings
;
; Common      : None
;
; Restrictions: None
;
; Side effects: None
;
; History     : Written 22 August 1997, D. Zarro, SAC/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

function inter_map,map,rmap,err=err,_extra=extra

err=''
on_error,1
if (not valid_map(map)) or (not valid_map(rmap))  then begin
 pr_syntax,'imap=inter_map(map,rmap)'
 return,-1
endif

xr=get_map_xp(rmap)
yr=get_map_yp(rmap)

xp=get_map_xp(map)
yp=get_map_yp(map)

xmax=max(xp)
xmin=min(xp)
ymax=max(yp)
ymin=min(yp)

;-- flag data outside reference map range

outside=where( (xr gt xmax) or (xr lt xmin) or $
               (yr gt ymax) or (yr lt ymin), count)

if count eq n_elements(rmap.data) then begin
 err='No overlapping data between input and reference map'
 message,err,/cont
 return,map
endif

imap=repack_map(map,xr,yr)
imap=rep_tag_value(imap,interp2d(map.data,xp,yp,xr,yr,_extra=extra),'data')

if count gt 0 then imap.data(outside)=0.

return,imap

end


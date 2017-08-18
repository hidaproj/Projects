;+
; Project     : SOHO-CDS
;
; Name        : WHERE_OFF_LIMB
;
; Purpose     : find indicies of points off solar limb
;
; Category    : imaging
;
; Syntax      : off_limb=where_off_limb(xr,yr,date)
;
; Inputs      : XR, YR = arcsec coordinates
;               DATE = observation date
;
; Outputs     : OFF_LIMB = indices of off_limb points
;
; Keywords    : COUNT = # of off limb points
;               VIEW = use L1 view
;
; History     : Written 4 March 1999, D. Zarro, SM&A/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

function where_off_limb,xr,yr,date,count=count,view=view,radius=radius

on_error,1

if (not exist(xr)) or (not exist(yr)) then begin
 pr_syntax,'off_limb=where_off_limb(xr,yr,date)'
 return,-1
endif

;-- default to current date for radius

err=''
tdate=anytim2utc(date,err=err)
if err ne '' then get_utc,tdate
pr=pb0r(tdate,soho=view,/arcsec)
radius=float(pr(2))

rot_pos=sqrt(float(xr)^2+float(yr)^2)

off_limb=where( rot_pos gt radius,count)
dprint,'% WHERE_OFF_LIMB: # of points off limb = ',num2str(count)

return,off_limb
end



;+
; Project     : HESSI
;
; Name        : GOES_FITS_PATH
;
; Purpose     : return URL path to GOES FITS files
;
; Category    : synoptic gbo sockets
;
; Syntax      : IDL> path=goes_fits_path()
;
; Inputs      : None
;
; Outputs     : URL path to GOES FITS files
;
; Keywords    :
;
; History     : Written 18 Feb 2001, D. Zarro, EITI/GSFC
;  14-Dec-2005 - call goes_server, and remove call to network.  network
;    keyword can be passed through extra, and will be returned from goes_server.
;
; Contact     : dzarro@solar.stanford.edu
;-


function goes_fits_path,_ref_extra=extra

path='/goes'
server=goes_server(/sdac,/full, _extra=extra)
return,server+path
end

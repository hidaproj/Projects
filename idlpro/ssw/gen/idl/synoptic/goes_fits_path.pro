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
;
; Contact     : dzarro@solar.stanford.edu
;-


function goes_fits_path,_ref_extra=extra,network=network

path='/goes'
server='http://hesperia.gsfc.nasa.gov'
network=have_network(server,_extra=extra)
return,server+path
end

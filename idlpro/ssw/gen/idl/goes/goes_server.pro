;+
; Project     : HESSI
;
; Name        : GOES_SERVER
;
; Purpose     : return first avaliable GOES data server
;
; Category    : synoptic sockets
;
; Inputs      : None
;
; Outputs     : SERVER = GOES data server name
;
; Keywords    : SDAC - If set, use SDAC archive on hesperia
;               FULL - If set, return full url
;               NETWORK = returns 1 if network to that server is up
;
; History     : 29-Dec-2001,  D.M. Zarro (EITI/GSFC)  Written
;  ?-2005, Dominic - add sdac keyword, and set server to hesperia for sdac.
;    Added full keyword to return full http://name.
;    Don't check sohowww alternate server if sdac is selected.
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-


function goes_server,_ref_extra=extra,sdac=sdac, full=full, network=network

;-- primary server

server= keyword_set(sdac) ? 'hesperia.gsfc.nasa.gov' : 'beauty.nascom.nasa.gov'
network=have_network(server,_extra=extra)

;-- if primary server is down, try secondary

if not network and not keyword_set(sdac)then begin
 server2='sohowww.nascom.nasa.gov'
 network=have_network(server2,_extra=extra)
 if network then server=server2
endif

return, keyword_set(full) ? 'http://'+server : server
end

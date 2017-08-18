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
; Keywords    : NETWORK = 1 if network is up
;
; History     : 29-Dec-2001,  D.M. Zarro (EITI/GSFC)  Written
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-


function goes_server,_ref_extra=extra,network=network

;-- primary server

server='beauty.nascom.nasa.gov'
network=have_network(server,_extra=extra)

;-- if primary server is down, try secondary

if not network then begin
 server2='sohowww.nascom.nasa.gov'
 network=have_network(server,_extra=extra)
 if network then return,server2
endif

return,server
end

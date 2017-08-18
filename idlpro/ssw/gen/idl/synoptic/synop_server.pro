;+
; Project     : HESSI
;
; Name        : SYNOP_SERVER
;
; Purpose     : return first available Synoptic data server
;
; Category    : synoptic sockets
;                   
; Inputs      : None
;
; Outputs     : SERVER = Synoptic data server name
;
; Keywords    : NETWORK = 1 if network is up
;               PATH = path to synoptic data
;               NO_CHECK = don't check network status
;               FULL_NAME = prepend 'http://'
;               SOHO = force SOHO server
;
; History     : 29-Dec-2001,  D.M. Zarro (EITI/GSFC)  Written
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-


function synop_server,path=path,_ref_extra=extra,network=network,$
                           no_check=no_check,full_name=full_name,soho=soho


check=1b-keyword_set(no_check)
full_name=keyword_set(full_name)
network=0b

;-- check primary server

if not keyword_set(soho) then begin
 path='/synoptic'
 server='beauty.nascom.nasa.gov'
 if check then network=have_network(server,_extra=extra) else network=1b
endif

;-- if primary server is down, try secondary

if not network then begin
 path='/data/synoptic'
 server='sohowww.nascom.nasa.gov'
 if check then network=have_network(server,_extra=extra) else network=1b
endif

if full_name then server='http://'+server
return,server
end

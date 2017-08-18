;+
; Project     : HESSI
;                  
; Name        : HAVE_NETWORK
;               
; Purpose     : check if network connection is available
;                             
; Category    : system utility sockets
;               
; Syntax      : IDL> a=have_network()
;
; Optional    : SERVER = server to lookup [def eq 'www.google.com']
; Inputs      :
;                                        
; Outputs     : 1/0 if yes/no
;
; Keywords    : INTERVAL = seconds between rechecking
;                          (otherwise use result of last check) 
;               RESET = set to force check without waiting INTERVAL (same as INTERVAL=0)
;                   
; History     : 8 Mar 2002, Zarro (L-3Com/GSFC)
;              22 Apr 2005, Zarro (L-3Com/GSFC) - return error message
;               when network is down.
;              1 Dec 2005, Zarro (L-3Com/GSFC) - removed http object
;               from common because of unwanted side effects.
;
; Contact     : dzarro@solar.stanford.edu
;-    

function have_network,server,verbose=verbose,err=err,_extra=extra,$
         interval=interval,reset=reset

common have_network,last_time,last_state,last_server

err=''
reset=keyword_set(reset)
verbose=keyword_set(verbose)

if reset then delvarx,last_time,last_state,last_server
if is_blank(server) then test_server='www.google.com' else test_server=server
if not is_number(interval) then interval=30

;-- return last state if same server and time less than interval

now=systime(/seconds)
if exist(last_time) and exist(last_state) and exist(last_server) then begin
 if (test_server eq last_server) and (now-last_time) lt interval then begin
  if not last_state then begin
   err='Network connection to '+last_server+' is unavailable'
   if verbose then message,err,/cont
  endif
  return,last_state
 endif
endif

last_time=now & last_server=test_server 

http=obj_new('http',err=err,verbose=verbose)
if is_string(err) then begin
 last_state=0b & return,last_state
endif
http->hset,retry=0
http->head,test_server,page,err=err

if is_blank(err) then last_state=1b else last_state=0b

if verbose and is_string(err) then message,err,/cont

obj_destroy,http

return,last_state

end

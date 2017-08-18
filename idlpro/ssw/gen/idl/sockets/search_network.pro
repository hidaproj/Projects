;+
; Project     : HESSI
;                  
; Name        : SEARCH_NETWORK
;               
; Purpose     : Enable/Disable network connection checking
;                             
; Category    : system utility sockets
;               
; Syntax      : IDL> search_network
;
; Outputs     : None
;
; Keywords    : ENABLE = enable network checking [def]
;               DISABLE = disable network checking
;                   
; Side Effects: Sets env SEARCH_NETWORK = 1 if enable 
;
; History     : 14 April 2002, Zarro (L-3Com/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-    

pro search_network,disable=disable,_extra=extra

value='1'
if keyword_set(disable) then value=''
mklog,'SEARCH_NETWORK',value
return
end


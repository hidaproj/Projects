;+
; Project     : HESSI
;                  
; Name        : WHERE2
;               
; Purpose     : wrapper around WHERE that returns COMPLEMENT for IDL versions lt 5.4
;                             
; Category    : utility
;               
; Syntax      : IDL> check=where2(array,count,complement=complement,ncomplement=ncomplement)
;
; Inputs      : ARRAY = array expression to check
; 
; Outputs     : CHECK = expression matching indicies
;               COUNT = # of matching indicies
;
; Keywords    : COMPLEMENT = expression non-matching indicies
;               NCOMPLEMENT = # of non-matching indicies
;                                   
; History     : Written, 16-April-2004, Zarro (L-3Com/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-    

function where2,array,count,complement=complement,ncomplement=ncomplement,_extra=extra

count=0
ncomplement=n_elements(array)
if ncomplement eq 0 then return,-1

;-- catch unplanned errors

error=0
catch,error
if error ne 0 then begin
 catch,/cancel
 return,-1
endif

;-- use the new better way for versions 5.4 or greater

if since_version('5.4') then $
 return,call_function('where',array,count,complement=complement,ncomplement=ncomplement,_extra=extra)

;-- use slower brute force (requires 2 calls to where) 

complement=lindgen(ncomplement)
check=where(array,count)
if count eq 0 then return,check

if count eq ncomplement then begin
 ncomplement=0 & complement=-1
 return,check
endif
 
complement[check]=-1
complement=where( temporary(complement) ne -1,ncomplement)
return,check

end

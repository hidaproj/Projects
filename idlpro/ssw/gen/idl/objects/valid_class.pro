;+
; Project     : HESSI
;
; Name        : VALID_CLASS
;
; Purpose     : check if input name is a valid object class name
;
; Category    : utility objects
;
; Explanation : checks for a valid class by trying to create an
;               object from it.
;
; Syntax      : IDL> valid=valid_class(class)
;
; Examples    : valid=valid_class('sxt')
;
; Inputs      : CLASS = class name 
;
; Outputs     : 1/0 if valid or invalid
;             : INDEX = indicies of valid class names
;
; History     : Written 20 May 1999, D. Zarro, SM&A/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-


function valid_class,name,index,count=count

count=0 & index=-1
if size(name,/tname) ne 'STRING' then return,0b

np=n_elements(name)
valid=bytarr(np)
for i=0,np-1 do begin
 tname=strtrim(name[i],2)
 status=0b
 if tname ne '' then begin
  expr='temp={'+tname+'}'
  status=execute(expr)
 endif
 valid[i]=status
endfor

if np eq 1 then valid=valid[0]
index=where(valid eq 1,count)
if count eq 1 then index=index[0]
return,valid 

end

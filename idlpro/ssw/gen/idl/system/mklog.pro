;+
; Project     : SOHO - CDS
;
; Name        : MKLOG
;
; Purpose     : define a logical (VMS) or environment (UNIX) variable
;
; Category    : Utility, OS
;
; Explanation : checks OS to determine which SET function to use
;
; Syntax      : IDL> mklog,name,value
;
; Inputs      : NAME  = string name of variable to define logical for
;             : VALUE = string name of logical 
;
; Keywords    : VERBOSE = print result
;
; Side effects: logical will become undefined if name=''
;
; History     : Written, 1-Sep-1992,  D.M. Zarro. 
;               Modified, 25-May-99, Zarro (SM&A/GSC) - add better OS check
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

pro mklog,name,value,verbose=verbose

on_error,1

sz=size(name)
np=n_elements(sz)
if sz(np-2) ne 7 then return
if not exist(value) then return

sz=size(value)
np=n_elements(sz)
svalue=value
if sz(np-2) eq 7 then svalue=chklog(value,/pre)
if  sz(np-2) eq 1 then svalue=fix(value)

os=strupcase(os_family())

case os of
 'VMS'   : begin
            if strtrim(svalue,2) eq '' then begin
             ok=chklog(name)
             if ok ne '' then call_procedure,'dellog',name
            endif  else call_procedure,'setlog',name,svalue
           end
 else    : setenv,strtrim(name,2)+'='+strtrim(string(svalue),2)
endcase
verbose=keyword_set(verbose)
if verbose then print,'% MKLOG: '+name+' = '+chklog(name)

return & end

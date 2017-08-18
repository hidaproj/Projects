;+
; Project     : HESSI
;
; Name        : get_def_times
;
; Purpose     : return default times
;
; Category    : HESSI, GBO, utility, time
;
; Explanation : If TSTART/TEND are not specified correctly, then
;               return current time and end of current day
;               as useful defaults
;
; Syntax      : IDL> dstart=get_def_times(tstart,tend,dend=dend)
;
; Opt. Inputs : TSTART = start time
;               TEND   = end time
;
; Outputs     : DSTART = TSTART or current time if invalid TSTART (TAI format)
;
; Opt. Outputs: None
;
; Keywords    : DEND = TEND or end of current day if invalid TEND
;               Inherits ANYTIM keywords
;               ROUND_TIMES = round start/end to start/end of day
;               NO_NEXT = don't include next day
;
; History     : 14-Nov-1999,  D.M. Zarro (SM&A/GSFC),  Written
;               22-Mar-2004, Zarro (L-3Com/GSFC) - added check for tend < tstart
;               7-Apr-2004, Zarro (L-3Com/GSFC) - fixed bug that returned
;                structure time instead of TAI (Sorry, Luis)
;               12-Nov-2005, Zarro (L-3Com/GSFC) - added VSO output:
;                yyyymmddhhmmss
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

function get_def_times,tstart,tend,dend=dend,_extra=extra,vso=vso,$
                       round_times=round_times,err=err,no_next=no_next

; TSTART = start time [def= start of current day]
; TEND   = end time [def = end of current day]


no_next=keyword_set(no_next) 
round_t=keyword_set(round_times)

err=''
err1=''
dstart=anytim2utc(tstart,err=err1)
secs_day= 86400.d
if err1 ne '' then get_utc,dstart
if round_t then dstart.time=0

err2=''
dend=anytim2utc(tend,err=err2)
if err2 ne '' then begin
 dend=dstart
 dend.time=0
 dend.mjd=dend.mjd+1
endif else begin
 if round_t then begin
  dend.time=0
  dend.mjd=dend.mjd+1
 endif
endelse

;-- ensure start < end

dstart=anytim2tai(dstart)
dend=anytim2tai(dend)

if dend lt dstart then begin
 temp=dend
 dend=dstart & dstart=temp
endif

if no_next then begin
 if (dstart ne dend) then dend=dend-secs_day
endif

;-- convert to other requested formats

if keyword_set(vso) then begin
 dstart=vso_format(dstart)
 dend=vso_format(dend)
 return,dstart
endif

if is_struct(extra) then begin
 dstart=anytim2utc(dstart,_extra=extra)
 dend=anytim2utc(dend,_extra=extra) 
endif

if (err1 ne '') and (err2 ne '') then err=trim(err1)+' '+trim(err2) 

return,dstart & end



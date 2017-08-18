;+
; Project     : HESSI
;
; Name        : RD_GOES
;
; Purpose     : read GOES data
;
; Category    : synoptic gbo
;
; Syntax      : IDL> rd_goes,times,data,trange=trange
;
; Inputs      : None
;
; Outputs     : TIMES = time array (SECS79)
;               DATA  = data array (# TIMES x 2)
;
; Keywords    : TRANGE=[TSTART, TEND] = time interval to select
;               TAI = TIMES in TAI format
;               NO_SEARCH = don't search each satellite
;
; History     : Written 18 Feb 2001, D. Zarro, EITI/GSFC
;               14-Dec-2005 - changed err message text
;
; Contact     : dzarro@solar.stanford.edu
;-

pro rd_goes,times,data,err=err,trange=trange,count=count,tai=tai,$
            _extra=extra,status=status,verbose=verbose,gdata=gdata,$
             type=type,gsat=gsat,no_search=no_search

;-- usual error checks

err=''
count=0
delvarx,times,data
verbose=keyword_set(verbose)
gsat=''
type=''
status=0
search=1b-keyword_set(no_search)

time_input=0
if exist(trange) then begin
 if n_elements(trange) eq 2 then begin
  err1=''
  t1=anytim2utc(trange(0),/vms,err=err1)
  err2=''
  t2=anytim2utc(trange(1),/vms,err=err2)
  time_input=(err1 eq '') and (err2 eq '')
 endif
 if not time_input then begin
  err='invalid TRANGE input'
  message,err,/cont
  return
 endif
endif else t1=get_def_times(dend=t2,/vms,/round)

;-- try GOES satellite entered on command line, else cycle till data is found

res='3 sec'
sats=goes_sat()
chk=have_tag(extra,'goe',index,/start,tag=tag)
ok=where_vector(tag,sats,count)
if count gt 0 then begin
 sat=sats[ok[0]]
 if search then begin
  others=where(sat ne sats)
  sat=[sat,sats[others]]
 endif
endif else sat=sats

if is_struct(extra) then extra=rem_tag(extra,index)
if have_tag(extra,'fiv',/start) then res='5 min'
if have_tag(extra,'one',/start) then res='1 min'

for i=0,n_elements(sat)-1 do begin
 nextra=add_tag(extra,1,sat[i])
 if verbose then message,'Trying '+sat[i]+'...',/cont

 rd_gxd,t1,t2,gdata,_extra=nextra,status=status,verbose=verbose
 type=sat[i]+' '+res

 if is_struct(gdata) then begin
  if verbose then message,'Found '+type+' data',/cont

;-- unpack the data

  if  n_params() eq 2 then begin
   if keyword_set(tai) then times=anytim(gdata,/tai) else times=anytim(gdata)
   data=[[gdata.lo],[gdata.hi]]
  endif

  count=n_elements(gdata)
  gsat=sat[i]
  return
 endif
endfor

err='No GOES data available for specified time.'
message,err,/cont
type=''
delvarx,gdata

return
end

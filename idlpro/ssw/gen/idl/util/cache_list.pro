;+
; Project     : HESSI
;
; Name        : CACHE_LIST
;
; Purpose     : Cache time-based listing 
;
; Category    : Utility
;
; Explanation : Useful for speeding up searches if time window does
;               not change, or if search is within time window of last
;               search. Each listing is stored in unique common block
;               that is dynamically named: cache_name, and in a dynamic
;               variable named: var_name.
;
; Syntax      : cache_list,name,times,list,tstart=tstart,tend=tend
;
; Inputs      : NAME = unique name for string ID 
;               LIST = list array to retrieve or cache (if /SET)
;               TIMES = TAI times of retrieved list elements
;
; Keywords    : TSTART = listing start time [def = min(TIMES)]
;               TEND = list end time        [def = max(TIMES)]
;               SET  = set to store list (def = GET)
;               COUNT = # or retrieved list elements
;               STATUS = 0 if something failed, or 1 for success
;
; History     : 20 Nov 1999 D. Zarro (SM&A/GSFC) - written
;
; Contact     : dzarro@solar.stanford.edu
;-

pro cache_list,name,list,times,tstart=tstart,tend=tend,$
    count=count,set=set,status=status,verbose=verbose

count=0 & status=0b
dset=keyword_set(set)
verb=keyword_set(verbose)

if is_blank(name) then begin
 err='cache name must be non-blank string'
 message,err,/cont
 return
endif
id=strtrim(name,2)

;-- remove any problematic characters from name
 
weird=['-','*','.',',','/','\','+','&','%','$']
for i=0,n_elements(weird)-1 do id=str_replace(id,weird(i),'_')

;-- check if time limits input.

v1=valid_time(tstart)
v2=valid_time(tend)
if (v1 and v2) then dstart=min([anytim2tai(tstart),anytim2tai(tend)],max=dend)

;-- inserting

if dset then begin

 if not exist(times) then begin
  err='missing list times'
  message,err,/cont
  return
 endif
 count=n_elements(list)

;-- if time limits are not input then min/max of TIMES.

 if (not v1) or (not v2) then dstart=min(times,max=dend)

;-- update common cache_id
                                                
if count eq 0 then $
  last={list:0,times:times,tstart:dstart,tend:dend,count:0} else $
   last={list:list,times:times,tstart:dstart,tend:dend,count:count}

 state=execute('common cache_'+id+',var_'+id)
 state=execute('var_'+id+'=last')
 status=1b
 if verb then message,'caching in "cache_'+id+'"',/cont
 return
endif

;-- retrieving

state=execute('common cache_'+id+',var_'+id)
state=execute('if exist(var_'+id+') then last=var_'+id)
if not is_struct(last) then return

;-- if time limits are not input then return all times

if (not v1) or (not v2) then begin
 count=n_elements(times) & status=1b
 if verb then message,'retrieving all data from "cache_'+id+'"',/cont
 return
endif

;-- check if time limits are within last time range

within_times= (dstart le last.tend) and (dstart ge last.tstart) and $
              (dend le last.tend) and (dend ge last.tstart)

if (within_times) and (last.count gt 0) then begin
 keep=where(last.times ge dstart and last.times le dend,count)
 if count gt 0 then begin
  if verb then message,'retrieving from "cache_'+id+'"',/cont
  list=(last.list)(keep)
  times=(last.times)(keep)
  if count eq 1 then begin
   list=list(0)
   times=times(0)
  endif
  status=1b
 endif
endif

return

end

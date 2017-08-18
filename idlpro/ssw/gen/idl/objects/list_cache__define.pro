;+
; Project     : HESSI
;
; Name        : LIST_CACHE__DEFINE
;
; Purpose     : Define a cache list object, whose contents persist
;               in memory even after object is destroyed. 
;               Yes, it uses common blocks, but their names are
;               dynamically created so there is never a conflict.
;
; Category    : Ancillary GBO Synoptic Objects
;
; Syntax      : IDL> c=obj_new('list_cache')
;
; Properties  : NAME = cache name
;               TSTART/TEND: start/end times used when creating list
;
; History     : Written 8 Apr 2000, D. Zarro, SM&A/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

;---------------------------------------------------------------------------
;-- constructor

function list_cache::init,name

ret=self->data_cache::init(name)

if ret eq 0 then return,ret

dprint,'% LIST_CACHE::INIT'

return,1

end

;--------------------------------------------------------------------------

pro list_cache::cleanup

self->data_cache::cleanup

return & end

;--------------------------------------------------------------------------
;-- set properties

pro list_cache::set,tstart=tstart,tend=tend,_extra=extra

self->data_cache::set,_extra=extra

err=''
dstart=anytim2tai(tstart,err=err)
if err eq '' then self.tstart=dstart

err=''
dend=anytim2tai(tend,err=err)
if err eq '' then self.tend=dend

t1=self.tstart & t2=self.tend
self.tend=t2 > t1
self.tstart= t1 < t2

return & end

;---------------------------------------------------------------------------
;-- show properties

pro list_cache::show

self->data_cache::show

tstart=0. & tend=0.
if self.tstart gt 0 then tstart=anytim2utc(self.tstart,/vms)
if self.tend gt 0 then tend=anytim2utc(self.tend,/vms)
print,'% tstart: ',tstart
print,'% tend:   ',tend

return & end
                
;----------------------------------------------------------------------------
;-- set cache data

pro list_cache::setdata,times,data,err=err

if (not self->valid_name(err)) then return

if not exist(times) then begin
 err='missing data times'
 message,err,/cont
 return
endif

;-- update common cache_id

dcount=n_elements(data)                                                
count=n_elements(times)
if dcount eq 0 then $
  last={data:0,times:times,tstart:self.tstart,tend:self.tend,count:0} else $
   last={data:data,times:times,tstart:self.tstart,tend:self.tend,count:count}

self->save,last

return & end

;---------------------------------------------------------------------------
;-- get cache data

pro list_cache::getdata,times,data,count=count,err=err

count=0 & delvarx,times,data

if (not self->valid_name(err)) then return

self->restore,last
if size(last,/tname) ne 'STRUCT' then return

within_times= (self.tstart le last.tend) and (self.tstart ge last.tstart) and $
              (self.tend le last.tend) and (self.tend ge last.tstart)

if (within_times) and (last.count gt 0) then begin
 keep=where(last.times ge self.tstart and last.times le self.tend,count)
 if count gt 0 then begin
  if count lt n_elements(last.times) then begin
   data=(last.data)[keep]
   times=(last.times)[keep]
  endif else begin
   data=last.data
   times=last.times
  endelse
 endif
endif
 
return & end
                                
;------------------------------------------------------------------------------
;-- define cache list object

pro list_cache__define                 

temp={list_cache,tstart:0d,tend:0d, inherits data_cache}

return & end



;+
; Project     : HESSI
;
; Name        : CACHE__DATA
;
; Purpose     : create a cache object, whose contents persist
;               in memory even after object is destroyed. 
;               Yes, it uses common blocks, but their names are
;               dynamically created so there is never a conflict.
;
; Category    : Ancillary GBO Synoptic Objects
;
; Syntax      : IDL> c=obj_new('data_cache')
;
; Properties  : NAME = cache name
;
; History     : Written 8 Apr 2000, D. Zarro, SM&A/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

;---------------------------------------------------------------------------
;-- constructor

function data_cache::init,name

self->set,name=name

return,1

end

;--------------------------------------------------------------------------
;-- destructor

pro data_cache::cleanup

dprint,'% DATA_CACHE: cleaning up...'
return & end 


;--------------------------------------------------------------------------
;-- set properties

pro data_cache::set,name=name,verbose=verbose

if is_number(verbose) then self.verbose= 0b > byte(verbose) < 1b

;-- remove any problematic characters from name

if is_string(name) then begin
 id=trim(name)
 weird=['-','*','.',',','/','\','+','&','%','$','_']
 for i=0,n_elements(weird)-1 do id=str_replace(id,weird[i],'')
 self.name=id
endif

return & end

;---------------------------------------------------------------------------
;-- show properties

pro data_cache::show

print,''
print,'CACHE properties:'
print,'----------------'
print,'% cache name: ',self.name

return & end
               
;---------------------------------------------------------------------------
;-- validate name

function data_cache::valid_name,err

valid=1b & err=''
if not is_string(self.name) then begin
 err='cache name must be non-blank'
 message,err,/cont
 valid=0b
endif

return,valid & end
      
;----------------------------------------------------------------------------
;-- set cache data

pro data_cache::setdata,times,data,err=err
           
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
  last={data:0,times:times,count:0} else $
   last={data:data,times:times,count:count}

self->save,last

return & end

;--------------------------------------------------------------------------
;-- save in common

pro data_cache::save,data

if not exist(data) then return
id=self.name
if self.verbose then message,'saving in "cache_'+id+'"',/cont
                                 
state=execute('common cache_'+id+',var_'+id)
state=execute('var_'+id+'=data')
                                 
return & end

;---------------------------------------------------------------------------

function data_cache::has_data

id=self.name
if self.verbose then message,'checking "cache_'+id+'"',/cont                               
state=execute('common cache_'+id+',var_'+id)
state=execute('present=exist(var_'+id+')')

return,present

end

;--------------------------------------------------------------------------
;-- restore from common

pro data_cache::restore,data
            
id=self.name                               
state=execute('common cache_'+id+',var_'+id)
state=execute('if exist(var_'+id+') then data=var_'+id)

if exist(data) and self.verbose then message,'checking "cache_'+id+'"',/cont
return & end

;---------------------------------------------------------------------------
;-- get cache data

pro data_cache::getdata,times,data,count=count,err=err

count=0 & delvarx,times,data
           
if (not self->valid_name(err)) then return

self->restore,last
if size(last,/tname) ne 'STRUCT' then return

count=last.count 
times=last.times 
data=last.data
 
return & end

;------------------------------------------------------------------------------
;-- define cache object

pro data_cache__define                 

temp={data_cache,name:'',verbose:0b}

return & end



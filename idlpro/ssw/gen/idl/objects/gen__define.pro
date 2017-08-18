;+
; Project     : HESSI
;
; Name        : GEN__DEFINE
;
; Purpose     : Define a GENeric object
;
; Category    : objects
;
; Explanation : This CLASS definition really only contains general methods
;               that can be inherited by other objects
;
; Inputs      : None
;
; Outputs     : Generic object with COPY and EXTRACT methods
;
; History     : Written 14 Oct 1999, D. Zarro, SM&A/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

function gen::init

self.debug = get_debug()
self.verbose =  fix( getenv('SSW_FRAMEWORK_VERBOSE') )
return, 1

end

;---------------------------------------------------------------------------

function gen::check_and_set, value_stored, new_value, $
                        yes_update = yes_update, $
                        execute = execute

yes_update = not same_data( value_stored, new_value )
IF yes_update THEN BEGIN
    if keyword_set( execute ) then call_method, execute, self
    return, new_value
endif else begin 
    return, value_stored
endelse

end

;--------------------------------------------------------------------------

pro gen::cleanup

return & end

;--------------------------------------------------------------------------

pro gen::methods,super=super
iprint,obj_methods(self,super=super)
return & end

;---------------------------------------------------------------------------
;-- extract properties (procedure)

;pro gen::extract,_ref_extra=extra

;if size(extra,/tname) ne 'STRING' then return

;self->extract2,_extra=extra

;return & end

;----------------------------------------------------------------------------

pro gen::extract2,_extra=extra

if size(extra,/tname) ne 'STRUCT' then return
        
class=obj_class(self)
s=execute('struct={'+class+'}')

tags=tag_names(extra)
for i=0,n_elements(tags)-1 do begin
 if have_tag(struct,extra.(i),index) then begin
  s=execute(extra.(i)+'=self.(index[0])')
 endif 
endfor

return & end

;--------------------------------------------------------------------------
;-- extract properties (function)

function gen::getprop,_extra=extra,count=count,err=err
 
;-- following trick allows method to distinguish between
;   when keyword is set as an actual keyword or as an output
;   variable


if keyword_set(count) and (not arg_present(count)) then extra=add_tag(extra,1,'count')
if keyword_set(err) and (not arg_present(err)) then extra=add_tag(extra,1,'err')


all=have_tag(extra,'all_props',/exact)
get_value=have_tag(extra,'get_value',/exact)

if all then extra=rem_tag(extra,'all_props')
if get_value then extra=rem_tag(extra,'get_value')

use_def=have_tag(extra,'default_val',/exact)

count=0
err=''
if use_def then begin
 rdef=extra.default_val 
 extra=rem_tag(extra,'default_val')
endif else rdef=''

if not all then begin 
 if size(extra,/tname) ne 'STRUCT' then return,rdef
 props=tag_names(extra)
 if n_elements(props) ne 1 then begin
  err='Only one property can be accessed at a time'
  message,err,/cont
  print,props
  return,rdef
 endif
 prop=trim(strlowcase(props[0]))
endif

class=obj_class(self)
status=execute('struct={'+class+'}')
if not status then return,rdef

;-- check if all properties are requested

props=strlowcase(tag_names(struct))

if all then begin
 struct_assign,self,struct
 for i=0,n_elements(props)-1 do begin
  np=n_elements(self.(i))
  for j=0,np-1 do begin
   if ptr_valid( self.(i)[j] ) then begin
    if exist( *(self.(i)[j]) ) and get_value then begin
     struct=rep_tag_value(struct,*(self.(i))[j],props[i])
    endif
   endif
  endfor
 endfor
 count=n_elements(struct)
 return,struct
endif

if not have_tag(struct,prop,index,exact=exact,/start) then begin
 err='Property does not exist'
 return,rdef
endif

;-- find first one in which first 3 letters match

nfound=n_elements(index)

if nfound gt 1 then begin
 i=-1
 repeat begin
  i=i+1
  chk=strmid(props(index[i]),0,3) eq strmid(prop,0,3)
 endrep until chk or (i eq (nfound-1))

 if chk then value=self.(index[i]) else begin
  err='Property does not exist'
  return,rdef
 endelse
endif else value=self.(index[0])

count=n_elements(value)
if size(value,/tname) ne 'POINTER' then begin
 if size(value,/tname) eq 'STRING' then value=trim2(value)
 return,value
endif
if not ptr_valid(value) then return,rdef

;-- if a valid pointer, then return it's value

count=n_elements(*value)
if count gt 0 then return,*value

return,rdef
end

;---------------------------------------------------------------------------
;-- privatize properties

pro gen::private,properties,caller,_extra=extra

if (datatype(extra) eq 'STC') and (datatype(properties) eq 'STR') and $
 (datatype(caller) eq 'STR') then begin

 if (strpos(strupcase(caller),'::INIT') eq -1) then begin
  extra=rem_tag(extra,properties)
  if datatype(extra) ne 'STC' then delvarx,extra
 endif
endif

return & end

;---------------------------------------------------------------------------
;-- extract name and dir parts for filename (using REGEXP)

pro gen::fbreak,file,dir,name

if since_version('5.3') then name=file_break(file,path=dir) else begin
 break_file,file,dsk,path,fname,ext
 dir=trim2(dsk+path)
 name=trim2(fname+ext)
endelse

return & end

;----------------------------------------------------------------------------

pro gen__define
  
  temp =  {gen, $
           debug:0B,  $
           verbose: 0B}
   
end
                                                                           

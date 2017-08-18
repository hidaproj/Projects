;-- package of new structure handling routines.
;-- .run new_struct
;-- in beta mode


function add_tag,struct,tag_value,tag_name,no_copy=no_copy,_extra=extra,err=err

dprint,'% ADD_TAG'

err=''

;-- if no structure entered, then create it

no_copy=keyword_set(no_copy)
if is_string(tag_name) and exist(tag_value) and (not is_struct(struct)) and $
 n_elements(tag_name) eq 1 then begin
  if no_copy then return,create_struct(tag_name,temporary(tag_value)) else $
   return,create_struct(tag_name,tag_value) 
endif

;-- input error checks

err='Input structure required'
if not exist(struct) then return,-1
if not is_struct(struct) then return,struct
err='Input tag value required'
if not exist(tag_value) then return,struct
err='Input scalar tag name required'
if is_blank(tag_name) then return,struct
if n_elements(tag_name) ne 1 then return,struct

;-- do simple scalar case first

err=''
nstruct=n_elements(struct)
if nstruct eq 1 then begin
 if no_copy then return,create_struct(struct,tag_name,temporary(tag_value)) else $
  return,create_struct(struct,tag_name,tag_value)
endif

;-- if struct is vector, then assume tag_value has same n_elements

sz=size(tag_value)
if sz[0] eq 1 then tvalue=tag_value[0]
if sz[0] gt 1 then begin
 ndim=sz[sz[0]]
 if (ndim ne nstruct) then begin
  err='# elements of tag value must equal # elements of structure'  
  message,err+' '+get_caller(),/cont
  return,struct
 endif

;-- remove last dimension since replicating below

 sz[sz[0]]=1
 tvalue=make_array(size=sz,/nozero)
endif

;-- create structure template with new tag added

if exist(tvalue) then $
 temp=create_struct(struct[0],tag_name,tvalue) else $
  temp=create_struct(struct[0],tag_name,tag_value) 

;-- replicate it and copy elements

temp=replicate(temp,nstruct)
struct_assign,struct,temp
ntags=n_tags(temp)
if no_copy then temp.(ntags-1)=temporary(tag_value) else $
 temp.(ntags-1)=tag_value

return,temp

end

 
     
function rem_tag,struct,tags,_extra=extra,err=err,free_mem=free_mem

dprint,'% REM_TAG'
err='Input structure required'
if not exist(struct) then return,-1
if not is_struct(struct) then return,struct
sz=size(tags,/type)
index_input=(sz gt 1) and (sz lt 6)
string_input=(sz eq 7)
err='Input tag names or tag indexes required'
if (not index_input) and (not string_input) then return,struct
if string_input and is_blank(tags) then return,struct

;-- create structure template with tags removed

err=''
stag_names=tag_names(struct)
ntags=n_elements(stag_names) & stag_index=lindgen(ntags)
for i=0,ntags-1 do begin
 if string_input then chk=where(trup(tags) eq stag_names[i],count) else $
  chk=where(long(tags) eq stag_index[i],count)
 if count eq 0 then begin
  piece=stag_names[i]+':struct.('+strtrim(i,2)+')'
  if not exist(temp) then temp=piece else temp=[temp,piece]
 endif else begin
  if not exist(rindex) then rindex=i else rindex=[rindex,i]
 endelse
endfor

;-- no tags to remove

n_remove=n_elements(rindex)
if n_remove eq 0 then return,struct

;-- free up memory 

if keyword_set(free_mem) then for i=0,n_remove-1 do free_var,struct.(i)

;-- all tags removed

if (n_remove eq ntags) then return,-1

ntemp=n_elements(temp)
if ntemp gt 1 then temp[0:ntemp-2]=temp[0:ntemp-2]+',$'
temp[ntemp-1]=temp[ntemp-1]+'}'
temp=['out={ $',temp]

;-- create function file to call

temp_dir=get_temp_dir()
temp_name='r'+get_rid(/time)
temp_funct=temp_name+'.pro'
temp_file=concat_dir(temp_dir,temp_funct)
openw,lun,temp_file,/get_lun
printf,lun,'function '+temp_name+',struct'
printf,lun,transpose(temp)
printf,lun,'return,out & end'
close_lun,lun

;-- execute temporary function

cd,current=current
cd,temp_dir
qsave=!quiet
!quiet=1
out=call_function(temp_name,struct[0])
!quiet=qsave
cd,current
file_delete,temp_file

;-- if struct is array then need to replicate it

nstruct=n_elements(struct)
if nstruct gt 1 then begin
 out=replicate(out,nstruct)
 struct_assign,struct,out,/nozero
endif

return,out

end

 
     
function rep_tag_name,struct,old,new,_extra=extra,err=err

;dprint,'% REP_TAG_NAME: ',get_caller()

err='Input structure required'
if not exist(struct) then return,-1
if not is_struct(struct) then return,struct
err='Input scalar old and new tag names required'
if is_blank(old) or is_blank(new) then return,struct
if (n_elements(old) ne 1) or (n_elements(new) ne 1) then return,struct
if trup(old) eq trup(new) then return,struct

err='Input tag name not found'
if not have_tag(struct,old,index) then return,struct

;-- avoid duplicate tags

err='Identical old and new tag names'
if have_tag(struct,new) then return,struct

;-- save contents of old tag name

err=''
value=struct.(index)

;-- remove old tag (skip if only one tag)

ntags=n_tags(struct)
if ntags gt 1 then out=rem_tag(struct,old)

;-- add new tag and it's value

return,add_tag(out,value,new,err=err)

end

 
     
function rep_tag_value,struct,value,name,no_add=no_add,$
          _extra=extra,err=err,free_mem=free_mem

;dprint,'% REP_TAG_VALUE: ',get_caller()

err='Input structure required'
if not exist(struct) then begin
 if keyword_set(no_add) then return,-1
 return,add_tag(struct,value,name,err=err,_extra=extra)
endif

err='Input tag value required'
if not exist(value) then return,struct
err='Input scalar tag name required'
if is_blank(name) then return,struct
if n_elements(name) ne 1 then return,struct

chk=have_tag(struct,name,index)
if keyword_set(no_add) then begin
 err='Input tag name not found'
 if not chk then return,struct
endif

;-- free memory associated with value before replacing

if keyword_set(free_mem) then free_var,struct.(index)

;-- remove tag (skip if only one tag)

err=''
ntags=n_tags(struct)
if ntags gt 1 then out=rem_tag(struct,name)

;-- add tag with new value

return,add_tag(out,value,name,err=err,_extra=extra)

end

 
     
function join_struct,struct1,struct2,_extra=extra,err=err

dprint,'% JOIN_STRUCT

err=''

if is_struct(struct1) and (not is_struct(struct2)) then return,struct1
if is_struct(struct2) and (not is_struct(struct1)) then return,struct2

err='Input equal array size structures required' 
if (not is_struct(struct1)) and (not is_struct(struct2)) then return,-1
if n_elements(struct1) ne n_elements(struct2) then return,-1

err=''
error=0
catch,error
if error ne 0 then begin
 catch,/cancel
 err=!err_string
 message,err,/cont 
 return,struct1
endif

;-- check for duplicate tag names
;-- tags in struct1 override those in struct2

tags_1=tag_names(struct1)
tags_2=tag_names(struct2)

ntags_2=n_elements(tags_2)
for i=0,ntags_2-1 do begin
 chk=where(tags_2[i] eq tags_1,count)
 if count gt 0 then begin
  if exist(rtag) then rtag=[rtag,tags_2[i]] else rtag=tags_2[i]
 endif
endfor

;-- return if all tags duplicated

nrtag=n_elements(rtag)
if nrtag eq ntags_2 then return,struct1


out=create_struct(struct1[0],rem_tag(struct2[0],rtag))

;-- rebuild if structure array

nstruct=n_elements(struct1)
if nstruct gt 1 then begin
 out=replicate(out,nstruct)
 struct_assign,struct1,out,/nozero
 struct_assign,struct2,out,/nozero
endif

return,out

end

     
function rep_struct_name,struct,new,_extra=extra,err=err

dprint,'% REP_STRUCT_NAME'

err='Input structure required'
if not exist(struct) then return,-1
if not is_struct(struct) then return,struct
err='Input scalar structure name required'
if is_blank(new) then return,struct
if n_elements(new) ne 1 then return,struct
err=''

error=0
catch,error
if error ne 0 then begin
 catch,/cancel
 err='Structure name already in use'
 message,err,/cont 
 return,struct
endif

out=create_struct(name=new,struct[0])

;-- replicate for structure array

nstruct=n_elements(struct)
if nstruct gt 1 then begin
 out=replicate(out,nstruct)
 struct_assign,struct,out,/nozero
endif

return,out
end

;+
; Project     : HESSI
;
; Name        : PTR_CLONE
;
; Purpose     : Clone a pointer by saving it to an IDL save file
;               and then restoring it into a new pointer
;
; Category    : utility pointers
;
; Syntax      : IDL> clone=ptr_clone(pointer)
;
; Inputs      : pointer = pointer to clone (array or scalar)
;
; Outputs     : CLONE = cloned pointer
;
; History     : Written 29 Nov 2002, D. Zarro (EER/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;
;-

function ptr_clone,pointer,err=err,_extra=extra

err='Invalid pointer entered'
if not exist(pointer) then begin
 message,err,/cont
 return,-1
endif

if size(pointer,/tname) ne 'POINTER' then begin
 message,err,/cont
 return,pointer
endif

return,clone_var(pointer)

end
;+
; Project     : HESSI
;
; Name        : STC_CLONE
;
; Purpose     : Clone a structure by saving it to an IDL save file
;               and then restoring it into a new structure
;
; Category    : utility structures
;
; Syntax      : IDL> clone=stc_clone(structure)
;
; Inputs      : structure = structure to clone (array or scalar)
;
; Outputs     : CLONE = cloned structure
;
; History     : Written 29 Nov 2002, D. Zarro (EER/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;
;-

function stc_clone,structure,err=err,_extra=extra

err='Invalid structure entered'
if not exist(structure) then begin
 message,err,/cont
 return,-1
endif

if size(structure,/tname) ne 'STRUCT' then begin
 message,err,/cont
 return,structure
endif

return,clone_var(structure)

end

pro new_struct

return & end     

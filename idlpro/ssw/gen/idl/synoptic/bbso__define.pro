;+
; Project     : HESSI
;
; Name        : BBSO__DEFINE

; Purpose     : Define a BBSO data object
;
; Category    : Ancillary GBO Synoptic Objects
;
; Syntax      : IDL> c=obj_new('bbso')
;
; History     : Written 7 June 2000, D. Zarro, EIT/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-
;-----------------------------------------------------------------------------
;-- init 

function bbso::init,_ref_extra=extra

ret=self->site::init(_extra=extra)
                     
if not ret then return,ret

ret=self->fits::init(_extra=extra)

if not ret then return,ret
           
self->setprop,/fits,rhost='ftp.bbso.njit.edu',delim='/',$
      org='day',topdir='/pub/archive',ftype='bbso_halph_fl*.*'

return,1

end

;----------------------------------------------------------------------------

pro bbso::cleanup

self->site::cleanup
self->fits::cleanup

return & end

;------------------------------------------------------------------------------
;-- SET method

pro bbso::setprop,jpeg=jpeg,fits=fits,err=err,_extra=extra

;-- set file type and location to download

if keyword_set(fits) then ext='.fts'
if keyword_set(jpeg) then ext='.jpg' 

self->site::setprop,ext=ext,_extra=extra,err=err

return & end

;-----------------------------------------------------------------------------
;-- get remote subdirectory

function bbso::get_sdir,_extra=extra

return,self->site::get_sdir(delim=self.delim,/full,_extra=extra,/no_next)

end

;----------------------------------------------------------------------------
;-- driver to ftp BBSO files to $SYNOP_DATA

pro bbso::synop,tstart,tend,_extra=extra

;-- default settings

message,'copying BBSO synoptic data',/cont


if not valid_time(tstart) then begin
 get_utc,utc
 utc.time=0
endif else begin
 utc=anytim2utc(tstart)
 utc.time=0
endelse

if not valid_time(tend) then begin
 tend=utc
 tend.mjd=tend.mjd+1
endif 

self->setprop,tstart=utc,tend=tend,/subdir,err=err,back=10,_extra=extra
if err ne '' then return

;-- start with FITS summary files

self->setprop,ldir='$SYNOP_DATA/images',/fits,err=err
self->copy,count=count1

;-- next grab flare files

self->setprop,ldir='$SYNOP_DATA/images',/fits,delim='',$
      topdir='/pub/HESSI',err=err,ftype='bbso_halph_f*.*'

self->copy,count=count2

if (count1 gt 0) or (count2 gt 0) then synop_link,'bbso',back=10,_extra=extra

;-- then do JPEGS

;self->setprop,ldir='$SYNOP_DATA/gif',/jpeg,err=err
;if err eq '' then self->copy


return & end

;----------------------------------------------------------------------------
;-- convert BBSO index to FITS standard

function bbso::index2fits,index,no_copy=no_copy,err=err

err=''
if datatype(index) ne 'STC' then return,-1

if keyword_set(no_copy) then nindex=temporary(index) else nindex=index
if not have_tag(nindex,'cdelt1') then nindex=add_tag(nindex,1.0544,'cdelt1')
if not have_tag(nindex,'cdelt2') then nindex=add_tag(nindex,1.0544,'cdelt2')
nindex=rep_tag_value(nindex,0.,'crval1')
nindex=rep_tag_value(nindex,0.,'crval2')
if have_tag(nindex,'cenx') then nindex=rep_tag_value(nindex,nindex.cenx,'crpix1') 
if have_tag(nindex,'ceny') then nindex=rep_tag_value(nindex,nindex.ceny,'crpix2') 

if (not have_tag(nindex,'crpix1')) or (not have_tag(nindex,'crpix2')) then begin
 err='BBSO file does not contain standard FITS pointing headers'
 message,err,/cont
endif

return,nindex

end

;-------------------------------------------------------------------------------
;-- VSO search of BBSO archive

function bbso::list,tstart,tend,count=count,_ref_extra=extra,times=times,$
              full_path=full_path,sizes=sizes,tai=tai

times='' & sizes='' & count=0l

;-- search VSO

dstart=get_def_times(tstart,tend,dend=dend,/vms)
tcat=vso_search(dstart,dend,source='bbso',wave='1-10000',_extra=extra)

;-- return if nothing found


if (1-is_struct(tcat)) then return,''
count = n_elements(tcat)

;-- parse out metadata

if keyword_set(tai) then times=anytim2tai(tcat.time.start) else times=tcat.time.start
sizes=strtrim(tcat.size,2)+' kB'
if keyword_set(full_path) then files=tcat.fileid else files=file_break(tcat.fileid)

return,files
end

;------------------------------------------------------------------------------------------
;-- BBSO site structure

pro bbso__define                 

self={bbso,inherits site, inherits fits}

return & end

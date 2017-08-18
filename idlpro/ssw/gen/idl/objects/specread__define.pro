;+
; Project     : HESSI
;
; Name        : specread__DEFINE
;
; Purpose     : Define a general SPECTROGRAM reader object
;
; Category    : Ancillary GBO Synoptic Objects
;
; Syntax      : IDL> c=obj_new('specread')
;
; History     : Written 18 Nov 2002, D. Zarro (EER/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-
;-----------------------------------------------------------------------------
;-- init 

function specread::init,_ref_extra=extra

if not self->site::init(_extra=extra) then return,0

if not self->specplot::init(_extra=extra) then return,0

self.fits_obj=obj_new('fits',/no_map)

return,1

end

;-----------------------------------------------------------------------

pro specread::cleanup

self->specplot::cleanup
self->site::cleanup

if obj_valid(self.fits_obj) then obj_destroy,self.fits_obj

return & end
                                                                                     
;-----------------------------------------------------------------------------
;-- get remote subdirectory id's based on file dates

function specread::get_sdir,_extra=extra
      
fids=self->site::get_sdir(/full,delim='/',_extra=extra)

return,fids

end

;----------------------------------------------------------------------------
;-- read method

pro specread::read,file,data,header=header,index=index,err=err,_extra=extra,$
                     nodata=nodata

err=''

if n_elements(file) gt 1 then begin
 err='Cannot read multiple files'
 message,err,/cont
 return
endif

;-- read main data

self.fits_obj->readfits,file,data,header=header,extension=0,_extra=extra,err=err,$
                       index=index,nodata=nodata

;-- read extension

if err ne '' then begin
 message,err,/cont
 return
endif

if keyword_set(nodata) then return

self.fits_obj->readfits,file,data1,header=header1,extension=1,_extra=extra,err=err,$
               index=index1

if err ne '' then begin
 message,err,/cont
 return
endif

;-- set FITS header info into spectrogram object

self->set_fits,file,index,data,index1,data1,header=header

return & end

;----------------------------------------------------------------------------
;-- driver to ftp files to $SYNOP_DATA

pro specread::synop,_extra=extra

message,'copying synoptic data',/cont

;-- default settings

get_utc,utc
utc.time=0
self->setprop,tstart=utc,back=20,/subdir,err=err,_extra=extra,/gzip

if err ne '' then return

self->setprop,ldir='$SYNOP_DATA/spectra',err=err

if err eq '' then self->copy
                                          
return & end
                                          

;------------------------------------------------------------------------------
;-- site structure

pro specread__define                 

self={specread,fits_obj:obj_new(),inherits site,inherits specplot}

return & end


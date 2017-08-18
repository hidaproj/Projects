;+
; Project     : HESSI
;
; Name        : RD_GOES_SDAC
;
; Purpose     : read GOES SDAC FITS data (a socket wrapper around GFITS_R)
;
; Category    : synoptic gbo
;
; Syntax      : IDL> rd_goes_sdac
;
; Inputs      : See GFITS_R keywords
;
; Outputs     : See GFITS_R keywords
;
; Keywords    : See GFITS_R keywords
;               STIME, ETIME = start/end times to search
;               NETWORK = force a network search
;
; History     : Written 15 June 2005, D. Zarro, (L-3Com/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-

pro rd_goes_sdac,stime=stime,etime=etime,_ref_extra=extra,remote=remote

;-- if $GOES_FITS defined, then read files locally

if (not valid_time(stime)) or (not valid_time(etime)) then begin
 pr_syntax,'rd_goes_sdac,stime=stime,etime=time,tarray=time,yarray=data'
 return
endif

remote=keyword_set(remote)

if is_dir('$GOES_FITS') and (1-remote) then begin
 gfits_r,stime=stime,etime=etime,_extra=extra,/sdac
 return
endif

;-- determine remote location of files

goes_url=goes_fits_path(network=network)
if not network then return

;-- determine which file names to copy

files=goes_fits_files(stime,etime,_extra=extra)
if is_blank(files) then begin
 message,'No matching GOES files found',/cont
 return
endif

;-- define GOES_FITS to temporary local directory. Use CATCH to
;   unset it if we have problems.

error=0
catch,error
if error ne 0 then begin
 catch,/cancel
 mklog,'GOES_FITS',''
 return
endif

goes_dir=goes_temp_dir()
if not is_dir(goes_dir) then mk_dir,goes_dir
mklog,'GOES_FITS',goes_dir

goes_files=goes_url+'/'+files
sock_copy,goes_files,out_dir=goes_dir,_extra=extra

;-- read downloaded files 

gfits_r,stime=stime,etime=etime,_extra=extra,/sdac

mklog,'GOES_FITS',''

;-- clean up old files 

old_files=file_since(older=10,patt='go*',count=count,path=goes_dir)
if count gt 0 then file_delete,old_files,/quiet

return & end












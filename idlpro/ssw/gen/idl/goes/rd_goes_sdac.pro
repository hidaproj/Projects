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
;               REMOTE = force a network search
;
; History     : Written 15 June 2005, D. Zarro, (L-3Com/GSFC)
;               Modified 24 Nov 2005, Zarro (L-3Com/GSFC) 
;                - preserve currently defined $GOES_FITS
;               Modified 26 Dec 2005, Zarro (L-3Com/GSFC) 
;                - support varied input time formats with anytim
;               Modified 30 Dec 2005, Zarro (L-3Com/GSFC)
;                - improved by only downloading required files
;
; Contact     : dzarro@solar.stanford.edu
;-

pro rd_goes_sdac,stime=stime,etime=etime,_ref_extra=extra,remote=remote,$
                 sat=sat

;-- if $GOES_FITS defined, then read files locally

if (not valid_time(stime)) or (not valid_time(etime)) then begin
 pr_syntax,'rd_goes_sdac,stime=stime,etime=time,tarray=time,yarray=data'
 return
endif

tstart=anytim(stime,/ecs) & tend=anytim(etime,/ecs)

remote=keyword_set(remote)

if is_dir('$GOES_FITS') and (1-remote) then begin
 gfits_r,stime=tstart,etime=tend,sat=sat,_extra=extra,/sdac
 return
endif

;-- determine remote location of files

goes_url=goes_fits_path(network=network,_extra=extra)
if not network then return

;-- define GOES_FITS to temporary local directory. Use CATCH to
;   unset it if we have problems.

goes_fits_sav=chklog('$GOES_FITS')

error=0
catch,error
if error ne 0 then begin
 message,err_state(),/cont
 catch,/cancel
 mklog,'GOES_FITS',goes_fits_sav
 return
endif

goes_dir=goes_temp_dir()
if not is_dir(goes_dir) then mk_dir,goes_dir
mklog,'GOES_FITS',goes_dir

;-- cycle thru each available GOES satellite until we get a match

sats=goes_sat(/number)

;-- start with user-specified satellite

if is_number(sat[0]) then begin
 chk=where(sat[0] eq sats,count,complement=complement,ncomp=ncomp)
 if ncomp gt 0 then sats=sats[complement]
 if count gt 0 then sats=[sat[0],sats]
endif

http=obj_new('http')
dsat=-1
for i=0,n_elements(sats)-1 do begin

 tsat=sats[i]

;-- determine which file names to copy

 files=goes_fits_files(tstart,tend,_extra=extra,sat=tsat,/no_comp)
 if is_blank(files) then continue

;-- check if they exist at the server, and download

 found_sat=0b
 goes_files=goes_url+'/'+files
 for k=0,n_elements(files)-1 do begin
  if http->file_found(goes_files[k],_extra=extra) then begin
   found_sat=1b
   http->copy,goes_files[k],out_dir=goes_dir,_extra=extra
  endif
 endfor

;-- if found, then read downloaded files 

 if found_sat then begin
  gfits_r,stime=tstart,etime=tend,sat=tsat,_extra=extra,error=error,/sdac,/no_retry
 
;-- if everything is ok, then bail out otherwise try another satellite

  if not error then break

 endif

endfor

;-- clean up old files 

obj_destroy,http
mklog,'GOES_FITS',goes_fits_sav
old_files=file_since(older=10,patt='go*',count=count,path=goes_dir)
if count gt 0 then file_delete,old_files,/quiet

return & end

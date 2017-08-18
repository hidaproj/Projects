;+
; Project     : HESSI
;
; Name        : EIT_COPY
;
; Purpose     : process & copy latest EIT FITS files to HESSI archive. 
;
; Category    : synoptic gbo
;
; Syntax      : IDL> eit_copy,tstart,tend,back=back,_extra=extra
;
; Inputs      : TSTART, TEND = start and end times to consider
;               [def = current day]
;
; Outputs     : EIT FITS files.
;
; Keywords    : BACK = days to look back
;               COUNT = # of files processed
;               CLOBBER = set to clobber existing files
;               PREFLARE = minutes of preflare data to include [def=5]
;               POSTFLARE = minutes of postflare data to include [def=10]
;               BY_TIME = process all times between TSTART/TEND
;                         [def is process overlapping flare times]
;               GOES = key off GOES events instead of RHESSI
;               THRESHOLD = peak count rate threshold for RHESSI-based
;                         selection 
;               GRID = same as /BY_TIME, but select files relative to
;                        a time grid [def = hourly]
;
; Restrictions: Unix only
;
; History     : Written 14 Aug 2001, D. Zarro (EITI/GSFC)
;               Modified 3 Sep 2006, Zarro (ADNET/GSFC)
;                - added /GRID
;
; Contact     : dzarro@solar.stanford.edu
;-

;----------------------------------------------------------------------------

pro eit_copy,tstart,tend,count=count,back=back,verbose=verbose,$
             clobber=clobber,_extra=extra,out_dir=out_dir,$
             reprocess=reprocess,goes=goes,threshold=threshold,$
             preflare=preflare,postflare=postflare,by_time=by_time,grid=grid

common eit_copy,flare_data

if os_family(/lower) ne 'unix' then begin
 message,'Sorry, Unix only',/cont
 return
endif

clobber=keyword_set(clobber) or keyword_set(reprocess)
verbose=keyword_set(verbose)
by_time=keyword_set(by_time) or keyword_set(grid)

;-- decode directory environments

if exist(out_dir) then begin
 if not test_dir(out_dir) then return
 synop_images=out_dir
endif else begin
 synop_images=chklog('$SYNOP_DATA/images')
 if not test_dir(synop_images) then return
 if verbose then message,'Copying to '+synop_images,/cont
endelse

dstart=get_def_times(tstart,tend,dend=dend,/utc)

;-- determine copy times

if is_number(back) then tback=back > 0 else tback=0
if tback gt 0 then begin
 dstart=dend
 dstart.mjd=dend.mjd-tback-1
endif

dstart=anytim2utc(dstart,/vms)
dend=anytim2utc(dend,/vms)
dprint,'% dstart,dend: ',dstart,', ',dend

if is_number(preflare) then pre=float(preflare) > 0. else pre=60.
if is_number(postflare) then post=float(postflare) > 0. else post=60.

case 1 of

keyword_set(goes): begin

;-- find overlapping GOES events

 rd_gev,dstart,dend,gev,status=status,/nearest
 if status gt 0 then begin
  message,'No GOES events during specified period',/cont
  return
 endif
 decode_gev,gev,gstart,gend,gpeak,/vms,class=class

;--- remove B flares

 flares=where(stregex(class,'(C|M|X)',/bool),nflares)

 if nflares eq 0 then begin
  message,'No flares above B level',/cont
  return
 endif

 gev=gev[flares] & gstart=gstart[flares] & gend=gend[flares]
 gpeak=gpeak[flares] & class=class[flares]

 if verbose then begin
  message,'Found following GOES events - '+arr2str(class,delim=','),/cont
 endif

;-- include preflare and postflare files

 gstart=anytim(gstart,/tai)-pre*60.
 gstart=anytim2utc(gstart,/vms)

;-- if M or X flare, force postflare = 30 mins

; m_or_x=where(stregex(class,'(M|X)',/bool),fcount)
; if fcount gt 0 then post=30.

 gend=anytim(gend,/tai)+post*60.
 gend=anytim2utc(gend,/vms)

end

by_time: begin
 gstart=dstart
 gend=dend
end

;-- check RHESSI catalog

else: begin
 if not is_struct(flare_data) then flare_data=hsi_read_flarelist()
 if is_number(threshold) then thresh=float(threshold) else thresh=100. 
 hstart=anytim(flare_data.start_time,/tai)
 hend=anytim(flare_data.end_time,/tai)
 check=where_times(hstart,tstart=dstart,tend=dend,count=count)
 if count eq 0 then begin
  message,'No matching RHESSI events during specified period',/cont
  return
 endif
 hstart=hstart[check] & hend=hend[check]
 check=where(flare_data[check].peak_countrate ge thresh,count)
 if count eq 0 then begin
  message,'No RHESSI events above '+trim(thresh)+' count rate threshold',/cont
  return
 endif
 gstart=hstart[check]-pre*60.
 gend=hend[check]+post*60
 gstart=anytim2utc(gstart,/vms)
 gend=anytim2utc(gend,/vms)
end

endcase

;-- find overlapping EIT files. Search Level Zero, then QL

nflares=n_elements(gstart)
for i=0,nflares-1 do begin
 if verbose then begin
  message,'Looking for EIT images near '+gstart[i],/cont
 endif
 ez_files=eit_files(gstart[i],gend[i],/lz,/quiet)
 ql_files=eit_files(gstart[i],gend[i],/quick,/quiet)
 efiles=trim2([ez_files,ql_files])
 keep=where(efiles ne '',kcount)
 if kcount gt 0 then files=append_arr(files,efiles[keep],/no_copy)
endfor

if is_blank(files) then begin
 message,'No EIT files found',/cont
 return
endif

files=get_uniq(files)
nfiles=n_elements(files)
eit=obj_new('eit')
eit->set,verbose=verbose

;-- create a time grid and pick files nearest each grid point

if keyword_set(grid) then begin
 tgrid=timegrid(gstart,gend,/hours,/tai,_extra=extra)
 etimes=dblarr(nfiles)
 if verbose then message,'Gridding...',/cont
 for i=0,nfiles-1 do begin
  eit->read,files[i],index=index,/nodata
  etimes[i]=anytim2tai(index.date_obs)
 endfor
 near=where_near(etimes,tgrid,count=gcount)
 if gcount eq 0 then return
 files=files[near]
 nfiles=n_elements(files)
endif

;-- construct output filename, check if it exists. If not (or clobber is set),
;   prep a new one.

count=0
for i=0,nfiles-1 do begin
 if verbose then message,'Checking '+files[i],/cont
 eit->read,files[i],index=index,/nodata
 ndata=n_elements(index)

 for k=0,ndata-1 do begin
  chk=stregex(index[k].object,'(dark|calibration|readout|continous|lamp)',/bool,/fold)
  if chk then begin
   if verbose then message,'Skipping engineering image',/cont
   continue
  endif

  oname=eit->get_name(index[k],ymd=ymd)
  if is_string(oname) then begin
   mk_sub_dir,synop_images,ymd,out_dir=sdir
   zname=oname+'.gz'
   oname=concat_dir(sdir,oname)
   zname=concat_dir(sdir,zname)
   prep=1b 

   if not clobber then begin
    chk1=loc_file(zname,count=zcount)
    chk2=loc_file(oname,count=ocount)
    prep=(zcount eq 0) and (ocount eq 0)
   endif

   if prep then begin
    count=count+1

    if k eq 0 then eit->read,files[i]

;-- normalize to earth orbit

    eit->earth_correct,k

;-- write file

    eit->write,oname,k
   
   endif
  
   chk=loc_file(oname,count=ocount)
   zip=ocount gt 0
   if zip then zip_files=append_arr(zip_files,oname,/no_copy) 

  endif

 endfor
endfor

obj_destroy,eit

;-- gzip new files

if is_string(zip_files) then begin
 chmod,zipfiles,/g_write,/g_read
 if verbose then message,'Compressing files',/cont
 espawn,'gzip -f '+zip_files,/background,/unique
endif

message,'Processed '+trim(count)+' files',/cont


return
end



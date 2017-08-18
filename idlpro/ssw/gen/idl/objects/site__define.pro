;+
; Project     : HESSI
;
; Name        : SITE__DEFINE
;
; Purpose     : Define a site object
;
; Category    : Ancillary GBO Synoptic Objects
;
; Syntax      : IDL> c=obj_new('site')
;
; History     : Written 4 Jan 2000, D. Zarro, SM&A/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

;---------------------------------------------------------------------------
;-- constructor

function site::init,_ref_extra=extra,err=err

err=''
success=self->ftp::init(_extra=extra,err=err)
                     
if success then begin
           
;-- defaults: 
;   times to current day, FITS extensions, caching on.

 tstart=anytim2utc(!stime)
 tstart.time=0
 tend=tstart
 tend.mjd=tend.mjd+1
 ext='.fit,.fts,'
 org='day'
 self->setprop,tstart=tstart,tend=tend,cache=1,ext=ext,org=org,$
      _extra=extra,err=err,/pair,clobber=0

 success=err eq ''

endif

r=self->fid::init() 
dprint,'% SITE::INIT ',success

return,success
end

;--------------------------------------------------------------------------

pro site::cleanup

self->ftp::cleanup
self->fid::cleanup

dprint,'% SITE::CLEANUP'

return & end

;--------------------------------------------------------------------------
;-- directory organization of remote data

function site::valid_org,org

if datatype(org,/tname) ne 'STRING' then return,0b

valid_orgs=['hour','day','month','year']

chk=where(strtrim(strlowcase(org),2) eq valid_orgs,count)

return, (count gt 0)

end

;--------------------------------------------------------------------------
;-- set properties

pro site::setprop,tstart=tstart,tend=tend,ext=ext,ftype=ftype,back=back,err=err,$
              rename=rename,subdir=subdir,topdir=topdir,cadence=cadence,$
              cache=cache,forward=forward,_extra=extra,$
              org=org,events=events,last_time=last_time,full=full,$
              no_order=no_order,gzip=gzip,bytes=bytes,delim=delim,smode=smode

err=''

;-- set FTP object properties

if is_struct(extra) then begin
 self->ftp::setprop,_extra=extra,err=err
 if err ne '' then return
endif

;-- rationalize some control properties

if size(delim,/tname) eq 'STRING' then self.delim=trim(delim)
if exist(full) then self.full=keyword_set(full)

if is_string(topdir) then begin
 if n_elements(topdir) gt 1 then self.topdir=arr2str(trim2(topdir)) else $
  self.topdir=trim2(topdir)
endif

if size(ext,/tname) eq 'STRING' then self.ext=trim2(ext)

if size(ftype,/tname) eq 'STRING' then begin
 if n_elements(ftype) gt 1 then self.ftype=arr2str(trim2(ftype)) else $
  self.ftype=trim2(ftype)
endif

if exist(bytes) then self.bytes=keyword_set(bytes)
if exist(rename) then self.rename=keyword_set(rename)
if exist(cache) then self.cache=keyword_set(cache)
if exist(subdir) then self.subdir=keyword_set(subdir)
if self->valid_org(org) then self.org=trim2(org)
if exist(last_time) then self.last_time=keyword_set(last_time)
if is_number(gzip) then self.gzip=-1 > fix(gzip) < 1
if exist(events) then self.events=keyword_set(events)
if is_number(smode) then self.smode=  0b > byte(smode) < 1b
if is_number(cadence) then  self.cadence =  float(cadence)

;-- rationalize times

terr=''
if exist(tstart) then begin
 t1=anytim2tai(tstart,err=terr)
 if terr ne '' then begin
  err=terr
  message,err,/cont
  return
 endif
 self.tstart=t1
endif

if exist(tend) then begin
 t2=anytim2tai(tend,err=terr)
 if terr ne '' then begin
  err=terr
  message,err,/cont
  return
 endif
 self.tend=t2         
endif    


if exist(back) then begin
 back=nint(back)
 day_sec=86400d
 if back ne 0 then begin  
  tend=self.tstart+day_sec
  tstart=tend-back*day_sec
  self.tstart=tstart
  self.tend=tend
 endif
endif

if exist(forward) then begin
 forward=nint(forward)
 if forward ne 0 then begin
  tstart=self.tstart
  tend=tstart+forward*86400d
  self.tstart=tstart
  self.tend=tend
 endif
endif


if keyword_set(no_order) then return

t1=self.tstart & t2=self.tend
self.tend=t2 > t1
self.tstart= t1 < t2

return & end
   
;---------------------------------------------------------------------------
;-- show properties

pro site::show

self->ftp::show

print,''
print,'SITE properties:'
print,'----------------'
print,'% topdir: ',self.topdir
print,'% ext: ',self.ext
print,'% ftype: ',self.ftype
print,'% rename: ',self.rename
print,'% cache: ',self.cache
print,'% subdir: ',self.subdir
print,'% ldir: ',self->getprop(/ldir)
print,'% org: ',self.org
print,'% gzip: ',self.gzip
print,'% bytes: ',self.bytes
print,'% full: ',self.full
print,'% delim: ',self.delim

if self.tstart gt 0 then print,'% tstart: ',anytim2utc(self.tstart,/vms)
if self.tend gt 0 then print,'% tend:   ',anytim2utc(self.tend,/vms)

return & end
               
 
;---------------------------------------------------------------------------
;-- filter files based on extension

function site::filter_ext,files,count=count,ss=ss

ext=self->getprop(/ext)
return,str_match(files,ext,count=count,found=ss)

end
 
;----------------------------------------------------------------------------
;-- filter files based on event time window (e.g. GOES events)

function site::filter_events,files,times,count=count,ss=ss

count=0 & sfiles='' & ss=-1
if size(files,/tname) ne 'STRING' then return,sfiles

;-- if just one file or no window is set, then just return what we have

count=n_elements(files)
events=self->getprop(/events)
if (not events) or (count eq 1) then return,files

dur=self->getprop(/dur)
ftimes=filt_events(times,dur,count=count)

ss=where(ftimes eq times,count)
if count eq 1 then ss=ss[0]
if count gt 0 then sfiles=files[ss]
return,sfiles & end
               
;----------------------------------------------------------------------------
;-- filter files based on times

function site::filter_time,files,times,count=count,ss=ss

count=0 & sfiles='' & ss=-1
if size(files,/tname) ne 'STRING' then return,sfiles

if not exist(times) then begin
 message,'file times need to be input',/cont
 return,sfiles
endif

;-- select on cadence

cadence=self->getprop(/cadence)
if cadence gt 0 then begin
 scad=cadence*60.
 diff=[scad,times[1:*]-times]
 ss=where( (times le self.tend) and (times ge self.tstart) and $
            abs(diff) ge scad,count)
endif else begin
 ss=where( (times le self.tend) and (times ge self.tstart),count)
endelse

if count eq 1 then ss=ss[0]
if count gt 0 then sfiles=files[ss]
 
return,sfiles & end

;-----------------------------------------------------------------------------
;-- define remote subdirectories

function site::get_sdir,_extra=extra

return,get_fid(self.tstart,self.tend,_extra=extra,delim=self.delim,$
               full=self.full)

end

;------------------------------------------------------------------------------
;-- validate required properties

function site::valid,list=list,err=err

err=''

if not self->ftp::valid(list=list,err=err) then return,0b

if self.tstart le 0 then begin
 err='missing start time'
 message,err,/cont
 return,0b
endif

if self.tend le 0 then begin
 err='missing end time'
 message,err,/cont
 return,0b
endif

dstart=anytim2utc(self.tstart,/ext,err=err)
if (err ne '') then begin
 message,'missing or invalid start time',/cont
 return,0b
endif

dend=anytim2utc(self.tend,/ext,err=err)
if (err ne '') then begin
 message,'missing or invalid end time',/cont
 return,0b
endif

return,1b & end

;---------------------------------------------------------------------------

function site::get_cache_id

rhost=self->getprop(/rhost)
topdir=self->getprop(/topdir)
ftype=self->getprop(/ftype)

return,rhost+'_'+topdir+'_'+ftype
              
end
 
;---------------------------------------------------------------------------
;-- cache for holding site listings

pro site::list_cache,times,data,count=count,set=set

count=0
verb=self->getprop(/verbose)
cache=self->getprop(/cache)

;-- create a LIST_CACHE object for storage

cache_id=self->get_cache_id()

dprint,'% site_id: ',cache_id

list_cache=obj_new('list_cache')
tstart=round_time(self.tstart,org=self.org)
tend=round_time(self.tend,org=self.org,/next)

list_cache->set,name=cache_id,tstart=tstart,tend=tend,/verb

if keyword_set(set) then begin
 list_cache->setdata,times,data
endif else begin
 if cache then list_cache->getdata,times,data,count=count
endelse
        
obj_destroy,list_cache    

return & end

;-----------------------------------------------------------------------------
;-- get file categories

function site::get_cats,files

np=n_elements(files)
if np eq 0 then return,'' else return,strarr(np)

end

;-----------------------------------------------------------------------------
;-- list remote files 

pro site::list,ofiles,times=otimes,sizes=osizes,count=count,cats=ocats,$
                      stimes=ostimes,err=err

count=0 & ofiles='' & osizes='' & otimes=-1.d & ocats='' & ostimes=''
err=''

if not self->valid(/list) then return

;-- check cache for recent listing 

self->list_cache,times,data,count=count

if (count eq 0) then begin

 self->search,ofiles,times=otimes,sizes=osizes,count=count

;-- cache results

 if count gt 0 then begin
  odata={files:ofiles[0],sizes:osizes[0],cats:ocats[0],stimes:ostimes[0]}
  data=replicate(odata,count)
  ocats=self->get_cats(ofiles)
  ostimes=anytim2utc(otimes,/ecs,/trun)
  data.files=ofiles
  data.sizes=osizes
  data.cats=ocats
  data.stimes=ostimes
  self->list_cache,otimes,data,/set
 endif

endif else begin

 otimes=times
 if is_struct(data) then begin 
  ofiles=data.files
  osizes=data.sizes
  ocats=data.cats
  ostimes=data.stimes
 endif else ofiles=data

endelse

;-- filter out extensions 

if count gt 0 then begin
 ofiles=self->filter_ext(ofiles,ss=ss,count=count)
 if count gt 0 then begin
  otimes=otimes[ss]
  osizes=osizes[ss]
  ocats=ocats[ss]
  ostimes=ostimes[ss]
 endif
endif

;-- filter out relevant times

if count gt 0 then begin
 ofiles=self->filter_time(ofiles,otimes,ss=ss,count=count)
 if count gt 0 then begin
  otimes=otimes[ss]
  osizes=osizes[ss]
  ocats=ocats[ss]
  ostimes=ostimes[ss]
 endif
endif

;-- if nothing left then blank out OFILES

if count eq 0 then begin
 ofiles='' & osizes='' & otimes=-1.d & ocats='' & ostimes=''
endif

over=n_params() eq 0
self->output,ofiles,over=over,head='remote file names:'

return & end

;-----------------------------------------------------------------------------
;-- output results

pro site::output,files,over=over,head=head

verbose=self->getprop(/verbose)
if (not verbose) and (not keyword_set(over)) then return

count=0
if size(files,/tname) eq 'STRING' then begin
 if size(head,/tname) eq 'STRING' then message,head,/cont,/noname
 ok=where(trim2(files) ne '',count)
 if count gt 0 then print,transpose(files[ok])
endif

if count eq 0 then message,'no matching files found by '+get_caller(),/cont

return & end
        

;--------------------------------------------------------------------------
;-- set remote subdirectories and patterns to search

pro site::set_rdir

sdirs=self->get_sdir()                            
topdir=self->get_topdir()
ndirs=n_elements(topdir)
ftypes=self->get_ftype()
ntypes=n_elements(ftypes)

if ntypes ne ndirs then begin
 message,'non-matching remote search directories and search patterns',/cont
endif

if is_string(sdirs,bdirs) then begin
 nbdirs=n_elements(bdirs)
 for i=0,ndirs-1 do begin
  rdir=append_arr(rdir,topdir[i]+'/'+bdirs)
  rpatt=append_arr(rpatt,replicate(ftypes[i],nbdirs))
 endfor
endif else begin
 rdir=topdir
 rpatt=ftypes
endelse

self->setprop,rdir=rdir,rpatt=rpatt

return & end


;-------------------------------------------------------------------------
;-- get remote top directories to search

function site::get_topdir
return,trim2(str2arr(self->getprop(/topdir)))
end

;-------------------------------------------------------------------------
;-- get remote file patterns to search

function site::get_ftype

ftype=self->getprop(/ftype)
ftype=trim2(str2arr(ftype))
chk=where( (ftype ne '') and (strpos(ftype,'*') eq -1), count)
if count gt 0 then ftype[chk]=ftype[chk]+'*'
empty=where(ftype eq '',count)
if count gt 0 then ftype[empty]='*'
return,comdim2(ftype)
end

;--------------------------------------------------------------------------
;-- search for remote files

pro site::search,files,_ref_extra=extra

;-- HTTP 

smode= self->getprop(/smode)
if smode then begin
 message,'using HTTP...',/cont
 self->hsearch,files,_extra=extra
 return
endif

;-- FTP 

message,'using FTP...',/cont
self->fsearch,files,_extra=extra

return & end

;---------------------------------------------------------------------------
;-- FTP search for files

pro site::fsearch,files,times=times,sizes=sizes,count=count,err=err

self->set_rdir

list_bytes=self->getprop(/bytes)

if list_bytes then self->dir,files,count=count else self->ls,files,count=count

;-- extract file size and time information 

if count gt 0 then files=self->get_files(files,times,sizes=sizes,err=err,count=count)

return & end

;----------------------------------------------------------------------------
;-- copy from remote SYNOP_DATA (called by copy)

pro site::copy_file,ofiles,count=count,err=err,progress=progress,$
                    cancelled=cancelled

count=0 & ofiles='' & cancelled=0b

rfiles=self->getprop(/rfile)
if not is_string(rfiles,err=err) then begin 
 message,err,/cont
 return
endif

;-- start with keeping remote and local filenames the same

delim=get_delim()
lfiles=str_replace(rfiles,'/',delim)
self->fbreak,lfiles,dir,names
lfiles=names
ldir=self->getprop(/ldir)
ldir=ldir[0]

;-- check if renaming files on the fly

rename=self->getprop(/rename)
if rename then begin
 lfiles=self->rename_files(lfiles,count=count)
 if count eq 0 then begin
  message,'Could not rename local files',/cont
  return
 endif
endif

;-- check if renaming to date-based subdirectory

subdir=self->getprop(/subdir)
if subdir then begin
 otimes=self->parse_time(rfiles,/tai)
 mk_fid,ldir,otimes,out_dir=ldirs
 ldir=ldirs
endif

lfiles=concat_dir(ldir,lfiles)
self->setprop,lfile=lfiles

;-- go get the files. Try HTTP copy first. If that fails, use FTP

count=0
if not subdir then if not test_dir(ldir[0],err=err) then return 

status=1b
smode=self->getprop(/smode)
if smode then begin
 self->hget,ofiles,err=err,count=count,progress=progress,cancelled=cancelled
 if cancelled then return
endif else begin
 message,'using FTP...',/cont
 if keyword_set(progress) then begin
  xtext,'Please wait. Downloading...',wbase=tbase,/just_reg,/center
  if allow_windows() then widget_control,/hour
 endif
 self->mget,ofiles,count=count,err=err,status=status
 xkill,tbase
endelse

;-- gzip them

gzip=self->getprop(/gzip)
if (count gt 0) and status and gzip then ofiles=self->gzip_file(ofiles)

return & end

;----------------------------------------------------------------------------
;-- HTTP copy files (unavailable for site object)

pro site::hget,files,err=err,count=count,_extra=extra

err='HTTP copy currently unavailable'
count=0
files=''

return

end

;----------------------------------------------------------------------------
;-- HTTP search files (unavailable for site object)

pro site::hsearch,files,times=times,err=err,sizes=sizes,count=count,_extra=extra

err='HTTP search currently unavailable'
count=0
files=''
times=0
sizes=''

return

end

;----------------------------------------------------------------------------
;-- gzip files

function site::gzip_file,files,_extra=extra

if is_blank(files) then return,''

gzip=self->getprop(/gzip)
case gzip of
  1: begin
      if self->getprop(/verbose) then message,'gzipping files...',/cont
      gzip,files,gfiles,_extra=extra
     end
 -1: begin
      if self->getprop(/verbose) then message,'unzipping files...',/cont
      gzip,files,gfiles,/unzip,_extra=extra
     end
 else: gfiles=files
endcase

return,gfiles & end

;-----------------------------------------------------------------------------
;-- copy file, returns copied filenames in CFILES

pro site::copy,cfiles,count=count,err=err,_extra=extra
cfiles='' & count=0 & err=''

verbose=self->getprop(/verbose)

if verbose then begin
 message,'copying files for interval -> '+anytim2utc(self.tstart,/vms)+' -- '+anytim2utc(self.tend,/vms),/cont,/noname
endif

;-- first get listing 

self->list,ofiles,count=count

;-- copy if files are found

if count gt 0 then begin
 self->setprop,rfile=ofiles
 self->copy_file,cfiles,count=count,_extra=extra
endif

if count gt 0 then self->output,cfiles,head='copied files (local names):'

return & end

;----------------------------------------------------------------------------
;-- site loc_file method

function site::loc_file,files,_ref_extra=extra

;-- if gzip'ing files, then this method needs to look for files with
;   .gz extension. Hence we replace any compressed names with .Z to .gz, and 
;   append .gz to uncompressed names

if is_blank(files) then return,''

gzip=self->getprop(/gzip)
lfiles=files
case gzip of
 1:  begin
      lfiles=str_replace(files,'.gz','')
      lfiles=str_replace(lfiles,'.Z','')
      lfiles=lfiles+'.gz'
     end
 -1: begin
      lfiles=str_replace(files,'.gz','')
      lfiles=str_replace(lfiles,'.Z','')
     end
else: do_nothing=1
endcase

return,loc_file(lfiles,_extra=extra)

end

;-----------------------------------------------------------------------------
;-- wrapper around parse_time

function site::parse_time,files,_ref_extra=extra

return,parse_time(files,_extra=extra)

end

;-----------------------------------------------------------------------------
;-- get file and sizes from listing

function site::get_files,input,times,sizes=sizes,err=err,ss=ss,count=count,$
                        _extra=extra
err=''
ss=-1 & sizes='' & count=0

;-- extract file times

times=self->parse_time(input,count=count,err=err,ss=ss)

if (err ne '') or (count eq 0) then return,''
if count eq 1 then ss=ss[0]
times=anytim(times[ss],/tai)

;-- extract file names
;-- find last instance of non-blank item

r='( ?[^ ]+)$'         
g=stregex(input[ss],r,/ext,/sub)
names=reform(g[1,*])
if count eq 1 then names=names[0]

;-- check if sizes requested

do_sizes=self->getprop(/bytes)
if not do_sizes then begin
 if count gt 1 then sizes=strarr(count) 
 return,names
endif

;-- figure out byte sizes
;-- find 5th instance of non-blank item

r='([^ ]+ +){5}'      
g=stregex(input[ss],r,/ext,/sub)
sizes=reform(g[1,*])
sizes=str_format(float(sizes)/1.e6,'(g0.3)')+' MB'
if count eq 1 then sizes=sizes[0]

return,names
end
                    
;------------------------------------------------------------------------------
;-- define site object

pro site__define                 

; topdir : top directory on remote site
; ext : filename extension
; ftype: filename type 
; tstart/tend: start/end times to copy
; rename : rename files when copying
; cache: cache listing
; subdir : copy files to named subdir (if renaming)
; org: directory resolution of remote files (hour, day, month, year)
; gzip: set to 1 to gzip files when copying, -1 to unzip
; events: only copy files that overlap event times
; last_time: save last time interval when searching
; bytes: list remote file byte sizes
; delim: delimiter for remote subdirs (e.g.delim='/' for 99/02/01)
; full: use full year name for remote subdirs (e.g. 2002/02/01)
; smode: 1 use HTTP, 0 use FTP
; cadence: time spacing between files in minutes


temp={site,topdir:'',ext:'',ftype:'',tstart:0d,tend:0d,events:0.,$
      rename:0b,cache:1b,subdir:0b,gzip:0,org:'',full:0b, smode:0b,$
      last_time:0b,bytes:0b,delim:'',inherits ftp, cadence:0.,inherits fid}

return & end

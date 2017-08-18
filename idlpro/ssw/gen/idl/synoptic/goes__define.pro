;+
; Project     : HESSI
;
; Name        : GOES__DEFINE
;
; Purpose     : Define a GOES lightcurve object
;
; Category    : synoptic objects
;
; Explanation : Object wrapper around PLOT_GOES
;
; Syntax      : IDL> new=obj_new('goes')
;
; History     : Written 31 Dec 2001, D. Zarro (EITI/GSFC)
;               Modified 24 Sept 2003, Zarro (GSI/GSFC) - fixed timerange plotting bug
;               Modified 21 Oct 2003, Zarro (GSI/GSFC) - fixed timeunits bug
;
; Contact     : dzarro@solar.stanford.edu
;               
;-

;---------------------------------------------------------------------------

function goes::init,_ref_extra=extra

if not self->utplot::init(_extra=extra) then return,0

if is_string(extra) then self -> set,_extra=extra

;-- default to start of current day

self->def_times,tstart,tend

self->set,tstart=tstart,tend=tend

;-- compile special socket versions of PLOT_GOES routines and initialize
;   pointer to hold lightcurves from PLOT_GOES
;   If self.use_utplot is set, then UTPLOT/XYPLOT objects are used for
;   storage

if not self->have_goes_path(_extra=extra) then return,0b

recompile,'sock_goes'
self.gdata=ptr_new(/all)
self.search=1b

return,1

end

;------------------------------------------------------------------------------
;-- flush out old temp files

pro goes::flush,days

if not is_number(days) then return 

old_files=file_since(older=days,patt='g*',count=count,path=goes_temp_dir())

if count gt 0 then file_delete,old_files,/quiet

return & end

;-----------------------------------------------------------------------------
pro goes::cleanup

message,'cleaning up...',/cont
ptr_free,self.gdata

self->flush
self->utplot::cleanup

return
end

;----------------------------------------------------------------------------
;-- trap cases of missing GOES software, databases, or network

function goes::allow_goes,err=err

if not self->have_goes_path(err=err) then return,0b

if self->have_goes_dir() then return,1b

if not allow_sockets(err=err) then return,0b

return,1b

end

;---------------------------------------------------------------------------
;-- default start and end times for GOES plot

pro goes::def_times,tstart,tend

get_utc,tstart
tstart.time=0
tend=tstart
tend.mjd=tend.mjd+1

tstart=anytim2tai(tstart)
tend=anytim2tai(tend)

return & end

;---------------------------------------------------------------------------
;-- check for PLOT_GOES in !path

function goes::have_goes_path,err=err

err=''
if not have_proc('plot_goes') then begin
 ygen=local_name('$SSW/yohkoh/gen/idl')
 if is_dir(ygen) then add_path,ygen,/expand,/quiet,/append
 if not have_proc('plot_goes') then begin
  err='YOHKOH/GEN branch of SSW needs to be installed'
  message,err,/cont
  return,0b
 endif
endif

return,1b

end

;---------------------------------------------------------------------------
;-- check for DIR_GEN directories

function goes::have_goes_dir

return,is_dir('$DIR_GEN_G81')

end

;----------------------------------------------------------------------------
;-- send progress message to user

pro goes::output,wbase=wbase

if self->have_goes_dir() then return

server=goes_server(network=network)
if network then $
 output='Please wait. Searching archives...' else $
  output='Network connection currently unavailable. Checking cached lightcurves...'


if allow_windows() and self->get(/verbose) then begin
 xtext,output,/just,wbase=wbase 
 widget_control,/hour
 wait,1 
endif

message,output,/cont

return & end

;---------------------------------------------------------------------------

function goes::getdata,_ref_extra=extra

return,self->get(_extra=extra,/data)

end

;---------------------------------------------------------------------------

function goes::get,_extra=extra,data=data,low=low,high=high,$
                   times=times,utbase=utbase,no_copy=no_copy,tai=tai,secs79=secs79

ktime=keyword_set(times) or arg_present(times)
kbase=keyword_set(utbase) or arg_present(utbase)
klow=keyword_set(low) or arg_present(low)
khigh=keyword_set(high) or arg_present(high)
kdata=keyword_set(data) or arg_present(data)

ktime2=keyword_set(times) and (not arg_present(times))
kbase2=keyword_set(utbase) and (not arg_present(utbase))
klow2=keyword_set(low) and (not arg_present(low))
khigh2=keyword_set(high) and (not arg_present(high))
kdata2=keyword_set(data) and (not arg_present(data))

utbase=self->getprop(/utbase)

if ktime or kdata or khigh or klow  then begin

 if not ptr_exist(self.gdata) then begin
  message,'No GOES data yet read in',/cont
  return,''
 endif

 if ktime then begin
  times=self->getprop(/xdata)
  tbase=self->getprop(/utbase)
  if keyword_set(tai) then times=temporary(times)+anytim(tbase,/tai)
  if keyword_set(secs79) then times=temporary(times)+anytim(tbase)
 endif

 if kdata or khigh or klow then begin
  if keyword_set(no_copy) then data=temporary(*self.gdata) else data=*self.gdata
  if klow or kdata then low=data.lo
  if khigh or kdata then high=data.hi
  if kdata then data=[[data.lo],[data.hi]]
 endif

endif

if ktime2 then return,times
if kbase2 then return,utbase
if klow2 then return,low
if khigh2 then return,high
if kdata2 then return,data

if is_struct(extra) then return,self->utplot::get(_extra=extra)

return,''

end

;---------------------------------------------------------------------------
;-- GOES set method

pro goes::set,tstart=tstart,tend=tend,search=search,use_utplot=use_utplot,$
              mode=mode,sat=sat,verbose=verbose,_extra=extra,$
              lstart=lstart,lend=lend,tchange=tchange

tchange=1b
if is_number(verbose) then self.verbose =  0b > verbose < 1b
if is_number(search) then self.search =  0b > search < 1b
if is_number(use_utplot) then self.use_utplot= 0b > use_utplot < 1b 

if valid_time(lstart) then self.lstart=anytim2tai(lstart)
if valid_time(lend) then self.lend=anytim2tai(lend)

;-- user can select GOES satellite by number (e.g. sat = 12) , name (sat = 'GOES12'),
;   or keyword (/goes12)

update=self->getprop(/update)

case 1 of
 have_tag(extra,'goe',/start,index): begin
  gsat=self->get_sat(extra)
  if gsat gt -1 then new_sat=gsat else message,'No such satellite - '+(tag_names(extra))[index],/cont
 end
 is_string(sat): begin
  chk=where(strup(sat) eq goes_sat(),count)
  if count gt 0 then new_sat=chk[0] else message,'No such satellite - '+trim(sat),/cont
 end
 is_number(sat): begin
  chk=where(sat eq goes_sat(/num),count)
  if count gt 0 then new_sat=chk[0] else message,'No such satellite - '+trim(sat),/cont
 end
 else: do_nothing=1
endcase

if is_number(new_sat) then begin
 old_sat=self->getprop(/sat)
 if not update and (old_sat ne new_sat) then self.update=1b
 self.sat=new_sat
endif

;-- choose time resolution mode either by number (e.g. mode=1) or keyword (e.g. /three)

if is_number(mode) then begin
 new_mode=0 > mode < 2
 old_mode=self->getprop(/mode)
 if not update and (old_mode ne new_mode) then self.update=1b
 self.mode=new_mode
endif else begin
 new_mode=self->get_mode(extra)
 old_mode=self->getprop(/mode)
 if not update and (old_mode ne new_mode) then self.update=1b
 self.mode=new_mode
endelse

if is_struct(extra) then self->utplot::set,_extra=extra

;-- read new data if desired times fall outside current data times (or there
;   is no data)

self->def_times,dstart,dend
if self->have_gdata() then self->last_times,dstart,dend

cstart=self->getprop(/tstart)
cend=self->getprop(/tend)

if valid_time(cstart) then dstart=anytim2tai(cstart)
if valid_time(cend) then dend=anytim2tai(cend)

if valid_time(tstart) then begin
 dstart=anytim2tai(tstart)
 if not valid_time(tend) then dend=dstart+24.*3600.d0
endif
if valid_time(tend) then dend=anytim2tai(tend) 


nstart=min([dstart,dend],max=nend)
dprint,'% GOES: requested times: '+anytim2utc(nstart,/vms)+' - '+anytim2utc(nend,/vms)

if self->have_gdata() then begin
 self->last_times,lstart,lend
 dprint,'% GOES: last read times: '+anytim2utc(lstart,/vms)+' - '+anytim2utc(lend,/vms)
 within= (nstart ge lstart) and (nstart le lend) and (nend ge lstart) and (nend le lend)
 tchange=(nstart ne lstart) or (nend ne lend)
 if (not update) and (not within) then self.update=1b
endif else self.update=1b

self.tstart=nstart
self.tend=nend

return
end

;------------------------------------------------------------------------
;-- remove duplicate keywords

pro goes::fix_keywords,extra

if is_struct(extra) then begin
 tags=tag_names(extra)
 chk=where(stregex(tags,'goe|one|thr|fiv',/fold) ne -1,count)
 if count gt 0 then extra=rem_tag(extra,chk)
 if not is_struct(extra) then delvarx,extra
endif

sat=self->getprop(/sat)
mode=self->getprop(/mode)
goes_res=['three_sec','one_min','five_min']
extra=add_tag(extra,1,goes_sat(sat))
extra=add_tag(extra,1,goes_res[mode])

return & end

;---------------------------------------------------------------------------
;-- GOES plot method

pro goes::plot,tstart,tend,err_msg=err,_ref_extra=extra,$
              timerange=timerange,use_utplot=use_utplot

err='' 
if not self->allow_goes(err=err) then return

if is_number(use_utplot) then do_utplot= 0b > use_utplot < 1b else $
 do_utplot=self->getprop(/use_utplot)

if valid_range(timerange,/allow,/time,zeros=zeros) then begin
 dprint,'% GOES: using TIMERANGE keyword',timerange
 if zeros then begin
  self->last_times,lstart,lend
  if valid_time(lstart) and valid_time(lend) then begin
   timerange=anytim2utc([lstart,lend],/ecs)
  endif
 endif
endif else begin
 self->read,tstart,tend,_extra=extra,err=err
 if is_string(err) then return
 if valid_time(tstart) and valid_time(tend) then timerange=anytim2utc([tstart,tend],/ecs)
endelse

;-- use UTPLOT plot method if use_utplot is set, else use PLOT_GOES

if do_utplot then begin
 self->utplot::plot,_extra=extra,err=err,timerange=timerange
endif else begin
 self->plot_goes,_extra=extra,err=err,timerange=timerange
endelse

return & end

;---------------------------------------------------------------------------
;-- GOES plot_goes method

pro goes::plot_goes,err=err,_extra=extra

;-- object wrapper around PLOT_GOES

err=''

if not self->have_gdata() then begin
 err='No lightcurve data found. Try using different time range'
 message,err,/cont
 return
endif

title=self->title()
t1=anytim2utc(self->getprop(/tstart),/ext)
t2=anytim2utc(self->getprop(/tend),/ext)
plot_goes,*self.gdata,timerange=[t1,t2],_extra=extra,title=title,/nodef
wshow2

return & end

;---------------------------------------------------------------------------
;-- GOES read method

pro goes::read,tstart,tend,err=err,file_id=file_id,_ref_extra=extra,$
              force=force,select=select

err=''
if not self->allow_goes(err=err) then return

;-- skip reading if time range or plot mode or GOES satellite didn't change

self->set,tstart=tstart,tend=tend,_extra=extra,tchange=tchange

have_data=self->have_gdata(count=count)

update=self->getprop(/update) or keyword_set(force)

dstart=anytim2utc(self->getprop(/tstart),/vms)
dend=anytim2utc(self->getprop(/tend),/vms)
select=keyword_set(select)

;-- if only time changed, the select subregion if within last time range

if have_data and (not update) then begin
 dprint,'% GOES: GOES time range, satellite, and resolution unchanged. Reading skipped.'
 if (not tchange) or (not select) then return
 gdata=self->get(times=times,/gdata,/secs79) 
 chk=where( (times le anytim(dend)) and (times ge anytim(dstart)),count)
 if count lt 2 then begin
  err='No lightcurve data during specified times' 
  message,err,/cont
  return
 endif
 if (count gt 0) then begin
  times=times[chk]
  gdata=gdata[chk]
 endif
 data=[[gdata.lo],[gdata.hi]]
endif else begin
 dprint,'% GOES::READ: reading data...'
 self->fix_keywords,extra
 search=self->getprop(/search)
 self->output,wbase=wbase
 rd_goes,times,data,trange=[dstart,dend],_extra=extra,err=err,$
         type=type,gdata=gdata,gsat=sat,no_search=1b-search,/verb
 count=n_elements(times)
 if count lt 2 then begin
  err='No lightcurve data during specified times'
  message,err,/cont
  no_remote=is_blank(chklog('GOES_REMOTE'))
  if no_remote then begin
   mklog,'GOES_REMOTE','1'
   message,'Checking remote GOES archives...',/cont
   self->read,tstart,tend,err=err,file_id=file_id,_extra=extra,$
              force=force,select=select
  endif
  return
 endif
 xkill,wbase
endelse

*self.gdata=temporary(gdata)
self->set,sat=sat,lstart=dstart,lend=dend,id=type
self.update=0b

;-- store in UTPLOT object 

mode=self->getprop(/mode)
sat=self->getprop(/sat)
use_utplot=self->getprop(/use_utplot)
data_unit='Flux (watts m-2)'
freq_unit='Wavelength (Ang)'
dim1_ids=['1.0-8.0 A','0.5 - 4.0 A']
file_id=self->mk_file_id(dstart,dend,mode,sat)

tmin=times[0]
times=temporary(times)-tmin
utbase=anytim(tmin,/vms)
self->set,xdata=times,ydata=data,utbase=utbase,$
       /ylog,/all,dim1_ids=dim1_ids,freq_unit=freq_unit,$
       data_unit=data_unit,/no_copy,filename=file_id,/dim1_sel

return & end

;--------------------------------------------------------------------------

function goes::title

res=['3 sec','1 min','5 min']
return,goes_sat(self.sat)+' '+res[self.mode]

end

;--------------------------------------------------------------------------
;-- make unique identifier

function goes::mk_file_id,tstart,tend,mode,sat

t1=trim(anytim2utc(tstart,/vms))
t2=trim(anytim2utc(tend,/vms))

file_id=self->title()+' : '+trim(t1)+' -> '+trim(t2)

return,file_id
end

;----------------------------------------------------------------------------
;-- extract GOES mode from keyword extra

function goes::get_mode,extra

modes=['thr','one','fiv']
nmodes=n_elements(modes)
if is_struct(extra) then begin
 for i=0,nmodes-1 do if have_tag(extra,modes[i],/start) then return,i
endif

if is_string(extra) then begin
 for i=0,nmodes-1 do begin
  textra=strup(extra)
  chk=where(strpos(extra,modes[i]) eq 0,count)
  if count gt 0 then return,i
 endfor
endif

return,self.mode

end

;----------------------------------------------------------------------------
;-- extract GOES satellite from keyword extra

function goes::get_sat,extra

if have_tag(extra,'goe',/start,index) then begin
 gsat=(tag_names(extra))[index]
 sat=stregex(gsat,'[0-9]+',/sub,/extra)
 if is_number(sat) then begin
  chk=where(fix(sat) eq goes_sat(/num),count)
  if count gt 0 then return,chk[0]
 endif
endif

return,-1

end

;---------------------------------------------------------------------------
;-- show properties

pro goes::help

if (not valid_time(self.tstart)) or (not valid_time(self.tend)) or $
   (not ptr_exist(self.gdata)) then begin
 message,'No GOES data read',/cont

endif else begin
 print,'% TSTART: ',anytim2utc(self.tstart,/vms)
 print,'% TEND:   ',anytim2utc(self.tend,/vms)
endelse

print,'% MODE: ',self.mode
print,'% DATA TYPE:  ',self->title()
print,'% USE_UTPLOT: ',self.use_utplot
print,'% UPDATE: ',self.update

return & end

;------------------------------------------------------------------------------
;-- get GOES data times

pro goes::times,t1,t2

t1=0.
t2=0.
if not self->have_gdata() then return
np=n_elements(*self.gdata)
t1=anytim((*self.gdata)[0],/tai)
t2=anytim((*self.gdata)[np-1],/tai)

return & end

;------------------------------------------------------------------------------
;-- get last read GOES times

pro goes::last_times,t1,t2

t1=self->getprop(/lstart)
t2=self->getprop(/lend)

return & end

;------------------------------------------------------------------------------
;-- have GOES data?

function goes::have_gdata,count=count

count=0l
chk=ptr_exist(self.gdata)

if chk then count=n_elements(*self.gdata)

return,chk

end

;---------------------------------------------------------------------------
;-- define GOES object

pro goes__define

; mode = 0,1,2 = 3 sec, 1 min, 5 min

goes_struct={goes,use_utplot:0b,tstart:0.d0,tend: 0.d0,sat:0,search:0b,$
              gdata:ptr_new(),lstart:0.d, lend:0.d, mode:0,update:0b,verbose:0b,inherits utplot}

return & end



;+
; Project     : HESSI
;
; Name        : SHOW_SYNOP__DEFINE
;
; Purpose     : widget interface to Synoptic data archive
;
; Category    : HESSI, Synoptic, Database, widgets, objects
;
; Syntax      : IDL> obj=obj_new('show_synop',group=group)
;
; Keywords    : GROUP = widget ID of any calling widget
;               PTR = pointer to last displayed object
;
; History     : 12-May-2000,  D.M. Zarro (SM&A/GSFC)  Written
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

;----------------------------------------------------------------------------
;-- event handler for GOES lightcurve plots

pro plot_goes_eh,event

 widget_control,event.top,get_uvalue=self
 if not self->check_times() then return
 info=self->get_info()

;-- create GOES lightcurve object

 if not obj_valid(!show_synop.goes_obj) then begin
  !show_synop.goes_obj=obj_new('goes_ltc',err=err)
  if is_string(err) then begin
   xack,err & return
  endif
 endif

;-- get GOES times and mode

 mode=widget_info(info.goes_mode_drop,/droplist_select)
 sat=widget_info(info.goes_sat_drop,/droplist_select)
 tstart=self->getprop(/tstart)
 tend=self->getprop(/tend)

;-- read GOES data

 widget_control,/hour
 !show_synop.goes_obj->read,tstart,tend,mode=mode,sat=goes_sat(sat),err=err,/select
 if is_string(err) then begin
  xack,err & return
 endif

 sat=!show_synop.goes_obj->getprop(/sat)
 widget_control,info.goes_sat_drop,set_droplist_select=sat

;-- check if PLOTMAN is being used. If so, we disable channel selection.

 use_plotman=self->getprop(/plotman)
 dim1_sel=(1-use_plotman)
 use_utplot=use_plotman
 !show_synop.goes_obj->set,use_utplot=use_utplot,dim1_sel=dim1_sel
 self->plot_data,!show_synop.goes_obj

 self->setprop,info=info

 return & end

;----------------------------------------------------------------------------

function show_synop::init,verbose=verbose,$
  plotman_obj=plotman_obj,reset=reset,_extra=extra

;-- defaults

if not have_widgets() then begin
 message,'widgets unavailable',/cont
 return,0
endif

;-- only single copy allowed

self->setprop,verbose=verbose

if keyword_set(reset) then xkill,'show_synop::setup'
if (xregistered('show_synop::setup') ne 0) then return,0

ret=self->synop::init()
if not ret then return,ret

;-- config file for saving state

if is_dir('$HOME',out=home) then save_dir=home else save_dir=get_temp_dir()

config_file=concat_dir(save_dir,'.show_synop_config')

;-- create !SHOW_SYNOP  for caching

defsysv,'!show_synop',exists=defined
if not defined then begin
 temp={searched:0b,fifo:obj_new('fifo'),plotman_obj:obj_new(),goes_obj:obj_new()}
 defsysv,'!show_synop',temp
endif else obj_compile,'fifo'

;-- reset state

if keyword_set(reset) then begin
 free_var,!show_synop
 !show_synop.searched=0b
 heap_gc
 file_delete,config_file,/quiet
endif

;-- create INFO state structure

mk_dfont,lfont=lfont,bfont=bfont

;--- if plotman object is passed on the command line, then send plots
;    to it, else use valid plotman object saved from last time
;    show_synop was called standalone


if obj_valid(plotman_obj) then begin
 if obj_valid(!show_synop.plotman_obj) then begin
  if plotman_obj ne !show_synop.plotman_obj then obj_destroy,!show_synop.plotman_obj
 endif
 !show_synop.plotman_obj=plotman_obj
endif

self.ptr=ptr_new(/all)

nmodes=3
info={tstart:0l,tend:0l,main:0l,lfont:lfont,bfont:bfont,brow:0l,srow:0l,$
      site_drop:0l,wave_drop:0l,type_drop:0l,$
      goes_mode_drop:0l,goes_sat_drop:0l,mult_sel:'',$
      slist:0l,flist:0l,cur_sel:'',cur_fsel:'',last_fsel:'',$
      site_base:0l,wave_base:0l,type_base:0l,sbuttons:[0l,0l,0l],$
      config_file:config_file,blabel:0l,mbuttons:replicate(0l,nmodes),$
      tbase:0l,hbase:0l}

self->setprop,info=info,smode=1

;-- setup widget interface

self->setup,_extra=extra

return,1

end



;-----------------------------------------------------------------------------
;-- validate user time inputs

function show_synop::check_times

 err1='' & err2='' & err3=''

 info=self->get_info()
 cur_tstart=anytim2utc(self->getprop(/tstart),/vms)

 widget_control,info.tstart,get_value=tstart
 if not valid_time(tstart,err1) then begin
  xack,err1,group=info.main
  widget_control,info.tstart,set_value=cur_tstart
 endif else begin
  tstart=anytim2utc(tstart,/vms)
  widget_control,info.tstart,set_value=tstart
  self->setprop,tstart=tstart,/no_order
 endelse

 cur_tend=anytim2utc(self->getprop(/tend),/vms)
 widget_control,info.tend,get_value=tend
 if not valid_time(tend,err2) then begin
  xack,err2,group=info.main
  widget_control,info.tend,set_value=cur_tend
 endif else begin
  tend=anytim2utc(tend,/vms)
  self->setprop,tend=tend,/no_order
  widget_control,info.tend,set_value=tend
 endelse

 if (err1 eq '') and (err2 eq '')  then begin
  if anytim2tai(tend) le anytim2tai(tstart) then begin
   err3='End time should be greater than Start time'
   xack,err3,group=info.main
  endif
 endif

 return,(err1 eq '') and (err2 eq '') and (err3 eq '')
 end

;----------------------------------------------------------------------------

pro show_synop::setup,timerange=timerange,_extra=extra

;-- create widgets

self->create_widgets,_extra=extra

info=self->get_info()

self->setprop,copy=1

;-- reconcile times

secs_per_day=24l*3600l
week=7l*secs_per_day

get_utc,tstart & tstart.time=0
tstart=(utc2tai(tstart)-3.*secs_per_day) > utc2tai(anytim2utc('2-dec-95'))
tend=anytim2utc(!stime)
tend.time=0
tend.mjd=tend.mjd+1
tend=utc2tai(tend)
tstart=anytim2utc(tstart,/vms)
tend=anytim2utc(tend,/vms)

;-- restore settings from last saved object
;   (catch any errors in case saved object has a problem)

reset=0
error=0
catch,error
if error ne 0 then  reset=1

if reset then file_delete,info.config_file,/quiet

widget_control,/hour
chk=loc_file(info.config_file,count=count)
verbose=self.verbose
if count gt 0 then begin
 restore,file=chk[0]
 if is_struct(props) then begin
  if have_tag(props,'last_count') and have_tag(props,'save_time') then begin
   now=anytim2tai(!stime)
   day_secs=24.d*3600.d
   if (now - props.save_time) le day_secs then begin
    message,'restoring configuration from - '+info.config_file,/cont
    self->setprop,_extra=props
   endif
  endif
 endif
 self->setprop,verbose=verbose
endif

catch,/cancel

;-- ensure that remote listing returns file sizes

self->setprop,/pair,/bytes

;-- use last saved times

if have_tag(props,'last_time') then begin
 if props.last_time then begin
  tstart=props.tstart
  tend=props.tend
 endif
endif

;-- override with command-line times

if n_elements(timerange) eq 2 then begin
 chk=valid_time(timerange)
 if (min(chk) ne 0) then begin
  tstart=timerange[0] & tend=timerange[1]
 endif
endif

widget_control,info.tstart,set_value=anytim2utc(tstart,/vms)
widget_control,info.tend,set_value=anytim2utc(tend,/vms)
self->setprop,tstart=tstart,tend=tend

;-- update GOES modes

goes_mode=0
if have_tag(props,'goes_mode') then goes_mode=0 > props.goes_mode < 1
widget_control,info.goes_mode_drop,set_droplist_select=goes_mode

goes_gsat=0
nsat=n_elements(goes_sat())
if have_tag(props,'goes_gsat') then goes_gsat=0 > props.goes_gsat < (nsat-1)
widget_control,info.goes_sat_drop,set_droplist_select=goes_gsat

;-- last file selection

if have_tag(props,'cur_fsel') then begin
 info.last_fsel=props.cur_fsel
 info.cur_fsel=props.cur_fsel
 self->setprop,info=info
endif

;-- set working directory

if self->getprop(/current) then self->setprop,ldir=curdir()

;-- update sort mode

sort_mode=self->getprop(/sort_mode) < 2
widget_control,info.sbuttons[sort_mode],/set_button

;-- update search mode type

search_mode=self->getprop(/mode)
widget_control,info.mbuttons[search_mode],/set_button
widget_control,info.site_base,map=search_mode eq 0
widget_control,info.type_base,map=search_mode eq 1
widget_control,info.wave_base,map=search_mode eq 2

;-- update site name

curr_site=self->getprop(/site)
names=self->list_synop_names(abbr)
chk=str_match(abbr,curr_site,count=count,found=found)
if count gt 0 then widget_control,info.site_drop,set_droplist_select=found[0]

;-- update wave type

curr_wave=self->getprop(/wave)
waves=self->list_synop_waves(abbr)
chk=str_match(abbr,curr_wave,count=count,found=found)
if count gt 0 then widget_control,info.wave_drop,set_droplist_select=found[0]

;-- update data type

curr_type=self->getprop(/types)
type=self->list_synop_dirs()
chk=str_match(type,curr_type,count=count,found=found)
if count gt 0 then widget_control,info.type_drop,set_droplist_select=found[0]

self->flist

;-- don't produce initial listing on startup

if (!show_synop.searched eq 0b) or (self.last_count eq 0) then begin
 search_mess='*** Press "Search" to list latest data files ***'
 widget_control,info.slist,set_value=search_mess
 widget_control,info.slist,set_uvalue=''
endif else self->slist,/quiet

;-- start  XMANAGER

xmanager,'show_synop::setup',info.main,/no_block,event='show_synop_event',$
 cleanup='show_synop_cleanup'

return & end

;-----------------------------------------------------------------------------
;-- create widgets

pro show_synop::create_widgets,group=group,modal=modal,no_plot=no_plot

info=self->get_info()

;-- load fonts

lfont=info.lfont
bfont=info.bfont

modal=keyword_set(modal)
info.main = widget_mbase(title = 'SHOW_SYNOP',group=group,$
                   modal=modal,/column,uvalue=self)

;-- setup timer

child=widget_base(info.main,map=0)
widget_control,child,set_uvalue='timer'

;-- operation buttons

curr=anytim2utc(!stime,/vms,/date)

widget_control,info.main, tlb_set_title ='SHOW SYNOP: '+curr


row1=widget_base(info.main,/row,/frame)
exitb=widget_button(row1,value='Done',uvalue='exit',font=bfont)
relistb=widget_button(row1,value='Relist',uvalue='reload',font=bfont)
conb=widget_button(row1,value='Configure',font=bfont,uvalue='config')

;-- date/time fields

row2=widget_base(info.main,/column,/frame)

trow=widget_base(row2,/row)
info.tstart=cw_field(trow,title= 'Start Time:  ',value=' ',xsize = 20,font=lfont)

info.tend=cw_field(trow,title='Stop Time:   ', value=' ',xsize = 20,font=lfont)

srow=widget_base(row2,/row)

left=widget_base(srow,/row)

xmenu2,['Site','Type','Subtype'],left,/row,/exclusive,font=lfont,/no_rel,$
      buttons=mbuttons,uvalue=['by_site','by_type','by_subtype'],$
      title='Search by: ',$
      lfont=lfont
info.mbuttons=mbuttons


right=widget_base(srow)

names=self->list_synop_names(abbr)
info.site_base=widget_base(right,/row,map=0)
info.site_drop=widget_droplist(info.site_base,value=names,font=bfont,uvalue=abbr,/dynamic)

info.type_base=widget_base(right,/row,map=0)
types=self->list_synop_dirs()
info.type_drop=widget_droplist(info.type_base,value=types,font=bfont,uvalue=types)

info.wave_base=widget_base(right,/row,map=0)
waves=self->list_synop_waves(abbr)
info.wave_drop=widget_droplist(info.wave_base,value=waves,font=bfont,uvalue=abbr,/dynamic)

;-- search button

junk=widget_base(row2,/row)

left=widget_base(junk,/row)

slabel1=widget_label(left,font=lfont,value='Press: ')
searchb=widget_button(left,value='Search',uvalue='search',font=bfont)
slabel2=widget_label(left,font=lfont,value=' to list data files')

right=widget_base(junk,/row)

goes_opts=[' 3 sec ',' 1 min ']

blank=widget_label(right,font=lfont,value=' ')
plotg=widget_button(right,value='Plot',font=bfont,event_pro='plot_goes_eh')
info.goes_mode_drop=widget_droplist(right,value=goes_opts,font=lfont,/dynamic)
info.goes_sat_drop=widget_droplist(right,value=goes_sat(),font=lfont,/dynamic)
slabel2=widget_label(right,font=lfont,value=' lightcurves')

;--  files list

row3=widget_base(info.main,/column,/frame)
blabel=widget_label(row3,value='Data Archive Search Results',font=lfont)


;-- sort buttons

xmenu2,['Filename','Decreasing Date','Increasing Date'],row3,/row,/exclusive,font=lfont,/no_rel,$
      buttons=sbuttons,uvalue=['by_file','by_date_d','by_date_i'],$
      title='Sort by: ',lfont=lfont

info.sbuttons=sbuttons
info.srow=widget_base(row3,/row)
dlabel1=widget_label(info.srow,font=lfont,value='Press: ')
downb=widget_button(info.srow,value='Download',font=bfont,uvalue='download')
dlabel2=widget_label(info.srow,font=lfont,value=' to copy selected file(s), and ')
detb=widget_button(info.srow,value='Details',font=bfont,uvalue='details')
dlabel3=widget_label(info.srow,font=lfont,value=' to display file information')


slabel='FILENAME                                    DATE_OBS              TYPE                 SIZE'
label=widget_list(row3,value=slabel,ysize=1,/frame,font='fixed')
info.slist=widget_list(row3,value='   ',ysize=8,font='fixed',/multiple)

;-- downloaded files list

row4=widget_base(info.main,/column,/frame)

ldir=self->getprop(/ldir)
info.blabel= widget_label(/dynamic,/align_left,row4,value='Currently Downloaded Files in: '+ldir,font=lfont)


info.brow=widget_base(row4,/row)
headb=widget_button(info.brow,value='View Header',font=bfont,uvalue='head')

plotb=widget_button(info.brow,value='Plot',font=bfont,uvalue='plot')

remb=widget_button(info.brow,value='Delete',font=bfont,uvalue='delete')


info.flist=widget_list(row4,value=' ',ysize=8,xsize=70,font='fixed',/multiple)

;-- realize widgets and start timer

widget_control,info.main,/realize
widget_control,child,timer=1.

self->setprop,info=info

return & end

;-----------------------------------------------------------------------------
;-- set INFO structure

pro show_synop::setprop,info=info,sort_mode=sort_mode,err=err,$
                        save_time=save_time,_extra=extra

err=''
if is_struct(extra) then self->synop::setprop,_extra=extra,err=err

if is_number(sort_mode) then self.sort_mode =  0 > sort_mode < 2

if valid_time(save_time) then self.save_time=anytim2tai(save_time)

if is_struct(info) then *self.ptr=info

return & end

;-----------------------------------------------------------------------------
;-- return widget state INFO

function show_synop::get_info

if not ptr_valid(self.ptr) then return,-1
if not exist(*self.ptr) then return,-1
return,*self.ptr
end

;----------------------------------------------------------------------------
;-- object event handler. Since actual methods cannot be event-handlers,
;   we shunt events thru this wrapper

pro show_synop_event,event
widget_control,event.top,get_uvalue=object
if obj_valid(object) then object->event,event
return & end

;-----------------------------------------------------------------------------
;-- main event handler

 pro show_synop::event,event

;-- retrieve object reference from uvalue of main widget

 widget_control,event.top,get_uvalue=self
 widget_control,event.id, get_uvalue=uvalue
 if not exist(uvalue) then uvalue=''
 bname=trim(uvalue[0])
 info=self->get_info()

;-- timer

 if bname eq 'timer' then begin
  child=widget_info(event.top,/child)
  widget_control,child,timer=1.

  widget_control,info.slist,get_uvalue=hold
  sens=(info.cur_sel[0] ne '') and (hold[0] ne '')
  widget_control,info.srow,sensitive=sens

  sens=(info.cur_fsel ne '')
  widget_control,info.brow,sens=sens

  if not obj_valid(!show_synop.fifo) then !show_synop.fifo=obj_new('fifo')
  return
 endif

;-- quit here

 if bname eq 'exit' then begin
  show_synop_cleanup,event.top
  xkill,event.top
  return
 endif

;-- sort mode

 chk=where(strlowcase(bname) eq ['by_file','by_date_d','by_date_i'],count)
 if count gt 0 then begin
  sort_mode=chk[0]
  self->setprop,sort_mode=sort_mode
  bname='search'
 endif

;-- search mode

 chk=where(strlowcase(bname) eq ['by_site','by_type','by_subtype'],count)

;-- restore last drop-list selections

 if count gt 0 then begin
  mode=chk[0]
  self->setprop,mode=mode
  widget_control,info.site_base,map=mode eq 0
  widget_control,info.type_base,map=mode eq 1
  widget_control,info.wave_base,map=mode eq 2
  self->setprop,info=info
 endif

;-- check time inputs and search pattern

 if (bname eq 'search') or (bname eq 'reload') then begin

;-- validate times

  if not self->check_times() then return

;-- drop list selections

  mode=self->getprop(/mode)

;-- search by site

  if (mode eq 0) then begin
   widget_control,info.site_drop,get_uvalue=duvalue
   drop_index=widget_info(info.site_drop,/droplist_select)
   new_site=trim(duvalue[drop_index])
   self->setprop,site=new_site
  endif

;-- search by type

  if (mode eq 1) then begin
   widget_control,info.type_drop,get_uvalue=duvalue
   type_index=widget_info(info.type_drop,/droplist_select)
   types=duvalue[type_index]
   self->setprop,types=types
  endif

;-- search by wave (or subtype)

  if (mode eq 2) then begin
   widget_control,info.wave_drop,get_uvalue=duvalue
   drop_index=widget_info(info.wave_drop,/droplist_select)
   new_wave=trim(duvalue[drop_index])
   self->setprop,wave=new_wave
  endif

  self->slist,reload=bname eq 'reload'
  !show_synop.searched=1b
  self->setprop,info=info
  return
 endif

;-- configure

 if bname eq 'config' then begin
  widget_control,/hour
  old_dir=self->getprop(/ldir)
  self->config,group=event.top,/no_types
  new_dir=self->getprop(/ldir)
  if (old_dir ne new_dir) then begin
   info.cur_fsel=''
   info.last_fsel=''
   self->setprop,info=info
   self->flist
  endif
 endif

;-- top list selection event

 if event.id eq info.slist then begin
  new_sel=widget_selected(info.slist)
  info=rep_tag_value(info,new_sel,'cur_sel')
  self->setprop,info=info

;-- highlight first file in download list

  self->fbreak,new_sel[0],sdir,sname
  widget_control,info.flist,get_uvalue=files
  self->fbreak,files,cdir,cname
  sel=where(sname eq cname,scount)
  if scount gt 0 then begin
   widget_control,info.flist,set_list_select=sel[0]
   info.cur_fsel=files[sel[0]]
   self->setprop,info=info
  endif
 endif

;-- bottom list selection event

 if event.id eq info.flist then begin
  new_sel=widget_selected(info.flist)
  info=rep_tag_value(info,new_sel,'mult_sel')
  info.cur_fsel=uvalue[event.index]
  self->setprop,info=info
 endif

;-- show selected file details

 if xalive(info.tbase) and (event.id eq info.slist) then begin
  if have_tag(event,'clicks') then begin
   if (event.clicks eq 1) then bname='details'
  endif
 endif

 if bname eq 'details' then self->file_info,info.cur_sel[0]

;-- download selected file

 if (event.id eq info.slist) then begin
  if have_tag(event,'clicks') then begin
   if (event.clicks eq 2) then bname='download'
  endif
 endif

 if bname eq 'download' then begin
  if is_blank(info.cur_sel[0]) then return
  widget_control,/hour
;  widget_control,info.main,sensitive=0
  lfile=self->fetch(info.cur_sel,err=err)
  print,lfile
  clobber=self->getprop(/clobber)
  if clobber then !show_synop.fifo->delete,lfile
;  widget_control,info.main,sensitive=1
  xshow,info.main
  if err eq '' then begin
   sfiles=info.cur_sel
   self->fbreak,sfiles,sdir,sname
   info.cur_fsel=sname[0]
   info.last_fsel=sname[0]
   self->setprop,info=info
   self->flist
  endif else xack,err,group=info.main
 endif

;-- delete from download list

 if bname eq 'delete' then begin
  have_files=is_string(info.mult_sel,cfiles)
  if have_files then begin
   for i=0,n_elements(cfiles)-1 do begin
    dfile=cfiles[i]
    self->fbreak,dfile,fdir,fname
    chk=xanswer('Delete '+fname+' from local directory?',$
                message_supp='Do not request confirmation for future deletes',$
                /suppre,/check,instruct='Delete ? ',space=1)

;-- delete local file, compressed copy, and cached copy

    if chk then begin
     file_delete,dfile,/quiet
     dprint,'..deleting '+dfile
;     compressed=find_compressed(dfile,/look)
;     file_delete,compressed,/quiet
     !show_synop.fifo->delete,dfile
    endif
   endfor
   info.cur_fsel=''
   info=rep_tag_value(info,'','mult_sel')
   self->setprop,info=info
   self->flist
  endif
 endif

 if (event.id eq info.flist) and have_tag(event,'clicks') then $
  if (event.clicks eq 2) then bname='plot'

;-- read header only

 if (bname eq 'head') then begin
  file_id=trim(info.cur_fsel)
  if (file_id eq '') then return

;-- check if header already read

  header=''
  !show_synop.fifo->get,file_id,data
  if obj_valid(data) then header=data->get(/header)

;-- otherwise read it

  if is_blank(header) then begin
   widget_control,/hour
   r=obj_new('reader')
   r->read,file_id,header=header,err=err,/nodata
   obj_destroy,r
   if err ne '' then begin
    xack,err,group=info.main
    return
   endif
  endif

  if is_string(header) then begin
   hbase=info.hbase
   desc=['File: '+file_id,' ']
   xpopup,[desc,header],wbase=hbase,group=info.main,tfont=info.lfont,bfont=info.bfont,$
           title='File Header Information',xsize=80
   info.hbase=hbase
   self->setprop,info=info
  endif else xack,['No header in: ',file_id],group=info.main,/info
 endif

;-- read & plot downloaded file

 if (bname eq 'plot') then begin

  file_id=trim(info.cur_fsel)
  if (file_id eq '') then return
  use_plotman=self->getprop(/plotman)
  nf=n_elements(info.mult_sel)
  if not use_plotman then nf=1

  for i=0,nf-1 do begin
   file_id=info.mult_sel[i]

;-- check if this selection already cached

   info.last_fsel=file_id
   self->setprop,info=info
   !show_synop.fifo->get,file_id,data
   status=0
   if (obj_valid(data))[0] then begin
    if have_method(data,'has_data') then status=data->has_data() 
   endif

;-- if not, read it

   new_file=0b
   if not status then begin
    r=obj_new('reader')
    xtext,'Please wait. Reading file...',wbase=tbase,/just_reg,/center,$
         group=info.main
    widget_control,/hour
    r->read,file_id,data,err=err
    obj_destroy,r
    xkill,tbase
    xshow,info.main
    if err ne '' then begin
     xack,err,group=info.main
     if obj_valid(data) then obj_destroy,data
     break
    endif
    new_file=1b
   endif 

;-- if using PLOTMAN, then disable channel selection options, and 
;   automatic color scaling

   cancel=0b
   plot_type=data->get(/plot_type)
   if plot_type eq 'image' then $
     data->set,log_scale=self->is_log(file_id),load_color=(1-use_plotman)

   if plot_type eq 'utplot' then begin
    data->set,dim1_sel=(1-use_plotman)
    data->options,cancel=cancel
   endif

   if not cancel then self->plot_data,data
   if new_file then !show_synop.fifo->set,file_id,data

  endfor
 endif

 return & end

;-----------------------------------------------------------------------------
;-- main plotter

 pro show_synop::plot_data,data

 if not obj_valid(data) then return

;-- use data's internal plot method if not using PLOTMAN

 widget_control,/hour
 use_plotman=self->getprop(/plotman)
 if not use_plotman then begin
  data->plot,err=err
  if is_string(err) then xack,err
  return
 endif

;-- create new plotman object if not already done so

 if not obj_valid(!show_synop.plotman_obj) then begin
  !show_synop.plotman_obj=obj_new('plotman',input=data, /multi)
 endif else status = !show_synop.plotman_obj->setdefaults(input=data)

 filename = data->get(/filename)
 desc = filename eq '' ? data->get(/id) : filename
 !show_synop.plotman_obj->new_panel, desc, /nodup

 return & end

;---------------------------------------------------------------------
;-- set log property of map

function show_synop::is_log,file

if is_blank(file) then return,0b
name=file_break(file)
return,stregex(name,'eit_|trac|sxi',/bool,/fold)

end

;--------------------------------------------------------------------------
;-- list currently downloaded files

pro show_synop::flist

info=self->get_info()
ldir=self->getprop(/ldir)
widget_control,info.blabel,set_value='Currently Downloaded Files in: '+ldir
widget_control,info.flist,set_uvalue='',set_value=''

widget_control,/hour

search_ext='\.fts|\.fits|\.ltc|.\spc|\.fit'
files=loc_file('*.*',path=ldir,count=count)
chk=stregex(files,search_ext,/fold)
ok=where(chk gt -1,count)

if count gt 0 then begin
 files=files[ok]
 self->fbreak,files,cdir,cnames
 files=get_uniq(files,sorder)
 cnames=cnames[sorder]
 widget_control,info.flist,set_uvalue=files,set_value=cnames
 self->fbreak,info.last_fsel[0],sdir,sname
 sel=where(sname eq cnames,scount)
 if scount eq 0 then sel=0
 if sel[0] gt -1 then begin
  widget_control,info.flist,set_list_select=sel[0]
  info.cur_fsel=files[sel[0]]
  info=rep_tag_value(info,info.cur_fsel,'mult_sel')
  self->setprop,info=info
 endif
endif

return & end

;---------------------------------------------------------------------------
;-- list data archive

pro show_synop::slist,reload=reload,quiet=quiet

;-- initialize

info=self->get_info()

;-- start listing

verbose=1-keyword_set(quiet)
if verbose then $
 xtext,'Please wait. Searching archives...',wbase=tbase,/just_reg,/center,$
        group=info.main
old_cache=self->getprop(/cache)
if keyword_set(reload) then self->setprop,cache=0

widget_control,/hour
widget_control,info.main,sensitive=0

self->list,files,sizes=sizes,times=times,count=count,cats=cats,stimes=stimes
self->setprop,cache=old_cache

;-- remove annoying .Z files

;if count gt 0 then begin
; chk=stregex(strtrim(files,2),'(.Z|.gif)$')
; keep=where(chk eq -1,zcount)
; if (zcount gt 0) and (zcount lt count) then begin
;  files=files[keep]
;  sizes=sizes[keep]
;  times=times[keep]
;  cats=cats[keep]
; endif
;endif

widget_control,info.main,sensitive=1
xkill,tbase
xshow,info.main
if count eq 0 then begin
 mode=self->getprop(/mode)
 site=self->getprop(/site)
 wave=self->getprop(/wave)
 types=self->getprop(/types)
 case mode of
  0:  no_files='site "'+site+'"'
  1:  no_files='type "'+types+'"'
  else: no_files='subtype "'+wave+'"'
 endcase
 no_files='No files matching '+no_files+' during specified time range'
 widget_control,info.slist,set_value=no_files
 widget_control,info.slist,set_uvalue=''
 return
endif

;-- format output

widget_control,/hour

fname=file_break(files)
;ftimes=anytim2utc(times,/trun,/vms)

;-- sort output

sort_mode=self->getprop(/sort_mode) < 2

if (n_elements(fname) gt 1) then begin
 if sort_mode eq 0 then sorder=reverse(bsort(fname)) else $
  sorder=bsort(times,reverse=sort_mode eq 1)
 stimes=stimes[sorder]
 fname=fname[sorder]
 cats=cats[sorder]
 files=files[sorder]
 sizes=sizes[sorder]
endif

mcat=str_cut(cats,20,pad=22)
fcat=str_cut(fname,36,pad=40)
fdata=temporary(fcat)+temporary(stimes)+'      '+temporary(mcat)+sizes

widget_control,info.slist,set_value=fdata
widget_control,info.slist,set_uvalue=files
chk=where(info.cur_sel[0] eq files,count)
if count gt 0 then widget_control,info.slist,set_list_select=chk[0]

return & end

;---------------------------------------------------------------------------
;-- widget cleanup

pro show_synop_cleanup,id

dprint,'% show_synop_cleanup...'

widget_control,id,get_uvalue=object

if obj_valid(object) then begin
 info=object->get_info()
 if is_struct(info) then begin
  xtext_reset,info
  obj_destroy,object
 endif
endif
return & end

;----------------------------------------------------------------------------
;-- object cleanup

pro show_synop::cleanup

dprint,'% show_synop::cleanup...'

info=self->get_info()
if not is_struct(info) then return

widget_control,/hour

self->setprop,save_time=!stime

props=self->getprop(/all_props)

;-- clean up GOES files

if obj_valid(!show_synop.goes_obj) then !show_synop.goes_obj->flush,2

;-- save some useful configuration info

ptags=tag_names(props)
for i=0,n_tags(props)-1 do begin
 type=size(props.(i),/tname) 
 if (type eq 'POINTER') or (type eq 'OBJREF') then $
  rtags=append_arr(rtags,ptags[i])
endfor
props=rem_tag(props,rtags)

goes_mode=0 & goes_gsat=0
if xalive(info.goes_mode_drop) then begin
 goes_mode=widget_info(info.goes_mode_drop,/droplist_select)
 goes_gsat=widget_info(info.goes_sat_drop,/droplist_select)
endif
props=rep_tag_value(props,goes_mode,'goes_mode')
props=rep_tag_value(props,goes_gsat,'goes_gsat')
props=add_tag(props,info.cur_fsel,'cur_fsel')
props=add_tag(props,self->getprop(/ldir),'ldir')

self->fbreak,info.config_file,fdir
if test_dir(fdir,/quiet) then begin
 message,'saving configuration to - '+info.config_file,/cont
 save,file=info.config_file,props
endif

self->synop::cleanup

ptr_free,self.ptr
xkill,'show_synop::setup'

return & end

;----------------------------------------------------------------------------
;-- read header of remote file

pro show_synop::rhead,file,header

header=''
if is_blank(file) then return

;if is_compressed(file) then begin
; message,'Cannot read compressed file header.',/cont
; return
;endif

rchk=self->is_remote(file[0],class=class)

if rchk then begin
 robj=obj_new(class)
 robj->read,file[0],header=header,/nodata,/remote
 obj_destroy,robj
endif

return & end

;--------------------------------------------------------------------------
;-- display site info based on filename

pro show_synop::file_info,file

if not is_string(file) then return

widget_control,/hour

desc=['File: '+file_break(file),'Source: '+self->file_site(file),self->file_desc(file)]

self->rhead,file,header
if is_string(header) then desc=[desc,header]

;-- create pop-up

info=self->get_info()
tbase=info.tbase
xpopup,desc,wbase=tbase,group=info.main,tfont=info.lfont,bfont=info.bfont,$
            xsize=80,title='Remote File Information'
info.tbase=tbase
self->setprop,info=info

return
end


;----------------------------------------------------------------------------
;-- match file site abbr with full site name

function show_synop::file_site,file

if not is_string(file) then return,''

self->fbreak,file,dir,fname

sites=self->list_synop_sites()
nsites=n_elements(sites)
i=-1 & count=0
while (i lt (nsites-1)) and (count eq 0) do begin
 i=i+1
 look=where(strpos(strlowcase(fname),strlowcase(sites[i].abbr)) gt -1,count)
endwhile

if count gt 0 then return,sites[i].name else return,'Unrecognized site name'

end

;----------------------------------------------------------------------------
;-- match file desc abbr with full description

function show_synop::file_desc,file

if not is_string(file) then return,''

self->fbreak,file,dir,fname

desc=self->list_synop_desc()
ndesc=n_elements(desc)
for i=0,ndesc-1 do begin
 look=where(strpos(strlowcase(fname),'_'+strlowcase(desc[i].abbr)) gt -1,count)
 if (count gt 0) then out=append_arr(out,desc[i].desc)
endfor

if exist(out) then return,out else return,''

end


;------------------------------------------------------------------------------
;-- SHOW_SYNOP definition

pro show_synop__define

self={show_synop,save_time:0.d,sort_mode:0,ptr:ptr_new(), inherits synop}

return & end

;+                                                                                           
; Project     : HESSI                                                                        
;                                                                                            
; Name        : SYNOP_DEFINE                                                                 
;                                                                                            
; Purpose     : Define a SYNOP data object                                                   
;                                                                                            
; Category    : GBO Synoptic Objects                                                         
;                                                                                            
; Syntax      : IDL> c=obj_new('synop')                                                      
;                                                                                            
; History     : Written5 March 2000, D. Zarro, SM&A/GSFC                                     
;               Modified 29 March 2006, Zarro (L-3Com/GSFC) - fixed remote file checking     
;                                                                                            
; Contact     : dzarro@solar.stanford.edu                                                    
;-                                                                                           
;-----------------------------------------------------------------------------               
;-- init                                                                                     
                                                                                             
function synop::init,_ref_extra=extra                                                        
                                                                                             
success=self->site::init(_extra=extra)                                                       
                                                                                             
if success then begin                                                                        
 user_synop_data=chklog('USER_SYNOP_DATA')                                                   
 if not is_dir(user_synop_data) then begin                                                   
  user_synop_data=curdir()                                                                   
  if not write_dir(user_synop_data) then user_synop_data=get_temp_dir()                      
 endif                                                                                       
 dprint,'% SYNOP::INIT -> USER_SYNOP_DATA: ',user_synop_data                                 
                                                                                             
 self->setprop,org='day',types='images',ext='',ldir=user_synop_data,/pair,$                  
  synop_dir='/pub/data/synoptic',rhost='smmdac.nascom.nasa.gov',err=err,mode=1,/bytes,$      
  plotman=have_proc('plotman__define')                                                       
                                                                                             
 success=err eq ''                                                                           
endif                                                                                        
                                                                                             
if success eq 0 then return,0b                                                               
                                                                                             
success=self->synop_db::init()                                                               
                                                                                             
dprint,'% SYNOP::INIT ',success                                                              
                                                                                             
return,success                                                                               
                                                                                             
end                                                                                          
                                                                                             
;----------------------------------------------------------------------------                
                                                                                             
pro synop::cleanup                                                                           
                                                                                             
http=self->getprop(/http)                                                                    
if obj_valid(http) then obj_destroy,http                                                     
                                                                                             
self->site::cleanup                                                                          
self->synop_db::cleanup                                                                      
                                                                                             
dprint,'% SYNOP::CLEANUP'                                                                    
                                                                                             
return & end                                                                                 
                                                                                             
;------------------------------------------------------------------------------              
;-- SET method                                                                               
                                                                                             
pro synop::setprop,types=types,last_count=last_count,site=site,err=err,$                     
                   wave=wave,mode=mode,synop_dir=synop_dir,$                                 
                   plotman=plotman,_extra=extra,rsearch=rsearch,current=current              
                                                                                             
err=''                                                                                       
                                                                                             
;-- set remote synop root directory name                                                     
                                                                                             
if is_string(synop_dir) then self.synop_dir=synop_dir                                        
                                                                                             
;-- set different search types                                                               
;   (comma delimited string: spectra, images, lightcurve)                                    
                                                                                             
self->set_synop_dirs,types                                                                   
                                                                                             
if is_string(wave) then self.wave=trim(wave)                                                 
if is_number(mode) then self.mode=(0 > mode < 2)                                             
if is_number(plotman) then self.plotman= -1 > plotman < 1                                    
if size(site,/tname) eq 'STRING' then self.site=trim(site)                                   
if is_number(last_count) then self.last_count=last_count                                     
if is_number(rsearch) then self.rsearch=  0b >  rsearch < 1b                                 
if is_number(current) then self.current = 0b > current < 1b                                  
                                                                                             
;-- set the rest                                                                             
                                                                                             
if is_struct(extra) then self->site::setprop,_extra=extra,err=err                            
                                                                                             
return & end                                                                                 
                                                                                             
                                                                                             
;-----------------------------------------------------------------------------               
                                                                                             
pro synop::open_socket,err=err,network=network,server=server,path=udir                       
                                                                                             
network=1b                                                                                   
if not allow_sockets(err=err) then return                                                    
server=synop_server(path=udir,network=network)                                               
if not network then err='Network connection currently unavailable'                           
if err ne '' then return                                                                     
                                                                                             
http=self->getprop(/http)                                                                    
if not obj_valid(http) then http=obj_new('http')                                             
http->open,server,err=err                                                                    
if err ne '' then return                                                                     
                                                                                             
self.http=http                                                                               
                                                                                             
return & end                                                                                 
                                                                                             
;----------------------------------------------------------------------------                
;-- HTTP copy files                                                                          
                                                                                             
pro synop::hget,ofiles,count=count,err=err,_extra=extra,cancelled=cancelled                  
                                                                                             
;-- ofiles = actual files copied                                                             
                                                                                             
err=''                                                                                       
count=0                                                                                      
ofiles=''                                                                                    
cancelled=0b                                                                                 
                                                                                             
self->open_socket,err=err                                                                    
if err ne '' then return                                                                     
                                                                                             
lfile=self->getprop(/lfile)                                                                  
if is_blank(lfile) then return                                                               
rfile=self->getprop(/rfile)                                                                  
verbose=self->getprop(/verbose)                                                              
clobber=self->getprop(/clobber)                                                              
                                                                                             
;-- check if remote file is locally available (e.g. via NFS)                                 
                                                                                             
nfiles=n_elements(rfile)                                                                     
for i=0,nfiles-1 do begin                                                                    
 chk=loc_file(rfile[i],count=count)                                                          
 if count eq 0 then begin                                                                    
  self.http->copy,rfile[i],lfile[i],_extra=extra,err=err,/progress,/slurp,/verb,$            
                    clobber=clobber,status=status,/no_change,cancelled=cancelled             
  if cancelled then break                                                                    
 endif else begin                                                                            
  chk=loc_file(lfile[i],count=count)                                                         
  if clobber or (count eq 0) then file_copy2,rfile[i],lfile[i]                               
  chk=loc_file(lfile[i],count=count)                                                         
  status= count eq 1                                                                         
 endelse                                                                                     
 if status then tfiles=append_arr(tfiles,lfile[i],/no_copy)                                  
endfor                                                                                       
                                                                                             
count=n_elements(tfiles)                                                                     
if count gt 0 then ofiles=temporary(tfiles)                                                  
                                                                                             
return & end                                                                                 
                                                                                             
;---------------------------------------------------------------------------                 
;-- copy remote files                                                                        
                                                                                             
pro synop::rcopy,files,ofiles,count=count,err=err,cancelled=cancelled,$                      
                       _extra=extra,lfiles=lfiles                                            
                                                                                             
;-- files = files to copy                                                                    
;-- ofiles = actual files copied                                                             
;-- lfiles = local filenames (not remote)                                                    
                                                                                             
ofiles='' & err='' & count=0 & cancelled=0b & lfiles=''                                      
                                                                                             
rok=self->is_remote(files,count=rcount)                                                      
if rcount eq 0 then begin                                                                    
 lfiles=files & return                                                                       
endif                                                                                        
doit=where2(rok,rcount,complement=complement,ncomplement=ncomplement)                        
rfiles=files[doit]                                                                           
if ncomplement gt 0 then lfiles=files[complement]                                            
clobber=self->getprop(/clobber)                                                              
ldir=self->getprop(/ldir)                                                              
for i=0,rcount-1 do begin                                                                                                                           
 sock_copy,rfiles[i],out_dir=ldir,_extra=extra,$                                            
             cancelled=cancelled,clobber=clobber,/no_change,/progress,$                      
             status=status,err=err,/verb                                                     
 if cancelled then break                                                                     
 if status then begin                                                                        
  nfile=concat_dir(ldir,file_break(rfiles[i]))                                               
  tfiles=append_arr(tfiles,nfile,/no_copy)                                                   
 endif                                                                                       
endfor                                                                                       
                                                                                             
;-- clean-up                                                                                 
                                                                                             
count=n_elements(tfiles)                                                                     
if count gt 0 then ofiles=temporary(tfiles)                                                  
                                                   
                                                                                             
return & end                                                                                 
                                                                                             
;-----------------------------------------------------------------------------               
;--copy specified file(s)                                                                    
                                                                                             
function synop::fetch,file,count=count,err=err,_extra=extra                                  
                                                                                             
count=0 & err=''                                                                             
if not is_string(file,cfile) then begin                                                      
 return,''                                                                                   
endif                                                                                        
                                                                                             
;-- check for remote site copies                                                             
                                                                                             
self->rcopy,cfile,rfiles,_extra=extra,count=count,err=err,cancelled=cancelled,lfiles=lfiles  
if is_blank(lfiles) then return,rfiles                                                       
                                                                                             
;-- copy non-remote sites                                                                    
                                                                                             
rcount=count                                                                                 
self->fbreak,cfile,cdir,cname                                                                
chk=where(trim2(cdir) ne '',scount)                                                          
                                                                                             
if scount eq 0 then return,rfiles                                                            
sfiles=cfile[chk]                                                                            
self->setprop,rfile=sfiles                                                                   
self->copy_file,lfiles,_extra=extra,count=lcount,err=err                                     
                                                                                             
if rcount gt 0 then ofiles=append_arr(lfiles,rfiles,/no_copy) else ofiles=temporary(lfiles)  
                                                                                             
count=n_elements(ofiles)                                                                     
                                                                                             
return,ofiles                                                                                
                                                                                             
                                                                                             
end                                                                                          
                                                                                             
;---------------------------------------------------------------------------                 
;-- get search directories and patterns by:                                                  
;   site (mode=0), type (mode=1), or by sub-type (mode=2)                                    
                                                                                             
function synop::get_search_dirs,patt=patt,err=err                                            
                                                                                             
def_patt=' '                                                                                 
def_dirs=self->list_synop_dirs(/lower)                                                       
count=n_elements(def_dirs)                                                                   
def_patt=replicate(def_patt,count)                                                           
                                                                                             
mode=self->getprop(/mode)                                                                    
if mode eq 1 then begin                                                                      
 sdirs=self->get_synop_dirs()                                                                
 ndirs=n_elements(sdirs)                                                                     
 patt=comdim2(replicate('*.f*t*',ndirs))                                                     
 return,sdirs                                                                                
endif                                                                                        
                                                                                             
site_db=self->list_synop_sites()                                                             
if mode eq 0 then begin                                                                      
 site=self->getprop(/site)                                                                   
 chk=where(site eq site_db.abbr,count)                                                       
 if count eq 0 then begin                                                                    
  err='Unsupported site: "'+site+'"'                                                         
  patt=''                                                                                    
  message,err,/cont                                                                          
  return,''                                                                                  
 endif                                                                                       
endif else begin                                                                             
 wave=self->getprop(/wave)                                                                   
 chk=where(wave eq site_db.wave,count)                                                       
 if count eq 0 then begin                                                                    
  err='Unsupported sub-type: "'+wave+'"'                                                     
  patt=''                                                                                    
  return,''                                                                                  
 endif                                                                                       
endelse                                                                                      
                                                                                             
chk=comdim2(chk)                                                                             
if count gt 0 then begin                                                                     
 output=(site_db.dir)[chk]                                                                   
 rpatt=(site_db.patt)[chk]                                                                   
 rabbr=(site_db.abbr)[chk]                                                                   
 nowild=where( stregex(rpatt,'\*+',/bool) eq 0,count)                                        
 if count gt 0 then rpatt[nowild]='*'+rpatt[nowild]                                          
 patt=rpatt                                                                                  
 chk=get_uniq(output+patt,sorder)                                                            
 sorder=comdim2(sorder)                                                                      
 patt=patt[sorder]                                                                           
 return,output[sorder]                                                                       
endif                                                                                        
                                                                                             
return,def_dirs                                                                              
                                                                                             
end                                                                                          
                                                                                             
;------------------------------------------------------------------------------              
;-- show properties                                                                          
                                                                                             
pro synop::show                                                                              
                                                                                             
self->site::show                                                                             
print,''                                                                                     
print,'SYNOP properties:'                                                                    
print,'-----------------'                                                                    
print,'% synop_dir: ',self.synop_dir                                                         
print,'% types: ',self.types                                                                 
print,'% site: ',self.site                                                                   
print,'% wave: ',self.wave                                                                   
print,'% last_count: ',self.last_count                                                       
print,'% mode: ',self.mode                                                                   
                                                                                             
return & end                                                                                 
                                                                                             
;------------------------------------------------------------------------------              
                                                                                             
pro synop::apply,event                                                                       
                                                                                             
xkill,event.top                                                                              
                                                                                             
return & end                                                                                 
                                                                                             
;----------------------------------------------------------------------------                
;-- widget base setup                                                                        
                                                                                             
pro synop::config,group=group,no_types=no_types                                              
                                                                                             
current=self->getprop(/current)                                                              
clobber=self->getprop(/clobber)                                                              
cache=self->getprop(/cache)                                                                  
ldir=self->getprop(/ldir)                                                                    
types=self->getprop(/types)                                                                  
last_time=self->getprop(/last_time)                                                          
plotman=self->getprop(/plotman)                                                              
rsearch=self->getprop(/rsearch)                                                              
save_config={clobber:clobber,cache:cache,ldir:ldir,$                                         
             types:types,current:current,$                                                   
             last_time:last_time,plotman:plotman,rsearch:rsearch}                            
                                                                                             
mk_dfont,bfont=bfont,lfont=lfont                                                             
base=widget_mbase(/column,title="SHOW_SYNOP DOWNLOAD OPTIONS",/modal,group=group,uvalue=self)
                                                                                             
                                                                                             
;-- save time interval                                                                       
                                                                                             
base1=widget_base(base,/column,/frame)                                                       
                                                                                             
trow=widget_base(base1,/row)                                                                 
xmenu2,['Yes','No'],trow,/row,/exclusive,font=lfont,/no_rel,$                                
      buttons=tbuttons,uvalue=['yes_save','no_save'],$                                       
      title='Save last search time interval? ',lfont=lfont                                   
widget_control,tbuttons[1-last_time],/set_button                                             
                                                                                             
;-- do remote searching                                                                      
                                                                                             
trow=widget_base(base1,/row)                                                                 
xmenu2,['Yes','No'],trow,/row,/exclusive,font=lfont,/no_rel,$                                
      buttons=rbuttons,uvalue=['yes_rsearch','no_rsearch'],$                                 
      title='Search remote site archives (e.g. NGDC, but can be slow)? ',lfont=lfont         
widget_control,rbuttons[1-rsearch],/set_button                                               
                                                                                             
;-- use caching                                                                              
                                                                                             
crow=widget_base(base1,/row)                                                                 
xmenu2,['Yes','No'],crow,/row,/exclusive,font=lfont,/no_rel,$                                
      buttons=sbuttons,uvalue=['yes_cache','no_cache'],$                                     
      title='Cache search results (recommended for speed)? ',lfont=lfont                     
widget_control,sbuttons[1-cache],/set_button                                                 
                                                                                             
;-- choice of data type                                                                      
                                                                                             
if (1-keyword_set(no_types)) then begin                                                      
 supp_types=self->list_synop_dirs()                                                          
 row1=widget_base(base1,/row)                                                                
 xmenu2,supp_types,row1,/row,/exclusive,font=lfont,$                                         
      buttons=dbuttons,uvalue=strlowcase(supp_types),$                                       
      title='Data types to download: ',lfont=lfont                                           
 curr_types=self->get_synop_dirs()                                                           
 val=where_vector(curr_types,supp_types,count)                                               
 if count gt 0 then widget_control,dbuttons[val[0]],/set_button                              
endif                                                                                        
                                                                                             
;-- clobber data?                                                                            
                                                                                             
row3=widget_base(base1,/row)                                                                 
xmenu2,['Yes','No'],row3,/row,/exclusive,font=lfont,/no_rel,$                                
      buttons=cbuttons,uvalue=['yes_clobber','no_clobber'],$                                 
      title='Overwrite existing files when downloading? ',lfont=lfont                        
widget_control,cbuttons[1-clobber],/set_button                                               
                                                                                             
;-- use PLOTMAN?                                                                             
                                                                                             
if have_proc('plotman__define') and (self->getprop(/plotman) ne -1) then begin               
 row3=widget_base(base1,/row)                                                                
 xmenu2,['Yes','No'],row3,/row,/exclusive,font=lfont,/no_rel,$                               
       buttons=cbuttons,uvalue=['yes_plotman','no_plotman'],$                                
       title='Use PLOTMAN for plotting? ',lfont=lfont                                        
 widget_control,cbuttons[1-plotman],/set_button                                              
endif                                                                                        
                                                                                             
;-- download directory                                                                       
                                                                                             
;row2=widget_base(base1,/row)                                                                 
;col21=widget_base(row2,/column)                                                              
;dlabel=widget_label(col21,value='Download directory: ',font=lfont,/align_left)               
;col22=widget_base(row2,/row)                                                                 
;dtext=widget_text(col22,value=ldir,xsize=25,/editable,uvalue='directory')                    
;dbutt=widget_button(col22,value='Browse',font=bfont,uvalue='browse')                         
                                                                                             
;-- write to current directory?                                                              
                                                                                             
;row3=widget_base(base1,/row)                                                                 
;xmenu2,['Yes','No'],row3,/row,/exclusive,font=lfont,/no_rel,$                                
;      buttons=cbuttons,uvalue=['yes_current','no_current'],$                                 
;      title='Always download to current working directory?',lfont=lfont                      
;widget_control,cbuttons[1-current],/set_button                                               
      
                                                                                       
row0=widget_base(base,/row,/align_center)                                                    
                                                                                             
doneb=widget_button(row0,value='Apply',uvalue='apply',$                                      
       font=bfont,/frame)                                                                    
cancb=widget_button(row0,value='Cancel',uvalue='cancel',$                                    
       font=bfont,/frame)                                                                    
                                                                                             
;-- share widget id's thru child's uvalue                                                    
                                                                                             
child=widget_info(base,/child)                                                               
;info={dtext:dtext,save_config:save_config}
widget_control,child,set_uvalue=save_config                                                         
                                                                                             
xrealize,base,/center                                                                        
                                                                                             
xmanager,'synop::config',base,event='synop_event'                                            
                                                                                             
return & end                                                                                 
                                                                                             
;----------------------------------------------------------------------------                
;-- object event handler. Since actual methods cannot be event-handlers,                     
;   we shunt events thru this wrapper                                                        
                                                                                             
pro synop_event,event                                                                        
widget_control,event.top,get_uvalue=object                                                   
if obj_valid(object) then object->synop_event,event                                          
end                                                                                          
                                                                                             
;----------------------------------------------------------------------------                
                                                                                             
pro synop::synop_event,event                                                                 
                                                                                             
widget_control,event.id,get_uvalue=uvalue                                                    
widget_control,event.top,get_uvalue=self                                                     
child=widget_info(event.top,/child)                                                          
widget_control,child,get_uvalue=save_config                                                         
                                                                                             
uvalue=trim(uvalue[0])                                                                       
                     
;-- determine selected SYNOP types                                                           
                                                                                             
tvalue=trim(uvalue[0])                                                                       
self->set_synop_dirs,tvalue                                                                  
                                                                                             
case tvalue of                                                                               
                                                                                             
 'yes_clobber': self->setprop,clobber=1                                                      
 'no_clobber': self->setprop,clobber=0                                                       
                                                                                             
 'yes_cache': self->setprop,cache=1                                                          
 'no_cache': self->setprop,cache=0                                                           
                                                                                             
 'yes_plotman': self->setprop,plotman=1                                                      
 'no_plotman': self->setprop,plotman=0                                                       
                                                                                             
 'yes_save': self->setprop,last_time=1                                                       
 'no_save': self->setprop,last_time=0                                                        
                                                                                             
 'yes_rsearch': self->setprop,rsearch=1                                                      
 'no_rsearch': self->setprop,rsearch=0                                                       
                                                                                             
 'yes_copy': self->setprop,copy=1                                                            
 'no_copy':  self->setprop,copy=2                                                            
                                                                                             
 'apply':xkill,event.top                                                                     
                                                                                             
 'cancel': begin                                                                             
   struct_assign,save_config,self,/nozero                                               
   xkill,event.top                                                                           
  end                                                                                        
                                                                                             
 else: return                                                                                
endcase                                                                                      
                                                                                             
return & end                                                                                 
                                                                                             
;-----------------------------------------------------------------------------               
;-- list method (allows multiple SYNOP types)                                                
                                                                                             
pro synop::list,files,_ref_extra=extra,count=count,err=err                                   
                                                                                             
count=0 & err=''                                                                             
sdirs=self->get_search_dirs(patt=patt,err=err)                                               
if err ne '' then return                                                                     
self->setprop,ftype=patt                                                                     
                                                                                             
if is_string(sdirs) then begin                                                               
 synop_dir=self->getprop(/synop_dir)                                                         
 self->setprop,topdir=synop_dir+'/'+sdirs                                                    
 self->site::list,files,_extra=extra,count=count,err=err                                     
endif                                                                                        
                                                                                             
self->setprop,last_count=count                                                               
                                                                                             
return & end                                                                                 
                                                                                             
;----------------------------------------------------------------------------                
;-- create unique cache id for storing search results                                        
                                                                                             
function synop::get_cache_id                                                                 
                                                                                             
site=self->getprop(/site)                                                                    
wave=self->getprop(/wave)                                                                    
type=self->getprop(/type)                                                                    
mode=self->getprop(/mode)                                                                    
ftype=self->getprop(/ftype)                                                                  
                                                                                             
opts=[site,type,wave]                                                                        
cache_id=opts[mode]+'_'+ftype                                                                
cache_id=str_replace(cache_id,'*','')                                                        
return, cache_id                                                                             
                                                                                             
end                                                                                          
                                                                                             
;----------------------------------------------------------------------------                
;-- set SYNOP search directories (images, spectra, lightcurves...)                           
                                                                                             
pro synop::set_synop_dirs,dirs                                                               
                                                                                             
in_dirs=self->valid_synop_dirs(dirs)                                                         
if in_dirs[0] eq '' then return                                                              
                                                                                             
self.types=arr2str(in_dirs)                                                                  
                                                                                             
return & end                                                                                 
                                                                                             
;----------------------------------------------------------------------------                
;-- get SYNOP directories (images, spectra, lightcurves...)                                  
                                                                                             
function synop::get_synop_dirs,count=count                                                   
                                                                                             
sdirs=str2arr(self.types)                                                                    
count=n_elements(sdirs)                                                                      
return,sdirs                                                                                 
                                                                                             
end                                                                                          
                                                                                             
;-----------------------------------------------------------------------------               
;-- get file categories                                                                      
                                                                                             
function synop::get_cats,files                                                               
                                                                                             
if is_blank(files) then return,''                                                            
                                                                                             
nfiles=n_elements(files)                                                                     
cats=replicate('?',nfiles)                                                                   
                                                                                             
self->fbreak,files,dir,names                                                                 
                                                                                             
sites=self->list_synop_sites()                                                               
         
for i=0,n_elements(sites)-1 do begin                                                         
 abbr=sites[i].abbr                                                                          
 patt=trim(str_replace(sites[i].patt,'*','.+'))                                              
 reg=abbr+'|'+patt                                                                           
 reg=patt
 chk=where(stregex(files,reg,/fold) gt -1,rcount)                                            
 if rcount gt 0 then cats[chk]=sites[i].wave+'/'+sites[i].dir                                
 left=where(cats eq '?',lcount)                                                              
 if lcount eq 0 then return,cats                                                             
endfor                                                                                       
                                                                                             
return,cats                                                                                  
                                                                                             
end                                                                                          
                                                                                             
;---------------------------------------------------------------------------                 
;-- HTTP listing                                                                             
                                                                                             
pro synop::hsearch,files,times=times,sizes=sizes,count=count,_ref_extra=extra        
                                                                                             
;-- search local sites                                                                       
                                                                                             
self->lsearch,files,times=times,sizes=sizes,count=count,_extra=extra                 
                                                                                             
;-- search remote site                                                                       
                                                                                             
self->rsearch,rfiles,times=rtimes,sizes=rsizes,count=rcount,_extra=extra                     
                                                                                             
if rcount gt 0 then begin                                                                                                     
 if count gt 0 then begin                                                                    
  files=append_arr(files,rfiles,/no_copy)                                                    
  sizes=append_arr(sizes,rsizes,/no_copy)                                                    
  times=append_arr(times,rtimes,/no_copy)                                                    
 endif else begin                                                                            
  files=temporary(rfiles)                                                                    
  sizes=temporary(rsizes)                                                                    
  times=temporary(rtimes)                                                                    
 endelse                                                                                     
 count=n_elements(files)                                                                     
endif                                                                                        
                                                                                            
                                                                                             
return & end                                                                                 
                                                                                             
;----------------------------------------------------------------------------                
;-- search local synoptic directories                                                        
                                                                                             
pro synop::lsearch,files,times=times,sizes=sizes,count=count,err=err                         
                                                                                             
err='' & files='' & count=0                                                                  
sizes='' & times=-1                                                                          
                                                                                             
;-- skip if searching a remote site                                                          
                                                                                             
mode=self->getprop(/mode)                                                                    
if mode eq 0 then begin                                                                      
 site=self->getprop(/site)                                                                   
 sites=self->list_synop_sites()                                                              
 chk=where((site eq sites.abbr) and (sites.remote ne ''),dcount)                             
 if dcount gt 0 then return                                                                  
endif                                                                                        
                                                                                             
self->open_socket,err=err,path=udir                                                          
if err ne '' then return                                                                     
                                                                                             
sdirs=self->get_sdir()                                                                       
rdirs=get_uniq(self->get_search_dirs(patt=rpatt))                                            
delim='/'                                                                                    
cache=self->getprop(/cache)                                                                  
                                                                                             
for i=0,n_elements(rdirs)-1 do begin                                                         
 for j=0,n_elements(sdirs)-1 do begin                                                        
  udate=ymd2date(sdirs[j],/tai)                                                              
  udir=synop_path(udate)                                                                     
  root=udir+delim+rdirs[i]+delim+sdirs[j]+delim                                              
  self.http->extract_href,root,names,sizes=fsizes,cache=cache,count=count                    
  if (count gt 0) then begin                                                                 
   names=root+temporary(names)                                                               
   temp=append_arr(temp,names,/no_copy)                                                      
   stemp=append_arr(stemp,fsizes,/no_copy)                                                   
  endif                                                                                      
 endfor                                                                                      
endfor                                                                                       
                                                                                             
;-- parse out files matching search pattern                                                  
                                                                                             
if is_string(temp) then begin                                                                
 files=temporary(temp)                                                                       
 sizes=temporary(stemp)                                                                      
 if is_string(rpatt) then begin                                                              
  reg=arr2str(rpatt,delim='|')                                                               
  reg=str_replace(reg,'.','\.')                                                              
  reg=str_replace(reg,'*','.*')                                                              
  chk=where(stregex(files,reg,/bool,/fold),count)                                            
  if count gt 0 then begin                                                                   
   files=files[chk]                                                                          
   sizes=sizes[chk]                                                                          
  endif else begin                                                                           
   files='' & sizes=''                                                                       
  endelse                                                                                    
 endif                                                                                       
endif                                                                                        
                                                                                             
;-- parse out times                                                                          
                                                                                             
if count gt 0 then begin                                                                     
 times=parse_time(files,count=count,err=err,ss=ss)                                           
 if count gt 0 then begin                                                                    
  files=files[ss]                                                                            
  times=anytim2tai(times[ss])                                                                
  sizes=sizes[ss]                                                                            
 endif                                                                                       
endif                                                                                        
                                                                                             
return & end                                                                                 
                                                                                             
;-----------------------------------------------------------------------------               
;-- search remote sites                                                                      
                                                                                             
pro synop::rsearch,files,times=times,sizes=sizes,count=count,err=err,_extra=extra            
                                                                                             
files='' & sizes='' & count=0 & times=-1 & err=''                                            
                                                                                             
sites=self->list_synop_sites()                                                               
remote=where(sites.remote ne '',scount)                                                      
if scount eq 0 then return                                                                   
mode=self->getprop(/mode)                                                                    
                                                                                             
;-- switch remote searching "on" if remote site is selected                                  
                                                                                             
if mode eq 0 then begin                                                                      
 site=self->getprop(/site)                                                                   
 chk=where((site eq sites.abbr) and (sites.remote ne ''),dcount)                             
 self->setprop,rsearch=(dcount gt 0)                                                         
endif                                                                                        
                                                                                             
if not self->getprop(/rsearch) then return                                                   
                                                                                             
;-- find which sites permit remote searching                                                 
                                                                                             
case mode of                                                                                 
  0: begin                                                                                   
      site=self->getprop(/site)                                                              
      chk=where((site eq sites.abbr) and (sites.remote ne ''),dcount)                        
     end                                                                                     
  1: begin                                                                                   
      type=self->getprop(/type)                                                              
      chk=where((type eq sites.dir) and (sites.remote ne ''),dcount)                         
     end                                                                                     
  2: begin                                                                                   
      wave=self->getprop(/wave)                                                              
      chk=where((wave eq sites.wave) and (sites.remote ne ''),dcount)                        
     end                                                                                     
  else: return                                                                               
endcase                                                                                      
                                                                                             
;-- perform remote search                                                                    
                                                                                             
if dcount gt 0 then begin                                                                    
 tstart=self->getprop(/tstart)                                                               
 tend=self->getprop(/tend)                                                                   
 rsites=(sites.remote)[chk]                                                                  
 for i=0,dcount-1 do begin                                                                   
                                                                                             
  error=0                                                                                    
  catch,error                                                                                
  if error ne 0 then begin                                                                   
   catch,/cancel                                                                             
   message,err_state(),/cont                                                                 
   continue                                                                                  
  endif                                                                                      
                                                                                             
  robj=obj_new(rsites[i])                                                                    
  if obj_valid(robj) then begin                                                              
   rfiles=robj->list(tstart,tend,sizes=rsizes,times=rtimes,count=rcount,$                  
              /round_times,err=err,_extra=extra,/full,/tai)   
   obj_destroy,robj                                                                          
   if is_string(err) then xack,err,/suppress                                                 
   if rcount gt 0 then begin                                                                 
    tfiles=append_arr(tfiles,rfiles,/no_copy)                                                
    tsizes=append_arr(tsizes,rsizes,/no_copy)                                                
    ttimes=append_arr(ttimes,rtimes,/no_copy)                                                
   endif                                                                                     
  endif                                                                                      
 endfor                                                                                      
endif                                                                                        
                                                                                             
count=n_elements(tfiles)                                                                     
if count gt 0 then begin                                                                     
 files=temporary(tfiles)                                                                     
 sizes=temporary(tsizes)                                                                     
 times=temporary(ttimes)                                                                     
endif                                                                                        
                                                                                             
return & end                                                                                 
                                                                                             
;------------------------------------------------------------------------------              
;-- SYNOP site structure                                                                     
                                                                                             
pro synop__define                                                                            
                                                                                             
self={synop,synop_dir:'',last_count:0l,mode:0, site:'', wave:'', plotman:0,$                 
      types:'',http:obj_new(), current:0b,rsearch:0b, inherits site, inherits synop_db}      
                                                                                             
return & end                                                                                 

;+
; Project     : HESSI
;
; Name        : FTP__DEFINE
;
; Purpose     : Define an FTP client object
;
; Category    : System
;
; Explanation : Object wrapper around FTP
;
; Syntax      : This procedure is invoked when a new FTP object is
;               created via:
;
;               IDL> new=obj_new('ftp')
;
;               Default settings are: 
;
;               user='anonymous'     ;-- anonymous ftp
;               pass='user@hostname'
;               ldir=cwd             ;-- copy to current directory
;               rdir='.'             ;-- copy from top login directory
;
; Examples    : ftp=obj_new('ftp')
;               ftp->open,host               ;-- connect to hostname
;               ftp->ls                      ;-- list remote directory
;               ftp->pass,password           ;-- set password
;               ftp->user,user_name          ;-- set username
;               ftp->cd,dir_name             ;-- set remote directory
;               ftp->lcd,dir_name            ;-- set local directory
;               ftp->mget,file_name          ;-- get file_name
;               ftp->pwd                     ;-- print working directory
;               ftp->show                    ;-- show current settings
;               ftp->put,file_name           ;-- put filename [not implemented]
;               ftp->setprop,rfile=rfile    ;-- set remote filename to copy
;               ftp->setprop,lfile=lfile    ;-- set local name of copied file
;               ftp->clobber,1 [0]           ;-- set to clobber existing file
;               ftp->getprop(/rdir,/ldir,/rfile) ;-- get remote dirs, local dirs, 
;                                                 ;remote file
;               
; Inputs      : 'ftp' = object classname
;              
; Outputs     : Object with above methods
;                  
; History     : Written 15 Nov 1999, D. Zarro, SM&A/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

;---------------------------------------------------------------------------
;-- set some default properties

function ftp::init,rhost,_extra=extra

ret=self->gen::init()

self->open,rhost
self->user,'anonymous'
self->pass,get_user_id()
self->cd,'.'
ldir=curdir()
if not write_dir(ldir) then begin
 ldir=get_temp_dir()
 message,'using local directory: '+ldir,/cont
endif
self->lcd,ldir
self->binary
self->clobber,1
self->setprop,lfile=''

dprint,'% FTP::INIT ',1b

return,1

end

;-----------------------------------------------------------------------------

pro ftp::cleanup     

ptr_free,self.rfile
ptr_free,self.rdir
ptr_free,self.lfile
ptr_free,self.ldir
self->gen::cleanup

dprint,'% FTP::CLEANUP '

return & end

;----------------------------------------------------------------------------
;-- move up one directory level

pro ftp::cdup

self->cd,'..'

return & end

;----------------------------------------------------------------------------
;-- change remote directory ("." and ".." are special)
         
pro ftp::cd,rdir

if n_elements(rdir) gt 1 then return
                
if not is_string(rdir) then begin
 self->setprop,rdir='.'
 return
endif

;-- move up a level

crdir=self->getprop(/rdir,count=count)
if (count ne 1) then return

if (strtrim(rdir,2) eq '..') then begin
 if not is_string(crdir) then return
 self->fbreak,crdir,up    
 if up ne '' then self->setprop,rdir=up
 return
endif 

;-- stay at current level

fp=strmid(rdir,0,1)

if (fp eq '.') or (fp eq '/') or (fp eq '\') then begin
 self->setprop,rdir=rdir
 return
endif

;-- move to specified level

if not is_string(crdir) then return
if trim(crdir) ne '' then self->setprop,rdir=crdir+'/'+rdir else $
 self->setprop,rdir=dir

return & end

;----------------------------------------------------------------------------
;-- change local directory

pro ftp::lcd,ldir
if n_elements(ldir) gt 1 then return
self->setprop,ldir=ldir

return & end

;-----------------------------------------------------------------------------
;-- connect to remote host

pro ftp::open,rhost
if is_string(rhost) then self->setprop,rhost=rhost

return & end

;----------------------------------------------------------------------------
;-- set to clobber

pro ftp::clobber,id

if not is_number(id) then id=1b
self->setprop,clobber=id

return & end

;----------------------------------------------------------------------------
;-- set for verbose

pro ftp::verbose,id

if not is_number(id) then id=1b
self->setprop,verbose=id

return & end

;-----------------------------------------------------------------------------
;-- set for binary copy

pro ftp::binary
self->setprop,/binary

return & end

;-----------------------------------------------------------------------------                                                       
;-- set for ASCII
                                                       
pro ftp::ascii                                                       
self->setprop,binary=0b

return & end

;-----------------------------------------------------------------------------                        
;-- print working directory
                        
pro ftp::pwd    

print,self->getprop(/rdir)

return & end

;------------------------------------------------------------------------------
;-- login

pro ftp::login,user,password
if is_string(user) then self->setprop,user=user
if is_string(pass) then self->setprop,password=password

return & end

;------------------------------------------------------------------------------
;-- set username

pro ftp::user,user
if is_string(user) then self.user=user

return & end

;------------------------------------------------------------------------------
;-- set password

pro ftp::pass,password
if is_string(password) then self->setprop,password=password

return & end

;-----------------------------------------------------------------------------
;-- set port number

pro ftp::port,rport
if is_number(rport) then self.rport=trim(rport)
return & end
         
;---------------------------------------------------------------------------
;-- set remote and local directory and file properties 

pro ftp::setprop,rhost=rhost,rfile=rfile,rdir=rdir,lfile=lfile,ldir=ldir,$
                  clobber=clobber,binary=binary,user=user,$
                  password=password,err=err,rpatt=rpatt,pair=pair,passive=passive,$
                  auto=auto,ascii=ascii,verbose=verbose,_extra=extra

err=''
if is_number(verbose) then self.verbose=   0b > fix(verbose) < 1b
if is_string(rhost) then self.rhost=trim(rhost)
if is_string(user) then self.user=trim(user)
if is_string(password) then self.password=trim(password)
if is_number(clobber) then self.clobber= 0b > fix(clobber) < 1b
if is_number(auto) then self.auto= 0b > fix(auto) < 1b
if is_number(binary) then self.binary= 0b > fix(binary) < 1b
if is_number(ascii) then self.binary= 0b > 1-fix(ascii) < 1b
if size(rpatt,/tname) eq 'STRING' then begin
 if n_elements(rpatt) gt 1 then self.rpatt=arr2str(strtrim(rpatt,2)) else $
  self.rpatt=strtrim(rpatt,2)
endif
if is_number(pair) then self.pair= 0b > fix(pair) < 1b
if is_number(passive) then self.passive = 0b > fix(passive) < 1b

;-- set remote filename (non-blank vector) 

if is_string(rfile,temp) then self->insert,temp,/rfile

;-- set remote directory (scalar)

if size(rdir,/tname) eq 'STRING' then self->insert,rdir,/rdir

;-- set local filename (vector)

local_delim=get_delim()
if size(lfile,/tname) eq 'STRING' then begin
 lfile=local_name(lfile)
 self->insert,lfile,/lfile
endif

;-- set local directory (scalar)
 
if size(ldir,/tname) eq 'STRING' then begin
 temp=chklog(ldir,/pre)
 temp=local_name(temp)
 use_it=1
 if temp ne '' then use_it=is_dir(temp)
 if use_it then self->insert,temp,/ldir else begin
  err='Non-existent directory - '+ldir
  message,err,/cont
 endelse
endif


return & end

;-------------------------------------------------------------------------------
; insert string values into pointer

pro ftp::insert,value,_extra=extra

if size(extra,/tname) ne 'STRUCT' then return
if size(value,/tname) ne 'STRING' then return

tags=tag_names(extra)
tself='self.'+tags[0]
status=execute('ptr='+tself)
if not status then return
ptr_alloc,ptr
;value=chklog(value,/prese) 

value=str_trail(strtrim(value,2),'/')                           
value=str_trail(strtrim(value,2),'\')
*ptr=value
status=execute(tself+'=ptr')
return & end

;-----------------------------------------------------------------------------------------
;-- ftp login commands

function ftp::login_cmd,cmds

windows=strlowcase(os_family()) eq 'windows'
if windows then begin
 cmds=trim([self.user,self.password])
endif else begin
 cmds=['open '+self.rhost+' '+self.rport,$
       'user '+self.user+' '+self.password]
endelse

if self->getprop(/passive) then cmds=[cmds,'passive']

return,cmds & end 

;----------------------------------------------------------------------------
;-- ftp launch command

function ftp::launch_cmd,ftp_input,debug=debug

windows=strlowcase(os_family()) eq 'windows'

if windows then begin
 fcmd='ftp -i -v -d -s:'+ftp_input+' '+self.rhost
 if trim(self.rport) ne '' then fcmd=fcmd+':'+self.rport
endif else begin
 if keyword_set(debug) then flags=' -inv ' else flags=' -in ' 
 auto=self->getprop(/auto)
 if auto then flags=str_replace(flags,'n','')
 fcmd='ftp '+flags+' < '+ftp_input
endelse
                    
return,fcmd & end

;-----------------------------------------------------------------------------
;-- ftp list method 

pro ftp::ls,files,_ref_extra=extra

self->list_cmd,'nlist',files,_extra=extra

return & end

;----------------------------------------------------------------------------
;-- ftp dir method (returns filenames & sizes in bytes)

pro ftp::dir,files,_ref_extra=extra

self->list_cmd,'dir',files,_extra=extra

return & end

;---------------------------------------------------------------------------
;-- core list command

pro ftp::list_cmd,cmd,files,count=count,err=err,_extra=extra

files='' & err='' & count=0

if (not self->valid(/list,err=err)) then return

temp_ftp=mk_temp_file('ftp.dat',/random,direc=get_temp_dir())
openw,lun,temp_ftp,/get_lun

cmds=self->login_cmd()

;-- determine remote directories

rdir=self->getprop(/rdir,count=nrdir)

;-- determine remote patterns

rpatt=self->getprop(/rpatt)
opatt=strtrim(str2arr(rpatt),2)
nrpatt=n_elements(opatt)

;-- if self.pair is set, then match each rdir with rpatt

pair=self->getprop(/pair)

if pair and (nrpatt ne nrdir) then begin
 message,'warning: unequal # of remote dirs and search patterns',/cont
endif

for j=0,nrpatt-1 do begin
 tpatt=opatt[j]
; if tpatt eq '*' then tpatt=''
 if not is_blank(tpatt) then tpatt='/'+tpatt
 if pair then tdir=rdir[j < (nrdir-1)] else tdir=rdir
 rtarget=append_arr(rtarget,tdir+tpatt,/no_copy)
endfor

rtarget=get_uniq(rtarget)

self->fbreak,rtarget,tdir,tname
blank=where(strtrim(tdir,2) eq '',bcount)
if bcount gt 0 then tdir[blank]='.'

count=n_elements(rtarget)
if is_string(cmd) then ls_cmd=cmd+' ' else ls='ls '
if os_family(/lower) eq 'unix' then ls_cmd='\'+ls_cmd
for i=0,count-1 do cmds=[cmds,ls_cmd+tdir[i]+'/'+tname[i]]
cmds=[cmds,'bye']

for k=0,n_elements(cmds)-1 do printf,lun,cmds[k]
close,lun & free_lun,lun

fcmd=self->launch_cmd(temp_ftp)
                            
rhost=self->getprop(/rhost)

;dprint,transpose(cmds)
espawn,fcmd,files,temp=rhost,/noshell,_extra=extra

file_delete,temp_ftp,/quiet

count=n_elements(files)
if n_params() eq 0 then begin
 if count eq 1 then print,files else print,transpose(files)
endif
 
return & end

;---------------------------------------------------------------------------
;-- ftp show settings method

pro ftp::show

print,''
print,'Remote hostname (RHOST): ',self->getprop(/rhost)
print,'Remote port (PORT): ',self.rport
print,'Remote password (PASS): ',self->getprop(/password)
print,'Remote username (USER): ',self->getprop(/user)
print,'Remote directory (RDIR): ',self->getprop(/rdir)
print,'Remote file names (RFILE): ',self->getprop(/rfile)
print,'Remote file pattern (RPATT): ',self->getprop(/rpatt)
print,'Local directory (LDIR): ',self->getprop(/ldir)
print,'Local file names (LFILE): ',self->getprop(/lfile)
print,'Binary copy mode (BINARY): ',self->getprop(/binary)
print,'Clobber (CLOBBER): ',self->getprop(/clobber)
print,'Pair (PAIR): ',self->getprop(/pair)
print,'Verbose (VERBOSE): ',self->getprop(/verbose)
print,''
        
help,rfile,self.rfile
help,rdir,self.rdir
help,lfile,self.lfile
help,ldir,self.ldir

return & end

;---------------------------------------------------------------------------
;-- ftp get

pro ftp::mget,ofiles,count=count,test=test,err=err,status=status,_extra=extra

ofiles='' & count=0 & err=''
status=0b

if not self->valid(err=err) then return

rhost=self->getprop(/rhost)
temp_ftp=mk_temp_file('ftp.dat',/random,direc=get_temp_dir())
cmds=self->login_cmd()

binary=self->getprop(/binary)
if self.binary then cmds=[cmds,'binary'] else cmds=[cmds,'ascii']

rfiles=self->getprop(/rfile)
lfiles=self->getprop(/lfile)
nfiles=n_elements(lfiles)

;-- if clobber is not set, then only copy non-existing files
 
clobber=self->getprop(/clobber)
for i=0,n_elements(rfiles)-1 do begin
 copy=1b
 if (not clobber) then begin
  cfile=self->loc_file(lfiles[i],count=lcount)
  copy=lcount eq 0 
 endif
 if copy then begin
  t_cmds=append_arr(t_cmds,['get '+rfiles[i]+' '+lfiles[i]],/no_copy)
  t_lfiles=append_arr(t_lfiles,lfiles[i],/no_copy)
 endif
endfor

if exist(t_cmds) then begin
 cmds=[cmds,t_cmds]
 cmds=[cmds,'bye']

 openw,lun,temp_ftp,/get_lun
 for k=0,n_elements(cmds)-1 do printf,lun,cmds[k]
 close,lun & free_lun,lun

 if keyword_set(test) then begin
  print,rd_ascii(temp_ftp)
  file_delete,temp_ftp,/quiet
  return 
 endif
 fcmd=self->launch_cmd(temp_ftp)
                            
 rhost=self->getprop(/rhost)
 espawn,fcmd,null,temp=rhost,/noshell,_extra=extra
 file_delete,temp_ftp,/quiet

;-- verify copy

 ofiles=loc_file(lfiles,count=count,/recheck) 
 if count eq 0 then message,'FTP unsuccessful',/cont else begin
  status=1b
  chmod,ofiles,/g_write,/g_read
  message,'Have '+trim(count)+' of '+trim(nfiles)+' files',/cont
  message,/noname,'Successfully copied '+trim(n_elements(t_lfiles))+' new file(s)',/cont
 endelse
endif else begin
 message,'No new files copied (clobber='+trim(self.clobber)+')',/cont
 ofiles=self->loc_file(lfiles,count=count,/recheck)
endelse

return & end

;---------------------------------------------------------------------------

function ftp::valid,list=list,err=err

;-- validate properties

rhost=self->getprop(/rhost)
if not is_string(rhost) then begin
 err='remote host name not specified'
 message,err,/cont
 return,0b
endif

if not is_string(self.user) then begin
 err='remote user name not specified'
 message,err,/cont
 return,0b
endif

if not is_string(self.password) then begin
 err='remote password not specified'
 message,err,/cont
 return,0b
endif

rdir=self->getprop(/rdir,count=nrdir)
if not is_string(rdir) then begin
 err='remote directory name(s) not specified'
 message,err,/cont
 return,0b
endif

if (1-keyword_set(list)) then begin

;-- make sure remote files have an associated remote directory

 rfile=self->getprop(/rfile,count=nrfile)  
 if not is_string(rfile) then begin
  err='remote filename(s) not specified'
  message,err,/cont
  return,0b
 endif

;-- if remote filenames do not have remote directories, then we use RDIR

 self->fbreak,rfile,dir,rname
 chk=where(dir eq '',count)
 if count eq nrfile then rfile=rdir[0]+'/'+rfile else begin
  if (count ne 0) then begin
   if (nrdir ne nrfile) then begin
    err='# of remote directories and filenames do not match'
    message,err,/cont
    return,0b
   endif         
   rfile[chk]=rdir[chk]+'/'+rfile[chk]
  endif
 endelse

 self->setprop,rfile=rfile

;-- make sure local files have an associated remote file and local directory
   
;-- if local filename is a single blank string, then we copy all
;   remote files to same name

 lfile=self->getprop(/lfile,count=nlfile)
 if (nlfile eq 1) and (lfile[0] eq '') then lfile=rname
 nlfile=n_elements(lfile)

 if nlfile ne nrfile then begin
  err='# of remote and local files do not match'
  message,err,/cont
  return,0b
 endif               

;-- if several local filenames are blank, then we use their remote names

 chk=where(lfile eq '',count)
 if count gt 0 then lfile[chk]=rname[chk]

;-- if local filenames do not have local directories, then we use LDIR

 self->fbreak,lfile,dir
 chk=where(dir eq '',count)
 if count gt 0 then begin
  ldir=self->getprop(/ldir)
  lfile[chk]=ldir[0]+get_delim()+lfile[chk]
 endif

 self->setprop,lfile=lfile
                     
;-- finally check for write access to local directories

 self->fbreak,lfile,dir
 chk=test_dir(dir,err=err)
 if err ne '' then return,0b
                      
endif

return,1b
end

;--------------------------------------------------------------------------
;-- ping object host

function ftp::ping

if is_string(self.rhost) then return,byte(is_alive(self.rhost) > 0) else $
 return,0b
end

;--------------------------------------------------------------------------
;-- LOC_FILE wapper

function ftp::loc_file,file,_ref_extra=extra

return,loc_file(file,_extra=extra)

end

;---------------------------------------------------------------------------
;-- define FTP structure 

pro ftp__define                 

rfile=ptr_new()
rdir=rfile
lfile=rfile
ldir=rfile

ftp_struct={ftp, rhost:'',$
                 rport:'',$
                 user:'',$
                 password:'',$
                 rfile:rfile,$
                 rdir:rdir,$
                 lfile:lfile,$
                 ldir:ldir,$
                 rpatt:'',$
                 clobber:0b,$
                 binary:1b,$
                 pair:0b,$
                 auto:0b,$
                 passive:0b,$
                 inherits gen}
return & end

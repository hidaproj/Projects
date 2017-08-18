;+
; Project     : SOHO - CDS
;
; Name        : UNIX_SPAWN
;
; Purpose     : spawn a shell command and return STDIO and STDERR
;
; Category    : System
;
; Explanation : regular IDL spawn command doesn't return an error message
;
; Syntax      : IDL> unix_spawn,cmd,out
;
; Inputs      : CMD = command(s) to spawn
;
; Keywords    : NOERROR = inhibit error output
;               COUNT = n_elements(out)
;               BACKGROUND = background the commands
;               TEMP_NAME = temporary name for spawned command files
;               NOSHELL = bypass shell
;
; Outputs     : OUT = output of CMD
;
; History     : Version 1,  18-Jan-2001, Zarro (EIT/GSFC)
;               20-Jan-01, Zarro (EITI/GSFC) - added IDL 5.4 capability
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

pro unix_spawn,cmd,out,count=count,noerror=noerror,background=background,$
                  test=test,old=old,temp_name=temp_name,noshell=noshell

out=''
count=0
if datatype(cmd) ne 'STR' then return
os=os_family(/lower)
if os ne 'unix' then return
want_out=n_params() eq 2
want_error=1-keyword_set(noerror)
noshell=keyword_set(noshell)
back=keyword_set(background)
if back then noshell=0b

;-- create temporary command file (BATCH_FILE) in which to insert all commands. 
;   This command is then executed by spawn once.

temp_dir=get_temp_dir()
pid='_'+get_rid()
if is_blank(temp_name) then temp_name='espawn_batch'

batch_file=concat_dir(temp_dir,temp_name)+pid+'.csh'


ncmd='"'+batch_file+'"'
if back then ncmd='"'+batch_file+' & '+'"'

;-- the new way

old=keyword_set(old)
flag=''
dprint,'% UNIX_SPAWN: noshell ',noshell

if idl_release(lower=5.4,/inc) and (not old) then begin
 state='spawn,'+ncmd
 if noshell then begin
  state=state+',/noshell'
  flag='-f'
 endif
 if want_out then begin
  state=state+',out,count=count'
  if want_error then state=state+',/stderr'
 endif
 file_append,batch_file,['#!/bin/csh '+flag,cmd,'exit'], /new
 spawn,['chmod','+x',batch_file],tout,/noshell
 if keyword_set(test) then begin
  message,'testing...',/cont
  print,state
  stop
 endif
 status=execute(state)
 goto,done
endif

;-- the old way

ncmds=n_elements(cmd)
tcmd=cmd
temp_file=concat_dir(temp_dir,'temp')
if is_batch() or not want_out then want_error=0b

if want_error then begin
 junk=temp_file+get_rid()                  
 for i=0,ncmds-1 do begin                
  tcmd(i)='('+cmd(i)+' ) >>& '+junk
 endfor
 tcmd=[tcmd,'\cat '+junk]
endif 

file_append,batch_file,['#!/bin/csh -f',tcmd,'exit'], /new
spawn,['chmod','+x',batch_file],tout,/noshell

arg='' & keys=''
if keyword_set(noshell) then keys=',/noshell'

if want_out then begin
 arg=',out' & keys=keys+',count=count'
endif

state=trim('spawn,'+ncmd+arg+keys)

if keyword_set(test) then begin
 message,'testing unix...',/cont
 print,tcmd
 stop
endif
s=execute(state)

;-- clean-up

done:
if n_elements(out) eq 1 then out=out(0)
if keyword_set(background) then return
if want_error then rm_file,junk
rm_file,batch_file

return & end

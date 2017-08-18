;+
; Project     : SOHO - CDS     
;                   
; Name        : APPLY_LOCK
;               
; Purpose     : create a LOCK file 
;               
; Category    : Planning
;               
; Explanation : creates a LOCK file with the creation date saved in file.
;               
; Syntax      : IDL> apply_lock,lock_file
;
; Inputs      : LOCK_FILE = lock file name (with path)
;               
; Keywords    : ERR - message string
;               QUIET - turn off printing
;               EXPIRE - seconds after which LOCK file expires
;
; History     : Version 1,  17-July-1996,  D M Zarro.  Written
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-            

pro apply_lock,lock_file,err=err,quiet=quiet,expire=expire,status=status

err='' & status=1
noisy=(1-keyword_set(quiet))

;-- valid input string filename?

if datatype(lock_file) ne 'STR' then begin
 err='Invalid input LOCK filename'
 goto,quit
endif

;-- create lock file

openw,lun,lock_file,/get_lun,err=error
if error then begin
 err='Write permission denied. No LOCK file created'
 goto,quit
endif

;-- insert creation time 

user_id=chklog('USER_ID')
if is_blank(user_id) then user_id=get_user_id()
user_pid=get_pid('/idl',tty=user_tty,count=count)
if exist(expire) then expiration=float(expire) else expiration=0.
printf,lun,!stime
printf,lun,expiration
printf,lun,user_id
printf,lun,user_pid(0)
printf,lun,user_tty(0)
close,lun & free_lun,lun

;-- allow GROUP write access to LOCK file

espawn,'chmod g+w '+lock_file,out

if noisy then message,'Created LOCK file - '+lock_file,/cont
status=1

quit:
if trim(err) ne '' then begin
 status=0 
 if noisy then message,err,/cont
endif

return & end



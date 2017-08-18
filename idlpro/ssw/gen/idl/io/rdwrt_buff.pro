;+
; Project     : HESSI
;
; Name        : RDWRT_BUFF
;
; Purpose     : read & write unformatted data buffer
;
; Category    : utility system
;
; Syntax      : IDL> rdwrt_buff,ilun,olun,chunk
;
; Inputs      : ILUN = logical unit to read from
;               OLUN = logical unit to write to
;               MAXSIZE = max size in bytes to read/write 
;
; Keywords    : BUFFSIZE = buffer size to read/write [def = 1 MB]
;               COUNTS = bytes actually read/written
;               ERR = error string
;               PROGRESS = set for progress meter
;               CANCELLED = 1 if reading was cancelled
;               SLURP = read without buffering
;               OMESSSAGE = output message (if /VERBOSE)
;
; History     : Written, 6 May 2002, D. Zarro (L-3Com/GSFC)
;             : Modified, 20 Sept 2003, D. Zarro (L-3Com/GSFC) 
;               - added extra error checks
;               Modified, 26 Dec 2005, Zarro (L-3Com/GSFC) 
;               - improved /VERBOSE output
;
; Contact     : dzarro@solar.stanford.edu
;-

pro rdwrt_buff,ilun,olun,maxsize,buffsize=buffsize,counts=counts,err=err,$
               progress=progress,_extra=extra,omessage=omessage,$
               verbose=verbose,cancelled=cancelled,slurp=slurp

cancelled=0b & err='' & counts=0l

;-- input checks

if not is_number(ilun) then return
if not is_number(olun) then return
if not (fstat(ilun)).open then return
if not (fstat(olun)).open then return
if not is_number(maxsize) then return
if not is_number(buffsize) then buffsize=1000000l

on_ioerror,done

;-- output presentation

if keyword_set(slurp) then buffsize=maxsize

show_progress=0b
if allow_windows() then begin
 show_verbose=keyword_set(verbose)
 show_progress=keyword_set(progress) 
 case 1 of
  show_progress: begin
   if (buffsize lt maxsize) then $ 
    pid=progmeter(/init,button='Cancel',_extra=extra,input=omessage) else begin
     xtext,omessage,/just_reg,wbase=wbase
     xkill,wbase
   endelse
  end
  show_verbose: begin
   if is_string(omessage) then begin
    for i=0,n_elements(omessage)-1 do message,omessage[i],/cont,noname=(i gt 0)    
   endif
  end
  else:do_nothing=1
 endcase
endif

;-- show progress bar if file size greater than buffsize (1Mb)

err_flag=1b
icounts=0l
ocounts=0l
istart=0l
repeat begin

 iend=(istart+buffsize-1) < (maxsize-1)
 bsize=iend-istart+1l
 if not exist(old_bsize) then data=bytarr(bsize,/nozero) else begin
  if bsize lt old_bsize then data=temporary(data[0:bsize-1])
 endelse

 if show_progress then begin
  val = float(icounts)/float(maxsize)
  dprint,'% val: ',val
  if val lt 1 then begin
   if widget_valid(pid) then begin
    if (progmeter(pid,val) eq 'Cancel') then begin
     xkill,pid
     message,'Downloading cancelled',/cont
     cancelled=1b
     on_ioerror,null
     return
    endif
   endif
  endif
 endif

;-- read and write buffsize bytes 

 readu,ilun,data,transfer=icount
 icounts=icounts+icount

retry:
 writeu,olun,data,transfer=ocount
 ocounts=ocounts+ocount
 istart=istart+icount
 old_bsize=bsize
endrep until (iend eq (maxsize-1))

;-- wrap up

err_flag=0b
done:
error=0
catch,error

;-- check if apparent end-of-file reached (can happen with proxy servers)

if (iend lt (maxsize-1)) then begin
 if eof(ilun) then begin
  err='EOF reached prematurely. Aborting...'
  message,/cont
  icount=(fstat(ilun)).transfer_count
  help,icount
  goto,bail
  data=data[0:icount-1]
  goto,retry
 endif
endif

if error ne 0 then begin
 message,err_state(),/cont
 catch,/cancel
 err_flag=1b
endif
 
on_ioerror,null

if err_flag or (icounts ne ocounts) then begin
 err='Problems with buffered read/write. Aborting...'
 message,err,/cont
 icount=(fstat(ilun)).transfer_count
 help,icount
 goto,bail
 if bsize eq maxsize then begin
  message,'Correcting...',/cont
  err=''
  goto,bail
  data=data[0:icount-1]
  goto,retry
 endif
 return
endif

counts=ocounts

bail:
xkill,wbase
xkill,pid
delvarx,data

return

end

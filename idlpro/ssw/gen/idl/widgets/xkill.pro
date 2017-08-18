;+
; Project     :	SDAC
;
; Name        :	XKILL
;
; Purpose     :	To kill a bunch of X widgets
;
; Explanation :	
;
; Use         :	XKILL,ID1,ID2,..............ID10
;              
; Inputs      :	IDn = widget id (or name) to kill
;
; Opt. Inputs : None.
;
; Outputs     :	None.
;
; Opt. Outputs:	None.
;
; Keywords    :	ALL = if set, then destroy all active widgets
;               WAIT = wait specified seconds before killing
;               NOCLEAN = do not kill related widget ID's
;
; Calls       :	None.
;
; Common      :	None.
;
; Restrictions:	Up to 10 specific widgets can be killed
;
; Side effects:	Specified widgets and their relations will be killed
;
; Category    :	Widgets
;
; Prev. Hist. :	None.
;
; Written     :	Dominic Zarro (ARC)
;
; Version     :	Version 1.0, 18 September 1993
;-

pro xkill,id0,id1,id2,id3,id4,id5,id6,id7,id8,id9,all=all,wait=wait,$
          noclean=noclean

cleanup=(not keyword_set(noclean))
cleanup=0
on_error,1

if n_elements(wait) ne 0 then wait,wait
if keyword_set(all) then begin
 if idl_release(lower=5,/incl) then dummy = widget_base() 
 widget_control,/reset,/clear_events,bad_id=destroyed
endif else begin
 defined=0
 for i=0,n_params()-1 do begin
  param=strcompress('id'+string(i,'(i2)'),/rem)
  stat=execute('defined = n_elements('+param+')')
  if defined gt 0 then begin
   id=0
   stat=execute('id = '+param)
   for k=0,n_elements(id)-1 do begin
    app_name=(datatype(id(k)) eq 'STR')
    if app_name then wid=get_handler_id(id(k)) else wid=id(k)
    if xalive(wid) then begin
     if cleanup then ghosts=xghost(wid)
     widget_control,wid,/destroy,/clear_events,bad_id=destroyed
     if cleanup then xkill,ghosts
    endif
;    stat=execute('delvarx,'+param)
   endfor
  endif
 endfor
endelse

return & end

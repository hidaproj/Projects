;+
; Project     :	SOHO/CDS
;
; Name        : XACK
;
; Purpose     : Make user acknowledge an action
;
; Use         : xack
;
; Inputs :    : ARRAY = message to user
;
; Outputs     : RESULT = result of DIALOG_MESSAGE
;
; Keywords    : 
;               GROUP = widget ID of calling widget.
;               SPACE = lines of border space surrounding message text.
;               INSTRUCT = optional instruction to supersede "Acknowlege" 
;               TITLE = Title of the pop-up widget
;               WARN  = set to call IDL warning function WIDGET_MESSAGE
;               TURN_OFF = set to show suppress future message button
;               BACK = set to unsuppress turned-off message
;               STOP_ICON = display STOP icon
;               SINSTRUCT = suppression instructions
;;
; Written     :	Version 1, Zarro (ARC/GSFC) 12 October 1994
;
; Modification: Version 2, April 19, 1996, Liyun Wang, GSFC/ARC
;                  Added TITLE keyword
;               Version 3, Sept 19, 2000, Zarro (EIT/GSFC)
;                  Updated for IDL 5
;               Version 4, May 8, 2002, Zarro (L-3Com/GSFC)
;                  Added call to DIALOG_MESSAGE
;
;-

pro xack_event,  event                         ;event driver routine

common xack_com,simage,searched,smess

widget_control, event.id, get_uvalue = uservalue
if not exist(uservalue) then uservalue=''
uservalue=trim(uservalue)

;-- force dialog box to foreground

if (uservalue eq 'push') then begin
 xshow,event.top
 widget_control,event.top,timer = 1
 return
endif

if (uservalue eq 'suppress') then s=suppress_message(smess,/add)

if uservalue eq 'close' then xkill,event.top

return & end

;--------------------------------------------------------------------------- 

pro xack,array,result,group=group,space=space,modal=modal,$
         bfont=bfont,instruct=instruct,icon=icon,stop_icon=stop_icon,$
         flash=flash,title=title,tfont=tfont,warn=warn,sinstruct=sinstruct,$
         turn_off=turn_off,back=back,suppress=suppress,_extra=extra,$
         dialog=dialog


if is_blank(array) then return

if not allow_windows() then begin
 message,arr2str(array,delim=' '),/cont
 return
endif

common xack_com
remove=keyword_set(back)

;-- check if this message is being suppressed

suppress=keyword_set(turn_off) or keyword_set(suppress)
if suppress_message(array,remove=remove) then begin
 dprint,'% XACK: message suppressed'
 return
endif

;--  if not passed as keyword, try to figure out group leader ID from caller

if not xalive(group) then begin
 group_le=get_handler_id(get_caller())
 if xalive(group_le) then group=group_le
endif                                                                  

new_vers=float(strmid(!version.release,0,3)) ge 4.
ready=keyword_set(dialog)
if new_vers and (not suppress) and ready then begin
 wbase=widget_mbase(/column,group=group,/modal,map=0)
 result=call_function('dialog_message',arr2str(array,delim=''),_extra=extra,/error,$
                       dialog_parent=wbase)
 xrealize,wbase,group=group,_extra=extra,/center,/screen
 xkill,wbase
 return
endif

;-- make widgets

mk_dfont,bfont=bfont,tfont=tfont

if datatype(title) ne 'STR' then title = ' '
wbase=widget_mbase(title=title,/column,uvalue='push',group=group,/modal)

;-- read icon file

if n_elements(searched) eq 0 then searched=0
sz=size(simage)
found=(sz(0) eq 2)
gif_supp=1-idl_release(lower=5.4,/inc)
icon=gif_supp and (keyword_set(icon) or keyword_set(stop_icon))
if icon then begin
 if n_elements(xsize) eq 0 then  xsize=64
 if n_elements(ysize) eq 0 then  ysize=64
 if not searched then begin
  look=loc_file('stop.gif',path=get_lib(),count=nf)
  if nf gt 0 then found=1
  if found then begin
   call_procedure,'read_gif',look(0),image,r,g,b
   simage=congrid(image,xsize,ysize)+!d.table_size-2
  endif
  searched=1
 endif
endif

if icon and found then begin
 tvlct,rs,gs,bs,/get
 r=rs & g=gs & b=bs
 junk = WIDGET_BASE(wbase, /row)
 draw=widget_draw(junk,xsize=xsize,ysize=ysize,uvalue='icon',$
                   retain=2,/button_event,/frame)
endif

if datatype(instruct) eq 'STR' then mess=instruct else mess='OK'

if datatype(array) eq 'STR' then begin
 if n_elements(space) eq 0 then begin
  sy=(n_elements(array) < 5)
  blank=replicate('',(sy/2 > 3))
 endif else begin
  if space gt 0 then blank=replicate('',space)
 endelse
 if n_elements(blank) gt 0 then sarr=[blank,array,blank] else sarr=array
 narr=n_elements(sarr)
 tysize=narr < 20
 wtext=widget_text(wbase,xsize=max(strlen(array)) > strlen(mess),$
                   ysize=tysize,value=sarr,font=tfont,scroll=narr gt 20)
 smess=array
endif
 
row2=widget_base(wbase,/column,/align_center)
c1=widget_base(row2,/row)
ackb=widget_button(c1,uvalue='close',/no_release,font=bfont,$
                   /frame,value=mess)

if suppress then begin
 row3=widget_base(wbase,/column)
 c2=widget_base(row3,/row)
 if datatype(sinstruct) eq 'STR' then supp_mess=sinstruct else $
  supp_mess='Do not show this message again'
 xmenu,supp_mess,c2,/column,/nonexclusive,uvalue='suppress',font=bfont
endif

                                                                       
;-- realize 

xrealize,wbase,group=group,_extra=extra,/screen

if exist(flash) and xalive(wtext) and (datatype(sarr) eq 'STR') then begin
 for i=0,flash-1 do begin
  wait,1
  widget_control,wtext,set_value=''
  widget_control,wtext,set_value=sarr
 endfor
endif

sav_index=-1
if icon and found then begin
 maxc=!d.table_size-1
 widget_control,draw,get_value=win_index
 r(maxc-1:maxc)=[255,255] & b(maxc-1:maxc)=[0,255] & g(maxc-1:maxc)=[0,255]
 tvlct,r,g,b
 sav_index=!d.window
 wset,win_index
 tv,simage
endif

;-- start timer event for pushing main widget to foreground

new_vers=timer_version()
if new_vers then widget_control,wbase, timer=1

;-- Make a beep if ICON is used

if icon then bell

xmanager,'xack',wbase,group=group,/modal
xmanager_reset,wbase,group=group,/modal

;-- set things back

if icon and found then tvlct,rs,gs,bs
if sav_index gt -1 then begin
 device,window=wind
 clook=where(sav_index eq wind,count)
 if count gt 0 then wset,sav_index
endif

return & end


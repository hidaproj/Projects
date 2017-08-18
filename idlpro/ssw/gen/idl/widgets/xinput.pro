;+
; Project     :	SOHO - CDS
;
; Name        : XINPUT
;
; Purpose     : Allow user to input text
;
; Use         : xinput,text
;
; Inputs      : None.
;
; Opt. Inputs : INSTRUCT = instructions for user
;
; Outputs     : TEXT = string response text entered by user
;
; Opt. Outputs: None.
;
; Keywords    :
;              GROUP = group leader of caller
;              MODAL = modal (make caller insensitive)
;              XOFF,YOFF = device (x,y) offsets of XINPUT base relative to caller
;              TFONT = text widget font
;              BFONT = button widget font
;              YSIZE = ysize of input text widget
;              TITLE = title for main base
;              STATUS = 0/1 if CANCELLED/ACCEPTED
;              ACCEPT_ENTER = Set to make ENTER key act as ACCEPT
;              NOBLANK = if set, the user cannot exit without typing something
;
; Category    : Widgets
;
; Written     :	Version 1, Zarro (ARC/GSFC) 23 October 1994
;
; Modified:   : Version 2, Liyun Wang, GSFC/ARC, March 24, 1995
;                  added MAX_LEN keyword
;               Version 3, Liyun Wang, GSFC/ARC, March 29, 1995
;                  Made the widget's width be at least MAX_LEN if
;                  MAX_LEN is passed
;               Version 4, Zarro, GSFC, August 15 1996
;                  Added second XMANAGER call to catch a problem
;                     occuring on SUMER's VMS system.
;                  Limited instruction window to maximum of 25 rows
;                     instead of 45 rows --- LYW
;               Version 5, SVHH, UiO, 22 October 1996
;                  Added /ACCEPT_ENTER keyword.
;               Version 6, DMZ, GSFC, 24 October 1996
;                  Initialize cursor position in text widget prior to
;                  exiting. This helps avoid IDL FONT error.
;               Version 7, DMZ, GSFC, 4 October 1996
;                  Added /NOBLANK
;               Kim, 28 March 2000.  Took /modal off xmanager call, and added group
;		and modal to call to widget_base if using > Version 5.0.
;		Kim, 6 April 2000.  Redid change of 28 March.
;               Modified, 8 April 2000 (Zarro, SM&A/GSFC) - wrapped first
;               call to widget base with call_function in case pre-5 compilers
;               complain about having modal keyword embedded.
;
;-

pro xinput_event,  event                         ;event driver routine

on_error,1

widget_control,event.top, get_uvalue = unseen
info=get_pointer(unseen,/no_copy)

if datatype(info) ne 'STC' then return

widget_control, event.id, get_uvalue = uservalue

if not exist(uservalue) then uservalue=''

if (uservalue eq 'push') then begin
 xshow,event.top
 widget_control,event.id,timer=.5
 goto,exit
endif

wtype=widget_info(event.id,/type)

;-- button widgets

if wtype eq  1 then begin
 bname=strtrim(uservalue,2)
 widget_control,info.wtext,get_value=text
 text=trim(text(0))
 widget_control,info.wtext,set_value=text
 if bname eq 'CANCEL' then begin
  info.status=0 & quit=1
 endif else quit=(text ne '') or (info.blank)
 if quit then begin
  xtext_reset,info
  xkill,event.top
 endif
endif

;-- text widgets

if wtype eq 3 then begin
 widget_control,event.id,get_value=text
 text=trim(text(0))
 if (text ne '') or (info.blank) then begin
  info=rep_tag_value(info,text,'text')
  if event.type eq 0 and info.accept then begin
   if event.ch eq 10b then begin
    xtext_reset,info
    xkill,event.top
   endif
  endif
 endif
endif

exit: set_pointer,unseen,info,/no_copy
return & end

;---------------------------------------------------------------------------

pro xinput,text,instruct,group=group,tfont=tfont,bfont=bfont,_extra=extra,$
           modal=modal,max_len=max_len,ysize=ysize,status=status,title=title,$
           accept_enter=accept_enter,noblank=noblank

on_error,1

if not have_widgets() then begin
 message,'widgets unavailable',/cont
 return
endif

caller=get_caller(stat)
if (stat) and (not xalive(group)) then xkill,/all

;-- fonts

mk_dfont,bfont=bfont,tfont=tfont

;-- make widgets

if datatype(title) ne 'STR' then mtitle='XINPUT' else mtitle=title

; Added group and modal keywords to widget_base if > Version 5.0 and group
; keyword is passed in and the group widget is alive.
; If the calling widget did not itself use "widget_base(/modal)", then don't do
; it here.  (If the calling widget used "xmanager,/modal", and we use
; "widget_base(/modal)" here, then nothing works.  widget_info(/modal) will return
; 0 if calling routine used "xmanager,/modal".)
; If can't set modal here, then set it below in xmanager call.  Kim, 3/28/00

modal_base = 0
if since_version('5.0') and xalive(group) then $
	if widget_info (group, /modal) then modal_base = 1
if modal_base then $
	wbase=call_function('widget_base',title=mtitle,/column, group=group, modal=modal) $
	else $
	wbase=widget_base(title=mtitle,/column)

;-- input text is default value

if n_elements(ysize) eq 0 then ysize=1
if datatype(text) eq 'STR' then def_value=text else def_value=''
def_value=strtrim(def_value,2)
text=def_value
if n_elements(max_len) eq 0 then max_len=0
sz=size(def_value)
if sz(0) eq 1 then ysize=(sz(1) < 10)
xsize=max(strlen(text))
if max_len gt 0 then xsize=xsize > max_len

;-- instruction box

if datatype(instruct) eq 'STR' AND keyword_set(instruct) then begin
 comment=instruct
 comment=[' ',comment,' ']
 csize = N_ELEMENTS(comment) < 25
 row1=widget_base(wbase,/column)
 wtext = WIDGET_TEXT(row1, xsize=MAX(STRLEN(instruct)) > xsize, $
                     ysize=csize, value=comment, font=tfont, $
                     scroll=csize GT 24)
endif

wtext=widget_text(wbase,xsize=xsize,ysize=ysize,/editable,/all,font=tfont,value=def_value)
row2=widget_base(wbase,/row)
temp1=widget_base(row2,/row)
quit=widget_button(temp1,value='CANCEL',uvalue='CANCEL',font=bfont,/no_rel)
temp2=widget_base(row2,/row)
ok=widget_button(temp2,value='ACCEPT',uvalue='ACCEPT',font=bfont,/no_rel)

;-- realize and position

xrealize,wbase,group=group,/screen,_extra=extra

;-- set text insertion point

if n_elements(def_value) eq 1 then text_sel= strlen(def_value)+1  else $
 text_sel=1

;-- invisible base in which to hold variables common to event handler

make_pointer,unseen
info={text:def_value,status:1,wtext:wtext,accept:keyword_set(accept_enter),$
      blank:1-keyword_set(noblank)}

set_pointer,unseen,info,/no_copy

;-- set up timer

if timer_version() then widget_control,row2,set_uvalue='push',timer=.5
widget_control,wbase,set_uvalue = unseen

; if didn't set modal on widget_base call, set it here.  Kim 3/28/00
xmanager,'xinput',wbase,group=group, modal=(modal_base eq 0)

if xalive(wbase) then xmanager

;-- retrieve user text

info=get_pointer(unseen,/no_copy)
free_pointer,unseen

;-- shorten string

if datatype(info) eq 'STC' then status=info.status else status=0

if not status then text=def_value else begin
 text=info.text
 if (max_len gt 0) then begin
  for i=0,n_elements(text)-1 do begin
   if max(strlen(text(i))) gt max_len then text(i)=strmid(text(i),0,max_len)
  endfor
 endif
endelse

if n_elements(text) eq 1 then text=text(0)
xshow,group

return & end



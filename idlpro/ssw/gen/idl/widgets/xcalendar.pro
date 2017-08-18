;+
; Project     : SOHO - CDS
;
; Name        : XCALENDAR
;
; Purpose     : widget interface to calendar 
;
; Category    : widgets
;
; Explanation :
;
; Syntax      : IDL> xcalander,date
;
; Inputs      : None
;
; Opt. Inputs : None
;
; Outputs     : DATE = selected date [ yy/mm/dd]
;
; Opt. Outputs: None
;
; Keywords    : GROUP = widget ID of any calling widget
;               MODAL = set to freeze calling widget
;
; Common      : None
;
; Restrictions: None
;
; Side effects: None
;
; History     : Version 1,  26-Dec-1995,  D.M. Zarro, L. Wang.  Written
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

pro xcalendar_event,event

;-- get pointer values

child=widget_info(event.top,/child)
if not exist(child) then return
widget_control,child, get_uvalue = unseen
info=get_pointer(unseen,/no_copy)
if datatype(info) ne 'STC' then return

widget_control, event.id, get_uvalue = uservalue
if (n_elements(uservalue) eq 0) then uservalue=''
bname=strtrim(uservalue,2)

;-- get timer event first

if (bname eq 'update') then begin
 if info.ut then begin
  get_utc,ctime, /ecs,/time
  add_on=' UT'
 endif else begin
  ctime=anytim2utc(!stime,/ecs,/time)
  add_on=''
 endelse
 
 widget_control,event.top,tlb_set_title='XCALENDAR '+strmid(ctime,0,8)+add_on
 widget_control,event.top, timer =info.delay
 goto,quit
endif

;-- in case user forgot to hit return

widget_control,info.ybase,get_value=ystring
ystring=strmid(strtrim(ystring(0),2),0,4)
if not is_number(ystring) then goto,quit

if (long(ystring) ne long(info.year)) and long(ystring) gt 0 then begin
 info.year=ystring & xcalendar_fill,info
endif

case bname of 

 'back': begin
   info.year=strtrim(string(long(info.year)-1 > 1),2)
   xcalendar_fill,info
  end

 'forward': begin
   info.year=strtrim(string(long(info.year)+1),2)
   xcalendar_fill,info
  end

 'done': xkill,event.top

 'month': begin
   get_months,months
   info.month=months(event.index)
   xcalendar_fill,info
  end

 else: begin
  day=strtrim(bname,2)
  if day ne '' then begin
   if xalive(info.last_button) then widget_control,info.last_button,/sens
   info.last_button=event.id
   widget_control,info.last_button,sensitive=0
   widget_control,info.ybase,/input
   info.day=day
   widget_control,info.dlabel,set_value=info.day
  endif
 end
endcase

quit: set_pointer,unseen,info,/no_copy

return & end

;=============================================================================

pro cal_info, month, year, start_square, num_days

l_month = month - 1
months = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

day = (julday(1, 1, year) + 1) mod 7
day1_next_year = (julday(1, 1, year+1) + 1) mod 7

case ((day1_next_year + 7 - day) mod 7) of
2 :
1 : months(1) = 28; not a leap year
else: months(8) = 19; 1752
endcase

for i = 0, l_month - 1 do day = day + months(i)
start_square = (day mod 7)
num_days = months(l_month)

return

end
 
;=============================================================================

pro get_months,months

months=['January','February','March','April','May','June','July','August',$
         'September','October','November','December']
       
return & end

;=============================================================================

pro xcalendar_fill,info

get_months,months

;-- month and year labels

month=info.month
year=info.year

if datatype(month) eq 'STR' then begin
 tmonth=strtrim(strupcase(month),2)
 mlook=where(strmid(tmonth,0,3) eq strmid(strupcase(months),0,3))
 imonth=mlook(0)+1 
endif else imonth=month+1
iyear=long(year)

widget_control,info.ybase,set_value=strtrim(string(iyear),2)
if widg_type(info.mbase) eq 'DROPLIST' then $
 widget_control,info.mbase,set_droplist_select=imonth-1 else $
  widget_control,info.mbase,set_value=imonth-1
cal_info,imonth,iyear,start_day,ndays

if xalive(info.last_button) then widget_control,info.last_button,/sens
days=strarr(42)+'   '
days(start_day:start_day+ndays-1)=string(indgen(ndays)+1,'(i3)')
chk=where(strtrim(info.day,2) eq strtrim(days,2),count)
if count eq 0 then info.day=max(days)
widget_control,info.dlabel,set_value=info.day 

k=-1 
for j=0,5 do begin
 m=0
 for i=0,6 do begin
  k=k+1
  value=days(k)
  svalue=fix(strtrim(value,2))
  if svalue eq 0 then m=m+1
  desens=((svalue eq fix(info.day)) and (svalue ne 0)) 
  widget_control,info.dbase(i,j),set_value=value,set_uvalue=value,$
                 sensitive=(svalue ne 0)
  if desens then begin
   last_button=info.dbase(i,j)
   widget_control,last_button,sensitive=0
   info.last_button=last_button
  endif
  widget_control,info.wbase(j),map=(m ne 7)
 endfor
endfor
widget_control,info.ybase,/input

return & end

;=============================================================================

pro xcalendar,date,group=group,modal=modal,just_reg=just_reg,ut=ut,_extra=extra

on_error,1

;-- defaults

if not have_widgets() then begin
 message,'widgets unavailable',/cont
 return
endif

caller=get_caller(stat)
if xregistered('xcalendar') ne 0 then return
xkill,'xcalendar'
;if stat and (not xalive(group)) then xkill,

;-- load fonts

mk_dfont,lfont=lfont,bfont=bfont
base=widget_base(title='XCALENDAR',/column)


;-- operation buttons

if not keyword_set(just_reg) then begin
 row1=widget_base(base,row=1,/frame)
 doneb=widget_button(row1,value='DONE',uvalue='done',font=bfont,/no_rel)
endif

;-- current date display

row2=widget_base(base,row=1,/frame)
get_months,months

new_vers=float(strmid(!version.release,0,3)) ge 4.
if new_vers then begin
 dlabel=widget_label(row2,font=bfont)
 mbase=call_function('widget_droplist',row2,$
 value=months,uvalue='month',font=bfont) 
endif else begin
 mbase = cw_bselector2(row2,months,/return_name, uvalue='month',/no_rel,$
                       font=bfont,label_id=dlabel,label_left='   ',/lframe_lab)
endelse

back=widget_button(row2,value='<<',uvalue='back',font=bfont,/no_rel)
ybase=widget_text(row2,value='',font=bfont,xsize=4,/edit)
forw=widget_button(row2,value='>>',uvalue='forward',font=bfont,/no_rel)

;-- day labels

row=widget_base(base,row=1)
days=['SUN','MON','TUE','WED','THU','FRI','SAT']
for i=0,6 do temp=widget_button(row,value=days(i),font=bfont,/no_rel)

;-- make actual calendar base (each day is a button)

cbase=widget_base(base,row=6)
wbase=lonarr(6) & dbase=lonarr(7,6)
for j=0,5 do begin
 wbase(j)=widget_base(base,row=1)
 for i=0,6 do dbase(i,j)=widget_button(wbase(j),value='   ',font=bfont,/no_rel)
endfor

make_pointer,unseen
info={dlabel:dlabel,mbase:mbase,ybase:ybase,wbase:wbase,dbase:dbase,last_button:-1l,$
      day:'',month:'',year:'',delay:1.d,ut:0}

;-- get current date and time and label each calendar button
;-- (override with user input)

if keyword_set(ut) then begin
 get_utc,ctime, /ecs,/time
 get_utc,cdate, /ecs,/date
 info.ut=1
endif else begin
 ctime=anytim2utc(!stime,/ecs,/time)
 cdate=anytim2utc(!stime,/ecs,/date)
endelse
widget_control,base,tlb_set_title='XCALENDAR '+strmid(ctime,0,8)

if exist(date) then begin
 ok=1
 if datatype(date) ne 'STR' then ok=fix(date) ne 0 
 if ok then begin
  err=''
  tdate=anytim2utc(date,/ecs,/date,err=err) 
  ok=(err eq '')
 endif
 if ok then cdate=tdate
endif
cdate=str_sep(cdate,'/')
info.month=strtrim(months(cdate(1)-1),2)
info.day=strtrim(string(fix(cdate(2))),2)
info.year=strtrim(cdate(0),2)


;-- realize and manage

xrealize,base,group=group,_extra=extra
xcalendar_fill,info

;-- set up timer

child=widget_info(base,/child)
if timer_version() then begin
 widget_control,base,set_uvalue='update'
 widget_control,base, timer =info.delay
endif

;-- stuff info structure into pointer

set_pointer,unseen,info,/no_copy
widget_control,child,set_uvalue=unseen

modal=keyword_set(modal) or xalive(group)
expr='xmanager,"xcalendar",base,group=group,just_reg=just_reg,modal=modal'
if idl_release(lower=5,/incl) then expr=expr+',/no_block'
s=execute(expr)

xmanager_reset,base,group=group,modal=modal,/no_block

if not xalive(base) then begin
 info=get_pointer(unseen,/no_copy)
 free_pointer,unseen
 if datatype(info) eq 'STC' then date=info.day+'-'+info.month+'-'+info.year
 xshow,group
endif
 
return & end


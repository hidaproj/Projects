;+
; Project     : HESSI
;
; Name        : WIDGET_MBASE
;
; Purpose     : same as WIDGET_BASE, but allows GROUP to be undefined when
;               using /MODAL
;
; Category    : utility widgets
;
; Syntax      : IDL> base=widget_mbase(parent,_extra=extra)
;
; Inputs      : PARENT = optional parent widget
;
; Keywords    : Same as WIDGET_BASE
;
; History     : Written 7 May 2000, D. Zarro, SM&A/GSFC
;               Modified, 17 Jan 2006, Zarro (L-3Com/GSFC) 
;               - added _REF_EXTRA
;
; Contact     : dzarro@solar.stanford.edu
;-

function widget_mbase,parent,_ref_extra=extra,group=group,modal=modal,map=map

;-- can use /MODAL on a widget_base if valid group is entered. Can't use /MAP

can_use_modal=keyword_set(modal) and since_version('5.0')

if exist(parent) then begin
 if can_use_modal then begin
  if xalive(group) then $
   return,call_function('widget_base',parent,_extra=extra,group=group,/modal) else $
    return,call_function('widget_base',parent,_extra=extra)
 endif
 return,call_function('widget_base',parent,_extra=extra,group=group,map=map)
endif

if can_use_modal then begin
 if xalive(group) then $
  return,call_function('widget_base',_extra=extra,group=group,/modal) else $
   return,call_function('widget_base',_extra=extra)
endif
return,call_function('widget_base',_extra=extra,group=group,map=map)

end


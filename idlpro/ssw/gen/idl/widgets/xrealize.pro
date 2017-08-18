;+
; Project     :	SOHO CDS/SUMER
;
; Name        : XREALIZE
;
; Purpose     : control placement of realized widgets
;
; Use         : XREALIZE,WBASE
;
; Inputs      : WBASE = widget base id
;
; Opt. Inputs : None.

; Outputs     : None.

; Opt. Outputs: None.
;
; Keywords    : XOFF, YOFF = user specified or computed offsets
;               GROUP = ID of group widget relative to which the
;                       offset values are calculated
;               SCREEN = center relative to main screen
;               CENTER = center relative to GROUP (if alive)
;
; Common      : None.
;
; Restrictions: None.
;
; Side effects: WBASE is realized at specified or computed offsets
;
; Category    : Widgets
;
; Prev. Hist. : None.
;
; Written     :	Zarro (ARC/GSFC) 17 September 1996
;-

pro xrealize,wbase,xoff=xoff,yoff=yoff,group=group,screen=screen,center=center

if not xalive(wbase) then return

if (n_elements(xoff) eq 0) and (n_elements(yoff) eq 0) then begin
 if keyword_set(center) or keyword_set(screen) then begin
  offsets = get_cent_off(wbase, group, valid=valid,screen=screen)
  if valid then begin
   xoff = offsets(0) & yoff=offsets(1)
  endif
 endif
endif

realized = widget_info(wbase, /realized)
if (n_elements(xoff) eq 0) and (n_elements(yoff) eq 0) then $
 widget_control, wbase,realize=(not realized) else begin
  if exist(xoff) then sx=',tlb_set_xoff=xoff' else sx=''
  if exist(yoff) then sy=',tlb_set_yoff=yoff' else sy=''
  sw='widget_control, wbase,realize=(not realized),/map,/show'
  s=execute(sw+sx+sy)
 endelse
return & end


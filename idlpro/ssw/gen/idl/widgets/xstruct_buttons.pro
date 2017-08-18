;+
; Project     : SOHO - CDS
;
; Name        : XSTRUCT_BUTTONS
;
; Purpose     : Create buttons and title for xstruct program
;
; Explanation : Creates "Cancel" and "Commit" buttons.  Also, places the
;               title received as one of the arguments.
;
; Use         : xstruct_buttons, wbase
;
; Inputs      : WBASE = widget base to place the buttons in
;
; Opt. Inputs : None.
;
; Outputs     : None.
;
; Opt. Outputs: None.
;
; Keywords    :
;               EDITABLE = make fields editable
;               ACCEPT = name for accept button [def = commit]
;               TITLE = optional widget title
;
; Calls       : --
;
; Common      : --
;
; Restrictions: --
;
; Side effects: None.
;
; Category    : Widgets
;
; Prev. Hist. : None.
;
; Written     : Zarro (ARC/GSFC) 20 August 1994
;
; Modified    :
;   06-Aug-2004, Sandhia Bansal, Took this piece of code out of xstruct and made it into
;                                a separate procedure.
;     index of changed tags
; Version     : 1
;-


pro xstruct_buttons, wbase, editable=editable, accept=accept, title=title


;-- put DONE button in first row
row=widget_base(wbase,/row,/frame)
if editable then begin
   if datatype(accept) eq 'STR' then begin
      acc=trim(accept)
      first=strmid(acc,0,1)
      rest=strmid(acc,1,strlen(acc))
      acc=strupcase(first)+strlowcase(rest)
   endif else acc='Commit'

   abortb=widget_button(row,value='Cancel',uvalue='abort',/no_release,/frame,font=bfont)
   commb=widget_button(row,value=acc,uvalue='commit',/no_release,/frame,font=bfont)
   wtitle=widget_label(wbase,value=title)
endif else closeb=widget_button(row,value='Done',uvalue='done',/no_release,/frame,font=bfont)

if datatype(instruct) eq 'STR' then begin
   row=widget_base(wbase,/row)
   for k=0,n_elements(instruct)-1 do begin
      insb=widget_label(row,value=instruct(k),font=lfont)
   endfor
endif

return & end

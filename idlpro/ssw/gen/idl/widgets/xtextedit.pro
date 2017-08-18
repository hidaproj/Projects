;+
; Project     : SOHO - CDS     
;                   
; Name        : XTEXTEDIT
;               
; Purpose     : Simple widget text editor
;               
; Explanation : Simple editing of multi-line texts in widget programs
;
;               May also be used to send the text to any widget at the press
;               of a button (through the WIDGET_CONTROL,SET_VALUE mechanism)
;               by supplying the widget ID through the keyword SETV_ID. In
;               that case, you should also supply SETV_TEXT, which is the text
;               to be used on the "send" button.
;               
; Use         : XTEXTEDIT,TEXT
;    
; Inputs      : TEXT : The original text, may be modified by the user.
; 
; Opt. Inputs : None.
;               
; Outputs     : TEXT : Modified by user
;               
; Opt. Outputs: None
;               
; Keywords    : GROUP : Group leader
;               XSIZE,YSIZE : Size of text window, in characters
;
;               EXPLANATION : Text explaining the user what goes on..
;
; Calls       : default, datatype(), get_dfont(), xmanager
;
; Common      : None
;               
; Restrictions: ...
;               
; Side effects: ...
;               
; Category    : Widgets, text
;               
; Prev. Hist. : Needed it for XCFIT_BLOCK
;
; Written     : SVH Haugan, UiO, 10 October 1997
;               
; Modified    : Not yet.
;
; Version     : 1,  10 October 1997
;-            

PRO xtextedit_event,ev
  widget_control,ev.top,get_uvalue=info,/no_copy
  
  widget_control,ev.id,get_uvalue=uvalue
  
  CASE uvalue OF 
     
  'DONE':BEGIN
     widget_control,info.text_id,get_value=text
     handle_value,info.text_h,text,/set
     widget_control,ev.top,/destroy
     return
     ENDCASE
     
  'TEXT':BEGIN
     widget_control,ev.id,get_value=text
     handle_value,info.text_h,text,/set
     ENDCASE
     
  'SETV':BEGIN
     handle_value,info.text_h,text
     widget_control,info.setv_id,set_value=text
     ENDCASE
     
  END
  
  widget_control,ev.top,set_uvalue=info,/no_copy
END




PRO xtextedit,text,group=group,xsize=xsize,ysize=ysize,font=font,$
              explanation=explanation,setv_id=setv_id,setv_text=setv_text
  
  ffam = ['courier-medium-r-normal--14','fixed-medium-r-normal--14',$
          'terminal-medium-r-normal--14','fixed']
  
  xfont = get_dfont('*'+ffam+'*')
  
  IF xfont(0) EQ '' THEN xfont = get_dfont()

  default,text,['']
  twidth = (((max(strlen(text))+10) > 30) < 85)
  IF datatype(explanation) EQ 'STR' THEN $
     twidth = twidth > (max(strlen(explanation)) < 85)
  
  default,group,0L
  default,font,xfont(0)
  default,xsize,twidth
  default,ysize,20
  default,explanation,0
  default,setv_id,0L
  default,setv_text,'Send text'
  
  base = widget_base(/column,group=group,title='XTEXTEDIT')
  
  IF datatype(explanation) EQ 'STR' THEN BEGIN
     scroll=(n_elements(explanation) GT (ysize < 20))
     expl_id = widget_text(base,xsize=xsize,value=explanation,scroll=scroll,$
                           ysize= (n_elements(explanation) < ysize < 20),$
                           font=font)
  END
  
  text_id = widget_text(base,xsize=xsize,ysize=ysize,/scroll,value=text,$
                        /editable,/all_events,uvalue='TEXT',$
                        font=font)
  
  text_h = handle_create(value=text,/no_copy)
  
  bbase = widget_base(base,/row)
  
  done = widget_button(bbase,value='Finished editing',uvalue='DONE')
  
  IF xalive(setv_id) THEN BEGIN
     setv_b = widget_button(bbase,value=setv_text,uvalue='SETV')
  END
     
  info = {setv_id:setv_id,text_id:text_id,text_h:text_h}
  
  widget_control,base,set_uvalue=info
  
  widget_control,base,/realize
  
  xmanager,'XTEXTEDIT',base,/modal
  
  handle_value,text_h,text
END

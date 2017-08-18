;                                Time-stamp: <2010-09-20 05:32:48 satoshi>
;+
; SMART_T1OBS_GUI (AltaTest 20100413)
;
; 2016.03.30- TTI for new T1
; 2016.06.14  K. Otsuji enable parallel processing (using IDL_IDLbridge)
; 2016.06.20- TTI add 41 points observation
; 2016.08.03 TTI flat mode, 73pos, 15sec (with K.Ichimoto)
; 
; 
; To make sav file, do below
; .FULL_RESET_SESSION
; CD,'C:\Projects\idlpro\newT1'
; .COMPILE T1obs
; RESOLVE_ALL,skip=['GET_STEREO_LONLAT','GET_STEREO_ROLL']
; SAVE, /ROUTINES, FILENAME='T1obs.sav'
; 
;-

;========================include==============================;
@orcalib.pro
@utflib.pro
@set_t1param.pro
@prep_t1buffer.pro
@get_t1status.pro
@systemlib.pro
@image_smt.pro
@t1_sequence.pro



;**************************************************************
PRO QuitProcess

;--------------------------------------------------------------
COMMON LIB, gconst, wparam, svar, diag, state
COMMON bridge,bridge

  print, 'Number of fits files saved to PC1 local for today:' $
    + string(svar.FileID,format='(i10)')

  IF svar.FtpFailC GT 0 THEN BEGIN
      buttonPushed = $
        DIALOG_MESSAGE('*** Warning *** ' + $
                       'Number of fits files ftp transfer failed for today:' + $
                       string(svar.FtpFailC,format='(i10)') + $
                       '.  Do you want to transfer these files now? [OK?]')
            
      ;IF (input_key NE 'N') AND (input_key NE 'n') THEN $
        ret = RETRY_FTP()
      buttonPushed = $
        DIALOG_MESSAGE('*** fits files transfer completed ***')
  ENDIF

;  ret = LOG_FTP()
;  ret = FILTLOG_FTP()
;  ret = CMDLOG_FTP()
  
  MAKE_BEACON, /CLOSE, TimeRes = 0.d ; Close the beacon
  orcafin

  ;IF (wparam.test NE 1) THEN EXIT

  for i=0,n_elements(bridge)-1 do obj_destroy,bridge[i]

END

;**************************************************************
PRO T1OBS_Killed, widgetID
;--------------------------------------------------------------
;COMMON LIB, gconst, wparam, svar, diag, state
   ;WIDGET_CONTROL, widgetID, GET_UVALUE = state, /NO_COPY

   QuitProcess

END

;**************************************************************
PRO T1OBS_EventHdlr, event
;--------------------------------------------------------------
COMMON LIB, gconst, wparam, svar, diag, state
COMMON tf_orca, o,p,f,b,dwl_list

    IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_TIMER' THEN BEGIN
    ; This is the task that the widget performs when timer expires

 ;       WRAPPER_FILTER_INIT
        
        WIDGET_CLOCK
        
        AUTO_ENTRANCE_LID_CLOSE
        
        MAKE_BEACON, TimeRes = 5 ; [sec], make a beacon that T1obs is alive.

;        AUTO_FILTER_ZERORESET

;        WRAPPER_FILTER_GETTEMP

        START_BY_SKYCONDITION, TimeRes=5 ; [sec]

        AUTO_ENTRANCE_LID_OPEN

        PROG_SEQUENCE_START

        ; Re-register the timer
        WIDGET_CONTROL, event.top, /TIMER
    
    RETURN
    ENDIF

     ; Get the state structure stored in the user value of the window
;   WIDGET_CONTROL, event.top, GET_UVALUE = state, /NO_COPY

     ; Determine in which widget the event occurred
   CASE event.id OF

      state.wQuitButton: BEGIN
           ; Restore the state value before the widget app is destroyed
           ; so the KILL_NOTIFY procedure can still use it
;         WIDGET_CONTROL, SET_UVALUE = state, event.top, /NO_COPY

           ; Filter close
 ;        ret = FILTER_CLOSE()

           ; Exit the IDL Editor widget application
         WIDGET_CONTROL, event.top, /DESTROY

         RETURN
      ENDCASE

      state.wNormExp: BEGIN
        WIDGET_CONTROL, event.id, GET_VALUE=text
        svar.ExpNorm = FLOAT(text)
        print,svar.ExpNorm
        WIDGET_CONTROL, state.wNormExp, $
          SET_VALUE=STRING(svar.ExpNorm, format='(i6.1)')
        

        p.expo=svar.ExpNorm*10.^(-3.)   ;[Orca sec, exp usec]
   
        p=OrcaSetParam(expo=p.expo)
        WRITE_WIDGET_MESSAGE, '[Normal Exp]: Set to ' + text + ' [msec]', /FORCE
      ENDCASE
      
      state.wNormCad: BEGIN
        WIDGET_CONTROL, event.id, GET_VALUE=text
        svar.CadNorm = FLOAT(text)
        WIDGET_CONTROL, state.wNormCad, $
          SET_VALUE=STRING(svar.CadNorm, format='(i6.1)')
        WRITE_WIDGET_MESSAGE, '[Time cadence]: Set to ' + text + ' [sec]', /FORCE
      ENDCASE

      state.wNormSeqMod: BEGIN
          WIDGET_CONTROL, event.id, GET_VALUE=text
          CASE text[event.index] OF
              text[ gconst.NormalMode  ]: wparam.SqMode = gconst.NormalMode
              text[ gconst.SmallSize ]: wparam.SqMode = gconst.SmallSize
              text[ gconst.MinSize    ]: wparam.SqMode = gconst.MinSize
          ENDCASE
          WRITE_WIDGET_MESSAGE, '[' +text[event.index] + ']: Set.', /FORCE
      ENDCASE

      state.wNormSeqSt: BEGIN
          svar.AutoStart      = 1l
          wparam.TimObsStppd = gconst.Tomorrow

          ret = NORM_SEQUENCE(wparam.SqMode)
      ENDCASE

      state.wNormSeqDisp: BEGIN
        WIDGET_CONTROL, event.id, GET_VALUE=text
        CASE text[event.index] OF
          text[0]: svar.NormSeqDisp = 0
          text[1]: svar.NormSeqDisp = 1
        ENDCASE
        WRITE_WIDGET_MESSAGE, 'Show each images [' +text[event.index] + ']: Set.', /FORCE
      ENDCASE

      ;state.wNormSeqEd: print, 'wNormSeqEd'
      ;
      ;NormSeqEdが押されてたかどうかは、SEQUENCE()内でチェックされる
      ;


      state.wOneShWav: BEGIN
        WIDGET_CONTROL, event.id, GET_VALUE=text
        svar.WavOneSh = FLOAT(text)
        WIDGET_CONTROL, state.wOneShWav, $
          SET_VALUE=STRING(svar.WavOneSh, format='(i4.1)')
        WRITE_WIDGET_MESSAGE, '[OneShot Wavelength offset]: Set to ' + text + ' [pm]', /FORCE
      ENDCASE
      
      state.wOneShSt: BEGIN
           ret = TAKE_ONE_SHOT()
      ENDCASE




      state.wCustmSeqMod: BEGIN
          WIDGET_CONTROL, event.id, GET_VALUE=text
          CASE text[event.index] OF
              text[ gconst.GetDark   -100 ]: wparam.SqModeCustm = gconst.GetDark
              text[ gconst.GetFlat   -100 ]: wparam.SqModeCustm = gconst.GetFlat
              text[ gconst.FilterCk   -100 ]: wparam.SqModeCustm = gconst.FilterCk
              text[ gconst.FocusCk  -100 ]: wparam.SqModeCustm = gconst.FocusCk
              text[ gconst.Speckle  -100 ]: wparam.SqModeCustm = gconst.Speckle
              ELSE:
          ENDCASE
          WRITE_WIDGET_MESSAGE, '[' +text[event.index] + ']: Set for Custom OP.', /FORCE
      ENDCASE

      state.wCustmSeqSt: BEGIN
          ret = CUSTM_SEQUENCE(wparam.SqModeCustm)
      ENDCASE
      

      state.wPrVwWav: BEGIN
        WIDGET_CONTROL, event.id, GET_VALUE=text
        svar.WavPrVw = FLOAT(text)
        WIDGET_CONTROL, state.wPrVwWav, $
          SET_VALUE=STRING(svar.WavPrVw, format='(i4.1)')
        WRITE_WIDGET_MESSAGE, '[Preview: Wavelength offset]: Set to ' + text + ' [pm]', /FORCE
      ENDCASE

      state.wPrVwSt: BEGIN
          ret = PREVIEW()
      ENDCASE

;
;PrVwEdが押されてたかどうかは、PREVIEW()内でチェックされる
;

      state.wAutoTrackSt: BEGIN
          WIDGET_CONTROL, event.id, GET_VALUE=text
          CASE text[event.index] OF
              text[0]: svar.AutoTrack = 0
              text[1]: svar.AutoTrack = 1
          ENDCASE
          WRITE_WIDGET_MESSAGE, 'Auto tracking [' +text[event.index] + ']: Set.', /FORCE
      ENDCASE


      state.wShbrStart: BEGIN
          WIDGET_CONTROL, event.id, GET_VALUE=text
          IF ( DOUBLE(text) GE 0.1) AND ( DOUBLE(text) LE 9.9) THEN BEGIN
              WRITE_WIDGET_MESSAGE, '[Shbr Vol Auto Start]: Set to ' + text + ' [V]', /FORCE
              IF ( DOUBLE(text) NE wparam.ShbrIntObsStart ) THEN BEGIN
                 wparam.ShbrIntObsStart = DOUBLE(text)
                 WIDGET_CONTROL, state.wShbrStart, $
                 SET_VALUE=string(wparam.ShbrIntObsStart, form='(f4.1)')
              ENDIF
          ENDIF ELSE BEGIN
              WIDGET_CONTROL, event.id, SET_VALUE=string(wparam.ShbrIntObsStart, form='(f4.1)')
              WRITE_WIDGET_MESSAGE, '[Shbr Vol Auto Start]: Canceled.  (Range: [0.1 - 9.9] [V])', /FORCE
          ENDELSE
      ENDCASE

      state.wShbrStop: BEGIN
          WIDGET_CONTROL, event.id, GET_VALUE=text
          IF ( DOUBLE(text) GE 0.1) AND ( DOUBLE(text) LE 9.9) THEN BEGIN
              WRITE_WIDGET_MESSAGE, '[Shbr Vol Auto Stop]: Set to ' + text + ' [V]', /FORCE
              IF ( DOUBLE(text) NE wparam.ShbrIntObsStop ) THEN BEGIN
                 wparam.ShbrIntObsStop = DOUBLE(text)
                 WIDGET_CONTROL, state.wShbrStop, $
                 SET_VALUE=string(wparam.ShbrIntObsStop, form='(f4.1)')
              ENDIF
          ENDIF ELSE BEGIN
              WIDGET_CONTROL, event.id, SET_VALUE=string(wparam.ShbrIntObsStop, form='(f4.1)')
              WRITE_WIDGET_MESSAGE, '[Shbr Vol Auto Stop]: Canceled.  (Range: [0.1 - 9.9] [V])', /FORCE
          ENDELSE
      ENDCASE

      state.wUseShabarInf: BEGIN
          WIDGET_CONTROL, event.id, GET_VALUE=text
          CASE text[event.index] OF
              text[0]: svar.UseShabarInf = 0
              text[1]: svar.UseShabarInf = 1
          ENDCASE
          WRITE_WIDGET_MESSAGE, 'Use Shabar Info [' +text[event.index] + ']: Set.', /FORCE
      ENDCASE

      ELSE: ;dummy

   ENDCASE

     ; Reset the windows user value to the updated state structure
;   WIDGET_CONTROL, event.top, SET_UVALUE = state, /NO_COPY

END
;**************************************************************
PRO T1OBS_WIDGET,tlb

COMMON LIB,  gconst, wparam, svar, diag, state



;=== Create sub lebel for controll & status ===
slb        = WIDGET_BASE(tlb, /column, /frame)
slbd      = WIDGET_BASE(slb, /row, /align_center)
slb1      = WIDGET_BASE(slb, /row, /frame)
sl_con   = WIDGET_BASE(slb1, /column, /frame, space=10)
sl_sts   = WIDGET_BASE(slb1, /column, /frame)
slb2      = WIDGET_BASE(slb, /column, /frame, space=10)

;=== Attach a 200 x 50 draw status widget ===
wDrawSts = WIDGET_DRAW(slbd, XSIZE=300, YSIZE=40)

;=== Create sub lebel for normal operation ===
sl_norm  = WIDGET_BASE(sl_con, /column, /frame, space=5,xsize=210)

;=== Create sub lebel for take one shot ===
sl_one_sh = WIDGET_BASE(sl_con, /column, /frame, xsize=210)

;=== Create sub lebel for preview mode ===
sl_preview = WIDGET_BASE(sl_con, /column, /frame, xsize=210)

;=== Create sub lebel for custom sequence ===
sl_custm = WIDGET_BASE(sl_con, /column, /frame, xsize=210)



;=== Create sub lebel for Auto Tracking ===
sl_autotrack = WIDGET_BASE(sl_con, /column, /frame, xsize=210)

;=== Create sub lebel for Use Shabar Info ===
sl_useshabarinf = WIDGET_BASE(sl_con, /column, /frame, xsize=210)

;=== exit ===
wQuitButton   = WIDGET_BUTTON(sl_con, value="Quit",                    $
  /align_center,xsize=91, ysize=30)

dummy = WIDGET_LABEL(sl_sts,   value='>>> Status <<<')
dummy = WIDGET_LABEL(sl_norm,  value='>>> Normal Operation <<<')
dummy = WIDGET_LABEL(sl_one_sh,value='>>> Take One Shot <<<')
dummy = WIDGET_LABEL(sl_preview, value='>>> Preview Mode <<<')
dummy = WIDGET_LABEL(sl_custm, value='>>> Custom Sequence <<<')
dummy = WIDGET_LABEL(sl_autotrack, value='>>> Auto Tracking <<<')
dummy = WIDGET_LABEL(sl_useshabarinf, value='>>> Auto Run <<<') ; aka. Use Shabar Info

;=== Create sub lebel for system status ===
sl_sys  = WIDGET_BASE(sl_sts, /column)

;=== Create sub lebel for filter status ===
sl_filt = WIDGET_BASE(sl_sts, /column)

;=== Create sub lebel for camera status ===
sl_cam  = WIDGET_BASE(sl_sts, /column)

;=== Create sub lebel for telescope status ===
sl_tel  = WIDGET_BASE(sl_sts, /column)

;=== Create sub lebel for sky status ===
sl_sky  = WIDGET_BASE(sl_sts, /column)

;=== Create sub lebel for file status ===
sl_fil  = WIDGET_BASE(sl_sts, /column)

;=== entries for normal operation ===
sl_norm_b1    = WIDGET_BASE( sl_norm, /column, /align_left)

sl_norm_exp = WIDGET_BASE( sl_norm, /row, /align_left)
dummy       = WIDGET_LABEL(sl_norm_exp, value='Exposure time: ')
wNormExp  = WIDGET_TEXT( sl_norm_exp, value = STRING(svar.ExpNorm, format='(i6.1)'),$
  xsize=7, /edit)
dummy       = WIDGET_LABEL(sl_norm_exp, value='[msec]')

sl_norm_cad    = WIDGET_BASE( sl_norm, /row, /align_left)
dummy       = WIDGET_LABEL(sl_norm_cad, value='Time cadence: ')
wNormCad     = WIDGET_TEXT( sl_norm_Cad, value = STRING(svar.CadNorm, format='(i6.1)'),$
  xsize=5, /edit)
dummy       = WIDGET_LABEL(sl_norm_cad, value='[sec]')

; Build a drop list for sequence observations
wNormSeqMod = WIDGET_DROPLIST(sl_norm, VALUE=gconst.SqNames,/align_center)

sl_norm_disp=WIDGET_BASE( sl_norm, /row, /align_left)
dummy       = WIDGET_LABEL(sl_norm_disp, value='Show each images: ')
values       = ['OFF', 'ON']
wNormSeqDisp = WIDGET_DROPLIST(sl_norm_disp, VALUE=values,/align_right)

wNormSeqSt  = WIDGET_BUTTON(sl_norm, value="Seqence Start",          $
  /align_left,xsize=91, ysize=30)




wNormSeqEd  = WIDGET_BUTTON(sl_norm, value="Seqence Stop ",           $
  /align_right,xsize=91, ysize=30)


;sl_one_sh_exp = WIDGET_BASE( sl_one_sh, /row, /align_left)
;dummy       = WIDGET_LABEL(sl_one_sh_exp, value='Exposure time: ')
;wOneShExp  = WIDGET_TEXT( sl_one_sh_exp, value = STRING(gconst.ExpOneSh, format='(f4.0)'),$
;  xsize=5, /edit)
;dummy       = WIDGET_LABEL(sl_one_sh_exp, value='[usec]')

sl_one_sh_wav    = WIDGET_BASE( sl_one_sh, /row, /align_left)
dummy       = WIDGET_LABEL(sl_one_sh_wav, value='Wavelength offset: ')
wOneShWav     = WIDGET_TEXT( sl_one_sh_wav, value = STRING(svar.WavOneSh, format='(i4.1)'),$
  xsize=5, /edit)
dummy       = WIDGET_LABEL(sl_one_sh_wav, value='[pm]')
dummy =WIDGET_LABEL(sl_one_sh, value=' 0.8 [A] = 80 [pm]')

wOneShSt = WIDGET_BUTTON( sl_one_sh, VALUE="Take one shot",$
  /align_center)


; Build a drop list for sequence observations
wCustmSeqMod = WIDGET_DROPLIST(sl_custm, VALUE=gconst.CustmSqNames,/align_center)

wCustmSeqSt  = WIDGET_BUTTON(sl_custm, value="Custom Seqence Start",          $
  /align_left,xsize=132, ysize=30)

;=== entries for preview mode ===
;sl_prvw_exp = WIDGET_BASE( sl_preview, /row, /align_left)
;dummy       = WIDGET_LABEL(sl_prvw_exp, value='Exposure time: ')
;wPrVwExp  = WIDGET_TEXT( sl_prvw_exp, value = STRING(gconst.ExpPrVw, format='(f4.0)'),$
;  xsize=5, /edit)
;dummy       = WIDGET_LABEL(sl_prvw_exp, value='[usec]')

sl_prvw_wav    = WIDGET_BASE( sl_preview, /row, /align_left)
dummy       = WIDGET_LABEL(sl_prvw_wav, value='lambda offset: ')
wPrVwWav     = WIDGET_TEXT( sl_prvw_wav, value = STRING(svar.WavPrVw, format='(i4.1)'),$
  xsize=5, /edit)
dummy       = WIDGET_LABEL(sl_prvw_wav, value='[pm] 0.8A=80pm')

wPrVwSt = WIDGET_BUTTON( sl_preview, VALUE="Preview Start",$
  /align_left)
wPrVwEd = WIDGET_BUTTON( sl_preview, VALUE="Preview Stop",$
  /align_right)




;=== entries for auto tracking ===
values       = ['OFF', 'ON']
wAutoTrackSt = WIDGET_DROPLIST(sl_autotrack, VALUE=values,/align_center)

;=== entries for use shabar info ===
sl_shbr_th_start = WIDGET_BASE(sl_useshabarinf, /row, /align_left)
dummy       = WIDGET_LABEL(sl_shbr_th_start, value='Shabar Volt Auto Start: ')
wShbrStart  = WIDGET_TEXT( sl_shbr_th_start,                       $
  value = STRING(wparam.ShbrIntObsStart, format='(f4.1)'),  $
  xsize=4, /edit)
dummy       = WIDGET_LABEL(sl_shbr_th_start, value='[V]')

sl_shbr_th_stop = WIDGET_BASE(sl_useshabarinf, /row, /align_left)
dummy       = WIDGET_LABEL(sl_shbr_th_stop, value='Shabar Volt Auto Stop: ')
wShbrStop   = WIDGET_TEXT( sl_shbr_th_stop,                        $
  value = STRING(wparam.ShbrIntObsStop, format='(f4.1)'),   $
  xsize=4, /edit)
dummy       = WIDGET_LABEL(sl_shbr_th_stop, value='[V]')

values       = ['OFF', 'ON']
wUseShabarInf = WIDGET_DROPLIST(sl_useshabarinf, VALUE=values,/align_center)

;=== entries for system status ===
sl_sys_tim    = WIDGET_BASE( sl_sys, /row, /align_left,xsize=200)
dummy       = WIDGET_LABEL(sl_sys_tim, value='Time: ')
wStsSysTim  = WIDGET_TEXT( sl_sys_tim, xsize=19)
dummy       = WIDGET_LABEL(sl_sys_tim, value='[UT]')

sl_sys_ra     = WIDGET_BASE( sl_sys, /row, /align_left)
dummy       = WIDGET_LABEL(sl_sys_ra, value='Telescope RA:  ')
wStsSysRa   = WIDGET_TEXT( sl_sys_ra, xsize=11)
;    dummy       = WIDGET_LABEL(sl_sys_ra, value='[hms]')

sl_sys_dec    = WIDGET_BASE( sl_sys, /row, /align_left)
dummy       = WIDGET_LABEL(sl_sys_dec, value='Telescope DEC: ')
wStsSysDec  = WIDGET_TEXT( sl_sys_dec, xsize=11)
;    dummy       = WIDGET_LABEL(sl_sys_dec, value='[dms]')


sl_sys_lid     = WIDGET_BASE( sl_sys, /row, /align_left)
dummy       = WIDGET_LABEL(sl_sys_lid, value='T1 Enterance Lid:  ')
wStsSysLid   = WIDGET_TEXT( sl_sys_lid, xsize=11)

;sl_sys_Fpos     = WIDGET_BASE( sl_sys, /row, /align_left)
;dummy       = WIDGET_LABEL(sl_sys_Fpos, value='T1 Forcus pos:  ')
;wStsSysFpos   = WIDGET_TEXT( sl_sys_Fpos, xsize=11)



;=== entries for filter status ===
sl_filt_ofs   = WIDGET_BASE( sl_filt, /row, /align_left)
dummy       = WIDGET_LABEL(sl_filt_ofs, value='Filter Offset:    ')
wStsOffSetF = WIDGET_TEXT( sl_filt_ofs, xsize=6)
dummy       = WIDGET_LABEL(sl_filt_ofs, value='[mA]')

sl_filt_temp  = WIDGET_BASE( sl_filt, /row, /align_left)
dummy       = WIDGET_LABEL(sl_filt_temp, value='Filter Temp:   ')
wStsTempF   = WIDGET_TEXT( sl_filt_temp, xsize=16)
dummy       = WIDGET_LABEL(sl_filt_temp, value='[C]')


;=== entries for camera status ===
;sl_cam_exp    = WIDGET_BASE( sl_cam, /row, /align_left)
;dummy       = WIDGET_LABEL(sl_cam_exp, value='Camera Exposure:   ')
;wStsExpC    = WIDGET_TEXT( sl_cam_exp ,xsize=6)
;dummy       = WIDGET_LABEL(sl_cam_exp, value='[msec]')


;sl_tel_pscl   = WIDGET_BASE( sl_tel, /row, /align_left)
;dummy       = WIDGET_LABEL(sl_tel_pscl, value='Assumed plate scale: ')
;wStsPltScl  = WIDGET_TEXT( sl_tel_pscl, value=STRING(svar.PScale,format='(f6.4)'), xsize=6)
;dummy       = WIDGET_LABEL(sl_tel_pscl, value='[arcsec/pix]')

;=== entries for sky status ===
;sl_sky_img_max  = WIDGET_BASE( sl_sky, /row, /align_left)
;dummy         = WIDGET_LABEL(sl_sky_img_max, value='Max Int of Img (bin4): ')
;wStsSkyImgMax = WIDGET_TEXT( sl_sky_img_max, xsize=7)
;dummy         = WIDGET_LABEL(sl_sky_img_max, value='[DN]')
;
;sl_sky_img_int  = WIDGET_BASE( sl_sky, /row, /align_left)
;dummy         = WIDGET_LABEL(sl_sky_img_int, value='Median Int of Img center: ')
;wStsSkyImgInt = WIDGET_TEXT( sl_sky_img_int, xsize=7)
;dummy         = WIDGET_LABEL(sl_sky_img_int, value='[DN]')
;
;sl_sky_img_cnt  = WIDGET_BASE( sl_sky, /row, /align_left)
;dummy         = WIDGET_LABEL(sl_sky_img_cnt, value='(I_max - I_med) / I_med: ')
;wStsSkyImgCnt = WIDGET_TEXT( sl_sky_img_cnt, xsize=6)
;dummy         = WIDGET_LABEL(sl_sky_img_cnt, value=' [%]')

sl_sky_shabar_int  = WIDGET_BASE( sl_sky, /row, /align_left)
dummy            = WIDGET_LABEL(sl_sky_shabar_int, value='Intensity from Shabar: ')
wStsSkyShInt     = WIDGET_TEXT( sl_sky_shabar_int, xsize=9)
dummy            = WIDGET_LABEL(sl_sky_shabar_int, value='[V]')

;sl_sky_shabar_scnt = WIDGET_BASE( sl_sky, /row, /align_left)
;dummy            = WIDGET_LABEL(sl_sky_shabar_scnt, value='Scintillation from Shabar:')
;wStsSkyShScnt     = WIDGET_TEXT( sl_sky_shabar_scnt, xsize=9)
;dummy            = WIDGET_LABEL(sl_sky_shabar_scnt, value=' x 100 [%]')

;=== entries for file status ===
;sl_fil_total  = WIDGET_BASE( sl_fil, /row, /align_left)
;dummy       = WIDGET_LABEL(sl_fil_total, value='# of image-sets saved today:')
;wStsFilTot  = WIDGET_TEXT( sl_fil_total, xsize=5)

;  sl_fil_ftpf   = WIDGET_BASE( sl_fil, /row, /align_left)
;    dummy       = WIDGET_LABEL(sl_fil_ftpf, value= '# of files ftp failed:       ')
;    wStsFtpFail = WIDGET_TEXT( sl_fil_ftpf, xsize=5)

;=== message ===
sl_mess       = WIDGET_BASE( slb2, /column)
dummy       = WIDGET_LABEL(sl_mess, value='>>> messages <<<')
wMessage    = WIDGET_TEXT( sl_mess,                      $ ; create a display
  XSIZE=67, YSIZE=wparam.StsMessYm1 + 1, /SCROLL)

;=== Attach a 1024 x 1024 draw widget ===
wDraw = WIDGET_DRAW(tlb, XSIZE=gconst.WinSz_X, YSIZE=gconst.WinSz_Y)

;=== Make the window visible ===
WIDGET_CONTROL, tlb, /REALIZE

WIDGET_CONTROL, wDrawSts, GET_VALUE=wDrawStsId

; Retrieve the window ID from the draw widget.
; Set the draw widget as the current drawable area.
WIDGET_CONTROL, wDraw, GET_VALUE=wid
;WSET, wid

; Save the widget ids and other parameters to be accessed throughout
; this widget application.  This state structure will be stored
; in the user value of the window and can be retreived through the
; GET_UVALUE keyword of the IDL WIDGET_CONTROL procedure
state = {                              $
  TLB:         TLB,           $
  ;=== entries for normal operation ===
  ;            wFtpDirId:   wFtpDirId,     $
  wNormExp:    wNormExp,    $
  wNormCad:    wNormCad,       $
  wNormSeqMod: wNormSeqMod,   $
  wNormSeqSt:  wNormSeqSt,    $
  wNormSeqEd:  wNormSeqEd,    $
  wNormSeqDisp: wNormSeqDisp, $
  ;=== entries for status ===
  wStsSysTim:  wStsSysTim,    $
  wStsSysRa:   wStsSysRa,     $
  wStsSysDec:  wStsSysDec,    $
  wStsSysLid:  wStsSysLid,    $
;  wStsSysFpos: wStsSysFpos,  $
  wStsOffSetF: wStsOffSetF,   $
  wStsTempF:   wStsTempF,     $
;  wStsExpC:    wStsExpC,      $
  ;            wStsCcdTempC:wStsCcdTempC,  $
  ;            wStsCcdCPowC:wStsCcdCPowC,  $
  ;            wStsHSTempC: wStsHSTempC,   $
  ;            wStsNdTranT: wStsNdTranT,   $
;  wStsPltScl:  wStsPltScl,    $
;  wStsSkyImgMax:wStsSkyImgMax,$
;  wStsSkyImgInt:wStsSkyImgInt,$
;  wStsSkyImgCnt:wStsSkyImgCnt,$
  wStsSkyShInt:wStsSkyShInt,  $
;  wStsSkyShScnt:wStsSkyShScnt,$
;  wStsFilTot:  wStsFilTot,    $
  ;             wStsFtpFail: wStsFtpFail,   $
  ;=== entries for take one shot ===
  ;             wOneDStor:   wOneDStor,     $
;  wOneShExp:wOneShExp,$
  wOneShWav:wOneShWav,$
  wOneShSt:wOneShSt,$
  ;=== entries for custom sequences ===
  ;             wCSeqDStor:  wCSeqDStor,    $
  ;             wCSeqKFlatWav: wCSeqKFlatWav, $
  wCustmSeqMod:wCustmSeqMod,  $
  wCustmSeqSt: wCustmSeqSt,   $
  ;=== entries for preview mode ===
;  wPrVwExp:  wPrVwExp,    $
  wPrVwWav:  wPrVwWav,    $
  wPrVwSt:  wPrVwSt,    $
  wPrVwEd:  wPrVwEd,    $
  ;=== entries for focus mode ===
  ;            wFocusSt:    wFocusSt,      $
  ;=== entries for auto tracking ===
  wAutoTrackSt:wAutoTrackSt,  $
  ;=== entries for use shabar info ===
  wShbrStart:   wShbrStart,   $
  wShbrStop:   wShbrStop,     $
  wUseShabarInf:wUseShabarInf,  $
  ;=== entries for draw window ===
  wDraw:       wDraw,         $
  WID:         wid,           $
  wDrawSts:    wDrawSts,      $
  wDrawStsId:  wDrawStsId,    $
  wMessage:    wMessage,      $
  wQuitButton: wQuitButton    $
}

;  ; Save the state structure in the window's user value
;WIDGET_CONTROL, tlb, SET_UVALUE=state

END

;**************************************************************
PRO t1obs
;testmode  = 1
;LidChkOff = 1
;English   = 1
;
;--------------------------------------------------------------
COMMON LIB,  gconst, wparam, svar, diag, state
COMMON tf_orca, o,p,f,b,dwl_list
COMMON bridge,bridge

; o - obs control parameters
; f - UTF control parameters
; p - camera control parameters

SET_T1PARAM, gconst, wparam, svar, diag, testmode=testmode, LidChkOff=LidChkOff

;-----  prepare object array for parallel processing -----------

if ~isa(bridge,"Objref") then begin
  dmy=widget_base(title='T1obs',TLB_FRAME_ATTR=11,row=1,/align_center,ysize=100,xoff=800,yoff=400)
  txt=widget_label(dmy,value='Please wait....',/ALIGN_CENTER,xsize=200)
  WIDGET_CONTROL,dmy,/REALIZE

  nCPU=!CPU.HW_NCPU
  bridge=objarr(nCPU-1)
  for i=0,nCPU-2 do bridge[i]=IDL_IDLbridge()

  widget_control,dmy,/destroy
endif

;-----  initialize devices  --------------------
noDev=0 ; if 1, skip device control
utfdir='C:\Projects\data\TF40\'
utftbl='tf40def.tbl'
f=utf_init(utftbl,tbldir=utfdir,noDev=noDev)
utf_set_wl,f,dwl=0.
p=orcainit(noDev=noDev)
p=OrcaSetParam(expo=0.003,bin=1)

NDWmax=81
o={utf_obs_v0, $  ; UTF obs. control params
  nimg:   1,  $ ; # of image for 1 shot
  dt:   0., $ ; time step, sec
  extrig:   0,  $ ; 0: free, 1: extrn.trig.
  ndw:  11,     $ ; # of wavelength
  dvs:  fltarr(NDWmax), $ ; velocity (km/s), dwl/wl0*c
  fmodes: strarr(NDWmax), $ ; filter mode, 's', 'a', 'b'
  date:   '',   $ ; 'yymmdd'
  outdir:   '',   $
  fnam:   'utf',  $
  filename: ''  $
}


;=== Create a top level base ===
tlb = WIDGET_BASE(title='SMART T1 Observation', $
  /ROW, TLB_FRAME_ATTR=9)


T1OBS_WIDGET,tlb


WRITE_WIDGET_MESSAGE, 'Initialization finished.', /FORCE

     ; Register the timer
   WIDGET_CONTROL, tlb, /TIMER

     ; Register this widget application with the widget manager
   XMANAGER, 'T1OBS', tlb, modal=modal,$
     EVENT_HANDLER="T1OBS_EventHdlr", CLEANUP="T1OBS_Killed", /NO_BLOCK

END

;                                Time-stamp: <2011-08-11 17:47:09 morita>
;;; systemlib.pro
;  break_quit_check, write_widget_message, get_mtime_sts, log_ftp, retry_ftp
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO WIDGET_CLOCK

COMMON LIB, gconst, wparam, svar, diag, state

   current_time = SYSTIME(1)

  IF (current_time - svar.Clock) GE gconst.ClockRes THEN BEGIN
    systime_utc  = TAI2UTC(current_time + 378691200d0, /NOCORRECT, /EXTERNAL)
    WIDGET_CONTROL, state.wStsSysTim, SET_VALUE = UTC2STR(systime_utc,/TRUNCATE)
    svar.Clock = current_time
  ENDIF

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO WRITE_WIDGET_MESSAGE, message_in, NO_TIMESTAMP = NO_TIMESTAMP, FORCE = FORCE, $
  LOG_MODE = LOG_MODE

COMMON LIB, gconst, wparam, svar, diag, state
IF N_ELEMENTS(LOG_MODE) LT 1 THEN LOG_MODE = 0

IF KEYWORD_SET(NO_TIMESTAMP) THEN message_with_timestamp = message_in $
ELSE message_with_timestamp = systime() + ': ' + message_in

  ;*** write widget message 
  IF (LOG_MODE NE 1) AND (KEYWORD_SET(FORCE) OR (svar.SqStop NE 1)) THEN BEGIN
      WIDGET_CONTROL, state.wMessage, SET_VALUE = message_with_timestamp, $
        /APPEND, SET_TEXT_TOP_LINE = (svar.StsMesLines - wparam.StsMessYm1) > 0
      svar.StsMesLines = svar.StsMesLines + 1l
  ENDIF

;  ;*** wite command log
;  IF (LOG_MODE EQ 0) OR (LOG_MODE EQ 1) OR (LOG_MODE EQ 2) THEN BEGIN
      OPENU,       unit, gconst.pCmdLog, /GET_LUN
        dummy         =  FSTAT(unit)
        POINT_LUN, unit, dummy.size
        PRINTF,    unit, message_with_timestamp
      FREE_LUN,    unit
;  ENDIF
  
  ;*** wite filter log
  IF LOG_MODE EQ 2 THEN BEGIN
      OPENU,       unit, gconst.pFiltLog, /GET_LUN
        dummy         =  FSTAT(unit)
        POINT_LUN, unit, dummy.size
        PRINTF,    unit, message_with_timestamp
      FREE_LUN,    unit
  ENDIF

;  ;*** wite camera log
;  IF LOG_MODE EQ 3 THEN BEGIN
;  ENDIF

  ;*** update widget clock
  WIDGET_CLOCK

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO BREAK_CHECK

COMMON LIB, gconst, wparam, svar, diag, state

  IF svar.SqContinue EQ 1 THEN BEGIN

    BreakChk = WIDGET_EVENT(state.wNormSeqEd, /nowait)
    IF BreakChk.ID GT 0 THEN BEGIN
       WRITE_WIDGET_MESSAGE, '*** Sequence Stop encountered ***.'
       svar.SqContinue  = 0l
       svar.ProgSqStart = 0l
       svar.AutoStart   = 0l
       wparam.TimObsStppd = systime(1)

       WSET, state.wDrawStsId
       ERASE
       TV, BYTARR(300, 40) + 255
       ;XYOUTS, 50, 10, '!17Stop encountered', /device,charsize=1.6, color=255
       !p.font = 0
       XYOUTS, 50, 10, '!17'+diag.StopEncount, /device,charsize=1.6, color=255
       !p.font = -1
       WSET, state.wid
    ENDIF
  ENDIF
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION CLEAN_RA_POS, ra_pos, hh, mm, ss, sss

  asec     = ra_pos / 10.d 
  sss      = FIX( (asec - FIX(asec)) * 10) 
  hh       = FIX(  asec / 3600.d) 
  mm       = FIX( (asec - 3600.d*hh) / 60.d ) 
  ss       = FIX(  asec - 3600.d*hh  - 60.d*mm) 

  hh       = hh MOD 24 

  DELVARX, asec

  OUT = LONG( (ss + mm*60.d + hh*3600.d)*10.d + sss)

RETURN, OUT
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO CONV_DEC_POS, dec_pos, hh, mm, ss, sss, sign

  IF dec_pos LT 0 THEN sign = -1.d ELSE sign = 1.d
  asec     = ABS(dec_pos) / 10.d 
  sss      = FIX( (asec - FIX(asec)) * 10) 
  hh       = FIX(  asec / 3600.d) 
  mm       = FIX( (asec - 3600.d*hh) / 60.d ) 
  ss       = FIX(  asec - 3600.d*hh  - 60.d*mm) 

  DELVARX, asec

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


FUNCTION telescope_status, TelescopSts, silent=silent

COMMON LIB, gconst, wparam, svar, diag, state

  ON_ERROR,2

  ret       = 0
  check_tim = 0

  cadence_of_sts_update = FIX(gconst.CadenceSTS  * 1000); [msec]; cadence of sts-file update
  threshold_for_update  = FIX(gconst.ThTStUpdate * 1000); [msec]; threshold for waiting next chance
  wait_time             = (gconst.CadenceSTS - gconst.ThTStUpdate) $
                          + gconst.MtimeReso            ; [sec]; wate time if wait 
  bytes_one_status      = 184                           ; one line 205 bytes

  TelescopSts = { TelescopeStatus, $
                  RA_STS:   -1l,   $ ; RA status.
                  RA_POS:    0ll,  $ ; RA position.
                  DEC_STS:  -1l,   $ ; DEC status.
                  DEC_POS:   0ll,  $ ; DEC position.
                  CAM_STS1:  -1l,  $ ; Camera status 1
                  CAM_STS2:  -1l,  $ ; Camera status 2
                  CAM_POS:   -1l,   $ ; Camera position [um].
;                  ND_STS:   -1l,   $ ; ND status
;                  ND_POS:   -1l,   $ ; ND filter identification flag.
;                  ND_TRANS: -1.d,  $ ; ND filter transmission factor.
;                  FLAT_LENZ:-1l,   $ ; Flat lenz status    [0:OFF,   1:ON  ]
                  TimeStamp: 0d    $ ; Time stamp of these status
                }

  check_tim = (systime(1) - svar.Mtime_syslog) * 1000 MOD cadence_of_sts_update

  IF check_tim GT threshold_for_update THEN WAIT, wait_time

  ret = GET_T1STATUS(gconst.STS_FILE, sts)
  
  IF ret EQ 1 THEN BEGIN

    ;*** get RA_STS
    TelescopSts.RA_STS = sts.telescop.ra_sts2

    ; *** get RA_POS
    IF ( TelescopSts.RA_STS MOD 2 ) EQ 0 THEN $ 
      TelescopSts.RA_POS = sts.telescop.ra_pos

    ; *** get DEC_STS
    TelescopSts.DEC_STS = sts.telescop.dec_sts2

    ; *** get DEC_POS
    IF ( TelescopSts.DEC_STS MOD 2 ) EQ 0 THEN $
      TelescopSts.DEC_POS = sts.telescop.dec_pos

    ; *** get CAM_STS 1
    CASE sts.t1.focus_sts1 OF
      0:    TelescopSts.CAM_STS1 =  0 ;stable
      1:    TelescopSts.CAM_STS1 =  1 ;origin
      ELSE: TelescopSts.CAM_STS1 = -1 ;error
    ENDCASE
      
    ; *** get CAM_STS 2
    CASE sts.t1.focus_sts2 OF
      0:    TelescopSts.CAM_STS2 =  0 ;stable
      1:    TelescopSts.CAM_STS2 =  1 ;moving
      ELSE: TelescopSts.CAM_STS2 = -1 ;error
    ENDCASE

    ; *** get CAM_POS
;    IF ( sts.t1.focus_sts2 MOD 2 ) EQ 0 THEN $
      TelescopSts.CAM_POS = LONG(sts.t1.focus_pos/10.)

    ; *** get ND_STS
 ;   CASE sts.t1.nd_sts OF
 ;     0:    TelescopSts.ND_STS =  0 ;stable
 ;     1:    TelescopSts.ND_STS =  1 ;moving
 ;     2:    TelescopSts.ND_STS =  2 ;origin
 ;     4:    TelescopSts.ND_STS =  4 ;point
 ;     6:    TelescopSts.ND_STS =  6 ;point and origin
 ;     ELSE: TelescopSts.ND_STS = -1
 ;   ENDCASE

    ; *** get ND_POS
 ;   IF ( TelescopSts.ND_STS MOD 2 ) EQ 0 THEN BEGIN 
 ;     TelescopSts.ND_POS = sts.t1.nd_point
 ;     CASE TelescopSts.ND_POS OF
 ;       1:    TelescopSts.ND_TRANS = gconst.TransND1
 ;       2:    TelescopSts.ND_TRANS = gconst.TransND2
 ;       3:    TelescopSts.ND_TRANS = gconst.TransND3
 ;       4:    TelescopSts.ND_TRANS = gconst.TransND4
 ;       ELSE: TelescopSts.ND_TRANS = -1.d
 ;     ENDCASE
 ;   ENDIF

 ;   ; *** flat/clear
 ;   CASE sts.t1.flat_sts OF
 ;       2:    TelescopSts.FLAT_LENZ =  0 ;clear
 ;       4:    TelescopSts.FLAT_LENZ =  1 ;flat
 ;       0:    TelescopSts.FLAT_LENZ =  2 ;stopped
 ;       1:    TelescopSts.FLAT_LENZ =  3 ;moving
 ;       ELSE: TelescopSts.FLAT_LENZ = -1
 ;   ENDCASE

    ; *** entrance lid open/close
    CASE sts.t1.lid OF
      2:    svar.E_LID =  0 ;close
      4:    svar.E_LID =  1 ;open
      0:    svar.E_LID =  2 ;stable
      1:    svar.E_LID =  3 ;moving
      ELSE: svar.E_LID = -1
    ENDCASE
      
    ; *** Time stamp
    TelescopSts.TimeStamp = systime(1)

  ENDIF ELSE ret = -1

  IF (ret NE 1) THEN BEGIN
    IF KEYWORD_SET(silent) NE 1 THEN $
      WRITE_WIDGET_MESSAGE, '[TELESCOPE_STATUS()]: Failed.'
  ENDIF ELSE BEGIN  
    IF KEYWORD_SET(silent) NE 1 THEN $
      WRITE_WIDGET_MESSAGE, '[TELESCOPE_STATUS()]: Success.'

    ;*** update camera position status on widget
;    IF wparam.CameraPos NE TelescopSts.CAM_POS THEN BEGIN
;      wparam.CameraPos = TelescopSts.CAM_POS
;      WIDGET_CONTROL, state.wNormFocus, $
;        SET_VALUE=string(wparam.CameraPos, form='(i5)')
;    ENDIF

    ;*** update ND position status on widget
 ;   IF wparam.NDPos NE TelescopSts.ND_POS THEN BEGIN
 ;     wparam.NDPos = TelescopSts.ND_POS
 ;     WIDGET_CONTROL, state.wNormNd, $
 ;       SET_VALUE=string(wparam.NDPos, form='(i1)')
 ;     WIDGET_CONTROL, state.wStsNdTranT, $
 ;       SET_VALUE=string(TelescopSts.ND_TRANS*100., form='(i4)')
 ;   ENDIF
  ENDELSE

RETURN, ret
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION ELID_OPEN

COMMON LIB, gconst, wparam, svar, diag, state

  p1  = 0
  p2  = 0
  p3  = 0
  p4  = 0
  p5  = 0
  p6  = 0
  p7  = 4
  p8  = 0
  p9  = 0
  p10 = 0
  p11 = 0
  p12 = 0
  p13 = 0

  MAX_wait       = 60. ; [sec]

  cmd_file_check = FILE_TEST(gconst.DirCMD + gconst.CMD_FILE)

  ret = -1

  IF cmd_file_check NE 1 THEN BEGIN
;      openw,  9, gconst.DirCMD + gconst.CMD_FILE
      openw,  9, gconst.DirCMD + 'tmp.txt'
      printf, 9, PREP_T1BUFFER(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13)
      close,  9
      IF NOT wparam.test THEN FILE_MOVE, gconst.DirCMD + 'tmp.txt ', gconst.DirCMD + gconst.CMD_FILE  
      ret = 1
  ENDIF

  IF ret THEN BEGIN
      WRITE_WIDGET_MESSAGE, '[ELID_OPEN()]: START', /FORCE

      wait_count = 0.
      check      = 0
      WHILE ( check NE 1 ) DO BEGIN
          IF wait_count GT MAX_wait THEN BREAK

          WAIT, 0.25
          wait_count = wait_count + 0.25

          ret = TELESCOPE_STATUS( tstatus, /silent )
          IF ret THEN check = ( svar.E_LID EQ 1 )  ; 1 = open
          
          IF ( wait_count MOD 3 ) EQ 0  THEN BEGIN
            IF svar.E_LID EQ  0   THEN WRITE_WIDGET_MESSAGE, '*** Entrance Lid: Close  ***', /FORCE
            IF svar.E_LID EQ  1   THEN WRITE_WIDGET_MESSAGE, '*** Entrance Lid: Open   ***', /FORCE
            IF svar.E_LID EQ  2   THEN WRITE_WIDGET_MESSAGE, '*** Entrance Lid: Stable ***', /FORCE
            IF svar.E_LID EQ  3   THEN WRITE_WIDGET_MESSAGE, '*** Entrance Lid: Moving ***', /FORCE
            IF svar.E_LID EQ (-1) THEN WRITE_WIDGET_MESSAGE, '***  Entrance Lid: Error ***', /FORCE
          ENDIF
      ENDWHILE
  ENDIF

  Update_TelStatus


  ret = ( svar.E_LID EQ 1 )  ; 1 = open
  IF (ret NE 1) THEN                                       $
    WRITE_WIDGET_MESSAGE, '[ELID_OPEN()]: Failed.', /FORCE  $
  ELSE                                                     $
    WRITE_WIDGET_MESSAGE, '[ELID_OPEN()]: Success.', /FORCE

RETURN, ret
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION ELID_CLOSE

COMMON LIB, gconst, wparam, svar, diag, state

  p1  = 0
  p2  = 0
  p3  = 0
  p4  = 0
  p5  = 0
  p6  = 0
  p7  = 2
  p8  = 0
  p9  = 0
  p10 = 0
  p11 = 0
  p12 = 0
  p13 = 0

  MAX_wait       = 60. ; [sec]

  cmd_file_check = FILE_TEST(gconst.DirCMD + gconst.CMD_FILE)

  ret = -1

  IF cmd_file_check NE 1 THEN BEGIN
;      openw,  9, gconst.DirCMD + gconst.CMD_FILE
      openw,  9, gconst.DirCMD + 'tmp.txt'
      printf, 9, PREP_T1BUFFER(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13)
      close,  9
      IF NOT wparam.test THEN FILE_MOVE, gconst.DirCMD + 'tmp.txt ', gconst.DirCMD + gconst.CMD_FILE  
      ret = 1
  ENDIF

  IF ret THEN BEGIN
      WRITE_WIDGET_MESSAGE, '[ELID_CLOSE()]: START', /FORCE

      wait_count = 0.
      check      = 0
      WHILE ( check NE 1 ) DO BEGIN
          IF wait_count GT MAX_wait THEN BREAK

          WAIT, 0.25
          wait_count = wait_count + 0.25

          ret = TELESCOPE_STATUS( tstatus, /silent )
          IF ret THEN check = ( svar.E_LID EQ 0 )  ; 0 = close
          
          IF ( wait_count MOD 3 ) EQ 0  THEN BEGIN
            IF svar.E_LID EQ  0   THEN WRITE_WIDGET_MESSAGE, '*** Entrance Lid: Close  ***', /FORCE
            IF svar.E_LID EQ  1   THEN WRITE_WIDGET_MESSAGE, '*** Entrance Lid: Open   ***', /FORCE
            IF svar.E_LID EQ  2   THEN WRITE_WIDGET_MESSAGE, '*** Entrance Lid: Stable ***', /FORCE
            IF svar.E_LID EQ  3   THEN WRITE_WIDGET_MESSAGE, '*** Entrance Lid: Moving ***', /FORCE
            IF svar.E_LID EQ (-1) THEN WRITE_WIDGET_MESSAGE, '***  Entrance Lid: Error ***', /FORCE
          ENDIF
      ENDWHILE
  ENDIF
  
  Update_TelStatus

  ret = ( svar.E_LID EQ 0 )  ; 0 = close
  IF (ret NE 1) THEN                                       $
    WRITE_WIDGET_MESSAGE, '[ELID_CLOSE()]: Failed.', /FORCE  $
  ELSE                                                     $
    WRITE_WIDGET_MESSAGE, '[ELID_CLOSE()]: Success.', /FORCE

RETURN, ret
END



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO CAM_POS_ORG

COMMON LIB, gconst, wparam, svar, diag, state

  p1  = 0
  p2  = 0
  p3  = 0
  p4  = 0
  p5  = 0
  p6  = 0
  p7  = 0
  p8  = 1
  p9  = 8
  p10 = 0
  p11 = 0
  p12 = 0
  p13 = 0

  MAX_wait       = 180. ; [sec]

  cmd_file_check = FILE_TEST(gconst.DirCMD + gconst.CMD_FILE)

  ret = -1

  IF cmd_file_check NE 1 THEN BEGIN
;      openw,  9, gconst.DirCMD + gconst.CMD_FILE
      openw,  9, gconst.DirCMD + 'tmp.txt'
      PRINTF, 9, PREP_T1BUFFER(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13)
      close,  9
      IF NOT wparam.test THEN FILE_MOVE, gconst.DirCMD + 'tmp.txt ', gconst.DirCMD + gconst.CMD_FILE
      ret = 1
  ENDIF

  IF ret THEN BEGIN
      WRITE_WIDGET_MESSAGE, '[CAM_POS_ORG()]: Start.', /FORCE

      wait_count = 0.
      check      = 0
      WHILE ( check NE 1 ) DO BEGIN
          IF wait_count GT MAX_wait THEN BREAK

          WAIT, 0.25
          wait_count = wait_count + 0.25

          ret = TELESCOPE_STATUS( tstatus, /silent )
          IF ret THEN check = ( tstatus.CAM_STS1 EQ 1 ) AND ( tstatus.CAM_STS2 EQ 0 ) ; 1 - origin, 0 stable
      ENDWHILE
  ENDIF

  ret = ( tstatus.CAM_STS1 EQ 1 )  ; 1 - origin
  IF (ret NE 1) THEN                                       $
    WRITE_WIDGET_MESSAGE, '[CAM_POS_ORG()]: Failed.', /FORCE  $
  ELSE                                                     $
    WRITE_WIDGET_MESSAGE, '[CAM_POS_ORG()]: Success.', /FORCE

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO CAM_POS_SET, set_cam_pos

COMMON LIB, gconst, wparam, svar, diag, state

  p1  = 0
  p2  = 0
  p3  = 0
  p4  = 0
  p5  = 0
  p6  = 0
  p7  = 0
  p8  = 8
  p9  = 0
  p10 = LONG(set_cam_pos*10) ; in 0.1 um
  p11 = 0
  p12 = 0
  p13 = 0

  MAX_wait       = 10. ; [sec]

  cmd_file_check = FILE_TEST(gconst.DirCMD + gconst.CMD_FILE)

  ret = -1

  IF cmd_file_check NE 1 THEN BEGIN
;      openw,  9, gconst.DirCMD + gconst.CMD_FILE
      openw,  9, gconst.DirCMD + 'tmp.txt'
      printf, 9, PREP_T1BUFFER(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13)
      close,  9
      IF NOT wparam.test THEN FILE_MOVE, gconst.DirCMD +'tmp.txt ',gconst.DirCMD+gconst.CMD_FILE
      ret = 1
  ENDIF

  IF ret THEN BEGIN
      WRITE_WIDGET_MESSAGE, '[CAM_POS_SET()]: Start.', /FORCE

      wait_count = 0.
      check        = 0
      WHILE ( check NE 1 ) DO BEGIN
          IF wait_count GT MAX_wait THEN BREAK

          WAIT, 0.25
          wait_count = wait_count + 0.25

          ret = TELESCOPE_STATUS( tstatus )
          IF ret THEN check   = ( tstatus.CAM_POS EQ set_cam_pos )
      ENDWHILE
  ENDIF

  ret = ( tstatus.CAM_POS EQ set_cam_pos )
  IF (ret NE 1) THEN                                            $
    WRITE_WIDGET_MESSAGE, '[CAM_POS_SET()]: Failed.', /FORCE  $
  ELSE                                                          $
    WRITE_WIDGET_MESSAGE, '[CAM_POS_SET()]: Success.', /FORCE

END



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO ENT_LID_CHECK

COMMON LIB, gconst, wparam, svar, diag, state

  IF (svar.SqContinue EQ 1) AND (wparam.test NE 1) AND (wparam.LidChkOff NE 1) THEN BEGIN

    IF (svar.E_LID NE 1)  THEN BEGIN
        WRITE_WIDGET_MESSAGE, '*** Entrance lid is close!! ***.'
        svar.SqContinue = 0l
    ENDIF

  ENDIF

END



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO MAKE_BEACON, CLOSE=CLOSE, TimeRes=TimeRes

COMMON LIB, gconst, wparam, svar, diag, state

  IF N_ELEMENTS( TimeRes ) LT 1 THEN TimeRes = 1.d

  IF (SYSTIME(1) - svar.BeaconUpdateTim) GT TimeRes THEN BEGIN ; Check if update is necessary.

    IF KEYWORD_SET(CLOSE) EQ 1 THEN BEGIN
                               Beacon_Byte = 48b    ; 48b = '0' ; close
                               beacon_file = '/work/SMARTT1OP/beacon_off.txt'
                               LOG_MODE    = 1
    ENDIF ELSE BEGIN
                               Beacon_Byte = 49b    ; 49b = '1' ; alive
                               beacon_file = '/work/SMARTT1OP/beacon_on.txt'
                               LOG_MODE    = 0
    ENDELSE
    
    datasize = 1
             
    IF wparam.test NE 1 THEN BEGIN
;        ret = CALL_EXTERNAL(gconst.FtpDll, 'PutData', Beacon_Byte, datasize,   $
;                       gconst.Sever, beacon_file, gconst.UName, gconst.Pass, /cdecl)
;
;        ;*** check if the ftp destination file exist ***
;        ret = CALL_EXTERNAL(gconst.FtpDll, 'IsExist',                          $
;                        gconst.Sever, beacon_file, gconst.UName, gconst.Pass, /cdecl)
    ENDIF
                    
    svar.BeaconUpdateTim = SYSTIME(1)

    IF KEYWORD_SET(CLOSE) NE 1 THEN WRITE_WIDGET_MESSAGE, '[MAKE_BEACON()]:'

  ENDIF

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO Update_ShabarInf, TimeRes=TimeRes

COMMON LIB, gconst, wparam, svar, diag, state

;starttime = systime(1)

  IF N_ELEMENTS( TimeRes ) LT 1 THEN TimeRes = gconst.ShabarRes

  IF (SYSTIME(1) - svar.ShbrUpdateTim) GT TimeRes THEN BEGIN ; Check if update is necessary.

      uTimeRes = gconst.ShabarRes +1   ; +1 [sec] is a margin
            uTimeRes = gconst.ShabarRes +5   ; +1 [sec] is a margin
;      uTimeRes =1.8
      TimResIfShabarStopped = 10       ; [sec]

      finfo = FILE_INFO(gconst.DirCMD + gconst.Shabar_FILE,/NOEXPAND_PATH)
 ;     print,finfo
;  print,finfo.mtime
;  print,systime(1)
      Age_ShabarInf = (SYSTIME(1) - finfo.mtime)
print,Age_ShabarInf
;print,uTimeRes
      IF Age_ShabarInf LE uTimeRes THEN BEGIN
          ret = 0
          shabar_inf = ''
          OPENR,     unit, gconst.DirCMD + gconst.Shabar_FILE, /GET_LUN
          READF,     unit, shabar_inf
          FREE_LUN, unit

          print,shabar_inf

          shabar_inf_arr =  STRSPLIT(shabar_inf, ',', /EXTRACT)
          if n_elements(shabar_inf_arr) eq 6 then begin
          svar.TimeShabarStr = shabar_inf_arr[0]
          svar.IntShabarStr  = shabar_inf_arr[1]
          svar.IntShabar     = DOUBLE(shabar_inf_arr[1])
          svar.ScntShabarStr = shabar_inf_arr[2]
          endif
          

          IF (svar.IfShabarStopped EQ 1) AND $
             (svar.UseShabarInf EQ 1)   THEN BEGIN

             ;*** ftp
             MSG_STRING = SYSTIME(0) + ': HSHABAR RESTARTED.'
             MSG_STRING = BYTE(MSG_STRING)
             datasize   = N_ELEMENTS(MSG_STRING)
             msg_file = '/fwork/smart/T1Obs/hshabar_errors/hshabar_restart.txt'
             
             ret = CALL_EXTERNAL(gconst.FtpDll, 'PutData', MSG_STRING, datasize,   $
                                 gconst.Sever, msg_file, gconst.UName, gconst.Pass, /cdecl)
          ENDIF
          
          svar.IfShabarStopped = 0          
          svar.ValidShabarInf  = 1
      ENDIF ELSE BEGIN
          svar.TimeShabarStr = ''
          svar.IntShabarStr  = ''
          svar.IntShabar     = 0d
          svar.ScntShabarStr = ''
          
          IF (svar.IfShabarStopped EQ 0) AND $
             (Age_ShabarInf GT TimResIfShabarStopped) AND $
             (svar.UseShabarInf EQ 1)   THEN BEGIN

             svar.IfShabarStopped = 1

             ;*** ftp
             ERR_STRING = SYSTIME(0) + ': HSHABAR STOPPED.'
             ERR_STRING = BYTE(ERR_STRING)
             datasize   = N_ELEMENTS(ERR_STRING)
             err_file = '/fwork/smart/T1Obs/hshabar_errors/hshabar_errors.txt'
             
             ret = CALL_EXTERNAL(gconst.FtpDll, 'PutData', ERR_STRING, datasize,   $
                                 gconst.Sever, err_file, gconst.UName, gconst.Pass, /cdecl)
          ENDIF

          svar.ValidShabarInf = 0
      ENDELSE

      svar.ShbrUpdateTim = SYSTIME(1)

      WRITE_WIDGET_MESSAGE, '[UPDATE_SHABARINF()]:'

      WIDGET_CONTROL, state.wStsSkyShInt,  SET_VALUE=svar.IntShabarStr
;      WIDGET_CONTROL, state.wStsSkyShScnt, SET_VALUE=svar.ScntShabarStr

  ENDIF 

;PRINT, '[UPDATE_SHABARINF()]: took ', systime(1) - starttime, ' seconds'
; 0.015 sec

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO BREAK_BY_SKYCONDITION

COMMON LIB, gconst, wparam, svar, diag, state

  Update_ShabarInf

  DO_CHECK = 0
  IF svar.ValidShabarInf EQ 1 THEN DO_CHECK = 1
  IF svar.SqContinue     NE 1 THEN DO_CHECK = 0
  IF svar.UseShabarInf   NE 1 THEN DO_CHECK = 0
  IF svar.AutoStart      NE 1 THEN DO_CHECK = 0

  IF DO_CHECK THEN BEGIN
  
    IF svar.IntShabar LT wparam.ShbrIntObsStop THEN BEGIN
       WRITE_WIDGET_MESSAGE, '[BREAK_BY_SKYCONDITION]: Stop by clouds.'
       svar.SqContinue  = 0l
       svar.ProgSqStart = 0l
       wparam.TimObsStppd = systime(1)

       WSET, state.wDrawStsId
       ERASE
       TV, BYTARR(300, 40) + 255
       !p.font = 0
       ;XYOUTS, 50, 10, '!17'+diag.StopEncount, /device,charsize=1.6, color=255
       XYOUTS, 50, 10, '!17Stop by Cloud', /device,charsize=1.6, color=255
       !p.font = -1
       WSET, state.wid
    ENDIF

  ENDIF

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO START_BY_SKYCONDITION, TimeRes=TimeRes

COMMON LIB, gconst, wparam, svar, diag, state

  Update_ShabarInf, TimeRes=TimeRes

  DO_CHECK = 0
  IF svar.ValidShabarInf EQ 1 THEN DO_CHECK = 1
  IF svar.UseShabarInf   NE 1 THEN DO_CHECK = 0 
  IF svar.AutoStart      NE 1 THEN DO_CHECK = 0
  
  IF DO_CHECK THEN BEGIN
  
    IF svar.IntShabar GT wparam.ShbrIntObsStart THEN BEGIN
       WRITE_WIDGET_MESSAGE, '[START_BY_SKYCONDITION]: Start by sky cond.',/FORCE
       svar.ProgSqStart = 1l
       wparam.TimObsStppd = gconst.Tomorrow

       WSET, state.wDrawStsId
       ERASE
       TV, BYTARR(300, 40) + 255
       !p.font = 0
       ;XYOUTS, 50, 10, '!17'+diag.StopEncount, /device,,charsize=1.6, color=500000000
       XYOUTS, 40, 10, '!17Start by Sky Cond', /device,charsize=1.6, color=500000000
       !p.font = -1
       WSET, state.wid
    ENDIF

  ENDIF

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO AUTO_ENTRANCE_LID_CLOSE

COMMON LIB, gconst, wparam, svar, diag, state

  DO_CHECK = 0
  IF (systime(1) - wparam.TimObsStppd) GT gconst.WaitAutoLidClose THEN DO_CHECK = 1
  IF svar.UseShabarInf   NE 1 THEN DO_CHECK = 0 
  IF svar.AutoStart      NE 1 THEN DO_CHECK = 0

  IF DO_CHECK THEN BEGIN

    ret = TELESCOPE_STATUS( tstatus, /silent )

    IF (svar.E_LID EQ 0) THEN wparam.TimObsStppd = gconst.Tomorrow $
    ELSE BEGIN
    
       WRITE_WIDGET_MESSAGE, '[AUTO_ENTRANCE_LID_CLOSE]: Closing Lid.',/FORCE

       WSET, state.wDrawStsId
       ERASE
       
       !p.font = 0
       ;XYOUTS, 50, 10, '!17'+diag.StopEncount, /device,charsize=1.6, color=255
       XYOUTS, 35, 10, 'Auto Closing Entrace Lid', /device,charsize=1.6, color=255
       !p.font = -1
       WSET, state.wid

       ret = ELID_CLOSE()

       WSET, state.wDrawStsId
       ERASE
       !p.font = 0
       IF (ret EQ 1) THEN BEGIN
           wparam.TimObsStppd = gconst.Tomorrow
           XYOUTS,    85, 10, '!17'+diag.ATObsStopLC,  /device,charsize=1.6, color=255
       ENDIF ELSE BEGIN
           XYOUTS, 70, 10, '!17'+diag.ATObsStopErr, /device,charsize=1.6, color=255
       ENDELSE
       
       !p.font = -1
       WSET, state.wid
        
    ENDELSE

  ENDIF

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO AUTO_ENTRANCE_LID_OPEN

COMMON LIB, gconst, wparam, svar, diag, state

  DO_CHECK = 0
  IF svar.ProgSqStart    EQ 1 THEN DO_CHECK = 1
  IF svar.UseShabarInf   NE 1 THEN DO_CHECK = 0 
  IF svar.AutoStart      NE 1 THEN DO_CHECK = 0

  IF DO_CHECK THEN BEGIN

    ret = TELESCOPE_STATUS( tstatus, /silent )

    IF (svar.E_LID NE 1) THEN BEGIN
    
       WRITE_WIDGET_MESSAGE, '[AUTO_ENTRANCE_LID_Open]: Open Lid.',/FORCE

       WSET, state.wDrawStsId
       ERASE
       TV, BYTARR(300, 40) + 255
       !p.font = 0
       
       ;XYOUTS, 50, 10, '!17'+diag.StopEncount, /device,charsize=1.6, color=255
       XYOUTS, 35, 10, 'Auto Open Entrace Lid', /device,charsize=1.6, color=255

       ret = ELID_OPEN()

       IF (ret NE 1) THEN BEGIN
           svar.ProgSqStart = 0
           ERASE
           XYOUTS, 70, 10, '!17'+diag.ATObsStopErr, /device,charsize=1.6, color=255
       ENDIF
       
       !p.font = -1
       WSET, state.wid
        
    ENDIF

  ENDIF

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


FUNCTION get_mtime_sts

COMMON LIB, gconst, wparam, svar, diag, state

    retry_in_time      = 1. + gconst.CadenceSTS ; [sec]
    sampling_dtim      = gconst.MtimeReso ; [sec]

    svar.MTime_syslog  = systime(1)
    mtime_max          = svar.MTime_syslog + retry_in_time
    f_st               = FILE_INFO( gconst.STS_FILE )
    mtime_sec          = f_st.mtime

    ret   = 0
    WHILE ret LT 1 DO BEGIN ; This block spend < 1.25 sec
        f_st = FILE_INFO( gconst.STS_FILE )
        IF f_st.mtime NE mtime_sec THEN BEGIN
            svar.MTime_syslog = systime(1)
            ret                = 1
            BREAK
        ENDIF
        wait, sampling_dtim

        IF systime(1) GT mtime_max THEN ret = 2
    ENDWHILE

    WRITE_WIDGET_MESSAGE, '[GET_MTIME_STS()]: Done.'

RETURN, ret
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION CHECK_FTPDEST

COMMON LIB, gconst, wparam, svar, diag, state

IF wparam.test THEN BEGIN
    WRITE_WIDGET_MESSAGE,'[CHECK_FTPDEST()]: Make ' + wparam.FtpStore
    retval = 2l  
ENDIF ELSE BEGIN
  
    IF svar.CustomOp NE 1 THEN ftp_store = wparam.FtpStore ELSE ftp_store = wparam.FtpStoreCOP

    ;*** check if the ftp destination directory exist ***
    retval   = CALL_EXTERNAL(gconst.FtpDll, 'IsExist', gconst.Sever,                          $
                             ftp_store, gconst.UName,  gconst.Pass, /cdecl)

    CASE retval OF
        2:  WRITE_WIDGET_MESSAGE,'[CHECK_FTPDEST()]: OK'
        1: BEGIN
            WRITE_WIDGET_MESSAGE,'[CHECK_FTPDEST()]: Failed'
            buttonPushed =                                                                    $
              DIALOG_MESSAGE('[CHECK_FTPDEST()]: Failed: ' + ftp_store + ' is a file.')
        ENDCASE
        0: BEGIN
            ;*** make the ftp destination directory ***
            retval = CALL_EXTERNAL(gconst.FtpDll, 'MakeDir', gconst.Sever,                    $
                                   ftp_store, gconst.UName,  gconst.Pass, /cdecl)

            ;*** check if the ftp destination directory exist again ***
            retval = CALL_EXTERNAL(gconst.FtpDll, 'IsExist', gconst.Sever,                    $
                                   ftp_store, gconst.UName,  gconst.Pass, /cdecl)

            IF retval EQ 2 THEN                                                               $
              WRITE_WIDGET_MESSAGE,'[CHECK_FTPDEST()]: Made ' + ftp_store               $
            ELSE BEGIN
                WRITE_WIDGET_MESSAGE,'[CHECK_FTPDEST()]: Failed'
                buttonPushed =                                                                $
                  DIALOG_MESSAGE('[CHECK_FTPDEST()]: Make ' + ftp_store + ': Failed')
            ENDELSE
        ENDCASE
    ENDCASE
ENDELSE

RETURN, retval
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION log_ftp

COMMON LIB, gconst, wparam, svar, diag, state

  OPENR,     unit, wparam.DirStore + '/' + gconst.FtpLog, /GET_LUN
    dummy    =     FSTAT(unit)
    bytlog   =     BYTARR(dummy.size)
    READU,   unit, bytlog
  FREE_LUN,  unit
  datasize   = dummy.size

  IF wparam.test THEN retval = 1l  ELSE BEGIN
;    retval   = CALL_EXTERNAL(gconst.FtpDll, 'PutData', bytlog, datasize,           $
;                             gconst.Sever,  wparam.FtpStore + '/' + gconst.FtpLog, $
;                             gconst.UName,  gconst.Pass, /cdecl)

    ;*** check if the ftp destination file exist ***
;    retval   = CALL_EXTERNAL(gconst.FtpDll, 'IsExist',                            $ 
;                             gconst.Sever, wparam.FtpStore + '/' + gconst.FtpLog, $
;                             gconst.UName, gconst.Pass, /cdecl)
  ENDELSE


;  IF retval NE 1 THEN BEGIN     ; 1 = TRUE, 0 = FALSE
;      buttonPushed = $
;        DIALOG_MESSAGE('*** Warning: check the internet connection (1) ***')
;  ENDIF


  OPENR,     unit, wparam.DirStore + '/' + gconst.LastFLog, /GET_LUN
    dummy    =     FSTAT(unit)
    bytlog   =     BYTARR(dummy.size)
    READU,   unit, bytlog
  FREE_LUN,  unit
  datasize = dummy.size

  IF wparam.test THEN retval = 1l  ELSE BEGIN
;    retval   = CALL_EXTERNAL(gconst.FtpDll, 'PutData', bytlog, datasize, $
;                             gconst.Sever,  wparam.FtpStore + '/' + gconst.LastFLog, $
;                             gconst.UName,  gconst.Pass, /cdecl)

    ;*** check if the ftp destination file exist ***
;    retval   = CALL_EXTERNAL(gconst.FtpDll, 'IsExist',                            $ 
;                             gconst.Sever, wparam.FtpStore + '/' + gconst.LastFLog, $
;                             gconst.UName, gconst.Pass, /cdecl)
  ENDELSE

;  IF retval NE 1 THEN BEGIN     ; 1 = TRUE, 0 = FALSE
;      buttonPushed = $
;       DIALOG_MESSAGE('*** Warning: check the internet connection (2) ***')
;  ENDIF

RETURN, 1l
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION FILTLOG_FTP

COMMON LIB, gconst, wparam, svar, diag, state

  OPENR,     unit, gconst.pFiltLog, /GET_LUN
    dummy    =     FSTAT(unit)
    bytlog   =     BYTARR(dummy.size)
    READU,   unit, bytlog
  FREE_LUN,  unit
  datasize = dummy.size

  IF wparam.test THEN retval = 1l  ELSE BEGIN
    retval   = CALL_EXTERNAL(gconst.FtpDll, 'PutData', bytlog, datasize, $
                             gconst.Sever,  wparam.FtpLogStore + '/' + gconst.FiltLog, $
                             gconst.UName,  gconst.Pass, /cdecl)

    ;*** check if the ftp destination file exist ***
    retval   = CALL_EXTERNAL(gconst.FtpDll, 'IsExist',                            $ 
                             gconst.Sever, wparam.FtpLogStore + '/' + gconst.FiltLog, $
                             gconst.UName, gconst.Pass, /cdecl)
  ENDELSE

  IF retval NE 1 THEN BEGIN     ; 1 = TRUE, 0 = FALSE
      buttonPushed = $
       DIALOG_MESSAGE('*** Warning: check the internet connection (3) ***')
  ENDIF

RETURN, 1l
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION CMDLOG_FTP

COMMON LIB, gconst, wparam, svar, diag, state

   OPENR,     unit, gconst.pCmdLog, /GET_LUN
    dummy    =     FSTAT(unit)
    bytlog   =     BYTARR(dummy.size)
    READU,   unit, bytlog
  FREE_LUN,  unit
  datasize = dummy.size

  IF wparam.test THEN retval = 1l  ELSE BEGIN
    retval   = CALL_EXTERNAL(gconst.FtpDll, 'PutData', bytlog, datasize, $
                             gconst.Sever,  wparam.FtpLogStore + '/' + gconst.CmdLog, $
                             gconst.UName,  gconst.Pass, /cdecl)

    ;*** check if the ftp destination file exist ***
    retval   = CALL_EXTERNAL(gconst.FtpDll, 'IsExist',                            $ 
                             gconst.Sever, wparam.FtpLogStore + '/' + gconst.CmdLog, $
                             gconst.UName, gconst.Pass, /cdecl)
  ENDELSE

  IF retval NE 1 THEN BEGIN     ; 1 = TRUE, 0 = FALSE
      buttonPushed = $
       DIALOG_MESSAGE('*** Warning: check the internet connection (4) ***')
  ENDIF

RETURN, 1l
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION retry_ftp

COMMON LIB, gconst, wparam, svar, diag, state

  ftp_log     = wparam.DirStore + '/' + gconst.FtpLog

  retry_files = strarr(svar.FtpFailC)
  failflags   = bytarr(svar.FtpFailC) + 1b
  dummyc      = 0l
  dummyf      = ''

  OPENR,     unit, wparam.DirStore + '/' + gconst.FtpLog, /GET_LUN
    READF,   unit, dummyc
    FOR ii =0, svar.FtpFailC - 1 DO BEGIN
      READF,          unit, dummyf
      retry_files[ii] =     dummyf
    ENDFOR
  FREE_LUN, unit

  WHILE svar.FtpFailC GT 0 DO BEGIN
      fits_fname = retry_files[ svar.FtpFailC - 1 ]

      OPENR,        unit, wparam.DirStore + '/' + fits_fname, /GET_LUN
        dummy       =     FSTAT( unit )
        bytfitsdat  =     BYTARR( dummy.size )
        READU,      unit, bytfitsdat
      FREE_LUN,     unit

      datasize      = dummy.size
      IF wparam.test THEN retval = 1l  ELSE $
        retval     = CALL_EXTERNAL(gconst.FtpDll, 'PutData', bytfitsdat, datasize,    $
                                   gconst.Sever,  wparam.FtpStore + '/' + fits_fname, $
                                   gconst.UName,  gconst.Pass, /cdecl)

        ;*** check if the ftp destination file exist ***
        retval   = CALL_EXTERNAL(gconst.FtpDll, 'IsExist',                         $ 
                                 gconst.Sever, wparam.FtpStore + '/' + fits_fname, $
                                 gconst.UName, gconst.Pass, /cdecl)

      IF retval NE 1 THEN BEGIN ; 1 = TRUE, 0 = FALSE
          buttonPushed = $
           DIALOG_MESSAGE('*** Warning: check the internet connection (5), BREAK ***')
          BREAK
      ENDIF ELSE BEGIN
          svar.FtpFailC  = svar.FtpFailC - 1l 

          OPENU,    unit, wparam.DirStore + '/' + gconst.FtpLog, /GET_LUN
            PRINTF, unit, svar.FtpFailC
          FREE_LUN, unit
      ENDELSE
  ENDWHILE

  IF svar.FtpFailC NE 0 THEN BEGIN
      buttonPushed = $
        DIALOG_MESSAGE( '*** Warning *** ' + $
                        'Number of fits files ftp transfer failed:' + $
                        string(svar.FtpFailC, format='(i10)') )
  ENDIF

  IF svar.FtpFailC EQ 0 THEN BEGIN ; clear the ftp failed files from the log
      result = 1l
      OPENW,    unit, wparam.DirStore + '/' + gconst.FtpLog, /GET_LUN
        PRINTF, unit, svar.FtpFailC
      FREE_LUN, unit
  ENDIF ELSE result = 0l

RETURN, result
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO P2P_RA_DEC_SET, set_ra_pos, set_dec_pos

  COMMON LIB, gconst, wparam, svar, diag, state

  p1  = 8
  p2  = 0
  p3  = LONG( CLEAN_RA_POS( set_ra_pos, hh_ra, mm_ra, ss_ra, sss_ra ) )
  p4  = 8
  p5  = 0
  p6  = LONG(set_dec_pos)
  p7  = 0
  p8  = 0
  p9  = 0
  p10 = 0
  p11 = 0
  p12 = 0
  p13 = 0

  cmd_file_check = FILE_TEST(gconst.DirCMD + gconst.CMD_FILE)

  ret = TELESCOPE_STATUS( tstatus, /silent )
  IF ret THEN if_not_moving = ( ( tstatus.RA_STS MOD 2 ) EQ 0 ) AND ( ( tstatus.DEC_STS MOD 2 ) EQ 0 )

  ret = -1

  IF (cmd_file_check NE 1) AND if_not_moving THEN BEGIN
    openw,  9, gconst.DirCMD + 'tmp.txt'
    printf, 9, PREP_T1BUFFER(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13)
    close,  9
    IF NOT wparam.test THEN FILE_MOVE, gconst.DirCMD +'tmp.txt ',gconst.DirCMD+gconst.CMD_FILE
    ret = 1

    ;=== Print message for RA move
    astring  = STRING(hh_ra,format='(i3.2)') + ':' + STRING(mm_ra,format='(i2.2)')+ ':' + STRING(ss_ra,format='(i2.2)') + '.' + STRING(sss_ra,format='(i1.1)')
    WRITE_WIDGET_MESSAGE, '[P2P_RA_DEC_SET]: RA SET to ' + astring

    ;=== Print message for DEC move
    CONV_DEC_POS, set_dec_pos, hh, mm, ss, sss, sign
    astring  = STRING(sign*hh,format='(i3.2)') + ':' + STRING(mm,format='(i2.2)')+ ':' + STRING(ss,format='(i2.2)') + '.' + STRING(sss,format='(i1.1)')
    WRITE_WIDGET_MESSAGE, '[P2P_RA_DEC_SET]: DEC SET to ' + astring
  ENDIF ELSE BEGIN
    WRITE_WIDGET_MESSAGE, '[P2P_RA_DEC_SET]: skipped.'
  ENDELSE

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO P2P_RA_SET, set_ra_pos

  COMMON LIB, gconst, wparam, svar, diag, state

  p1  = 8
  p2  = 0
  p3  = LONG( CLEAN_RA_POS( set_ra_pos, hh, mm, ss, sss ) )
  p4  = 0
  p5  = 0
  p6  = 0
  p7  = 0
  p8  = 0
  p9  = 0
  p10 = 0
  p11 = 0
  p12 = 0
  p13 = 0

  ;  MAX_wait       = 10. ; [sec]

  cmd_file_check = FILE_TEST(gconst.DirCMD + gconst.CMD_FILE)

  ret = TELESCOPE_STATUS( tstatus, /silent )
  IF ret THEN if_not_moving = ( tstatus.RA_STS MOD 2 ) EQ 0

  ret = -1

  IF (cmd_file_check NE 1) AND if_not_moving THEN BEGIN
    openw,  9, gconst.DirCMD + 'tmp.txt'
    printf, 9, PREP_T1BUFFER(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13)
    close,  9
    IF NOT wparam.test THEN FILE_MOVE, gconst.DirCMD + 'tmp.txt ', gconst.DirCMD + gconst.CMD_FILE
    ret = 1

    astring  = STRING(hh,format='(i3.2)') + ':' + STRING(mm,format='(i2.2)')+ ':' + STRING(ss,format='(i2.2)') + '.' + STRING(sss,format='(i1.1)')
    WRITE_WIDGET_MESSAGE, '[P2P_RA_SET]: RA SET to ' + astring
  ENDIF ELSE BEGIN
    WRITE_WIDGET_MESSAGE, '[P2P_RA_SET]: skipped.'
  ENDELSE

  ;  IF ret THEN BEGIN
  ;      WRITE_WIDGET_MESSAGE, '[P2P_RA_SET()]: Start.', /FORCE
  ;
  ;      wait_count   = 0.
  ;      check        = 0
  ;      WHILE ( check NE 1 ) DO BEGIN
  ;          IF wait_count GT MAX_wait THEN BREAK
  ;
  ;          WAIT, 0.25
  ;          wait_count = wait_count + 0.25
  ;
  ;          ret = TELESCOPE_STATUS( tstatus )
  ;          IF ret THEN check   = ( tstatus.RA_POS EQ set_ra_pos )
  ;      ENDWHILE
  ;  ENDIF
  ;
  ;  ret = ( tstatus.RA_POS EQ set_ra_pos )
  ;  IF (ret NE 1) THEN                                            $
  ;    WRITE_WIDGET_MESSAGE, '[P2P_RA_SET()]: Failed.', /FORCE     $
  ;  ELSE                                                          $
  ;    WRITE_WIDGET_MESSAGE, '[P2P_RA_SET()]: Success.', /FORCE

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO P2P_DEC_SET, set_dec_pos

  COMMON LIB, gconst, wparam, svar, diag, state

  p1  = 0
  p2  = 0
  p3  = 0
  p4  = 8
  p5  = 0
  p6  = LONG(set_dec_pos)
  p7  = 0
  p8  = 0
  p9  = 0
  p10 = 0
  p11 = 0
  p12 = 0
  p13 = 0

  ;  MAX_wait       = 10. ; [sec]

  cmd_file_check = FILE_TEST(gconst.DirCMD + gconst.CMD_FILE)

  ret = TELESCOPE_STATUS( tstatus, /silent )
  IF ret THEN if_not_moving = ( tstatus.DEC_STS MOD 2 ) EQ 0

  ret = -1

  IF (cmd_file_check NE 1) AND if_not_moving THEN BEGIN
    openw,  9, gconst.DirCMD + 'tmp.txt'
    printf, 9, PREP_T1BUFFER(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13)
    close,  9
    IF NOT wparam.test THEN FILE_MOVE, gconst.DirCMD + 'tmp.txt ', gconst.DirCMD + gconst.CMD_FILE
    ret = 1

    CONV_DEC_POS, set_dec_pos, hh, mm, ss, sss, sign
    astring  = STRING(sign*hh,format='(i3.2)') + ':' + STRING(mm,format='(i2.2)')+ ':' + STRING(ss,format='(i2.2)') + '.' + STRING(sss,format='(i1.1)')
    WRITE_WIDGET_MESSAGE, '[P2P_DEC_SET]: DEC SET to ' + astring
  ENDIF ELSE BEGIN
    WRITE_WIDGET_MESSAGE, '[P2P_DEC_SET]: skipped.'
  ENDELSE

  ;  IF ret THEN BEGIN
  ;      WRITE_WIDGET_MESSAGE, '[P2P_DEC_SET()]: Start.', /FORCE
  ;
  ;      wait_count   = 0.
  ;      check        = 0
  ;      WHILE ( check NE 1 ) DO BEGIN
  ;          IF wait_count GT MAX_wait THEN BREAK
  ;
  ;          WAIT, 0.25
  ;          wait_count = wait_count + 0.25
  ;
  ;          ret = TELESCOPE_STATUS( tstatus )
  ;          IF ret THEN check   = ( tstatus.DEC_POS EQ set_dec_pos )
  ;      ENDWHILE
  ;  ENDIF
  ;
  ;  ret = ( tstatus.DEC_POS EQ set_dec_pos )
  ;  IF (ret NE 1) THEN                                            $
  ;    WRITE_WIDGET_MESSAGE, '[P2P_DEC_SET()]: Failed.', /FORCE    $
  ;  ELSE                                                          $
  ;    WRITE_WIDGET_MESSAGE, '[P2P_DEC_SET()]: Success.', /FORCE

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO TELESCOP_WAIT

  COMMON LIB, gconst, wparam, svar, diag, state

  ON_ERROR,2

  check = 1
  ret   = TELESCOPE_STATUS( tstatus )
  IF ret EQ 1 THEN check = ( ( tstatus.RA_STS MOD 2 ) EQ 0 ) AND ( ( tstatus.DEC_STS MOD 2 ) EQ 0 )

  IF check EQ 1 THEN WRITE_WIDGET_MESSAGE, '[TELESCOP_WAIT]: SKIP'

  WHILE ( check NE 1 ) DO BEGIN
    ;*** Break point
    BREAK_CHECK
    IF (svar.SqContinue LT 1) THEN BREAK

    WRITE_WIDGET_MESSAGE, '[TELESCOP_WAIT]: WAIT 0.5 sec'

    WAIT, 0.5
    ret = TELESCOPE_STATUS( tstatus, /silent )
    IF ret EQ 1 THEN check = ( ( tstatus.RA_STS MOD 2 ) EQ 0 ) AND ( ( tstatus.DEC_STS MOD 2 ) EQ 0 )
  ENDWHILE

END
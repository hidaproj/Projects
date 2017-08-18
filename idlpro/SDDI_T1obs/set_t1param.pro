;                                Time-stamp: <2011-10-12 19:30:26 morita>
;+
;
;  Set_T1Param.pro
;
;Global (common) variables and parameters difinition for SMART T1 observation
;based on T3 scripts by T. Kawate
;
;20100512  T.K.
;20100817  S.M.
;
;-

PRO set_t1param, gconst, wparam, svar, diag, testmode=testmode, LidChkOff=LidChkOff;, English=English

testmode  = KEYWORD_SET(testmode)
LidChkOff = KEYWORD_SET(LidChkOff)
;English   = KEYWORD_SET(English)

;IF !VERSION.OS_FAMILY EQ 'Windows' THEN slash_OSF = '\' ELSE slash_OSF = '/' 
slash_OSF = '\'

;*************************************************************
;address of T1 IDL library 
t1_libpath = 'C:\Projects\idlpro\newT1'
;t1_libpath='C:\Users\smart\IDLWorkspace85\Default'

;*************************************************************
;address of camera DLL
;camdll = 'C:dll\Alta3.dll'

;*************************************************************
;address of filter DLL
;filtdll = 'C:\dll\RS232C.dll'

;*************************************************************
;address of ftp DLL
ftpdll = 'C:\dll\ftptest.dll'

;*************************************************************
; Some directories & files (constants)

;rootdir_store   = 'd:\data'
rootdir_store   = '\\Smart-t1-room\g\data'
;rootdir_store   = '\\Smart-t1-room\d\data'
ftp_PartNamHead = '/smart'
dir_statuslog   = 'c:\log'   
dir_syslog      = '\\smartsys\log\'
;dir_cmd         = '\\smartsys\command\'
dir_cmd         = 't:'

;*************************************************************
;if test mode

IF testmode THEN BEGIN
    t1_libpath = '.'

    rootdir_store  = '.'
    dir_statuslog  = './log'
    dir_syslog     = '.'
    dir_cmd        = './cmd/'
    
    test_files     = FILE_SEARCH('/soda/kwasan_hida/testdata/*.fits.gz')
    English        = 1
ENDIF ELSE BEGIN ; additional initializations

;    spawn, 'net use s: /delete' ,/hide ;,/nowait
;    spawn, 'net use s: \\SMARTSYS\log' ,/hide ;,/nowait
;
    spawn, 'net use t: /delete' ,/hide ;,/nowait
    spawn, 'net use t: \\SMARTSYS\command' ,/hide ;,/nowait

;   spawn, 'net use d: /delete' ,/hide ;,/nowait
;   spawn, 'net use d: \\SMART-T1-ROOM\d' ,/hide ;,/nowait

    test_files     = ''
ENDELSE

;*************************************************************
; Set up calendar

caldat, systime(0,/JULIAN),mm,dd,yy
today = string(yy,format='(i4.4)')+string(mm,format='(i2.2)')+string(dd,format='(i2.2)')

CASE mm OF
  1:  mon = 'Jan'
  2:  mon = 'Feb'
  3:  mon = 'Mar'
  4:  mon = 'Apr'
  5:  mon = 'May'
  6:  mon = 'Jun'
  7:  mon = 'Jul'
  8:  mon = 'Aug'
  9:  mon = 'Sep'
  10: mon = 'Oct'
  11: mon = 'Nov'
  12: mon = 'Dec'
ENDCASE
todayiso = string(dd,format='(i2.2)') + '-' + mon + '-' $
  + string(yy,format='(i4.4)')
  
;*************************************************************
; Set up some parameters for image display  ！修正必要！

;=== set the following value ===
t1_naxis2     = 1024.  ; [pix]  ;表示用？かつbininng?
median_box_sz = 1024   ; [pix]


t1_xcen_raw     = 1024 ; 
t1_ycen_raw     = 1024 ;
;amask_rad       = 1060 ; [pix], aperture mask radius at 12-Oct-2011
;amask_in_margin = -25  ; [pix]


t1_xcen         = 500 ; for display
t1_ycen         = 500 ; 
;amask_rad_view  = 530   ; 2011-Sep(50mm) for display
;===============================

;*************************************************************
;Move to the T1 IDL library directory
CD, t1_libpath

;*************************************************************
; Some initializations for this application

;plate_scl_file = t1_libpath + slash_OSF + 'plate_scl.prm'
;plate_scl_str  = ''
plate_scl      = '1.23' ; [arcsec/pix]  

dir_store = rootdir_store + slash_OSF + today

ftp_datedir    =   today + '/T1/' + today
ftp_paramfile  = t1_libpath + slash_OSF + 'ftp_partition.prm'
ftp_part_id    = 1                             ; {1,2}: default = 1

ftp_idfile     = t1_libpath + slash_OSF + 'ftp_id.prm'
ftp_id         = ''

cmd_log   = 'cmd_'    + today + '.txt'
filt_log  = 'filter_' + today + '.txt'
;cam_log   = 'cam_'    + today + '.txt'

pcmd_log  = dir_statuslog + slash_OSF + cmd_log
pfilt_log = dir_statuslog + slash_OSF + filt_log
;pcam_log  = dir_statuslog + slash_OSF + cam_log

sts_file  = dir_syslog    + '/'   + 'sts' + today + '.txt'
;sts_file  = t1_libpath    + '/'   + 'sts20110627.txt'

lastflog  = 'lastfile'    + today + '.txt' 
plastflog = dir_store     + '/'   + lastflog
incr_id   = 0l ; incremental file ID (= number of fits files saved)


ftp_log   = 'ftpfail'     + today + '.log'
pftp_log  = dir_store     + '/'   + ftp_log
ftpfailc  = 0l ; count of fits files ftp transfer failed

ftp_logstore = '/smart3/smart/T1_filter_log'

dir_store_cop = rootdir_store + slash_OSF + today + 'ex'

;*************************************************************
; Some initializations for this application again
;
;  ret    = (FILE_INFO(plate_scl_file)).EXISTS
;  IF ret EQ 1 THEN BEGIN        ; read
;      OPENR,    unit, plate_scl_file, /GET_LUN
;      READF,    unit, plate_scl_str
;      FREE_LUN, unit
;
;      IF (FLOAT(plate_scl_str) GT 0.555) AND (FLOAT(plate_scl_str) LT 0.575) $ ; for safety
;      THEN plate_scl = FLOAT(plate_scl_str)
;  ENDIF ELSE BEGIN              ; stop
;      PRINT, '*** We need '+plate_scl_file+'!! ***'
;      STOP
;  ENDELSE

ret    = (FILE_INFO(ftp_idfile)).EXISTS
  IF ret EQ 1 THEN BEGIN        ; read
      OPENR,    unit, ftp_idfile, /GET_LUN
      READF,    unit, ftp_id
      FREE_LUN, unit
  ENDIF ELSE BEGIN              ; stop
      PRINT, '*** We need '+ftp_idfile+'!! ***'
      STOP
  ENDELSE

ret  = (FILE_INFO(dir_store)).DIRECTORY
  IF NOT ret THEN spawn, 'mkdir -p ' + dir_store

ret    = (FILE_INFO(ftp_paramfile)).EXISTS
  IF ret NE 1 THEN BEGIN        ; create
      OPENW,    unit, ftp_paramfile, /GET_LUN
      PRINTF,   unit, ftp_part_id
      FREE_LUN, unit
  ENDIF ELSE BEGIN              ; read
      OPENR,    unit, ftp_paramfile, /GET_LUN
      READF,    unit, ftp_part_id
      FREE_LUN, unit
  ENDELSE

ftp_store     = ftp_PartNamHead + string(ftp_part_id, format='(i1)') + $
  '/' + ftp_datedir

ftp_store_cop = ftp_store + 'ex'

ret    = (FILE_INFO(sts_file)).EXISTS
  IF NOT ret THEN sts_file = dir_syslog + '/' + 'fake_stsyyyymmdd.txt'

ret    = (FILE_INFO(plastflog)).EXISTS
  IF ret NE 1 THEN BEGIN        ; create
      OPENW,    unit, plastflog, /GET_LUN
      PRINTF,   unit, incr_id
      FREE_LUN, unit
  ENDIF ELSE BEGIN              ; read
      OPENR,    unit, plastflog, /GET_LUN
      READF,    unit, incr_id
      FREE_LUN, unit
  ENDELSE


ret  = (FILE_INFO(pftp_log)).EXISTS
  IF ret NE 1 THEN BEGIN        ; create
      OPENW,    unit, pftp_log, /GET_LUN
      PRINTF,   unit, ftpfailc
      FREE_LUN, unit
  ENDIF ELSE BEGIN              ; read
      OPENR,    unit, pftp_log, /GET_LUN
      READF,    unit, ftpfailc
      FREE_LUN, unit
  ENDELSE

ret  = (FILE_INFO(pcmd_log)).EXISTS
  IF ret NE 1 THEN BEGIN        ; create
      OPENW,    unit, pcmd_log, /GET_LUN
      ;PRINTF,   unit, ''
      FREE_LUN, unit
  ENDIF

ret  = (FILE_INFO(pfilt_log)).EXISTS
  IF ret NE 1 THEN BEGIN        ; create
      OPENW,    unit, pfilt_log, /GET_LUN
      ;PRINTF,   unit, ''
      FREE_LUN, unit
  ENDIF


;=== auto setup ==================
 ;+=== make aperture mask ===　　　　　！修正必要！
; 
 DIST_CIRCLE,circle,2048,t1_xcen_raw,t1_ycen_raw
 circle[0:4,*]       = MAX(circle) ; remove edge
 circle[2043:2047,*] = MAX(circle) ; remove edge
 circle[*,0:4]       = MAX(circle) ; remove edge
 circle[*,2043:2047] = MAX(circle) ; remove edge
 amask = FLTARR(2048, 2048)
; amask[WHERE(circle LT amask_rad + amask_in_margin)] = 1.
 amask[24:2023,24:2023]=1
 ;-=== make aperture mask ===

;winsz_x   = t1_naxis2
;winsz_y   = t1_naxis2 + 94
winsz_x   = 1000
winsz_y   = 1000

;  theta_tmp          = FINDGEN(91)*4*!pi/180.
;  circle_y         = t1_ycen + amask_rad_view * COS(theta_tmp)
;  ss = where (circle_y LE t1_naxis2, sn)
;  theta_tmp[ss[0]-1]    = ACOS((t1_naxis2 - t1_ycen) / amask_rad_view)
;  theta_tmp[ss[sn-1]+1] = 2*!pi - theta_tmp[ss[0]-1]
;circle_x = t1_xcen + amask_rad_view * SIN(theta_tmp)
;circle_y = t1_ycen + amask_rad_view * COS(theta_tmp)
;  ss = where(circle_y GE t1_naxis2, sn)
;  IF sn GE 1 THEN circle_y[ss] = t1_naxis2

;mag0        = 4096. / 1024.
;t1_ycen_raw = fix( (         542.5   + 0.5) * mag0 - 0.5 ) + 1

RSun       = ( PB0R(todayiso+' 03:00', /arcsec, soho=0) )[2]   ; 03:00 UT is 12:00 JST
RSunPix    = FIX( RSun / plate_scl )

ss_med_area = WHERE(circle LT 0.5*RSunPix)


med_xmin = t1_xcen_raw - fix(median_box_sz/2)
med_xmax = med_xmin + median_box_sz
med_ymin = t1_ycen_raw - fix(median_box_sz/2)
med_ymax = med_ymin + median_box_sz
;==================================

;*************************************************************
;default parameters for observation


  sqnames = ['Normal Mode (65 points)','Small (7 points)','Dark Mode']

  custmsqnames = ['Get Dark images','Focus check','for Speckle']
                       
 
;  WavStdObs = [0] ;+ OfSetMecZero
;  ExpStdObs = [30]
  
  ExpNorm = 2
  CadNorm = 35.0
  
  WavOneSh = 0.0
;  ExpOneSh = 30

  WavPrVw = 0.0
;  ExpPrVw = 30


;  WavStdObs=findgen(20)*25.-200.
  WavStdObs=findgen(65)*25.-800.
  WavSmlObs=[-1.2,-0.8,-0.5,0.0,+0.5,+0.8,+1.2]*100.



  gconst={global_constant,            $
;      CamDll:       camdll,           $ ; address of camera DLL
;      maxstrlen:    127b,             $ ; camera max str length 
;      Height:       4098l,            $ ; Height  max=4098 (biny=1)
;      Width:        4098l,            $ ; Width   max=4098 (binx=1)
;      Fullwell:     4095l,            $ ; [DN] Full well of CCD pixels (12 bit).
;     FiltDll:      filtdll,          $ ; address of filter DLL
;      FBytSiz:      1024,            $ ; filter buffer byte size
;      FiltWaitT:    6d,               $ ; [sec] Wait time for Lyot filter shift     
;      FiltWaitT:    6d,               $ ; [sec] Wait time for Lyot filter shift
;      FiltWaitTWav: 800,              $ ; [mA] Assumed wavelength shift for the FiltWaitT
;      F_TempDisTim: 26,               $ ; [sec] Filter temp disable time(= 26 [sec]) 
;      F_TempCadence:10,                $ ; [sec] Filter temp cadence(= 9 [sec]) + 1 [sec] margin
;      F_TempFUpdtT: 180d,             $ ; [sec] Filter temp fource update cadence(= 3 [min])
;      AutoZResetT:  180d,             $ ; [sec] Wait time for auto filter zero reset.
;      FiltTyp:      'LYOT_FILTER_HA', $ ; filter type          
      FtpDll:       ftpdll,           $ ; address of ftp DLL
      DarkImgNum:   100,               $ ; number of dark images at one exp value.
;      FlatFieldNum: 30,               $ ; number of flat feild images at a fil pos.
;      KFlatFieldNum: 3,               $ ; number of images at a fil pos and a sun center pos for Kuhn flat.
;      KFlatPos:     KFlatPos,         $ ; [arcsecs in degrees and HA] displacements for Khun flat.
;      WhtLgtCadence:3600l,            $ ; [sec] white light images obs cadence(= 1 [hour])
;      WhtLgtCadence:36000l,            $ ; [sec] white light images obs cadence(= 1 [hour])
;      Wait1stWhtLgt:2l,               $ ; Wait for the 1st white light obs (= 2 sets of norm obs)
;      WavCalCadence:36000l,           $ ; [sec] Lyot Filt wave cal obs cadence(= 10 [hours])
;      Wait1stWavCal:4l,               $ ; Wait for the 1st wave cal obs (= 4 sets of standard obs)
;      HiSpdErptCadence:180l,          $ ; [sec] High Speed Erupt obs cadence(= 3 [min])
      WaitAutoLidClose:300l,          $ ; [sec] Wait time for auto entrance-lid close (= 1 [min]).
      Tomorrow:     systime(1)+86400d,$ ; [sec] A system clock some time in "tomorrow".  
;      Sever:        'smartftp.hida',  $ ; ftp server
      Sever:         '3.9.0.210', $ 
      UName:        'smart',          $ ; ftp user name
      Pass:         ftp_id,           $ ; for ftp
      STS_FILE:     sts_file,         $ ; System log file name.
      CMD_FILE:     'T1_command.txt', $ ; Command file name.
      Shabar_FILE:  'shabar.txt',     $ ; Shabar Info file name.
      DirCMD:       dir_cmd,          $ ; Path to the command file.
      LastFLog:     lastflog,         $ ; name of the last file log
      FtpLog:       ftp_log,          $ ; name of the ftp transfer failed log
      CadenceSTS:   0.250,            $ ; [sec]; cadence of sts-file update
      ThTStUpdate:  0.180,            $ ; [sec]; threshold for waiting next chance
      MtimeReso:    0.010,            $ ; [sec]; resoluion of STS file mtime
      CamPosMax:    55000l,           $ ; Camera position max [um]
      CamPosMid:    47500l,           $ ; Camera position mid [um]
      CamPosMin:    40000l,           $ ; Camera position min [um]
 ;     AutoTrackWav: WavStdObs[6],     $ ; [Angstrome] Lyot Filter Wavelength offset for auto tracking. p12�̂��Ɓ@7�g���O��
 ;     NAutoTrackWav:WavStdObs[3],     $ ; [Angstrome] Lyot Filter Wavelength offset for no auto tracking.
 ;     AutoTrackWav: WavStdObs[0],     $ ; [Angstrome] Lyot Filter Wavelength offset for auto tracking.
 ;    NAutoTrackWav: WavStdObs[0],     $ ; [Angstrome] Lyot Filter Wavelength offset for no auto tracking.
      ThMedian:     400.,             $ ; [DN]  Threshold median int. for auto tracking.
      RaCutoff:     1.6,              $ ; 24.0 [arcsec in HA] Cut off for RA move in auto tracking. default = 1.0
      DecCutoff:    15.0,             $ ; [arcsec in Degrees] Cut off for RA move in auto tracking. default = 15.0
      RaUpLimit:    40.,              $ ; [arcsec in HA] Upper limit for RA move in auto tracking.
      DecUpLimit:   600.,             $ ; [arcsec in Degrees] Upper limit for RA move in auto tracking.
 ;     TransND1:     TransND1,         $ ; Transmission of ND1 
 ;     TransND2:     TransND2,         $ ; Transmission of ND2 
 ;     TransND3:     TransND3,         $ ; Transmission of ND3 
 ;     TransND4:     TransND4,         $ ; Transmission of ND4
 ;     NDTransOrder: NDTransOrder,     $ ; ND transmission order. ID 0 is the maximum.
      ClockRes:     1,                $ ; [sec]; time resolution of widget clock.
      ShabarRes:    2,                $ ; [sec]; time resolution of shabar data update.
      SqNames:      sqnames,          $ ; names of sequences
      CustmSqNames: custmsqnames,     $ ; names of custom sequences
 ;     OfSetMecZero: OfSetMecZero,     $ ; [mA] offset of the mechanical zero for 50mm Lyot Filter.
      WavNormObs:    WavStdObs,        $ ; wavelength set for the standard obs. sequences.
      WavSmlObs:    WavSmlObs,        $ ; exposure set for the standard obs. sequences.
  ;     WavHiSpdErpt: WavHiSpdErpt,     $ ; wavelength set for the High Speed Eruption mode sequences.
 ;     ExpHiSpdErpt: ExpHiSpdErpt,     $ ; exposure set for the High Speed Eruption mode sequences.
 ;     WavWavCal:    WavWavCal,        $ ; wavelength set for the Wave Cal sequences.
 ;     ExpWavCal:    ExpWavCal,        $ ; exposure set for the Wave Cal sequences.
      FtpParamFil:  ftp_paramfile,    $ ; name of ftp target partition index file.
      FtpPNamHead:  ftp_PartNamHead,  $ ; name of ftp target partition wo index. 
      FtpDateDir:   ftp_datedir,      $ ; name of ftp destination wo partition.
      CmdLog:       cmd_log,          $ ; name of command log.
      FiltLog:      filt_log,         $ ; name of filter log.
 ;     CamLog:       cam_log,          $ ; name of camera log.
      pCmdLog:      pcmd_log,         $ ; name of command log with its path.
      pFiltLog:     pfilt_log,        $ ; name of filter log with its path.
 ;     pCamLog:      pcam_log,         $ ; name of camera log with its path.
 ;     pWhtLgtLog:   pwhtlgtlog,       $ ; name of white light obs log with its path.
 ;     pWavCalLog:   pwavcallog,       $ ; name of wave cal obs log with its path.
; Not parameters but constants
      NormalMode:   0,                $ ; index of normal mode
      SmallSize:    1,                $ ; index of high cadence mode (1 min cadense)
;      STD_HiSpdE:   2,                $ ; index of high cadence + High Speed Eruption
;      FlatMode:     3,                $ ; index of flat mode
      DarkMode:     2,                $ ; index of dark mode
 ;     WhiteLight:   10,               $ ; index of white light mode            
      OneSh:        10,               $ ; index of white light mode
      PreviewMode:  20,               $ ; index of preview mode
 ;     FocusMode:    30,               $ ; index of focus mode
 ;     HiSpdErpt:    40,               $ ; index of High Speed Eruption mode
      GetDark:      100,              $ ; index of TF check
      FocusCk:      101,              $ ; index of foucus check
      Speckle:      102,              $ ; index of Speckle
 ;     LIGHT:        0,                $ ; index of camera light mode
 ;     Dark:         1,                $ ; index of camera dark mode
 ;     Flat:         2,                $ ; index of camera flat mode (light + flat lenz)
 ;     WavCal:       3,                $ ; index of wave cal mode (light + multi image spectrum)
 ;     PSmart1:      '1',              $ ; index of smartftp:/smart1 partition
 ;     PSmart2:      '2',              $ ; index of smartftp:/smart2 partition
      XC_RAW:       t1_xcen_raw,      $ ; [pix] X center of aperture mask.
      YC_RAW:       t1_ycen_raw,      $ ; [pix] Y center of aperture mask.
      RSun:         RSun,             $ ; [arcsec] Solar Radius in arcseconds.
      SS_MedArea:   ss_med_area,      $ ; positions of pixels to get a median intensity of the sun.
      MedBoxXmin:   med_xmin,         $ ; X coord of lower left corner of Int median box.
      MedBoxXmax:   med_xmax,         $ ; X coord of upper right corner of Int median box.
      MedBoxYmin:   med_ymin,         $ ; Y coord of lower left corner of Int median box.
      MedBoxYmax:   med_ymax,         $ ; Y coord of upper right corner of Int median box.
      WinSz_X:      winsz_x,          $ ; X size of display window
      WinSz_Y:      winsz_y,          $ ; Y size of display window
      AMask:        amask             $ ; 2D array (4098 x 4098 [pix]), aperture mask.
 ;     Circle_Xcen:  t1_xcen,          $ ; [pix] position X of the center of light stop circle.
 ;     Circle_Ycen:  t1_ycen,          $ ; [pix] position Y of the center of light stop circle.
 ;     Circle_X:     circle_x,         $ ; X position of points of light stop circle.
 ;     Circle_Y:     circle_y          $ ; Y position of points of light stop circle.
         }

  wparam={widget_param,               $
      SqMode:       gconst.NormalMode,$ ; Sequence mode
      SqModeCustm:  gconst.GetDark,   $ ; Memory for custom sequence mode
      CameraPos:    gconst.CamPosMid, $ ; Camera focus position [um]
;      expo:         20l,              $ ; exposure time [usec]
;      binx:         2l,               $ ; Binning X
;      biny:         2l,               $ ; Binning Y
;      Height:       4098l,            $ ; Height  max=4098 (biny=1)
;      Width:        4098l,            $ ; Width   max=4098 (binx=1)
;      RegionX:      0l,               $ ; start of region read out,pixel,left edge
;      RegionY:      0l,               $ ; start of region read out,pixel,top edge
;      FiltOffset:   0l,               $ ; filter offset [mA]
 ;     NDPos:        0l,               $ ; ND taret index (1-4)
 ;     KFlatWav:     0l,               $ ; Kuhn Flat Wavelength [mA]
      TodayIso:     todayiso,         $ ; Today ISO in JST
      DirStore:     dir_store,        $ ; save directory for normal operation
      FtpStore:     ftp_store,        $ ; ftp target directory
      FtpLogStore:  ftp_logstore,     $ ; ftp target directory for filter log
      FtpPartId:    ftp_part_id,      $ ; partition ID of the ftp target directory.
;      DirStoreCOP:  dir_store_cop,    $ ; save directory for custom operation
      FtpStoreCOP:  ftp_store_cop,    $ ; ftp target directory for custom operation
      ShbrIntObsStop:  1.0,           $ ; [V] Shabar Intensity in which Observation stop.
      ShbrIntObsStart: 1.5,           $ ; [V] Shabar Intensity in which Observation start.
      TimObsStppd:  gconst.Tomorrow,  $ ; [sec] Time when observation stopped.
;
      StsMessYm1:   10,               $ ; Y size (lines) of the messages window - 1
      Test:         testmode,         $ ; 0: normal mode, 1: test mode   
      LidChkOff:    LidChkOff         $ ; 0: normal mode, 1: Lid   
     }

  svar={static_variable,                    $
      Clock:        SYSTIME(1),            $ ; system time for widget clock
      AutoTrack:    0l,                     $ ; Auto track, 1:ON, 0:OFF.
      NormSeqDisp: 0l,                     $      
      UseShabarInf: 0l,                     $ ; Use Shabar Info, 1:ON, 0:OFF.
      AutoStart:    0l,                     $ ; Auto Start by Shabar Info, 1:ON, 0:OFF.
      PScale:       plate_scl,              $ ; [arcsec/pix] Plate scale.
      RSunPix:      RSunPix,                $ ; [pix] Solar Radius in original resolution pixels.
      ThSunRErr:    RSunPix * 0.01,         $ ; [pix] Threshold error (1%) of detected solar radius for auto tracking.
      SqContinue:   0l,                     $ ; Flag for sequence continue.
      SqStop:       1l,                     $ ; Flag for sequence stop.
      ProgSqStart:  0l,                     $ ; Flag for program sequence start in the timer block.
 ;     WhtLgtObsStart:0l,                    $ ; Flag for white light obs. start in the normal mode sequence.
 ;     WavCalSqStart:0l,                     $ ; Flag for wave cal sequence start in the normal mode sequence.
 ;     HiSpdErptStart:0l,                    $ ; Flag for High Speed Eruption Obs. start in STD sequence.
 ;     FiltZeroRstStart:1l,                  $ ; Flag for Filter Zero Reset start.
 ;     CustomOp:     0l,                     $ ; Flag for custom operation.
 ;     FiltInitDone: 0l,                     $ ; Flag if filter init has done.
 ;     FiltJustInit: 0l,                     $ ; Flag if filter init has just done.
      ProgSqMode:   gconst.NormalMode,      $ ; Program sequence mode in the timer block.
      ProgSqMode2:  gconst.NormalMode,      $ ; Program sequence mode in the timer block (slot2).
 ;     PreFiltOffset:9999l,                  $ ; previous filter offset [mA]
 ExpNorm:      ExpNorm,          $ ; exposure set for the standard obs. sequences.
 CadNorm:      CadNorm,          $ ; time cadence for the standard obs. sequenees.
 WavOneSh:     WavOneSh,         $ ; wavelength set for the one shot
 ;ExpOneSh:     ExpOneSh,         $ ; exposure set for the one shot
 WavPrVw:     WavPrVw,         $ ; wavelength set for the preview
 ;ExpPrVw:     ExpPrVw,         $ ; exposure set for the preview
     FiltOffset:   0l,                      $ ; filter offset [mA]
 ;    FiltSetTim:   systime(1) - gconst.F_TempDisTim,   $ ; filter set time
      Temp_F:       9999d,                  $ ; filter temperature
 ;     FiltTempTim:  systime(1) - gconst.F_TempCadence,  $ ; filter temperature get time
 ;     WhtLgtTim:    WhtLgtTim,              $ ; Last white light obs. time
 ;     WaveCalTim:   WaveCalTim,             $ ; Last wave cal sequence time
 ;     HiSpdErptTim: SYSTIME(1) - gconst.HiSpdErptCadence, $ ; [sec] Last High Speed Eruption Obs. time
      Flare:        0l,                     $ ; Flag for flare
      IntMax:       0l,                     $ ; Maximum Intensity of binned image (bin 4 for averaging) [DN].
      IntMedian:    0l,                     $ ; Median of image intensity around the light stop center [DN].
      IntContrast:  0d,                     $ ; (IntMax - IntMedian) / IntMedian.
      IntShabar:    0d,                     $ ; Light intensity from shabar sensor [V].
      IntShabarStr: '',                     $ ; Light intensity from shabar sensor [V] (String format).
      ScntShabarStr:'',                     $ ; Scintillation from shabar sensor x 100 [%] (String format).
      TimeShabarStr:'',                     $ ; Time stamp of shabar sensor data [JST] (String format).
      IfShabarStopped:1l,                   $ ; If HSHABAR stopped, [1:Stopped, 0:Working].
      ValidShabarInf:0l,                    $ ; If shabar info is enough new, [1:Valid, 0:Not Valid].
      ShbrUpdateTim:0d,                     $ ; Time stamp of Shabar info updated time [sec in JST]
      BeaconUpdateTim:0d,                   $ ; Time stamp of beacon updated time [sec in JST]
      IsSaturated:  0l,                     $ ; Flag if saturated.
      NumSaturated: 0l,                     $ ; Number of images saturated in one sequence.
      SunXCount:    t1_xcen_raw,            $ ; [pix] X center of The Sun
      SunYCount:    t1_ycen_raw,             $ ; [pix] Y center of The Sun
;      Do_NDUp:      0l,                     $ ; Flag if ND UP is necessary.
;      Do_NDDown:    0l,                     $ ; Flag if ND DOWN is necessary.
      E_LID:        0l,                     $ ; Entrance lid status [0:close, 1:open]
 ;     F_ZeroReset:  0l,                     $ ; Filter Zero Reset after a sequence [0:done, 1:not yet]
      MTime_syslog: systime(1),             $ ; system log mtime in msec resolution 
      StsMesLines:  0l,                     $ ; number of the messages lines
      FileID:       incr_id,                $ ; Incremental file ID
 ;     WhtLgtID:     whtlgt_id,              $ ; Number of white light obs. for today
 ;     WavCalID:     wavcal_id,              $ ; Number of wave cal obs. for today
      TestFiles:    test_files,             $ ; filenames of test data for the test mode.
      TestFNum:     N_ELEMENTS(test_files), $ ; Number of the test data for the test mode.
      TestFileID:   0l,                     $ ; Incremental test file ID
      FtpFailC:     ftpfailc                $ ; count of fits files ftp transfer failed
        }
        
  diag ={dialogue,                                       $
      InitObsSys:    'Initializing obs. system. Wait.',    $ ; Dialogue for initializing system.
      ObsReady:      'Observation READY.',                 $ ; Observation READY.
      StopEncount:   'Stop encountered.',                  $ ; Stop encountered.
      ObsStd:        'Observing',                          $ ; Standard Obs.
      ObsStop:       'Sequence STOPPED',                   $ ; Sequence STOPED.
      ObsStopLC:     'Sequence STOPPED (Lid Close)',       $ ; Sequence STOPED (Lid close).
      ObsStopErr:    'Sequence STOPPED (Lid Error?)',      $ ; Sequence STOPED (Lid Error?).
      ATObsStd:      'Observing (auto)',                   $ ; Standard Obs. (auto run).
      ATObsStop:     'Sequence STOPPED (auto)',            $ ; Sequence STOPPED (auto run).
      ATObsStopLC:   'Sequence STOPPED (auto, Lid Close)', $ ; Sequence STOPPED (auto run, Lid close).
      ATObsStopErr:  'Sequence STOPPED (auto, Lid Error?)',$ ; Sequence STOPPED (auto run, Lid Error?).
      dummy:         'dummy'                               $ ; dummy
        }
        
 
END

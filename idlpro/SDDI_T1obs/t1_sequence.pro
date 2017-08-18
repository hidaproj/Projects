;                                Time-stamp: <2012-09-20 16:22:25 satoshi>
;
;	2016.08.13	k.i.	GET_FLAT positions
;
;SDDI_GET,SAVE_IMG_CUBE
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION check_interval, last_time

  delta_t = systime(1) - last_time

  RETURN, delta_t
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;FUNCTION SDDI_GET,wl=wl
;
;
;COMMON LIB, gconst, wparam, svar, diag, state
;COMMON RAWIMAGEINFO, rawimage, cstatus, fstatus, tstatus, date_obs, exptime
;COMMON tf_orca, o,p,f,b
;
;
;exptime  = exp
;p.expo=exp*10.^(-3.)   ;[Orca sec, exp usec]
;print,p.expo
;p=OrcaSetParam(expo=p.expo)
;if n_elements(wl) eq 1 then begin
;  svar.FiltOffset =wl*10.  ;[T1Obs mA, wl pm]
;utf_set_wl,f,dwl=wl/1000.,wait=0.1 ;[TF nm, wl pm]
;WIDGET_CONTROL, state.wStsOffSetF, SET_VALUE=string(svar.FiltOffset,format="(i7.1)")
;WIDGET_CONTROL, state.wStsTempF, SET_VALUE=string(f.b[0].temp,format="(f6.2)")
;endif
;
;date_obs = SYSTIME(1)
;if n_elemetns(capture) eq 1 then orca_capture else dmy=orcaobs(nimg=1,/noread) $
;  if n_elements(wl) eq 1 then begin
;  svar.FiltOffset =wl*10.  ;[T1Obs mA, wl pm]
;  utf_set_wl,f,dwl=wl/1000.,wait=0.1 ;[TF nm, wl pm]
;  WIDGET_CONTROL, state.wStsOffSetF, SET_VALUE=string(svar.FiltOffset,format="(i7.1)")
;  WIDGET_CONTROL, state.wStsTempF, SET_VALUE=string(f.b[0].temp,format="(f6.2)")
;endif
;
;
;
;imgs=intarr(2048,2048)+500
;imgs=rotate(imgs,6)
;
;
;svar.IntMax       = MAX( REBIN(rawimage[1:4096,1:4096], 1024, 1024) )  ; bin 4 for avaraging (~2.3"x2.3" area)
;svar.IntMedian    = MEDIAN(imgs[gconst.SS_MedArea] )
;svar.IntContrast  = DOUBLE(svar.IntMax - svar.IntMedian) / DOUBLE(svar.IntMedian) * 100.
;
;
;RETURN,imgs
;
;
;END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION TAKE_ONE_SHOT

  COMMON LIB, gconst, wparam, svar, diag, state
  COMMON RAWIMAGEINFO, rawimage, cstatus, fstatus, tstatus, date_obs, exptime, date_obs1, date_obs2
  COMMON tf_orca, o,p,f,b,dwl_list


  WRITE_WIDGET_MESSAGE, '*** Take One Shot Start ***.', /FORCE
  WSET, state.wid
  ERASE
  WSET, state.wDrawStsId
  TV, BYTARR(300, 40) + 255
  XYOUTS, 150, 10, '!17Take One Shot', /device, charsize=1.6, color=500000000,alignment=0.5

  WSET, state.wid

  ;help,tstatus

  exptime=svar.ExpNorm

  dwl_list=strarr(10)
  dwl_list[0]='['+string(svar.WavOneSh,format="(i5.1)")+']'

  ;rawimage=SDDI_GET(svar.ExpNorm,wl=svar.WavOneSh)
  date_obs = SYSTIME(1)
  dmy=orcaobs(nimg=1,/noread)


  wl=svar.WavOneSh
  svar.FiltOffset =wl*10.  ;[T1Obs mA, wl pm]
  utf_set_wl,f,dwl=wl/1000.,wait=0.1 ;[TF nm, wl pm]
  WIDGET_CONTROL, state.wStsOffSetF, SET_VALUE=string(svar.FiltOffset,format="(i7.1)")
  ;WIDGET_CONTROL, state.wStsTempF, SET_VALUE=string(f.b[0].temp,format="(f6.2)")
  WIDGET_CONTROL, state.wStsTempF, SET_VALUE=string(f.b[0].temp,format="(f6.2)")+',  '+string(f.b[6].temp,format="(f6.2)")
  imgs=orca_getimg(nimg=1,/nowait)

  ;rawimage=rotate(imgs,6)
  rawimage=imgs


  date_obs1=date_obs
  date_obs2=date_obs

  ret=IMAGE_DISP()
  svar.IntMedian    = MEDIAN(imgs[gconst.SS_MedArea] )
  DSKFIND_SMT, rawimage, gconst.AMask, svar.RSunPix, SunXCount, SunYCount, SunRout
  svar.SunXCount = SunXCount
  svar.SunYCount = SunYCount
  WRITE_WIDGET_MESSAGE, '[DSKFIND_SMT]:',/force
;
;  dumy=wparam.DirStore
;  wparam.DirStore='\\Smart-t1-room\g\SDDI_oneshot\'

  ret=SAVE_IMAGE_CUBE(rawimage,dir='\\smart-t1-room\g\SDDI_oneshot')
  print,'Update_TelStatus' & tic
  Update_TelStatus
  toc
;  wparam.DirStore=dumy


  WRITE_WIDGET_MESSAGE, '*** Take One shot: finished ***.', /FORCE

  WSET, state.wDrawStsId
  ERASE
  XYOUTS, 150, 10, '!17Take One Shot: finished', /device,charsize=1.6, color=255,alignment=0.5

  svar.SqStop     = 1l

  RETURN, 1
END



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION SEQUENCE_LIGHT,wl_arr

  COMMON LIB, gconst, wparam, svar, diag, state
  COMMON RAWIMAGEINFO, rawimage, cstatus, fstatus, tstatus, date_obs, exptime, date_obs1, date_obs2
  COMMON tf_orca, o,p,f,b,dwl_list

  
  svar.SqContinue=1


  nn_wl=n_elements(wl_arr)

  
  dwl_list=strarr(10)

  for ii=0,nn_wl-1 do begin
    id_c=fix(ii/10)
    dwl_list[id_c]=dwl_list[id_c]+string(wl_arr[ii],format="(i4.1)")+','
  endfor

  for ii=0,9 do dwl_list[ii]='['+dwl_list[ii]+']'

  last_time= systime(1)-svar.CadNorm-1.0
  imgs=uintarr(2048,2048,nn_wl)

  ;set capturemode sequence
  orca_capturemode_sequence,nimg=1

  ;continuous capturing start
  orca_capture
  icen=where(wl_arr eq 0) & icen=icen[0]

  WHILE svar.SqContinue DO BEGIN

    ck_intval=check_interval(last_time)


    if ck_intval ge svar.CadNorm then begin
      last_time= systime(1)

      for i=0,nn_wl-1 do begin
        wl=wl_arr[i]

        utf_set_wl,f,dwl=wl/1000.,wait=0.1 ;[TF nm, wl pm]
        svar.FiltOffset =wl*10.  ;[T1Obs mA, wl pm]
        WIDGET_CONTROL, state.wStsOffSetF, SET_VALUE=string(svar.FiltOffset,format="(i7.1)")
        WIDGET_CONTROL, state.wStsTempF, SET_VALUE=string(f.b[0].temp,format="(f6.2)")+',  '+string(f.b[6].temp,format="(f6.2)")
        date_obs = SYSTIME(1)
        rawimage=orca_getimg(nimg=1);,/nowait)
        imgs[*,*,i]=rawimage

        if svar.NormSeqDisp then ret=IMAGE_DISP()

        WRITE_WIDGET_MESSAGE, 'Taking '+string(i+1,format="(i3.1)")+'/'+string(nn_wl,format="(i3.1)")
        case i of 
                   0: date_obs1=date_obs
                icen: date_obsc=date_obs
             nn_wl-1: date_obs2=date_obs
               else:
        endcase
      endfor

      rawimage=rotate(rawimage,6)
      ;print,gconst.XC_RAW,gconst.YC_RAW
      AUTO_TRACK,gconst.XC_RAW,gconst.YC_RAW
      Update_ShabarInf
      
      

      rawimage=imgs[*,*,icen]
      svar.IntMedian    = MEDIAN(rawimage[gconst.SS_MedArea] )
      svar.FiltOffset=0
      ret=IMAGE_DISP()
      
      DATE_OBS_UTC      = TAI2UTC(date_obsc + 378691200d0, /NOCORRECT, /EXTERNAL)
      time_iso          = UTC2STR(DATE_OBS_UTC)+'Z'

      profile_dat=imgs[1024,1024,*]
      get_lun,Unit
      openw,Unit,gconst.pFiltLog,/append
      ;     openw,Unit,'c:\log\test.dat',/append
      printf,Unit,format="(a,',',2(f6.2,','),"+string(nn_wl-1,format="(i2.2)")+"(f10.1,','),f10.1)",time_iso,f.b[0].temp,f.b[6].temp,profile_dat
      close,Unit
      free_lun,Unit
   
      plot,wl_arr/100.,profile_dat,psym=1,pos=[0.1,0.05,0.3,0.25], $
          xrange=[-3,3],/xstyle,chars=1,syms=1,/noerase
      
      date_obs=date_obsc
      lefttime = svar.CadNorm -(systime(1)-last_time)
      WRITE_WIDGET_MESSAGE, 'Waiting...'+string(lefttime,form='(f5.1)')+'sec'
      ret=SAVE_IMAGE_CUBE(imgs)


    endif

    ;wait,0.5
    ck=WIDGET_EVENT(state.wNormSeqEd, /nowait)
    if ck.id gt 0 then begin
      WRITE_WIDGET_MESSAGE, '*** Sequence Stop encountered ***.'
      svar.SqContinue=0
    endif

    wait,0.1 ; (Comment out by otsuji 2016.06.14)

    BREAK_BY_SKYCONDITION

  ENDWHILE


  RETURN, 1
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;FUNCTION SEQUENCE_DARK
;
;  COMMON LIB, gconst, wparam, svar, diag, state
;
;  ;*** Set ProgSqStart flag for returning to the Normal Operation.
;  svar.ProgSqStart = 1l
;  ;*** Set ProgSqMode for returning to the Normal Operation.
;  svar.ProgSqMode  = svar.ProgSqMode2
;
;
;  ;*** Unset SqContinue flag.
;  svar.SqContinue = 0l


  ;蓋しめる

  ;露出時間でダークをとる(Normalのexp)を使う

  ;通常観測に戻る


;  RETURN, 1
;END



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION PREVIEW

  COMMON LIB, gconst, wparam, svar, diag, state
  COMMON RAWIMAGEINFO, rawimage, cstatus, fstatus, tstatus, date_obs, exptime,date_obs1,date_obs2
  COMMON tf_orca, o,p,f,b

  svar.SqStop      = 0l

  WRITE_WIDGET_MESSAGE, '*** Preview Start ***.', /FORCE
  WSET, state.wid
  ERASE
  WSET, state.wDrawStsId
  TV, BYTARR(300, 40) + 255
  XYOUTS, 150, 10, '!17Preview mode', /device, charsize=1.6, color=500000000,alignment=0.5

  WSET, state.wid

  ;  ret = GET_MTIME_STS()   ; [sec] with msec resolution  ; SM

  svar.SqContinue = 1l

  ;  ret   = FILTER_SET( wl_arr[0] )
  ret   = GET_MTIME_STS()
  ;  WRAPPER_FILTER_WAIT
  ;  WIDGET_CONTROL, state.wStsOffSetF, SET_VALUE=string(svar.FiltOffset, format = '(i5)')
  utf_set_wl,f,dwl=svar.WavPrVw*10.^3
  svar.FiltOffset =svar.WavPrVw*10.
  WIDGET_CONTROL, state.wStsOffSetF, SET_VALUE=string(svar.FiltOffset,format="(i7.1)")
  WIDGET_CONTROL, state.wStsTempF, SET_VALUE=string(f.b[0].temp,format="(f6.2)")+','+string(f.b[6].temp,format="(f6.2)")
  ;set capturemode sequence
  orca_capturemode_sequence,nimg=1

  ;continuous capturing start
  orca_capture

  WHILE svar.SqContinue DO BEGIN

    date_obs = SYSTIME(1)
    date_obs1=date_obs
    date_obs2=date_obs


    ;dmy=orcaobs(nimg=1,/noread)

    rawimage=orca_getimg(nimg=1);,/nowait)

    ;    rawimage=rotate(rawimage,6)

    ;    rawimage=intarr(2048,2048)+4.0

    ret      = IMAGE_DISP()

    ;    wait,2
    ck=WIDGET_EVENT(state.wPrVwEd, /nowait)
    if ck.id gt 0 then svar.SqContinue=0
  ENDWHILE



  WRITE_WIDGET_MESSAGE, '*** Preview Stopped ***.', /FORCE

  WSET, state.wDrawStsId
  ERASE
  XYOUTS, 150, 10, '!17Preview STOPPED', /device,charsize=1.6, color=255,alignment=0.5

  svar.SqStop     = 1l
  ;  ret      = TELESCOPE_STATUS(tstatus)

  print,'Update_TelStatus' & tic
  Update_TelStatus
  toc


  RETURN, 1l
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION FOCUS_MODE

  COMMON LIB, gconst, wparam, svar, diag, state
  COMMON RAWIMAGEINFO, rawimage, cstatus, fstatus, tstatus, date_obs, exptime,date_obs1,date_obs2
  COMMON tf_orca, o,p,f,b

  ;forcusをある範囲でふりながら画像をとってく
  ;波長は、-0.6Aとか？
  ;コントラスト？を計算して
  ;値と小さくした画像を表示
  ;おすすめfocus値を表示
  ;セットは、PC7で

;  print,'Update_TelStatus' & tic
;  Update_TelStatus
;  toc

  Update_TelStatus
  ra_org=tstatus.RA_POS
  dec_org=tstatus.DEC_POS
  print,tstatus.RA_STS,tstatus.DEC_STS
  RETURN, 1l
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;FUNCTION TF_CHECK
FUNCTION GET_DARK

  COMMON LIB, gconst, wparam, svar, diag, state
  COMMON RAWIMAGEINFO, rawimage, cstatus, fstatus, tstatus, date_obs, exptime,date_obs1,date_obs2
  COMMON tf_orca, o,p,f,b

  ;大辻君のにわたす

  ret=ELID_CLOSE()
orca_idle
  date_obs = SYSTIME(1)
  date_obs1=date_obs

  dmy=orcaobs(nimg=gconst.DarkImgNum,/noread)
  date_obs = SYSTIME(1)
  date_obs2=date_obs

  imgs=orca_getimg(nimg=gconst.DarkImgNum,/nowait)
  rawimage=imgs[*,*,50]

  ret=IMAGE_DISP()
  ; ret=SAVE_IMAGE_CUBE(imgs)

  ;SAVE_DIR      = wparam.DirStore
  DATE_OBS_UTC      = TAI2UTC(date_obs1 + 378691200d0, /NOCORRECT, /EXTERNAL)
  time_iso          = UTC2STR(DATE_OBS_UTC)+'Z'

  DATE_OBS_UTC      = TAI2UTC(date_obs1 + 378691200d0, /NOCORRECT, /EXTERNAL)
  p.date_obs          = UTC2STR(DATE_OBS_UTC)+'Z'
  DATE_OBS_UTC2      = TAI2UTC(date_obs2 + 378691200d0, /NOCORRECT, /EXTERNAL)
  p.date_obs2          = UTC2STR(DATE_OBS_UTC2)+'Z'


  nax=size(imgs,/n_dimension)
  dim=size(imgs,/dimension)
  nax1=dim[0]
  if nax ge 2 then nax2=dim[1] else nax2=1
  if nax ge 3 then nax3=dim[2] else nax3=1

  fits_fname =                                             $
    string((DATE_OBS_UTC.YEAR MOD 100), format='(i2.2)') + $
    string(DATE_OBS_UTC.MONTH, format='(i2.2)')          + $
    string(DATE_OBS_UTC.DAY, format='(i2.2)')            + $
    'T'                                                  + $
    string(DATE_OBS_UTC.HOUR, format='(i2.2)')           + $
    string(DATE_OBS_UTC.MINUTE, format='(i2.2)')         + $
    string(DATE_OBS_UTC.SECOND, format='(i2.2)')

  ;save,imgs,exptime,fil=SAVE_DIR+'\'+fits_fname+'.sav'

  dwl_list=strarr(10)

  fh=strarr(36)
  fh[0] =string('SIMPLE  = ','T',format='(a10,a20," /")')
  fh[1] =string('BITPIX  = ',16,format='(a10,i20," /")')
  fh[2] =string('BZERO   = ',0,format='(a10,i20," /")')
  fh[3] =string('BSCALE  = ',1,format='(a10,i20," /")')
  fh[4] =string('NAXIS   = ',nax,format='(a10,i20," /")')
  fh[5] =string('NAXIS1  = ',nax1,format='(a10,i20," /")')
  fh[6] =string('NAXIS2  = ',nax2,format='(a10,i20," /")')
  fh[7] =string('NAXIS3  = ',nax3,format='(a10,i20," /")')
  fh[8] =string('DATE_OBS= ',"'"+p.date_obs+"'",format='(a10,a30," /")')
  fh[9] =string('DATE_OB2= ',"'"+p.date_obs2+"'",format='(a10,a30," /")') ; end time of integ
  fh[10]=string('OBSERVAT= ','Hida',format='(a10,a20," /")')
  fh[11]=string('TELESCOP= ','SMART',format='(a10,a20," /")')
  fh[12]=string('INSTRMNT= ','SDDI',format='(a10,a20," /")')
  fh[13]=string('EXP     = ',p.expo,format='(a10,f20.10," /")')
  fh[14]=string('WAVE0   = ',f.wl0,format='(a10,f20.2," / nm")')
  fh[15]=string('DWLLIST0= ',dwl_list[0],format='(a10,a53," /")');
  fh[16]=string('DWLLIST1= ',dwl_list[1],format='(a10,a53," /")');
  fh[17]=string('DWLLIST2= ',dwl_list[2],format='(a10,a53," /")');
  fh[18]=string('DWLLIST3= ',dwl_list[3],format='(a10,a53," /")');
  fh[19]=string('DWLLIST4= ',dwl_list[4],format='(a10,a53," /")');
  fh[20]=string('DWLLIST5= ',dwl_list[5],format='(a10,a53," /")');
  fh[21]=string('DWLLIST6= ',dwl_list[6],format='(a10,a53," /")');
  fh[22]=string('DWLLIST7= ',dwl_list[7],format='(a10,a53," /")');
  fh[23]=string('DWLLIST8= ',dwl_list[8],format='(a10,a53," /")');
  fh[24]=string('DWLLIST9= ',dwl_list[9],format='(a10,a53," /")');
  fh[25]='TEMP    =  '
  fh[26]='LCRET   =  '
  fh[27]='LCVOLT  =  '
  fh[28]='INT_SHBR'+'= '+ string(svar.IntShabar,format='(E19.10)')
  fh[29]='SCNTSHBR'+'= '+ string(DOUBLE(svar.ScntShabarStr),format='(E19.10)')
  fh[30]='CRPIX1  = '+string(svar.SunXCount,format="(f6.1)")
  fh[31]='CRPIX2  = '+string(svar.SunYCount,format="(f6.1)");
  ;fh[32]='COMMENT = none'
  ;fh[33]='HISTORY = RAW'
  fh[32]=string(' ',format='(a10)')
  fh[33]=string(' ',format='(a10)')
  fh[34]=string(' ',format='(a10)')
  fh[35]=string('END       ',format='(a10)')
  blnk80=string(' ',format='(a80)')
  fh=strmid(fh+blnk80,0,80)

  file='\\smart-t1-room\g\SDDI_dark\' + fits_fname+'_dark.fits'
  get_lun,Unit
  openw,Unit,file
  for j=0,35 do begin
    writeu,Unit,fh[j]
  endfor
  byteorder,imgs
  writeu,Unit,imgs
  close,Unit
  free_lun,Unit
  byteorder,imgs



  WRITE_WIDGET_MESSAGE, '[IMAGE_SAVE()]: ' + fits_fname+'_dark', /FORCE

  RETURN, 1l
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION GET_FLAT

  COMMON LIB, gconst, wparam, svar, diag, state
  COMMON RAWIMAGEINFO, rawimage, cstatus, fstatus, tstatus, date_obs, exptime,date_obs1,date_obs2
  COMMON tf_orca, o,p,f,b,dwl_list
  COMMON bridge,bridge

;  nn_wl=65
;  wl_arr=findgen(nn_wl)*25.-800.
;  imgs=uintarr(2048,2048,nn_wl)
caldat, systime(0,/JULIAN),mm,dd,yy
today = string(yy,format='(i4.4)')+string(mm,format='(i2.2)')+string(dd,format='(i2.2)')

  dir='\\smart-t1-room\g\SDDI_flat\'+today  ;保存先
  spawn,'mkdir -p '+dir
  
orca_idle

;  nn_wl=9
;  wl_arr=findgen(nn_wl)*25.-100.
 
   nn_wl=73
   wl_arr=findgen(nn_wl)*25.-900. ;波長
  
  nn_add=20 ;積算枚数
  
  
  dwl_list=strarr(10)

  for ii=0,nn_wl-1 do begin
    id_c=fix(ii/10)
    dwl_list[id_c]=dwl_list[id_c]+string(wl_arr[ii],format="(i4.1)")+','
  endfor

  for ii=0,9 do dwl_list[ii]='['+dwl_list[ii]+']'
  imgs=uintarr(2048,2048,nn_wl)
  
  Update_TelStatus
  ra_org=tstatus.RA_POS
  dec_org=tstatus.DEC_POS
  print,tstatus.RA_STS,tstatus.DEC_STS
  
  move_pos_arr_org=[ $ 
  [-65-5,-820], $
  [-65-5,820], $
  [-45,640],$
  [-45,0],$
  [-45,-640],$
  [-30,-320],$
  [-30,0],$
  [-30,320],$
  [-13,80],$
  [-13,0],$
  [-13,-80], $
  [0,-640],$
  [0,-320],$
  [0,-80],$
  [0,0], $
  [0,80], $
  [0,320],$
  [0,640], $
  [7+5,80],$
  [7+5,0],$
  [7+5,-80],$
  [17+5,-320],$
  [17+5,0], $
  [17+5,320],$
  [35+5,640],$
  [35+5,0],$
  [35+5,-640],$
  [50+5,-820],$
  [50+5,820]$
  ]
  
  move_pos_arr=[ $ ; Ichimoto 2016.08.13
 [-65-5,-820], [-65-5,-520], [-65-5,520],  [-65-5,820], $
 [-45,700], [-45,0], [-45,-700],$
 [-15, 0], [-5, 0], $
 [0,-820], [0,-600], [0, -160], [0, -40], [0,0], [0, 20], [0, 80], [0, 320], [0, 700], [0, 820], $
 [3,0], [8, 0], [27, 0], $
 [35+5,700], [40,0], [35+5,-700], $
 [50+5,-820], [50+5,-520], [50+5,520], [50+5,820] $
 ]

   
  date_obs = SYSTIME(1)
  date_obs1=date_obs


ss=size(move_pos_arr)
nn_pos=ss[2]
ra0=0
dec0=0

  for jj=0,nn_pos-1 do begin

  RA_move=move_pos_arr[0,jj]
  
  set_ra_pos  = ra_org  + LONG(RA_move  * 10.d)
  DEC_move= move_pos_arr[1,jj]
  set_dec_pos = dec_org + LONG(DEC_move * 10.d)

  if RA_move ne ra0 and DEC_move ne dec0 then P2P_RA_DEC_SET, set_ra_pos, set_dec_pos
  if RA_move eq ra0 and DEC_move ne dec0 then P2P_DEC_SET, set_dec_pos
  if RA_move ne ra0 and DEC_move eq dec0 then P2P_RA_SET, set_ra_pos

  ra0=RA_move
  dec0=DEC_move
  
  wait,2
  
;  Update_TelStatus 
  ret=TELESCOPE_STATUS( tstatus, /silent )
  print,tstatus.RA_STS,tstatus.DEC_STS
  ck_fin_ra=0 & ck_fin_dec= 0
  while (ck_fin_ra +ck_fin_dec) ne 2 do begin
ret=TELESCOPE_STATUS( tstatus, /silent )
print,tstatus.RA_STS,tstatus.DEC_STS
  if tstatus.RA_STS eq 2 then ck_fin_ra=1
  if tstatus.DEC_STS eq 0 then ck_fin_dec=1
  wait,1
;  Update_TelStatus

  endwhile
  

  date_obs = SYSTIME(1)
  date_obs1=date_obs

  for ii=0,nn_wl-1 do begin

    wl=wl_arr[ii]

    utf_set_wl,f,dwl=wl/1000.,wait=0.1 ;[TF nm, wl pm]
    WIDGET_CONTROL, state.wStsOffSetF, SET_VALUE=string(wl*10,format="(i7.1)")

    wait,0.1
    orca_idle
    data=orcaobs(nimg=nn_add);,/noread)
    date_obs = SYSTIME(1)
    date_obs2=date_obs
   ; data=orca_getimg(nimg=nn_add,/nowait)

    ;** rebin img w/ bridge **
    if 0 then begin
    done=0b
    while done ne 1 do begin
      for i=0,n_elements(bridge)-1 do begin
        if bridge[i]->status() eq 0 then begin
          bridge[i]->SetVar,'data',data
          bridge[i]->SetVar,'rawimage',rawimage
          bridge[i]->Execute,'rawimage=congrid(data,2048,2048,1)',/nowait
          done=1b
          break      
        endif
      endfor
    endwhile
    endif
    rawimage=congrid(data,2048,2048,1)
    imgs[*,*,ii]=rawimage

    

  endfor

  ret=IMAGE_DISP()

;  DSKFIND_SMT, rawimage, gconst.AMask, svar.RSunPix, SunXCount, SunYCount, SunRout
  DSKFIND_T1, rawimage, SunXCount, SunYCount, SunRou, RSunPix=svar.RSunPix, /SDDI
  svar.SunXCount = SunXCount
  svar.SunYCount = SunYCount
  print,SunXCount,SunYCount
  WRITE_WIDGET_MESSAGE, '[DSKFIND_SMT]:',/force
  
  profile_dat=imgs[SunXCount,SunYCount,*]
  
  plot,wl_arr/100.,profile_dat,psym=1,pos=[0.1,0.05,0.7,0.25], $
    xrange=[-10,10],/xstyle,chars=1,syms=1,/noerase
  plots,[2048-SunYCount-48],[2048-SunXCount-48],/dev,psym=4,color=0

  
  
  
  
  ret=SAVE_IMAGE_CUBE(imgs,dir=dir)

  print,jj
  endfor
  
  set_ra_pos  = ra_org  
  
  set_dec_pos = dec_org 

  P2P_RA_DEC_SET, set_ra_pos, set_dec_pos 
  wait,2

  ;  Update_TelStatus
  ret=TELESCOPE_STATUS( tstatus, /silent )
  print,tstatus.RA_STS,tstatus.DEC_STS
  ck_fin_ra=0 & ck_fin_dec= 0
  while (ck_fin_ra +ck_fin_dec) ne 2 do begin
    ret=TELESCOPE_STATUS( tstatus, /silent )
    print,tstatus.RA_STS,tstatus.DEC_STS
    if tstatus.RA_STS eq 2 then ck_fin_ra=1
    if tstatus.DEC_STS eq 0 then ck_fin_dec=1
    wait,1
    ;  Update_TelStatus

  endwhile

  RETURN, 1l

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION FILTER_CHECK

  COMMON LIB, gconst, wparam, svar, diag, state
  COMMON RAWIMAGEINFO, rawimage, cstatus, fstatus, tstatus, date_obs, exptime,date_obs1,date_obs2
  COMMON tf_orca, o,p,f,b

  

  ;wl
  ;utf_set_wl,f,dwl=wl
  ;imgs=OrcaObs(nimg=100,/nowait)
  ;imgs=orca_getimg(nimg=100)
  ;data=imgs


  RETURN, 1l
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION SPECKLE_DATA

  COMMON LIB, gconst, wparam, svar, diag, state
  COMMON RAWIMAGEINFO, rawimage, cstatus, fstatus, tstatus, date_obs, exptime,date_obs1,date_obs2
  COMMON tf_orca, o,p,f,b

  ;speckleかけるようのデータをとる

  ;wl
  ;utf_set_wl,f,dwl=wl
  ;imgs=OrcaObs(nimg=100,/nowait)
  ;imgs=orca_getimg(nimg=100)
  ;data=imgs


  RETURN, 1l
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION NORM_SEQUENCE, sqmode

  COMMON LIB, gconst, wparam, svar, diag, state

  svar.SqStop      = 0l
  ;  svar.F_ZeroReset = 1l

  WRITE_WIDGET_MESSAGE, '[SEQUENCE()]: ** Sequence Start **'
  WSET, state.wDrawStsId
  TV, BYTARR(300, 40) + 255
;  CASE sqmode OF
;    gconst.DarkMode :   XYOUTS, 150, 10, '!17Dark image', /device, charsize=1.6, color=500000000,alignment=0.5
;    ELSE:              BEGIN
      !p.font = 0
      IF (svar.AutoStart EQ 1) AND (svar.UseShabarInf EQ 1) THEN $
        XYOUTS, 150, 10, '!17'+diag.ATObsStd, /device,charsize=1.6, color=500000000,alignment=0.5 ELSE $
        XYOUTS, 150, 10, '!17'+diag.ObsStd, /device,charsize=1.6, color=500000000,alignment=0.5
      !p.font = -1
;    END
;  ENDCASE
  WSET, state.wid

  ret = GET_MTIME_STS()   ; [sec] with msec resolution  ; SM

  ;  ret = TELESCOPE_STATUS(tstatus)


  ;  ret = CHECK_FTPDEST()
  svar.ProgSqMode=sqmode

  CASE sqmode OF
;    gconst.DarkMode   : ret = SEQUENCE_DARK()
    gconst.NormalMode   : ret = SEQUENCE_LIGHT(gconst.WavNormObs)
    gconst.SmallSize  : ret = SEQUENCE_LIGHT(gconst.WavSmlObs)
    gconst.MinSize  : ret = SEQUENCE_LIGHT(gconst.WavMinObs)
    ;     gconst.OneSh:       ret= TAKE_ONE_SHOT()
    ELSE:
  ENDCASE

  ;  ret = CAMERA_CLOSE()
  ;  ret = FILTER_CLOSE()

  ;  ret = LOG_FTP()
  ;  IF ret EQ 1 THEN WRITE_WIDGET_MESSAGE, '[LOG_FTP()]: Log files FTP transferred.'

  WRITE_WIDGET_MESSAGE, '[SEQUENCE()]: ** Sequence Stopped **'

  WSET, state.wDrawStsId
  ERASE
  !p.font = 0
  IF (svar.AutoStart EQ 1) AND (svar.UseShabarInf EQ 1) THEN BEGIN
    CASE svar.E_LID OF
      0: XYOUTS,    150, 10, '!17'+diag.ATObsStopLC,  /device,charsize=1.6, color=255,alignment=0.5
      1: XYOUTS,    150, 10, '!17'+diag.ATObsStop,    /device,charsize=1.6, color=255,alignment=0.5
      ELSE: XYOUTS, 150, 10, '!17'+diag.ATObsStopErr, /device,charsize=1.6, color=255,alignment=0.5
    ENDCASE
  ENDIF ELSE BEGIN
    CASE svar.E_LID OF
      0: XYOUTS,   150, 10, '!17'+diag.ObsStopLC,  /device,charsize=1.6, color=255,alignment=0.5
      1: XYOUTS,   150, 10, '!17'+diag.ObsStop,    /device,charsize=1.6, color=255,alignment=0.5
      ELSE: XYOUTS, 150, 10, '!17'+diag.ObsStopErr, /device,charsize=1.6, color=255,alignment=0.5
    ENDCASE
  ENDELSE
  !p.font = -1
  WSET, state.wid

  svar.SqStop     = 1l

  RETURN, 1l
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION CUSTM_SEQUENCE, sqmode

  COMMON LIB, gconst, wparam, svar, diag, state

  svar.SqStop      = 0l
  ;  svar.F_ZeroReset = 1l

  WRITE_WIDGET_MESSAGE, '[SEQUENCE()]: ** Sequence Start **'
  WSET, state.wDrawStsId
  TV, BYTARR(300, 40) + 255
  CASE sqmode OF
    gconst.GetDark:    XYOUTS, 150, 10, '!17Get dark images', /device, charsize=1.6, color=500000000,alignment=0.5
    gconst.GetFlat:    XYOUTS, 150, 10, '!17Get flat images', /device, charsize=1.6, color=500000000,alignment=0.5
    gconst.FilterCk:    XYOUTS, 150, 10, '!17Filter adjust', /device, charsize=1.6, color=500000000,alignment=0.5
    ;    gconst.TFcheck :   XYOUTS, 150, 10, '!17TF check', /device, charsize=1.6, color=500000000,alignment=0.5
    gconst.FocusCk :   XYOUTS, 150, 10, '!17Focus check', /device, charsize=1.6, color=500000000,alignment=0.5
    gconst.Speckle :   XYOUTS, 150, 10, '!17Get data for Speckle', /device, charsize=1.6, color=500000000,alignment=0.5
    ELSE:
  ENDCASE
  WSET, state.wid

  ret = GET_MTIME_STS()   ; [sec] with msec resolution  ; SM

  ;  ret = TELESCOPE_STATUS(tstatus)


  ;  ret = CHECK_FTPDEST()

  CASE sqmode OF
    gconst.GetDark:    ret=GET_DARK()
    gconst.GetFlat:    ret=GET_FLAT()
    gconst.FilterCk:   ret=FILTER_CHECK()
    ;    gconst.TFcheck :   ret= TF_CHECK()
    gconst.FocusCk :   ret= FOCUS_MODE()
    gconst.Speckle :   ret= SPECKLE_DATA()


    ;    gconst.DarkMode   : ret = SEQUENCE_DARK()
    ;     gconst.OneSh:       ret= TAKE_ONE_SHOT()
    ELSE:             ;  ret = SEQUENCE_LIGHT()
  ENDCASE

  ;  ret = CAMERA_CLOSE()
  ;  ret = FILTER_CLOSE()

  ;  ret = LOG_FTP()
  ;  IF ret EQ 1 THEN WRITE_WIDGET_MESSAGE, '[LOG_FTP()]: Log files FTP transferred.'

  WRITE_WIDGET_MESSAGE, '[SEQUENCE()]: ** Sequence Stopped **'

  WSET, state.wDrawStsId
  ERASE
  !p.font = 0
  CASE svar.E_LID OF
    0: XYOUTS,   150, 10, '!17'+diag.ObsStopLC,  /device,charsize=1.6, color=255,alignment=0.5
    1: XYOUTS,   150, 10, '!17'+diag.ObsStop,    /device,charsize=1.6, color=255,alignment=0.5
    ELSE: XYOUTS, 150, 10, '!17'+diag.ObsStopErr, /device,charsize=1.6, color=255,alignment=0.5
  ENDCASE

  !p.font = -1
  WSET, state.wid

  svar.SqStop     = 1l

  RETURN, 1l
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO PROG_SEQUENCE_START

  COMMON LIB, gconst, wparam, svar, diag, state

  IF svar.ProgSqStart EQ 1l THEN BEGIN

    ;*** Clear the ProgSqStart flag.
    svar.ProgSqStart = 0l
    svar.SqContinue  = 1l
    wparam.SqMode    = svar.ProgSqMode

    WRITE_WIDGET_MESSAGE, $
      '[PROG_SEQUENCE_START()]: ' + gconst.SqNames[svar.ProgSqMode], /FORCE

    ret = NORM_SEQUENCE(wparam.SqMode)
  ENDIF

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

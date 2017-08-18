;                                Time-stamp: <2012-05-25 01:09:48 satoshi>
;;; image.pro
;  image_save, image_disp
;
;  2016.8.8   k.i., k.o.   fits header LCOFFSET, temp  f6.3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO AUTO_TRACK, PointX, PointY

COMMON LIB, gconst, wparam, svar, diag, state
COMMON RAWIMAGEINFO, rawimage, cstatus, fstatus, tstatus, date_obs, exptime, date_obs1, date_obs2

DSKFIND_SMT, rawimage, gconst.AMask, svar.RSunPix, SunXCount, SunYCount, SunRout
svar.SunXCount = SunXCount
svar.SunYCount = SunYCount
WRITE_WIDGET_MESSAGE, '[DSKFIND_SMT]:',/force
Update_TelStatus

check_auto_track = 1
IF svar.IntMedian LT gconst.ThMedian     THEN check_auto_track = 0 
IF svar.AutoTrack NE 1                   THEN check_auto_track = 0

IF check_auto_track THEN BEGIN

  WRITE_WIDGET_MESSAGE, '[AUTO_TRACK]:',/force
 Is_SunCen_Valid = 0
  IF ABS(SunRout - svar.RSunPix) LT svar.ThSunRErr THEN Is_SunCen_Valid = 1

;  IF Is_SunCen_Valid EQ 1 THEN $
    WRITE_WIDGET_MESSAGE, '*** Plate Scale = ' + STRING(gconst.RSun/SunRout,format='(f6.4)') + ' [asec/pix] ***',/force

;*** RA
  RA_move = 0.
  IF Is_SunCen_Valid EQ 1 THEN BEGIN
    RA_move = PointX - SunXCount
    RA_move = RA_move * svar.PScale / 15.
  ENDIF
  PRINT, 'RA_MOVE', RA_move

;*** DEC
  DEC_move = 0.
  IF Is_SunCen_Valid EQ 1 THEN BEGIN
    DEC_move = PointY - SunYCount
    DEC_move = DEC_move * svar.PScale
  ENDIF
  PRINT, 'DEC_MOVE', DEC_move
  
;*** make move 
;  ret           = TELESCOPE_STATUS(tstatus)
  Update_TelStatus
  check_ra_move  = 0
  check_dec_move = 0

  IF (ABS(RA_move) GT gconst.RaCutoff) AND (ABS(RA_move) LT gconst.RaUpLimit) THEN BEGIN
    check_ra_move  = 1
    set_ra_pos  = tstatus.RA_POS  + LONG(RA_move  * 10.d)
  ENDIF

  IF (ABS(DEC_move) GT gconst.DecCutoff) AND (ABS(DEC_move) LT gconst.DecUpLimit) THEN BEGIN
    check_dec_move = 1
    set_dec_pos = tstatus.DEC_POS - LONG(DEC_move * 10.d)
  ENDIF

  CASE 1 OF
    check_ra_move  AND check_dec_move:        P2P_RA_DEC_SET, set_ra_pos, set_dec_pos
    check_ra_move  AND (check_dec_move EQ 0): P2P_RA_SET,     set_ra_pos
    check_dec_move AND (check_ra_move EQ 0):  P2P_DEC_SET,    set_dec_pos
    ELSE:
  ENDCASE

ENDIF

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO  Update_TelStatus 

COMMON LIB, gconst, wparam, svar, diag, state
COMMON RAWIMAGEINFO, rawimage, cstatus, fstatus, tstatus, date_obs, exptime, date_obs1, date_obs2

ret = TELESCOPE_STATUS(tstatus)
;print,tstatus.RA_POS
;print,tstatus.DEC_POS
ret=CLEAN_RA_POS( tstatus.RA_POS, hh_ra, mm_ra, ss_ra, sss_ra )
CONV_DEC_POS, tstatus.DEC_POS, hh, mm, ss, sss, sign
astring  = STRING(hh_ra,format='(i3.2)') + ':' + STRING(mm_ra,format='(i2.2)')+ ':' + STRING(ss_ra,format='(i2.2)') + '.' + STRING(sss_ra,format='(i1.1)')
WIDGET_CONTROL, state.wStsSysRa, SET_VALUE=astring
astring  = STRING(sign*hh,format='(i3.2)') + ':' + STRING(mm,format='(i2.2)')+ ':' + STRING(ss,format='(i2.2)') + '.' + STRING(sss,format='(i1.1)')
WIDGET_CONTROL, state.wStsSysDec, SET_VALUE=astring


case svar.E_LID of 
  0: astring='Close'
  1: astring='Open'
  3: astring='Moving'
  else: astring='?????'
endcase
WIDGET_CONTROL, state.wStsSysLid, SET_VALUE=astring

;astring=string(tstatus.CAM_POS,format="(f8.1)")
;WIDGET_CONTROL, state.wStsSysFpos, SET_VALUE=astring


END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION SAVE_IMAGE_CUBE, imgs,dir=dir

COMMON LIB, gconst, wparam, svar, diag, state
COMMON RAWIMAGEINFO, rawimage, cstatus, fstatus, tstatus, date_obs, exptime, date_obs1, date_obs2
COMMON tf_orca, o,p,f,b,dwl_list
COMMON bridge,bridge

if n_elements(dir) eq 0 then SAVE_DIR      = wparam.DirStore else SAVE_DIR=dir

DATE_OBS_UTC      = TAI2UTC(date_obs1 + 378691200d0, /NOCORRECT, /EXTERNAL)
time_iso          = UTC2STR(DATE_OBS_UTC)+'Z'

;DATE_OBS_UTC      = TAI2UTC(date_obs1 + 378691200d0, /NOCORRECT, /EXTERNAL)
p.date_obs          = UTC2STR(DATE_OBS_UTC)+'Z'
DATE_OBS_UTC2      = TAI2UTC(date_obs2 + 378691200d0, /NOCORRECT, /EXTERNAL)
p.date_obs2          = UTC2STR(DATE_OBS_UTC2)+'Z'

DATE_OBS_UTC      = TAI2UTC(date_obs + 378691200d0, /NOCORRECT, /EXTERNAL)
fits_fname =                                             $
  string((DATE_OBS_UTC.YEAR MOD 100), format='(i2.2)') + $
  string(DATE_OBS_UTC.MONTH, format='(i2.2)')          + $
  string(DATE_OBS_UTC.DAY, format='(i2.2)')            + $
  'T'                                                  + $
  string(DATE_OBS_UTC.HOUR, format='(i2.2)')           + $
  string(DATE_OBS_UTC.MINUTE, format='(i2.2)')         + $
  string(DATE_OBS_UTC.SECOND, format='(i2.2)')

;IF (mode eq gconst.Dark) THEN fits_fname = fits_fname + '-d'
jpeg_fname = fits_fname +'.jpg'
fits_fname = fits_fname + '.fits'


;dwl_list=strarr(10)
;dwl_list[0]='[-4000,0000,4000]'


;-------------------

case size(imgs,/type) of
  1: bitpix=8
  2: bitpix=16
  3: bitpix=32
  4: bitpix=32
  5: bitpix=64
  6: bitpix=64  ; ? complex
  7: bitpix=-1  ; string
  8: bitpix=-1  ; struct
  9: bitpix=128 ; ? dcomplex
  10: bitpix=-1  ; pointer
  11: bitpix=-1  ; objref
  12: bitpix=16  ; uint
  13: bitpix=32  ; ulong
  14: bitpix=64  ; long64
  15: bitpix=64  ; ulong64
endcase
nax=size(imgs,/n_dimension)
dim=size(imgs,/dimension)
nax1=dim[0]
if nax ge 2 then nax2=dim[1] else nax2=1
if nax ge 3 then nax3=dim[2] else nax3=1

fh=strarr(36)
fh[0] =string('SIMPLE  = ','T',format='(a10,a20," /")')
fh[1] =string('BITPIX  = ',bitpix,format='(a10,i20," /")')
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
nb=n_elements(f.b)
ctemp='['+string(f.b[0].temp,form='(f6.3)')
for i=1,nb-1 do ctemp=ctemp+','+string(f.b[i].temp,form='(f6.3)')
fh[25]='TEMP    = '+ctemp+']'
cret='['+string(f.b[0].ret*360,form='(i4)')
for i=1,nb-1 do cret=cret+','+string(f.b[i].ret*360,form='(i4)')
fh[26]='LCRET   = '+cret+']'
cvolt='['+string(f.b[0].volt,form='(f6.3)')
for i=1,nb-1 do cvolt=cvolt+','+string(f.b[i].volt,form='(f6.3)')
fh[27]='LCVOLT  = '+cvolt+']'
;'CAM-POS '+'= '+ string(tstatus.cam_pos,format='(i20)'),              $
fh[28]='INT_SHBR'+'= '+ string(svar.IntShabar,format='(E19.10)')
fh[29]='SCNTSHBR'+'= '+ string(DOUBLE(svar.ScntShabarStr),format='(E19.10)')
;fh[28]='TIMESHBR'+'= '+ time_shabar_iso
fh[30]='CRPIX1  = '+string(svar.SunXCount,format="(f6.1)")
fh[31]='CRPIX2  = '+string(svar.SunYCount,format="(f6.1)");
;fh[32]='COMMENT = none'
;fh[33]='HISTORY = RAW'
fh[32]='LCOFFSET= '+f.Offset_info
fh[33]=string(' ',format='(a10)')
fh[34]=string(' ',format='(a10)')
fh[35]=string('END       ',format='(a10)')
blnk80=string(' ',format='(a80)')
fh=strmid(fh+blnk80,0,80)

file=SAVE_DIR + '\' + fits_fname
;get_lun,Unit
;openw,Unit,file
;for j=0,35 do begin
;  writeu,Unit,fh[j]
;endfor
;byteorder,imgs
;writeu,Unit,imgs
;close,Unit
;free_lun,Unit

;searching free bridge
done=0b
while done ne 1 do begin
  for i=0,n_elements(bridge)-1 do begin
    if bridge[i]->status() eq 0 then begin
      byteorder,imgs
      bridge[i]->SetVar,'file',file
      bridge[i]->SetVar,'fh',fh
      bridge[i]->SetVar,'imgs',imgs
      print,'Writing fits' & tic
      bridge[i]->Execute,'get_lun,Unit & openw,Unit,file & for j=0,35 do writeu,Unit,fh[j] & writeu,Unit,imgs & close,Unit & free_lun,Unit',/nowait
      toc
      done=1b
      byteorder,imgs
      break      
    endif
  endfor
endwhile


WRITE_WIDGET_MESSAGE, '[IMAGE_SAVE()]: ' + fits_fname, /FORCE

;write_jpeg,SAVE_DIR+'\'+jpeg_fname,bytscl(rawimage)


RETURN, 1

END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



FUNCTION IMAGE_DISP

COMMON LIB, gconst, wparam, svar, diag, state
COMMON RAWIMAGEINFO, rawimage, cstatus, fstatus, tstatus, date_obs, exptime, date_obs1, date_obs2

DATE_OBS_UTC = TAI2UTC((date_obs1+date_obs2)/2. + 378691200d0, /NOCORRECT, /EXTERNAL)
DATE_OBS_STR = utc2str(DATE_OBS_UTC,/TRUNCATE)

WAVE_F_Label = svar.FiltOffset
exptime=svar.ExpNorm

   TVSCL, rotate(REBIN(rawimage, 1024, 1024,/sample),6),-12,-12

;  TVSCL, ROTATE(REBIN(rawimage, 1024, 1024), 5) ; < 280. > 210.
  
;  TV, BYTARR(1024,94),0,1024 

  XYOUTS, 6,   1094-118, '!17'+DATE_OBS_STR+' UT',/device,charsize=1.6
  XYOUTS, 6,   1070-118, '!17* Lyot Filter Offset: '+ $
    string(WAVE_F_Label, format='(i6)') + ' mA',/device,charsize=1.6
  XYOUTS, 6,   1046-118, '!17* EXP Time: '+string(exptime, format='(i6)') + $
    ' msec',/device,charsize=1.6

;  XYOUTS, 518, 1094, '!17* Offset from Mech Zero: '+ $
;    string(fstatus.WAVE_F, format='(i6)') + ' mA',/device,charsize=1.6

  XYOUTS, 518, 1070-118, '!17* Int. (Median): '+string(svar.IntMedian, format='(i6)') + $
    ' DN', /device,charsize=1.6
 
 
   
;  PLOTS, gconst.Circle_X, gconst.Circle_Y,  /dev, color=255, thick=3

WRITE_WIDGET_MESSAGE, '[IMAGE_DISP()]: '

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



FUNCTION FOCUS_DISP, uimg, xcnr, ycnr, contrast_img, mean_img, shbr_scnt, campos, nolabel=nolabel

COMMON LIB, gconst, wparam, svar, diag, state
COMMON RAWIMAGEINFO, rawimage, cstatus, fstatus, tstatus, date_obs, exptime

DATE_OBS_UTC = TAI2UTC(date_obs + 378691200d0, /NOCORRECT, /EXTERNAL)
DATE_OBS_STR = utc2str(DATE_OBS_UTC,/TRUNCATE)

;WAVE_F_Label = fstatus.WAVE_F - gconst.OfSetMecZero
;
;  uimg = ROTATE(uimg, 5)
;  TVSCL,uimg, xcnr, ycnr  
;
;  XYOUTS, xcnr+3, ycnr+29, '!6Contrast: ' +string(contrast_img, format='(f5.2)'), /device,charsize=1.3
;  XYOUTS, xcnr+3, ycnr+53, '!6Mean Int: ' +string(mean_img,     format='(i5)'), /device,charsize=1.3
;  XYOUTS, xcnr+3, ycnr+77, '!6Scntlltn: ' +string(shbr_scnt,    format='(f7.4)'), /device,charsize=1.3
;
;  IF KEYWORD_SET(nolabel) NE 1 THEN BEGIN
;    PLOTS, [0, 1023], [ycnr,ycnr], /dev, color=0, thick=2
;
;    XYOUTS, xcnr+3, ycnr+5,  '!6Cam Pos: '  +string(campos, format='(i5)'), /device,charsize=1.3
;
;    TV, BYTARR(1024,94),0,1024 
;
;    XYOUTS, 6,   1094, '!17'+DATE_OBS_STR+' UT',/device,charsize=1.6
;    XYOUTS, 6,   1070, '!17* Lyot Filter Offset: '+ $
;      string(WAVE_F_Label, format='(i6)') + ' mA',/device,charsize=1.6
;    XYOUTS, 6,   1046, '!17* EXP Time: '+string(exptime, format='(i6)') + $
;      ' msec',/device,charsize=1.6
;
;    XYOUTS, 518, 1094, '!17* Offset from Mech Zero: '+ $
;      string(fstatus.WAVE_F, format='(i6)') + ' mA',/device,charsize=1.6
;  ENDIF
 
WRITE_WIDGET_MESSAGE, '[FOCUS_DISP()]: '

END


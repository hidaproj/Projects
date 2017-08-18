; orcalib.pro

; 2013.05.11  k.i., t.k.
; 2013.06.10  k.i.,             OrcaSetParm  Binning ????
; 2013.11.03  k.i., t.k.,       savefits_p, p-struct
; 2014.04.01  k.i., m.s.,       partial frame
; 2014.05.06  k.i.,             'VARTYPE = UINT' key
; 2014.05.11  k.i.,             X0, Y0, BIN FITS keyword
; 2014.05.18  k.i.,             bug fix, noDev
; 2014.05.21  k.i.,             orca_widget, orca_handle
; 2014.05.24  k.i.              preview hist minor change
; 2015.06.20  k.i.              orca_settrigmode, savefits_p
; 2015/07/24  k.i.              orca_widget
; 2015/08/23  k.i.,t.s.,a.t.    orcaobs nowait, orca_getimg
; 2015.09.01  k.i.,a.t.         savefits_p  DATE_OBS,OB2  (a23)
; 2016.03.26  k.i.,k.o.         orcaobs  nimg=1
; 2016.05.08  k.i.              orca_capture, orca_getimg if nimg=1...
; 2016.06.22  k.o.              remove file keyword from orca_getimg.pro
; 2016.07.25  k.o.              return uint array in orca_getimg.pro / continuous capturing
; 2016.07.27  k.o.              subarray
; 2016.07.29  t.a.              OrcaOutTriggerProgramable, OrcaOutTriggerDefault
; 2016.08.09  k.o.              bug in binning, remove orca_fin and orca_init in resetting bin/subarray, x0,y0,w,h->long
; 2016.09.25  k.i.              orca_capturemode_snap
; 2016.11.11  k.o.              refactoring
; 2016.11.11  t.a.              add keyword 'grabimg_wait' in orcaobs
; 2016.12.06  k.o.              store imgs array and its size info in common block orca4 & paraless processing for fits output
; 2016.12.08  k.o.		orcaobs imgs[*,*,i] -> imgs[0,0,i]

;**************************************************************
function p_orca4

  p={orca4_param,   $
    expo:        0.1,      $   ; exposure time [sec]
    framerate:   float(30),      $   ; frame rate [frame/sec]
    gain:        0,        $   ; gain 0-28
    bin:         1,        $   ; Binning XY 1-8
    Width:       2048l,          $   ; Width  max=2048 (binx=1), (cam pixel)/bin
    Height:      2048l,          $   ; Height max=2048 (biny=1), (cam pixel)/bin
    RegionX:     0l,             $   ; left edge of ROI, pixel, (cam pixel)/bin
    RegionY:     0l,             $   ; top edge of ROI, pixel, (cam pixel)/bin
    TrigMode:    'Internal',     $   ; trigger mode, 'Internal' or 'Start'
    TrigPol: 'Negative', $   ; trigger polarity, 'Negative' or 'Positive'
    date_obs:    '',             $   ; yyyy-mm-ddThh:mm:ss.sss
    date_obs2:   '',             $   ; yyyy-mm-ddThh:mm:ss.sss
    timesys :    'JST',          $   ; yyyy-mm-ddThh:mm:ss.sss
    observat:    'Hida',         $   ;
    telescop:    'DST',          $   ;
    instrume:    'VS',           $   ;
    camera:      'ORCA4',      $   ;
    wave:    '',             $   ; wavelength
    data_typ:  'OBJECT', $   ; 'OBJECT','DARK', 'FLAT'
    clock:       0l,     $   ; TimeStanmpFrequency [Hz]
    timelo:      0,            $   ; Time stamp, lower 32-bits
    timehi:      0,            $   ; Time stamp, upper 32-bits
    status:      0             $   ; Status
  }

  return,p
end

;**************************************************************
function OrcaInit,noDev=noDev0

  common orca4,orcadll,p,img,noDev,imgs,imgss

  orcadll='C:\Projects\cprog\VS2010\orca4\x64\Debug\orca4.dll'
  if keyword_set(noDev0) then noDev=1 else noDev=0
  if not noDev then err=call_external(orcadll,'OrcaInit',/all_value,/cdecl)

  p=p_orca4()
  img=uintarr(p.width,p.height)
  
  imgss=[0l,0l,0l]

  return,p

end


;**************************************************************
pro OrcaFin

  common orca4,orcadll,p,img,noDev

  if not noDev then err=call_external(orcadll,'OrcaFin',/all_value,/cdecl)

end


;**************************************************************
function OrcaSetParam,expo=expo,gain=gain,bin=bin, $
  width=width,height=height,regionx=regionx,regiony=regiony, $
  framerate=framerate

  common orca4,orcadll,p,img,noDev

  ;  expo - exposure, sec, float
  ;  bin  - 1,2,4,,
  ;  gain - 0,1,2,,,,20 (not implemented)
  ;  width, height, regionx,y, framerate  (not implemented)

  ;ret=call_external(orcadll,'CamIdle',/all_value,/cdecl)

  ;revise exposure?
  if isvalid(expo) then begin
    revexpo=expo ne p.expo
  endif else revexpo=0b

  ;revise binnig?
  if isvalid(bin) then begin
    revbin=bin ne p.bin
  endif else revbin=0b

  ;revise subarray?
  revsub=0b
  if isvalid(width) then begin
    revsub or=width ne p.width
  endif else revsub or=0b
  if isvalid(height) then begin
    revsub or=height ne p.height
  endif else revsub or=0b
  if isvalid(regionx) then begin
    revsub or=regionx ne p.regionx
  endif else revsub or=0b
  if isvalid(regiony) then begin
    revsub or=regiony ne p.regiony
  endif else revsub or=0b

  if ~revbin and ~revsub then begin
    if revexpo then begin
      if ~noDev then ret=call_external(orcadll,'SetOrcaExpo',expo,/all_value,/cdecl)
      p.expo=expo
    endif
    return,p
  endif

  ;only subarray revised
  if revsub and ~revbin then begin
    if isvalid(regionx) then p.RegionX=regionx
    if isvalid(regiony) then p.RegionY=regiony
    if isvalid(width) then p.Width=width
    if isvalid(height) then p.Height=height

    if ~noDev then begin
      ret=call_external(orcadll,'SetOrcaBin',p.bin,/all_value,/cdecl)

      r=call_external(orcadll,'QuerySubArray')
      if r eq 1 then r=call_external(orcadll,'EnableSubArray')

      r=call_external(orcadll,'SetSubArray',0l,0l,long(p.Width*p.bin),long(p.Height*p.bin))
      r=call_external(orcadll,'SetSubArray',long(p.RegionX*p.bin),long(p.RegionY*p.bin),long(p.Width*p.bin),long(p.Height*p.bin))

      full=p.RegionX eq 0 and p.RegionY eq 0 and p.Width*p.bin eq 2048 and p.Height*p.bin eq 2048
      if full then r=call_external(orcadll,'DisableSubArray')
    endif
  endif

  ;only bin revised
  if revbin and ~revsub then begin
    p.Width  =p.Width*p.bin/bin
    p.Height =p.Height*p.bin/bin
    p.RegionX=p.RegionX*p.bin/bin
    p.RegionY=p.RegionY*p.bin/bin
    if ~noDev then begin
      ret=call_external(orcadll,'SetOrcaBin',bin,/all_value,/cdecl)

      r=call_external(orcadll,'QuerySubArray')
      if r eq 1 then r=call_external(orcadll,'EnableSubArray')

      r=call_external(orcadll,'SetSubArray',0l,0l,long(p.Width*bin),long(p.Height*bin))
      r=call_external(orcadll,'SetSubArray',long(p.RegionX*bin),long(p.RegionY*bin),long(p.Width*bin),long(p.Height*bin))

      full=p.RegionX eq 0 and p.RegionY eq 0 and p.Width eq 2048/bin and p.Height eq 2048/bin
      if full then r=call_external(orcadll,'DisableSubArray')
    endif
    p.bin=bin
  endif

  img=uintarr(p.width,p.height)

  return,p
end


;**************************************************************
function orcaobs,file=filename,nimg=nimg,nowait=nowait,noread=noread,grabimg_wait=grabimg_wait

  common orca4,orcadll,p,img,noDev,imgs,imgss

  if not keyword_set(nimg) then nimg=1

  if keyword_set(filename) then begin
    if not noDev then r=call_external(orcadll,'GrabImgToFile',nimg,filename,/all_value,/cdecl)
    return,0
  endif

;  if not noDev then begin
;     r=call_external(orcadll,'GrabImg',nimg)
;  endif
  if not noDev then begin
    if not keyword_set(grabimg_wait) then begin  
      r=call_external(orcadll,'GrabImg',nimg)
    endif else begin
      r=call_external(orcadll,'GrabImg_Wait',nimg)
    endelse
  endif

  if keyword_set(nowait) then return,-1

  if not noDev then begin
    r=call_external(orcadll,'OrcaWait')
  endif

  if keyword_set(noread) then return,-1

  timstmp=lonarr(1)

  if nimg gt 1 and max([p.width,p.height,nimg] ne imgss) then imgs=uintarr(p.width,p.height,nimg)
  for i=0,nimg-1 do begin
    if not noDev then begin
      r=call_external(orcadll,'GoIdl',i,img,timstmp) ; timstmp not word
    endif
    if nimg eq 1 then return,img
    imgs[0,0,i]=img
  endfor

  imgss=[p.width,p.height,nimg]

  return,imgs

end

;**************************************************************
function orca_getimg,nimg=nimg,nowait=nowait

  ;  copy img to IDL
  common orca4,orcadll,p,img,noDev,imgs,imgss

  if not keyword_set(nimg) then nimg=1

  if not noDev and not keyword_set(nowait) then begin
    r=call_external(orcadll,'OrcaWait')
  endif

  timstmp=lonarr(1)
  if nimg eq 1 and p.RegionX eq 0 and p.Width eq 2048 and p.RegionY eq 0 and p.Height eq 2048 then begin
    r=call_external(orcadll,'GoIdl',0,img,timstmp)
    return,uint(img)
  endif
  if nimg gt 1 and max([p.width,p.height,nimg] ne imgss) then imgs=uintarr(p.width,p.height,nimg)
  if not noDev then for i=0,nimg-1 do begin
    r=call_external(orcadll,'GoIdl',i,img,timstmp) ; timstmp not word
    imgs[0,0,i]=img
  endfor

  imgss=[p.width,p.height,nimg]

  return,imgs

end

;**************************************************************
pro orca_capturemode_sequence,nimg=nimg

  common orca4,orcadll,p,img,noDev

  if not noDev then begin
    print,'Capture (orca_capturemode_sequence)'
    r=call_external(orcadll,'CaptureModeSequence',nimg)
  endif

end

;**************************************************************
pro orca_capturemode_snap,nimg=nimg

  common orca4,orcadll,p,img,noDev

  if n_elements(nimg) eq 0 then nimg=1
  if not noDev then begin
    print,'Capture (orca_capturemode_snap)'
    r=call_external(orcadll,'CaptureModeSnap',nimg)
  endif

end


;**************************************************************
pro orca_capture
  common orca4,orcadll,p,img,noDev

  if not noDev then begin
    r=call_external(orcadll,'Capture')
    r=call_external(orcadll,'OrcaWait')
  endif

end


;**************************************************************
pro orca_idle

  common orca4,orcadll,p,img,noDev

  if not noDev then begin
    ret=call_external(orcadll,'CamIdle',/all_value,/cdecl)
  endif

end


;**************************************************************
pro orcaprev_event,ev

  common orcprv_com, wd, p, img1

  case (ev.id) of
    wd.START: begin
      nbins=128 & imax=2l^16 -1
      ii=findgen(nbins)/nbins*imax
      while ev.id ne wd.STOP do begin
        ev = widget_event(wd.STOP,/nowait)
        img1=OrcaObs(nimg=1)
        tvscl,rebin(img1,wd.nx,wd.ny)>0
        h=histogram(img1,max=imax,min=0,nbins=nbins)
        plot,ii,h,psym=10, $
          /noerase,/xstyle,charsize=0.5,pos=[0.05,0.05,0.4,0.3],color=0
      endwhile
    end
    wd.STOP: begin
    end
    wd.SNAP: begin
      img1=OrcaObs(nimg=1)
      tvscl,rebin(img1,wd.nx,wd.ny)
    end
    wd.PROFS: begin
      profiles,img1
    end
    wd.EXPO: begin
      p.expo=float(gt_wdtxt(ev.id))
      p=OrcaSetParam(expo=p.expo)
    end
    wd.BIN: begin
      p.bin=fix(gt_wdtxt(ev.id))
      p=OrcaSetParam(bin=p.bin)
    end
    wd.Exit:  begin
      WIDGET_CONTROL, /destroy, ev.top
      return
    end
    else:stop
  endcase

end

;**************************************************************
pro orcaprev,p0,img=img0

  common orcprv_com, wd, p, img1

  p=p0
  p=OrcaSetParam(expo=p.expo,bin=p.bin)

  wd={wd_prv, $
    START:    0l, $
    STOP:   0l, $
    SNAP:   0l, $
    PROFS:    0l, $
    EXPO:   0.01, $
    BIN:    1, $
    nx:   512, $
    ny:   512, $
    Exit:   0l $
  }

  window,0,xs=wd.nx,ys=wd.ny

  base = WIDGET_BASE(title='ORCA4 prev.', /column)
  b1=widget_base(base, /row )
  wd.START=widget_button(b1, value="Start", uvalue = "START")
  wd.STOP=widget_button(b1, value="Stop", uvalue = "STOP")
  wd.SNAP=widget_button(b1, value="Snap", uvalue = "SNAP")
  wd.PROFS=widget_button(b1, value="Profs", uvalue = "PROFS")
  b2=widget_base(base, /row )
  dmy = widget_label(b2, value='exp:')
  wd.EXPO = widget_text(b2,value=string(p.expo,form='(f5.3)'), xsize=5, uvalue='EXPO',/edit)
  dmy = widget_label(b2, value='s  bin:')
  wd.BIN = widget_text(b2,value=string(p.bin,form='(i2)'), xsize=2, uvalue='BIN',/edit)
  b3=widget_base(base, /row )
  wd.Exit=widget_button(b3, value="Exit", uvalue = "EXIT")


  widget_control, base, /realize
  XMANAGER, 'orcaprev', base;, /modal

  p0=p
  img0=img1

end


;**************************************************************
pro orca_settrigmode,mode,polarity=polarity

  common orca4,orcadll,p,img,noDev

  case mode of
    'Internal':   r=call_external(orcadll,"SetTriggerModeInternal",/all_value,/cdecl)
    'Start':  r=call_external(orcadll,"SetTriggerModeStart",/all_value,/cdecl)
    else:   print,'Orca trig mode ',mode,' not defined!'
  endcase

  if keyword_set(polarity) then begin
    case polarity of
      'Negative':   r=call_external(orcadll,"SetTriggerPolarityNegative",/all_value,/cdecl)
      'Positive':   r=call_external(orcadll,"SetTriggerPolarityPositive",/all_value,/cdecl)
      else:   print,'Polarity',polarity,' not defined!'
    endcase

  endif

end


;**************************************************************
pro settriggermode

  common orca4,orcadll,p,img,noDev

  r=call_external(orcadll,"SetTriggerMode",/all_value,/cdecl)

end

;**************************************************************
pro settriggerpolarity

  common orca4,orcadll,p,img,noDev

  r=call_external(orcadll,"SetTriggerPolarity",/all_value,/cdecl)

end


;**************************************************************
pro savefits_p,imgs,p,file=file

common bridge,bridge

  ;p={orca4_param,   $
  ;   expo:        0.1,     $   ; exposure time [sec]
  ;   framerate:   float(30),       $   ; frame rate [frame/sec]
  ;   gain:        0,       $   ; gain 0-28
  ;   bin:          1,        $   ; Binning XY 1-8
  ;   Width:       2048,          $   ; Width  max=2048 (binx=1)
  ;   Height:      2048,          $   ; Height  max=2048 (biny=1)
  ;   RegionX:     0,             $   ; start of region read out,pixel,left edge
  ;   RegionY:     0,             $   ; start of region read out,pixel,top edge
  ;   TrigMode:    'Internal',    $   ; trigger mode, 'Internal' or 'Start'
  ;   TrigPol:  'Negative', $   ; trigger polarity, 'Negative' or 'Positive'
  ;   clock:       79861111l,   $   ; TimeStanmpFrequency [Hz]
  ;   timelo:      0,             $   ; Time stamp, lower 32-bits
  ;   timehi:      0,             $   ; Time stamp, upper 32-bits
  ;   status:      0            $   ; Status

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
  fh[2] =string('NAXIS   = ',nax,format='(a10,i20," /")')
  fh[3] =string('NAXIS1  = ',nax1,format='(a10,i20," /")')
  fh[4] =string('NAXIS2  = ',nax2,format='(a10,i20," /")')
  fh[5] =string('NAXIS3  = ',nax3,format='(a10,i20," /")')
  fh[6] =string('DATE_OBS= ',p.date_obs,format='(a10,a23," /")')
  fh[7] =string('DATE_OB2= ',p.date_obs2,format='(a10,a23," /")') ; end time of integ
  fh[8] =string('TIMESYS = ',p.timesys,format='(a10,a20," /")')
  fh[9] =string('OBSERVAT= ',p.observat,format='(a10,a20," /")')
  fh[10]=string('TELESCOP= ',p.telescop,format='(a10,a20," /")')
  fh[11]=string('CAMERA  = ',p.camera,format='(a10,a20," /")')
  fh[12]=string('EXP     = ',p.expo,format='(a10,f20.10," /")')
  fh[13]=string('DATA_TYP= ',p.data_typ,format='(a10,a20," /")')
  fh[14]='VARTYPE = UINT'
  fh[15]=string('X0      = ',p.RegionX,format='(a10,i20," /")')
  fh[16]=string('Y0      = ',p.RegionY,format='(a10,i20," /")')
  fh[17]=string('BIN     = ',p.bin,format='(a10,i20," /")')
  fh[18]=string('TRIGMODE= ',p.TrigMode,format='(a10,a20," /")')
  fh[19]=string('TRIGPOL = ',p.TrigPol,format='(a10,a20," /")')
  fh[20]='COMMENT = none'
  fh[21]='HISTORY = RAW'
  fh[22:34]=string(' ',format='(a10)')
  fh[35]=string('END       ',format='(a10)')
  blnk80=string(' ',format='(a80)')
  fh=strmid(fh+blnk80,0,80)

  ;get_lun,Unit
  ;openw,Unit,file
  ;for j=0,35 do begin
  ;  writeu,Unit,fh[j]
  ;endfor
  ;byteorder,imgs
  ;writeu,Unit,imgs
  ;close,Unit
  ;free_lun,Unit
  ;byteorder,imgs

  ;searching free bridge
  done=0b
  while done ne 1 do begin
    for i=0,n_elements(bridge)-1 do begin
      if bridge[i]->status() eq 0 then begin
        bridge[i]->SetVar,'file',file
        bridge[i]->SetVar,'fh',fh
        bridge[i]->SetVar,'imgs',imgs
        bridge[i]->Execute,'get_lun,Unit & openw,Unit,file & for j=0,35 do writeu,Unit,fh[j] & byteorder,imgs & writeu,Unit,imgs & close,Unit & free_lun,Unit',/nowait
        done=1b
        break
      endif
    endfor
  endwhile

end

;**************************************************************
pro set_wdroi,wd,p1 ; set ROI widget

  widget_control,wd.X0,set_value=string(p1.RegionX,form='(i4)')
  widget_control,wd.Y0,set_value=string(p1.RegionY,form='(i4)')
  widget_control,wd.WIDTH,set_value=string(p1.Width,form='(i4)')
  widget_control,wd.HEIGHT,set_value=string(p1.Height,form='(i4)')
end

;**************************************************************
pro OrcaOutTriggerDefault

  common orca4,orcadll,p,img,noDev

  err=call_external(orcadll,'SetOutputTriggerProgramableHSP',0,/all_value,/cdecl)

end
;**************************************************************
pro OrcaOutTriggerExposure

  common orca4,orcadll,p,img,noDev

  rr=call_external(orcadll,'SetOutputTriggerProgramableHSP',1,/all_value,/cdecl)

end

;**************************************************************
pro orca_handle, ev, wd, p, img1

  dbin=2048./wd.p.wx/p.bin

  case (ev.id) of
    wd.PRV_START: begin
      nbins=128 & imax=2l^16 -1
      ii=findgen(nbins)/nbins*imax
      x0=p.regionx/dbin
      y0=p.regiony/dbin

      orca_idle

      ;set capturemode sequence
      orca_capturemode_sequence,nimg=1

      ;continuous capturing start
      orca_capture

      set_plot,'z'
      device,set_resolution=[p.Width,p.Height]/dbin
      set_plot,'win'
      
      while ev.id ne wd.PRV_STOP do begin
        img1=orca_getimg(nimg=1);,/nowait)

        img=rebin(img1,p.Width/dbin,p.Height/dbin)>0
        dmin=wd.p.min
        dmax=wd.p.max
        if wd.p.log_on then begin
          img=alog10(img>1)
          dmin=alog10(dmin>1)
          dmax=alog10(dmax>1)
        endif
        img=wd.p.AUTOSCL_ON?bytscl(img):bytscl(img,min=dmin,max=dmax)

        set_plot,'z'
        tv,img
        if wd.p.hist_on then begin
          h=histogram(img1,max=imax,min=0,nbins=nbins)
          plot,ii,h,psym=10, $
            /noerase,/xstyle,charsize=0.5,pos=[0.05,0.05,0.27,0.2],color=127
        endif
        if wd.p.mmm_on then begin
          mmm=uint([min(img1,max=max),mean(img1),max])
          xyouts,0,1,/norm,'!C'+strjoin(['MIN ','MEAN','MAX ']+' '+string(mmm),'!C'),color=127
        endif
        tmp=tvrd()
        set_plot,'win'
        tvscl,tmp,p.regionx/dbin,p.regiony/dbin
      
        ev = widget_event(wd.PRV_STOP,/nowait)
      endwhile
      orca_idle    ;capture stop
    end
    wd.EXPO: begin
      p.expo=float(gt_wdtxt(ev.id))
      p=OrcaSetParam(expo=p.expo)
    end
    wd.BIN: begin
      p.bin=fix(gt_wdtxt(ev.id))
      dbin=2048./wd.p.wx/p.bin
      p=OrcaSetParam(bin=p.bin)
      set_wdroi,wd,p
    end
    wd.X0: begin
      p=OrcaSetParam(regionx=fix(gt_wdtxt(ev.id))/(4/p.bin)*(4/p.bin))
      set_wdroi,wd,p
    end
    wd.Y0: begin
      p=OrcaSetParam(regiony=fix(gt_wdtxt(ev.id))/(4/p.bin)*(4/p.bin))
      set_wdroi,wd,p
    end
    wd.WIDTH: begin
      p=OrcaSetParam(width=fix(gt_wdtxt(ev.id))/(4/p.bin)*(4/p.bin))
      set_wdroi,wd,p
    end
    wd.HEIGHT: begin
      p=OrcaSetParam(height=fix(gt_wdtxt(ev.id))/(4/p.bin)*(4/p.bin))
      set_wdroi,wd,p
    end
    wd.CBOX: begin
      box_cur1,x0, y0, nx, ny
      four=4/p.bin
      x0-=(x0 mod four)
      y0-=(y0 mod four)
      nx-=(nx mod four)
      ny-=(ny mod four)
      p=OrcaSetParam(regionx=x0*dbin,regiony=y0*dbin,width=nx*dbin,height=ny*dbin)
      set_wdroi,wd,p
    end
    wd.FULL: begin
      p=OrcaSetParam(regionx=0,regiony=0,width=2048/p.bin,height=2048/p.bin)
      set_wdroi,wd,p
    end
    wd.AUTOSCL:begin
      wd.p.AUTOSCL_ON=widget_info(wd.AUTOSCL,/button_set)
      widget_control,widget_info(wd.MIN,/parent),sensitive=~wd.p.AUTOSCL_ON
    end
    wd.LOG:begin
      wd.p.LOG_ON=widget_info(wd.LOG,/button_set)
    end
    wd.MIN:begin
      widget_control,wd.MIN,get_value=tmp
      wd.p.MIN=long(tmp)
    end
    wd.MAX:begin
      widget_control,wd.MAX,get_value=tmp
      wd.p.MAX=long(tmp)
    end
    else:
  endcase

end

;************************************************************************
function orca_widget,base,p

  wd={wd_orca_V01,  $
    p: {orca_gui, $
      wx:   1024,   $ ; window x-size for image display
      wy:   1024,   $ ;        y-size
      hist_on:  1,  $ ; histgram on/off
      mmm_on:  1, $ min/mean/max on/off
      autoscl_on:  1, $ ; use TVSCL or TV
      log_on:  0, $ ;log display
      min:  0l, $ ;display min
      max:  65535l $ ;display max
    },$
    PRV_START:  0l,   $
    PRV_STOP: 0l,   $
    EXPO:   0l,   $
    BIN:    0l, $
    X0:   0l, $
    Y0:   0l, $
    WIDTH:    0l, $
    HEIGHT:   0l, $
    FULL:   0l, $
    CBOX:   0l, $
    HIST:   0l, $
    AUTOSCL:   0l, $
    LOG:  0l, $
    MIN:  0l, $
    MAX:  0l $
  }

  b2=widget_base(base, /colum, /frame)
  dmy = widget_label(b2, value='>>> ORCA <<<')
  b21=widget_base(b2, /row)
  dmy = widget_label(b21, value='exp:')
  wd.EXPO = widget_text(b21,value=string(p.expo,form='(f5.3)'), xsize=5, uvalue='EXPO',/edit)
  dmy = widget_label(b21, value='sec, Prev.')
  wd.PRV_START=widget_button(b21, value="Start", uvalue = "PRV_START")
  wd.PRV_STOP=widget_button(b21, value="Stop", uvalue = "PRV_STOP")
  b22=widget_base(b2, /row)
  dmy = widget_label(b22, value='ROI: ')
  wd.FULL=widget_button(b22, value="full", uvalue = "FULL")
  wd.CBOX=widget_button(b22, value="Cbox", uvalue = "CBOX")
  dmy = widget_label(b22, value='   bin:')
  wd.BIN = widget_text(b22,value=string(p.bin,form='(i2)'), xsize=2, uvalue='BIN',/edit)

  b23=widget_base(b2, /row)
  dmy = widget_label(b23, value='x0:')
  wd.X0 = widget_text(b23,value=string(p.RegionX,form='(i4)'), xsize=4, uvalue='X0',/edit)
  dmy = widget_label(b23, value='y0:')
  wd.Y0 = widget_text(b23,value=string(p.RegionY,form='(i4)'), xsize=4, uvalue='Y0',/edit)
  dmy = widget_label(b23, value='nx:')
  wd.WIDTH = widget_text(b23,value=string(p.Width,form='(i4)'), xsize=4, uvalue='WIDTH',/edit)
  dmy = widget_label(b23, value='ny:')
  wd.HEIGHT = widget_text(b23,value=string(p.Height,form='(i4)'), xsize=4, uvalue='HEIGHT',/edit)

  b24=widget_base(b2, /row)

  dmy = widget_label(b24, value='AUTOSCL:')
  b24b=widget_base(b24, /row,/ nonex)
  wd.AUTOSCL= widget_button(b24b,value='')
  widget_control,wd.AUTOSCL,set_button=wd.p.autoscl_on

  dmy = widget_label(b24, value='LOG:')
  b24c=widget_base(b24, /row,/ nonex)
  wd.LOG= widget_button(b24c,value='')
  widget_control,wd.LOG,set_button=wd.p.log_on

  b25=widget_base(b2, /row,sensitive=~wd.p.autoscl_on)
  dmy = widget_label(b25, value='MIN:')
  wd.MIN = widget_text(b25,value=string(wd.p.MIN,form='(i6)'), xsize=6, uvalue='MIN',/edit)
  dmy = widget_label(b25, value='MAX:')
  wd.MAX = widget_text(b25,value=string(wd.p.MAX,form='(i6)'), xsize=6, uvalue='MAX',/edit)

  return,wd
  
end


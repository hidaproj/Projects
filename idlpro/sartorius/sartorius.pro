; sartorius.pro

; 2016.01.28  t.a.
; 2016.02.19  t.a.
; 2016.07.13  K. Otsuji PC replacement
; 2016.07.26  K. Otsuji rapid acquisition / fits output
; 2016.07.28  K. Otsuji contrast of jpeg output
; 2016.08.04  K. Otsuji fixed display intensity range, enlarge display size


;CD, 'C:\projects\idl\sartorius'
;.FULL_RESET_SESSION
;.COMPILE sartorius
;RESOLVE_ALL
;RESOLVE_ROUTINE,['tic','toc']
;SAVE, /ROUTINES, FILENAME='sartorius.sav'

;=============================================
; include
@makolib


;=============================================
pro sartorius_event, ev
  ;---------------------------------------------

  common widget,wp,wd,p
  common buffer,img,simg,rimg

  IF (TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_TIMER') then begin  ;timer event

    tic
    ;imgs=mako_obs(wp.p,img,header=header)
    imgs=mako_getimg(p,img,header=header)
    if n_elements(imgs) ne 0 then begin
      imgs=rotate(imgs,7)
      simg=bytscl(imgs,min=0,max=2l^12-1)
      rimg=frebin(simg,wp.wx,wp.wy)
      tv,rimg
      xyouts,/norm,0.05,0.95,wp.status+' now',color=255
      if total(simg eq 255) gt 0.01*p.NAXIS1*p.NAXIS2 then xyouts,/norm,0.5,0.5,'saturate!!',color=0,align=0.5

      if wp.status eq 'save' then begin
        caldat,systime(/JULIAN,/UTC),mon,day,year,hour,minu,seco
        filename=string(mon,format='(i2.2)')+ $
          string(day,format='(i2.2)')+  $
          string(hour,format='(i2.2)')+ $
          string(minu,format='(i2.2)')+ $
          string(seco,format='(i2.2)')+ $
          'FBin'+string(wp.p.FGBINX,format='(i1.1)')+'Bit12'+$
          wp.topname
        xyouts,/norm,0.05,0.05,'saved '+filename+'.fits',color=255

        ;save,imgs,header,file=wp.savedir+'save\'+filename+'.sav'
        writefits,wp.savedir+'fits\'+filename+'.fits',imgs,struct2fitshead(header)
        write_jpeg,wp.savedir+'jpeg\'+filename+'.jpeg',simg,quality=80

      endif

      elapsed=toc()
      ;elapsed=0.4
      waitsec=wp.cadence-elapsed>0
      print,waitsec,'sec wait'
      widget_control,ev.top,timer=waitsec ;timer start

      return
    endif else xyouts,/norm,0.05,0.95,'getimg error',color=255
  endif
  
  uname=widget_info(ev.id,/uname)
  widget_control,wd.status,set_value=uname

  case uname of
    'exptime':begin
      widget_control,ev.id,get_value=value
      wp.p.EXPTIME=long(value)>41l<153000000l
      set_parameters,wp.p
      widget_control,wd.exptime,set_value=wp.p.EXPTIME

      widget_control,wd.status,set_value='set exposure time '+strtrim(wp.p.EXPTIME,2)

      print,''
      print,'set exposure time', wp.p.EXPTIME
    end
    'cadence':begin
      widget_control,ev.id,get_value=value
      wp.cadence=value>0
      widget_control,wd.cadence,set_value=wp.cadence

      widget_control,wd.status,set_value='set cadence '+strtrim(wp.cadence,2)

      print,''
      print,'set cadence', wp.cadence

      if wp.status eq 'preview' or wp.status eq 'save' then widget_control,ev.top,timer=0 ;timer reset

    end
    'topname':begin
      widget_control,ev.id,get_value=value
      wp.topname=strcompress(string(value),/remove_all)
      widget_control,wd.topname,set_value=wp.topname

      widget_control,wd.status,set_value='set filename '+wp.topname

      print,''
      print,'set filename ',wp.topname
    end
    'prevst':begin
      widget_control,wd.prevst,sensitive=0
      widget_control,wd.stop,sensitive=1
      print,'preview start'
      wp.status='preview'
      widget_control,wd.status,set_value='preview now'
      widget_control,ev.top,timer=0 ;timer start
    end
    'stop':begin
      widget_control,wd.exptime,sensitive=1
      widget_control,wd.prevst,sensitive=1
      widget_control,wd.stop,sensitive=0
      widget_control,wd.savest,sensitive=1
      widget_control,wd.topname,sensitive=1
      print,'stopeed'
      wp.status='stopped'
      widget_control,wd.status,set_value=wp.status
      widget_control,ev.top,timer=-1 ;timer stop
      tv,rimg
      xyouts,/norm,0.05,0.95,'stopped',color=255
      if total(simg eq 255) gt 0.01*p.NAXIS1*p.NAXIS2 then xyouts,/norm,0.5,0.5,'saturate!!',color=0,align=0.5
    end
    'savest':begin
      widget_control,wd.savest,sensitive=0
      widget_control,wd.exptime,sensitive=0
      widget_control,wd.topname,sensitive=0
      widget_control,wd.stop,sensitive=1
      print,'save start'
      wp.status='save'
      widget_control,wd.status,set_value='saving now'
      widget_control,ev.top,timer=0 ;timer start
    end
    'exit':begin
      mako_stopstream
      mako_fin
      widget_control,ev.top,timer=-1 ;timer stop
      widget_control,/destroy, ev.top
    end
    else:print,'no uname'
  endcase

END

;=============================================
pro sartorius
  ;---------------------------------------------
  common widget,wp,wd,p
  common buffer,img,simg,rimg

  caldat,systime(/JULIAN),mon,day,year,hour,minu,seco

  p=mako_init()
  if p.status eq 0 then return

  wp={widget_param,   $
    p:    p,  $
    topname:  'cen',  $
    savedir:  'D:\data\'+string(year,format='(i4.4)')+  $
    string(mon,format='(i2.2)')+      $
    string(day,format='(i2.2)')+'\',      $
    ;wx:   2048/4, $
    ;wy:   2048/4, $
    wx:   800, $
    wy:   800, $
    status:   ' ',  $
    cadence:  1.  $
  }

  img=uintarr(wp.p.NAXIS1,wp.p.NAXIS2)  ;buffer

  wd={wd_cdio,      $
    exptime:  0l, $
    cadence:  0l, $
    prevst:   0l, $
    ;preven:    0l, $
    savest:   0l, $
    ;saveen:    0l, $
    stop:   0l, $
    topname:  0l, $
    status:   0l, $
    exit:   0l, $
    draw:  0l $
  }

  set_parameters,wp.p
  if is_dir(wp.savedir) eq 0 then spawn,'mkdir '+wp.savedir
  if is_dir(wp.savedir+'fits\') eq 0 then spawn,'mkdir '+wp.savedir+'fits\'
  if is_dir(wp.savedir+'jpeg\') eq 0 then spawn,'mkdir '+wp.savedir+'jpeg\'

  fnt='ＭＳ Ｐゴシック*BOLD*20'
  fntsmall='ＭＳ Ｐゴシック*BOLD*10'

  main=widget_base(title='SARTORIUS Ha observation',/row)

  main1=widget_base(main,/column)

  base1=widget_base(main1,/row,/frame)
  wd.exptime=cw_field(base1,title='EXPO TIME: ',FONT=fnt,fieldfont=fnt,xsize=8,/ulong, $
    value=wp.p.EXPTIME,uname='exptime',/RETURN_EVENTS)
  lab=widget_label(base1,value=' usec',FONT=fnt)

  base2=widget_base(main1,/row,/frame)
  wd.cadence=cw_field(base2,title='CADENCE:   ',FONT=fnt,fieldfont=fnt,xsize=8,/float, $
    value=wp.cadence,uname='cadence',/RETURN_EVENTS)
  lab=widget_label(base2,value=' sec',  FONT=fnt)

  base3=widget_base(main1,/row,/frame)
  wd.prevst=widget_button(base3,uname='prevst',value='PREVIEW',   /align_center,FONT=fnt)
  wd.stop  =widget_button(base3,uname='stop',  value='   STOP   ',/align_center,FONT=fnt,sensitive=0)
  wd.savest=widget_button(base3,uname='savest',value='   SAVE   ',/align_center,FONT=fnt)

  base4=widget_base(main1,/row,ysize=15)

  base5=widget_base(main1,/row,/frame)
  lab=cw_field(base5,title='FOLDER:      ',FONT=fnt,fieldfont=fnt,value=wp.savedir,/noedit)

  base6=widget_base(main1,/column,/frame,xpad=2,ypad=2)
  lab=widget_label(base6,value='FILENAME:', FONT=fnt,/align_left)
  base6a=widget_base(base6,/row)
  wd.topname=cw_field(base6a,title='mmddhhmmssFBin1Bit12',FONT=fnt,fieldfont=fnt, $
    value=wp.topname,uname='topname',xsize=4,/RETURN_EVENTS)
  lab=widget_label(base6a,value='.fits', FONT=fnt)

  base7=widget_base(main1,/row,ysize=15)

  base8=widget_base(main1,/column,/frame)
  wd.status=cw_field(base8,title='STATUS:',FONT=fnt,fieldfont=fnt, $
    value=wp.status,uname='status',/noedit,xsize=23)

  base9=widget_base(main1,/column,/align_right)
  wd.exit=widget_button(base9,value='EXIT',uname='exit',FONT=fnt)

  main2=widget_base(main,/column)

  wd.draw=widget_draw(main2,XSIZE=wp.wx,YSIZE=wp.wy,/frame,uname='draw')

  widget_control,main,/realize

  !p.font=0
  device,decomposed=0,set_font=fnt

  mako_startstream,p

  xmanager,'sartorius',main


END

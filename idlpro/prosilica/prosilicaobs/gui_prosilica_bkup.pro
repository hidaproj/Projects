;PRO GUI_Prosilica
;+
;
;  GUI_Prosilica.pro
;
;Main debug procedure by widget for SMART T3 prosilica observation
;based on DST Poralization script by T. Anan
;
;20100512  T.K.
;20100603  T.K.
;20100814  T.K.
;
;==========================headder============================;
;PRO GUI_Prosilica_event, ev
;PRO GUI_Prosilica
;
;-

;=========================include=============================;
@ObsLib
;=========================main================================;


;**************************************************************
PRO GUI_Prosilica_event, ev
;--------------------------------------------------------------
common lib,wd,wp,wpdef
widget_control, ev.id, get_uvalue=uvalue,get_value=value

print,'uvalue=',uvalue
ev_seqst=widget_event(wd.bt_st,/nowait)
ev_ed=widget_event(wd.bt_ed,/nowait)
ev_exit=widget_event(wd.bt_exit,/nowait)

  case uvalue of
    'expo'       : begin
                    wpp=wp
                    wp.expo=long(value)
                    if (wp.expo lt 10) then begin
                      wp=wpp
                      widget_control,wd.tx_expo,set_value=string(wp.expo, form='(i5)')
                    endif
                    widget_control,wd.lb_expo,set_value=strcompress(wp.expo)+'        .'
                    end
    'gain'       : begin
                    wpp=wp
                    wp.gain=fix(value)
                     if (wp.gain lt 0) || (wp.gain gt 30) then begin
                      wp=wpp
                      widget_control,wd.tx_gain,set_value=string(wp.gain, form='(i5)')
                    endif  
                    widget_control,wd.lb_gain,set_value=strcompress(wp.gain)+'        .'
                   end          
    'nimg'       : begin
                    wpp=wp
                    wp.nimg=fix(value)
                    if (wp.nimg lt 1) ||(wp.nimg gt 100) then begin
                      wp=wpp
                      widget_control,wd.tx_nimg,set_value=string(wp.nimg, form='(i5)')
                    endif
                    widget_control,wd.lb_nimg,set_value=strcompress(wp.nimg)+'        .'
                   end
    'frate'      : begin
                    wpp=wp
                    wp.framerate=float(value)
                     if (wp.frate gt 30) then begin
                      wp=wpp
                      widget_control,wd.tx_frate,set_value=string(wp.framerate, form='(i5)')
                    endif            
                    widget_control,wd.lb_frate,set_value=strcompress(wp.framerate)+'        .'
                   end
    'binx'       : begin
                    wpp=wp
                    wp.binx=fix(value)
                    if (wp.binx lt 1) then wp.binx=wpp.binx $
                      else wp.Width=long(float(wpp.Width)/wp.binx)
                    widget_control,wd.tx_binx,set_value=string(wp.binx, form='(i5)')
                    widget_control,wd.tx_Width,set_value=string(wp.Width, form='(i5)')
                    widget_control,wd.lb_bin,set_value=' x: '+strcompress(wp.binx)+' y: '+strcompress(wp.biny)+'        .'
                    widget_control,wd.lb_xy,set_value=' x: '+strcompress(wp.Width)+' y: '+strcompress(wp.Height)+'        .'
                   end
    'biny'       : begin
                    wpp=wp
                    wp.biny=fix(value)
                    if (wp.biny lt 1) then wp.biny=wpp.biny $
                      else wp.Height=long(float(wpp.Height)/wp.biny)
                    widget_control,wd.tx_biny,set_value=string(wp.biny, form='(i5)')
                    widget_control,wd.tx_Height,set_value=string(wp.Height, form='(i5)')
                    widget_control,wd.lb_bin,set_value=' x: '+strcompress(wp.binx)+' y: '+strcompress(wp.biny)+'        .'
                    widget_control,wd.lb_xy,set_value=' x: '+strcompress(wp.Width)+' y: '+strcompress(wp.Height)+'        .'
                    end
    'height'     : begin
                    wpp=wp
                    wp.Height=fix(value)
                    if (wp.Height*wp.biny+wp.RegionY gt 1200) ||(wp.Height lt 0) then wp.Height=wpp.Height
                    widget_control,wd.tx_Height,set_value=string(wp.Height, form='(i5)')
                    widget_control,wd.lb_xy,set_value=' x: '+strcompress(wp.Width)+' y: '+strcompress(wp.Height)+'        .'
                   end
    'width'      : begin
                    wpp=wp
                    wp.Width=fix(value) 
                    if (wp.Width*wp.binx+wp.RegionX gt 1600) ||(wp.Width lt 0) then wp.Width=wpp.Width
                    widget_control,wd.tx_Width,set_value=string(wp.Width, form='(i5)')
                    widget_control,wd.lb_xy,set_value=' x: '+strcompress(wp.Width)+' y: '+strcompress(wp.Height)+'        .'
                    end
    'regionx'    : begin
                    wpp=wp
                    wp.RegionX=fix(value)
                    if (wp.RegionX gt 1600) ||(wp.RegionX lt 0) then wp.RegionX=wpp.RegionX $
                      else wp.Width=min([long(float(1600-wp.RegionX)/wp.binx),wp.Width])
                    widget_control,wd.tx_RegionX,set_value=string(wp.RegionX, form='(i5)')
                    widget_control,wd.lb_region,set_value=' x: '+strcompress(wp.RegionX)+' y: '+strcompress(wp.RegionY)+'        .'
                    widget_control,wd.tx_Width,set_value=string(wp.Width, form='(i5)')
                    widget_control,wd.lb_xy,set_value=' x: '+strcompress(wp.Width)+' y: '+strcompress(wp.Height)+'        .'
                  end
    'regiony'    : begin
                    wpp=wp
                    wp.RegionY=fix(value) 
                    if (wp.RegionY gt 1200) ||(wp.RegionY lt 0) then wp.RegionY=wpp.RegionY $
                      else wp.Height=min([long(float(1600-wp.RegionY)/wp.biny),wp.Height])
                    widget_control,wd.tx_RegionY,set_value=string(wp.RegionY, form='(i5)')
                    widget_control,wd.lb_region,set_value=' x: '+strcompress(wp.RegionX)+' y: '+strcompress(wp.RegionY)+'        .'
                    widget_control,wd.tx_Height,set_value=string(wp.Height, form='(i5)')
                    widget_control,wd.lb_xy,set_value=' x: '+strcompress(wp.Width)+' y: '+strcompress(wp.Height)+'        .'
                   end

    'seqint'    : begin
                    wpp=wp
                    wp.SeqInt=fix(value) 
                    if (wp.SeqInt lt 0) then wp.SeqInt=wpp.SeqInt
                    widget_control,wd.tx_seqint,set_value=string(wp.SeqInt, form='(i5)')
                    widget_control,wd.lb_seqint,set_value=' seqence interval: '+strcompress(wp.SeqInt)+'        .'
                   end


    'svdir'      : begin
                    wpp=wp
                    wp.svdir=value
                    widget_control,wd.lb_savedir,set_value=' '+wp.svdir
                    end
    'fname'      : begin
                    wpp=wp
                    wp.fname=value
                   end
    'nf'         : begin
                    wpp=wp
                    wp.nf=value
                    widget_control,wd.lb_nf,set_value=strcompress(wp.nf)+'        .'
                   end
    'wavelength' : begin
                    wpp=wp
                    wp.wavelength=value
                     widget_control,wd.lb_wavelength,set_value=' '+wp.wavelength+'        .'
                    end
    'telescope'  : begin
                    wpp=wp
                    wp.telescope=value
                    widget_control,wd.lb_telescope,set_value=wp.telescope+'        .'
                   end
    'program'  : begin
                    wpp=wp
                    wp.program=value
                    widget_control,wd.lb_program,set_value=wp.program+'                             .'

                   end

     ;===Reset to default parameters==;
    'reset'      : begin
                    wp=wpdef
                    Refresh,wp,wd
                    end

     ;========Sequence Start=======;
    'seq_st'     : begin
                     SEQ_START:
                     fn=strarr(wp.nf)
                     widget_control,wd.lb_stat,set_value=strcompress(wp.nf)+' sequence start .'
                     widget_control,wd.lb_svname,set_value='                              .'
                     brkflag=0
                     sttime=systime(1)
                     for i=0,wp.nf-1 do begin
                       ev_seqend=widget_event(wd.bt_ed,/nowait)
                       if ev_seqend.id ne 0 then brkflag=1
                       while systime(1)-sttime lt i*wp.SeqInt do begin
                         if ev_seqend.id ne 0 then goto, OBSFIN

                         Refresh,wp,wd
                       endwhile 
                       print,'===== '+strcompress(string(i+1),/remove_all)+' ====='
                       fn[i]=NormalObs(wp) 
                       widget_control,wd.lb_stat,set_value='sequence'+strcompress(i+1)+'/'+strcompress(wp.nf)+' finished'
                       widget_control,wd.lb_svname,set_value='wrote '+fn[i]

                       PrevObs,wp
 ;                      if uvalue eq 'seq_ed' then brkflag=1
                       Refresh,wp,wd
                       ev_seqend=widget_event(wd.bt_ed,/nowait)
                       ev_emp=widget_event(/nowait)
                       if brkflag eq 1  then goto, OBSFIN

                     endfor
                     OBSFIN:
                      widget_control,wd.lb_stat,set_value='the sequence'+strcompress(i)+'/'+strcompress(wp.nf)+ ' completed! Sequence End'
                     SEQEND:
                      brkflag_p=1
                   end
    ;=========== preview ==========;
    'prev_st'    : begin
                     ; CamInit
                     CamSetParam,wp
                     if wp.Width gt 1000 then  window,0,xs=wp.Width/2,ys=wp.Height/2 $
                       else  window,0,xs=wp.Width,ys=wp.Height
                     img=intarr(wp.Width,wp.Height)
                     widget_control,wd.lb_stat,set_value='previewing  .'
                     widget_control,wd.lb_svname,set_value='                              .'
                     WSET, 0
                     brkflag_p=0
                     while (brkflag_p eq 0) do begin
                       PrevImg,img
                       ev1=widget_event(wd.bt_ed,/nowait)	; END
                       ev2=widget_event(wd.bt_exit,/nowait)	; Exit
                       ev3=widget_event(wd.bt_st,/nowait)	; Seq.Start
                       ev_emp=widget_event(/nowait)

                       brkflag_p=ev1.id+ev2.id+ev3.id
                       ;if uvalue ne 'prev_st' then brkflag_p=1
                     endwhile
                     time=gettime()
                     widget_control,wd.lb_stat,set_value='preview end '+time
                     widget_control,wd.lb_svname,set_value='                              .'
                     if ev1.id ne 0 then goto, SEQEND
                     if ev2.id ne 0 then goto, EXIT_WINDOW
                     if ev3.id ne 0 then goto, SEQ_START
                   end
    
    ;=======exit observation======;
    'EXIT'       : begin
                     EXIT_WINDOW:
                      if !d.window ne -1 then wdelete
                     WIDGET_CONTROL, /destroy, ev.top  
			CamFin
                   end
    else        :
  endcase

end

;**************************************************************
PRO GUI_Prosilica
;--------------------------------------------------------------
common lib,wd,wp,wpdef
ParamLib,prodll,wp
;***********************************************************

CamInit

;************************************************************
;set parameters with widget
wpdef=wp
 wd={widget_param2, $
      bt_st:        0l,        $
      bt_prst:     0l,         $ 
      bt_ed:     0l,        $
      bt_exit:     0l,        $ 
      bt_reset:    0l,        $
      lb_expo:       0l,        $
      lb_gain:       0l ,       $
      lb_frate:       0l,        $
      lb_nimg:       0l,        $
      lb_bin:       0l,        $
      lb_xy:       0l,        $
      lb_region:       0l,        $
      lb_savedir:       0l,        $
      lb_wavelength:       0l,        $
      lb_nf:       0l,        $
      lb_stat:       0l,        $
      lb_svname:    0l,         $
      lb_seqint:    0l,         $
      lb_telescope: 0l,        $
      lb_program:   0l,         $
      tx_expo:      0l,         $
      tx_gain:      0l,         $
      tx_frate:      0l,         $
      tx_nimg:      0l,         $
      tx_binx:      0l,         $
      tx_biny:      0l,         $
      tx_Width:      0l,         $
      tx_Height:      0l,         $
      tx_RegionX:      0l,         $
      tx_RegionY:      0l,         $
      tx_nf:      0l,         $
      tx_seqint:    0l,       $
      tx_fname:      0l,         $
      tx_wavelength: 0l,        $
      tx_telescope: 0l,        $
      tx_program:   0l         $
     }


main = WIDGET_BASE(title='Prosilica Observation',/column)

  ;== Set Parameter ==;
lab= widget_label(main,value='>>> Set Parameter <<<');,font=2)
  bs_spcall=widget_base(main, /row, /frame)
  bs_spc1=widget_base(bs_spcall, /column,xsize=200)
  bs_spc2=widget_base(bs_spcall, /column,xsize=200)

  bs_spr11=widget_base(bs_spc1, /row,/align_right)
    lab=widget_label(bs_spr11,value='   expo(mininum 10 usec)  : ')
    wd.tx_expo=widget_text(bs_spr11,value=string(wp.expo, form='(i5)'), xsize=6, ysize=1, uvalue='expo',/edit)
  bs_spr12=widget_base(bs_spc2, /row,/align_right)
    lab=widget_label(bs_spr12,value='        gain(0-5) : ')
     wd.tx_gain=widget_text(bs_spr12,value=string(wp.gain, form='(i5)'), xsize=6, ysize=1,uvalue='gain',/edit)

  bs_spr21=widget_base(bs_spc1, /row,/align_right)
    lab=widget_label(bs_spr21,value='   frame rate (frame/sec)  : ')
     wd.tx_frate=widget_text(bs_spr21,value=string(wp.framerate, form='(i5)'), xsize=6, ysize=1, uvalue='frate',/edit)
  bs_spr22=widget_base(bs_spc2, /row,/align_right)
    lab=widget_label(bs_spr22,value='      framenum(0-100)   : ')
     wd.tx_nimg=widget_text(bs_spr22,value=string(wp.nimg, form='(i5)'), xsize=6, ysize=1, uvalue='nimg',/edit)

;  bs_spr31=widget_base(bs_spc1, /row,/align_right)
;    lab=widget_label(bs_spr31,value='   sequence rate (file/sec)  : ')
;    text=widget_text(bs_spr31,value=string(wd.seqrate, form='(i5)'), xsize=6, ysize=1, uvalue='seqrate',/edit)


  bs_spr41=widget_base(bs_spc1, /row,/align_right)
    lab=widget_label(bs_spr41,value='      binx     : ')
     wd.tx_binx=widget_text(bs_spr41,value=string(wp.binx, form='(i5)'), xsize=6, ysize=1, uvalue='binx',/edit)
  bs_spr42=widget_base(bs_spc2, /row,/align_right)
    lab=widget_label(bs_spr42,value='      biny     : ')
     wd.tx_biny=widget_text(bs_spr42,value=string(wp.biny, form='(i5)'), xsize=6, ysize=1, uvalue='biny',/edit)
 
  bs_spr51=widget_base(bs_spc1, /row,/align_right)
    lab=widget_label(bs_spr51,value='    Width     : ')
     wd.tx_Width=widget_text(bs_spr51,value=string(wp.Width, form='(i5)'), xsize=6, ysize=1, uvalue='width',/edit)
  bs_spr52=widget_base(bs_spc2, /row,/align_right)
    lab=widget_label(bs_spr52,value='    Height    : ')
     wd.tx_Height=widget_text(bs_spr52,value=string(wp.Height, form='(i5)'), xsize=6, ysize=1, uvalue='height',/edit)


  bs_spr61=widget_base(bs_spc1, /row,/align_right)
    lab=widget_label(bs_spr61,value='   offset X : ')
     wd.tx_RegionX=widget_text(bs_spr61,value=string(wp.RegionX, form='(i5)'), xsize=6, ysize=1, uvalue='regionx',/edit)
  bs_spr62=widget_base(bs_spc2, /row,/align_right)
    lab=widget_label(bs_spr62,value='   offset Y  : ')
     wd.tx_RegionY=widget_text(bs_spr62,value=string(wp.RegionY, form='(i5)'), xsize=6, ysize=1, uvalue='regiony',/edit)


  bs_spr71=widget_base(bs_spc1, /row,/align_right)
      lab=widget_label(bs_spr71,value=' Number of Set : ')
      wd.tx_nf=widget_text(bs_spr71,value=string(wp.nf, form='(i5)'), xsize=6, uvalue='nf',/edit)
  bs_spr72=widget_base(bs_spc2, /row,/align_right)
    lab=widget_label(bs_spr72,value=' sequence interval[s] : ')
     wd.tx_seqint=widget_text(bs_spr72,value=string(wp.SeqInt, form='(i5)'), uvalue = "seqint", xsize=6, ysize=1,/edit) 
 
  wd.bt_reset=widget_button(main, value="Reset Parameters", uvalue = "reset",/align_center)


  ;== Observation ==;
lab_ob = widget_label(main,value='>>> Observation <<<');,font=2)
  bs_ob=widget_base(main, /column, /frame,/align_center)
    bs_sv0=widget_base(bs_ob, /row,/align_right)
      lab=widget_label(bs_sv0,value='Save Directory : ')
       text=widget_text(bs_sv0,value=wp.svdir, xsize=45, uvalue='svdir',/edit)

   bs_sv1=widget_base(bs_ob, /row,/align_right)
      lab=widget_label(bs_sv1,value='   File Name    : ')
      wd.tx_fname=widget_text(bs_sv1,value=wp.fname, xsize=45, uvalue='fname',/edit)

  bs_svr=widget_base(bs_ob, /row)
  bs_svc1=widget_base(bs_svr, /column,xsize=200)
  bs_svc2=widget_base(bs_svr, /column,xsize=200)
        

   bs_sv21=widget_base(bs_svc1, /row,/align_right)
      lab=widget_label(bs_sv21,value=' Wavelength [A] : ')
      wd.tx_wavelength=widget_text(bs_sv21,value=string(wp.wavelength), uvalue='wavelength',/edit,xsize=6)
   bs_sv22=widget_base(bs_svc2, /row,/align_right)
      lab=widget_label(bs_sv22,value='Telescope : ')
       wd.tx_telescope=widget_text(bs_sv22,value=wp.telescope, uvalue='telescope',/edit,xsize=20)
 
   bs_sv3=widget_base(bs_ob, /row,/align_right)
      lab=widget_label(bs_sv3,value=' Program : ')
       wd.tx_program=widget_text(bs_sv3,value=wp.program, uvalue='program',/edit,xsize=45)

  bs_sb=widget_base(main, /column,/align_center)
    bs_sb1=widget_base(bs_sb, /row,/align_center)
      wd.bt_st=widget_button(bs_sb1, value="Seqence Start", uvalue = "seq_st",/align_center,xsize=183)
      wd.bt_prst=WIDGET_BUTTON(bs_sb1,uvalue='prev_st',value='Preview Start',/align_center,xsize=183)
      
  wd.bt_ed=WIDGET_BUTTON(main,uvalue='seq_ed',value='Seqence Stop',/align_center,xsize=183)

  bs_wvf=widget_base(main, /column, /frame)
  bs_wv=widget_base(bs_wvf, /row, /align_center)
  bs_wv1=widget_base(bs_wv,/column)
  bs_wv2=widget_base(bs_wv,/column)

   lab=widget_label(bs_wv1,value='exposure [usec]',/align_right)
   wd.lb_expo=widget_label(bs_wv2,value=strcompress(wp.expo)+'        .',/align_left)
   lab=widget_label(bs_wv1,value='gain',/align_right)
   wd.lb_gain=widget_label(bs_wv2,value=strcompress(wp.gain)+'        .',/align_left)
   lab=widget_label(bs_wv1,value='framerate [frame/sec]',/align_right)
   wd.lb_frate=widget_label(bs_wv2,value=strcompress(wp.framerate)+'        .',/align_left)
   lab=widget_label(bs_wv1,value='frame per sequence',/align_right)
   wd.lb_nimg=widget_label(bs_wv2,value=strcompress(wp.nimg)+'        .',/align_left)
   lab=widget_label(bs_wv1,value='number of set',/align_right)
   wd.lb_nf=widget_label(bs_wv2,value=strcompress(wp.nf)+'        .',/align_left)
   lab=widget_label(bs_wv1,value='binning',/align_right)
   wd.lb_bin=widget_label(bs_wv2,value=' x: '+strcompress(wp.binx)+' y: '+strcompress(wp.biny)+'        .',/align_left)
   lab=widget_label(bs_wv1,value='pixel num',/align_right)
   wd.lb_xy=widget_label(bs_wv2,value=' x: '+strcompress(wp.Width)+' y: '+strcompress(wp.Height)+'        .',/align_left)
   lab=widget_label(bs_wv1,value='offset of FOV',/align_right)
   wd.lb_region=widget_label(bs_wv2,value=' x: '+strcompress(wp.RegionX)+' y: '+strcompress(wp.RegionY)+'        .',/align_left)
   lab=widget_label(bs_wv1,value='seqence interval [sec]:',/align_right)
   wd.lb_seqint=widget_label(bs_wv2,value=strcompress(wp.SeqInt)+'        .',/align_left)

   lab=widget_label(bs_wv1,value='save directory',/align_right)
   wd.lb_savedir=widget_label(bs_wv2,value=' '+wp.svdir,/align_left)
   lab=widget_label(bs_wv1,value='wavelength [A]',/align_right)
   wd.lb_wavelength=widget_label(bs_wv2,value=' '+wp.wavelength+'        .',/align_left)
   lab=widget_label(bs_wv1,value='Observatory',/align_right)
   lab=widget_label(bs_wv2,value=' '+wp.observatory+'        .',/align_left)
   lab=widget_label(bs_wv1,value='Telescope',/align_right)
   wd.lb_telescope=widget_label(bs_wv2,value=' '+wp.telescope+'        .',/align_left)
   lab=widget_label(bs_wv1,value='Program',/align_right)
   wd.lb_program=widget_label(bs_wv2,value=' '+wp.program+'                                         .',/align_left)

   lab=widget_label(bs_wv1,value=' ',/align_right)
   lab=widget_label(bs_wv2,value=' ',/align_right)
   lab=widget_label(bs_wv1,value='<<STATUS>>',/align_right)
   wd.lb_stat=widget_label(bs_wv2,value='                                                    .',/align_left)
   bs_wv3=widget_base(bs_wvf, /column)
   wd.lb_svname=widget_label(bs_wv3,value='                                                                                              .',/align_left)
   
   
    wd.bt_exit = widget_button(main, value="Exit", uvalue = "EXIT")
widget_control, main, /realize
XMANAGER,'GUI_Prosilica',main,modal=modal


END

;**********************************************************
PRO Refresh,wp,wd
;----------------------------------------------------------


widget_control,wd.tx_expo,set_value=string(wp.expo, form='(i5)')
widget_control,wd.lb_expo,set_value=strcompress(wp.expo)+'        .'
widget_control,wd.tx_gain,set_value=string(wp.gain, form='(i5)')
widget_control,wd.lb_gain,set_value=strcompress(wp.gain)+'        .'
widget_control,wd.tx_nimg,set_value=string(wp.nimg, form='(i5)')
widget_control,wd.lb_nimg,set_value=strcompress(wp.nimg)+'        .'
widget_control,wd.tx_frate,set_value=string(wp.framerate, form='(i5)')
widget_control,wd.lb_frate,set_value=strcompress(wp.framerate)+'        .'
widget_control,wd.tx_binx,set_value=string(wp.binx, form='(i5)')
widget_control,wd.tx_biny,set_value=string(wp.biny, form='(i5)')
widget_control,wd.tx_Width,set_value=string(wp.Width, form='(i5)')
widget_control,wd.tx_Height,set_value=string(wp.Height, form='(i5)')
widget_control,wd.lb_bin,set_value=' x: '+strcompress(wp.binx)+' y: '+strcompress(wp.biny)+'        .'
widget_control,wd.lb_xy,set_value=' x: '+strcompress(wp.Width)+' y: '+strcompress(wp.Height)+'        .'
widget_control,wd.tx_RegionX,set_value=string(wp.RegionX, form='(i5)')
widget_control,wd.tx_RegionY,set_value=string(wp.RegionY, form='(i5)')
widget_control,wd.lb_region,set_value=' x: '+strcompress(wp.RegionX)+' y: '+strcompress(wp.RegionY)+'        .'
widget_control,wd.lb_nf,set_value=strcompress(wp.nf)+'        .'
widget_control,wd.tx_nf,set_value=strcompress(wp.nf)+'        .'
widget_control,wd.lb_seqint,set_value=strcompress(wp.SeqInt)+'        .'
widget_control,wd.tx_seqint,set_value=string(wp.SeqInt, form='(i5)')
widget_control,wd.lb_wavelength,set_value=' '+wp.wavelength+'        .'
widget_control,wd.tx_wavelength,set_value=strcompress(wp.wavelength)






return
end

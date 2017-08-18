;PRO T3_GUItest
;+
;
;  T3_GUItest.pro
;
;Main debug procedure by widget for SMART T3 prosilica observation
;based on DST Poralization script by T. Anan
;
;20100512  T.K.
;20100603  T.K.
;
;==========================headder============================;
;PRO T3_GUItest_event, ev
;PRO T3_GUItest
;
;-

;=========================include=============================;
@T3_ObsLib
;=========================main================================;


;**************************************************************
PRO T3_GUItest_event, ev
;--------------------------------------------------------------
common lib,wd,wp,wpdef

widget_control, ev.id, get_uvalue=uvalue,get_value=value

print,'uvalue=',uvalue
ev_seqst=widget_event(wd.bt_st,/nowait)
ev_prevend=widget_event(wd.bt_pred,/nowait)
ev_seqend=widget_event(wd.bt_ed,/nowait)
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
    'ra_h'       : begin
                    wpp=wp
                    wp.ra_h=string(value,format='(i2.2)')
                    widget_control,wd.lb_ra,set_value=' '+wp.ra_h+':'+wp.ra_m+':'+wp.ra_s+'        .'
                   end
    'ra_m'       : begin
                    wpp=wp
                    wp.ra_m=string(value,format='(i2.2)')
                    widget_control,wd.lb_ra,set_value=' '+wp.ra_h+':'+wp.ra_m+':'+wp.ra_s+'        .'                 
                   end
    'ra_s'       : begin
                    wpp=wp
                    wp.ra_s=string(value,format='(i2.2)')
                    widget_control,wd.lb_ra,set_value=' '+wp.ra_h+':'+wp.ra_m+':'+wp.ra_s+'        .'
                   end
    'dec_d'      : begin
                    wpp=wp
                    wp.dec_d=string(value,format='(i3.3)')
                    widget_control,wd.lb_dec,set_value=' '+wp.dec_d+':'+wp.dec_m+':'+wp.dec_s+'        .'
                   end
    'dec_m'      : begin
                    wpp=wp
                    wp.dec_m=string(value,format='(i2.2)')
                    widget_control,wd.lb_dec,set_value=' '+wp.dec_d+':'+wp.dec_m+':'+wp.dec_s+'        .'                  
                   end
    'dec_s'      : begin
                    wpp=wp
                    wp.dec_s=string(value,format='(i2.2)')
                    widget_control,wd.lb_dec,set_value=' '+wp.dec_d+':'+wp.dec_m+':'+wp.dec_s+'        .'
                   end
    'kusabi_f'   : begin
                    wpp=wp
                    wp.kusabi_f=string(value,format='(i3.3)')
                    widget_control,wd.lb_kusabi,set_value=' front:'+wp.kusabi_f+' rear:'+wp.kusabi_r+'        .'
                   end
    'kusabi_r'   : begin
                    wpp=wp
                    wp.kusabi_r=string(value,format='(i3.3)')
                   widget_control,wd.lb_kusabi,set_value=' front:'+wp.kusabi_f+' rear:'+wp.kusabi_r+'        .'
                   end
     ;===Reset to default parameters==;
    'reset'      : begin
                    wp=wpdef
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
                   end
    
     ;========Sequence Start=======;
    'seq_st'     : begin
                     SEQ_START:
                     fn=strarr(wp.nf)
                     widget_control,wd.lb_stat,set_value=strcompress(wp.nf)+' sequence start .'
                     widget_control,wd.lb_svname,set_value='                              .'
                     brkflag=0
                     for i=0,wp.nf-1 do begin
                       if ev_seqend.id ne 0 then brkflag=1
                           print,'===== '+strcompress(string(i+1),/remove_all)+' ====='
                         fn[i]=NormalObs(wp) 
                         widget_control,wd.lb_stat,set_value='sequence'+strcompress(i+1)+'/'+strcompress(wp.nf)+' finished'
                         widget_control,wd.lb_svname,set_value='wrote '+fn[i]

                         PrevObs,wp
 ;                      if uvalue eq 'seq_ed' then brkflag=1
 
                         ev_seqend=widget_event(wd.bt_ed,/nowait)
                         ev_emp=widget_event(/nowait)
                       if brkflag eq 1  then break

                     endfor
                      widget_control,wd.lb_stat,set_value='the sequence'+strcompress(i)+'/'+strcompress(wp.nf)+ ' completed! Sequence End'
                   end

    ;=========== preview ==========;
    'prev_st'    : begin
                     T3_CamInit
                     T3_CamSetParam,wp
                     if wp.Width gt 1000 then  window,0,xs=wp.Width/2,ys=wp.Height/2 $
                       else  window,0,xs=wp.Width,ys=wp.Height
    
                     img=intarr(wp.Width,wp.Height)
                     widget_control,wd.lb_stat,set_value='previewing  .'
                     widget_control,wd.lb_svname,set_value='                              .'
                     WSET, 0
                     brkflag_p=0
                     while (brkflag_p eq 0) do begin
                       T3_PrevImg,img
                       ev_prevend=widget_event(wd.bt_pred,/nowait)
                       ev_exit=widget_event(wd.bt_exit,/nowait)
                       ev_seqst=widget_event(wd.bt_st,/nowait)
  
                       ev_emp=widget_event(/nowait)
                       brkflag_p=ev_prevend.id+ev_exit.id+ev_seqst.id
               ;        if uvalue ne 'prev_st' then brkflag_p=1
                     endwhile
                     time=FinishObs()
                     widget_control,wd.lb_stat,set_value='preview end '+time
                     widget_control,wd.lb_svname,set_value='                              .'
                     if ev_exit.id ne 0 then goto, EXIT_WINDOW
                     if ev_seqst.id ne 0 then goto, SEQ_START
                   end
    
    ;=======exit observation======;
    'EXIT'       : begin
                     EXIT_WINDOW:
                     if !d.window ne -1 then wdelete
                     WIDGET_CONTROL, /destroy, ev.top  
                   end
    else        :
  endcase

end

;**************************************************************
PRO T3_GUItest
;--------------------------------------------------------------
common lib,wd,wp,wpdef
T3_ParamLib,prodll,wp
;***********************************************************
;set parameters with widget
wpdef=wp
 wd={widget_param2, $
      bt_st:        0l,        $
      bt_ed:        0l,        $
      bt_prst:     0l,         $ 
      bt_pred:     0l,        $
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
      lb_focus:       0l,        $
      lb_kusabi:       0l,        $
      lb_ra:       0l,        $
      lb_dec:       0l,        $
      lb_stat:       0l,        $
      lb_svname:    0l,         $
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
      tx_fname:      0l,         $
      tx_kusabi_f:      0l,         $
      tx_kusabi_r:      0l,         $
      tx_ra_h:      0l,         $
      tx_ra_m:      0l,         $
      tx_ra_s:      0l,         $
      tx_dec_d:      0l,         $
      tx_dec_m:      0l,         $
      tx_dec_s:      0l         $
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
    lab=widget_label(bs_spr72,value=' sequence interval : ')
     text=widget_text(bs_spr72,value='continuous', xsize=10, ysize=1)
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
      lab=widget_label(bs_sv21,value=' Wave Length : ')
       text=widget_text(bs_sv21,value=wp.wavelength, xsize=6, uvalue='wavelength')
      lab=widget_label(bs_sv21,value='[A]')
   bs_sv22=widget_base(bs_svc2, /row,/align_right)
      lab=widget_label(bs_sv22,value=' Focus position : ')
       text=widget_text(bs_sv22,value='72000', xsize=6)
      lab=widget_label(bs_sv22,value='[um]')
 
   bs_sv31=widget_base(bs_svc1, /row,/align_right)
      lab=widget_label(bs_sv31,value='       kusabi ')
      lab=widget_label(bs_sv31,value=' front: ')
       wd.tx_kusabi_f=widget_text(bs_sv31,value=wp.kusabi_f, xsize=6, uvalue='kusabi_f',/edit,  $
          xoffset=100000)
   bs_sv32=widget_base(bs_svc2, /row,/align_right)
      lab=widget_label(bs_sv32,value=' rear: ')
       wd.tx_kusabi_r=widget_text(bs_sv32,value=wp.kusabi_r, xsize=6, uvalue='kusabi_r',/edit,  $
          xoffset=100000)
          
    bs_sv41=widget_base(bs_svc1, /row,/align_right)
      lab=widget_label(bs_sv41,value='RA :')
       wd.tx_ra_h=widget_text(bs_sv41,value=wp.ra_h, xsize=4, uvalue='ra_h',/edit,  $
          xoffset=100000)
      lab=widget_label(bs_sv41,value=':')
      wd.tx_ra_m=widget_text(bs_sv41,value=wp.ra_m, xsize=4, uvalue='ra_m',/edit,  $
          xoffset=100000)
      lab=widget_label(bs_sv41,value=':')
      wd.tx_ra_s=widget_text(bs_sv41,value=wp.ra_s, xsize=4, uvalue='ra_s',/edit,  $
          xoffset=100000)

    bs_sv42=widget_base(bs_svc2, /row,/align_right)
      lab=widget_label(bs_sv42,value='  DEC :')
       wd.tx_dec_d=widget_text(bs_sv42,value=wp.dec_d, xsize=4, uvalue='dec_d',/edit,  $
          xoffset=100000)
      lab=widget_label(bs_sv42,value=':')
       wd.tx_dec_m=widget_text(bs_sv42,value=wp.dec_m, xsize=4, uvalue='dec_m',/edit,  $
          xoffset=100000)
      lab=widget_label(bs_sv42,value=':')
      wd.tx_dec_s=widget_text(bs_sv42,value=wp.dec_s, xsize=4, uvalue='dec_s',/edit,  $
          xoffset=100000)

  bs_sb=widget_base(bs_ob, /column,/align_center)
    bs_sb1=widget_base(bs_sb, /row,/align_center)
      wd.bt_st=widget_button(bs_sb1, value="Seqence Start", uvalue = "seq_st",/align_center,xsize=183)
      wd.bt_ed=widget_button(bs_sb1, value="Seqence Stop", uvalue = "seq_ed",/align_center,xsize=183)
      
    bs_pr=widget_base(main, /row,/align_center)
    wd.bt_prst=WIDGET_BUTTON(bs_pr,uvalue='prev_st',value='Preview Start',/align_center,xsize=183)
    wd.bt_pred=WIDGET_BUTTON(bs_pr,uvalue='prev_en',value='Preview Stop',/align_center,xsize=183)

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
   lab=widget_label(bs_wv1,value='save directory',/align_right)
   wd.lb_savedir=widget_label(bs_wv2,value=' '+wp.svdir,/align_left)
   lab=widget_label(bs_wv1,value='wavelength [A]',/align_right)
   wd.lb_wavelength=widget_label(bs_wv2,value=' '+wp.wavelength+'        .',/align_left)
   lab=widget_label(bs_wv1,value='Focal position [um]',/align_right)
   wd.lb_focus=widget_label(bs_wv2,value=strcompress(72000)+'        .',/align_left)
   lab=widget_label(bs_wv1,value='kusabi',/align_right)
   wd.lb_kusabi=widget_label(bs_wv2,value=' front:'+wp.kusabi_f+' rear:'+wp.kusabi_r+'        .',/align_left)
   lab=widget_label(bs_wv1,value='RA',/align_right)
   wd.lb_ra=widget_label(bs_wv2,value=' '+wp.ra_h+':'+wp.ra_m+':'+wp.ra_s+'        .',/align_left)
   lab=widget_label(bs_wv1,value='DEC',/align_right)
   wd.lb_dec=widget_label(bs_wv2,value=' '+wp.dec_d+':'+wp.dec_m+':'+wp.dec_s+'        .',/align_left)
   lab=widget_label(bs_wv1,value=' ',/align_right)
   lab=widget_label(bs_wv2,value=' ',/align_right)
   lab=widget_label(bs_wv1,value='<<STATUS>>',/align_right)
   wd.lb_stat=widget_label(bs_wv2,value='                                                    .',/align_left)
   bs_wv3=widget_base(bs_wvf, /column)
   wd.lb_svname=widget_label(bs_wv3,value='                                                                                              .',/align_left)
   
   
    wd.bt_exit = widget_button(main, value="Exit", uvalue = "EXIT")
widget_control, main, /realize
XMANAGER,'T3_GUItest',main,modal=modal


END



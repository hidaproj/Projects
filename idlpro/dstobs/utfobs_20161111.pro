; utfobs.pro
@orcalib
@utflib

;**************************************************************
function version
  ver='0.0' ; '2014/05/21 k.i.    from orcagui.pro
  ver='1.0' ; '2014/06/10 k.i., m.h.  many
  ver='1.1' ; '2014/06/11 k.i.    savefits_pf, FtsHed
  ver='1.2' ; '2014/06/17 k.i.    dvset bug., dretc sign
  ver='1.3' ; '2014/10/18 k.i.    dt bug.
  ver='1.5' ; '2014/10/18 m.h.    wl0 correction in WSCAN
  ver='1.6' ; '2014/11/16 k.i.,m.h.   obs_start bug fix, wscan line center
  ver='1.7' ; '2015/01/27 m.h.    dwl selection not only km/s but also nm
  ver='1.8' ; '2015/05/05 k.i.    time interval bug., line center set
  ver='1.9' ; '2015/06/21 k.i.,m.h.   wscan, set offset bug fix
  ver='2.0' ; '2015/08/14 k.i.,a.t.,t.s.,u.k.   double mode, ex.trigger mode
  ver='2.1' ; '2015/08/23 k.i.,a.t.,t.s   Orca nowait, Dout for trigger mode
  ver='2.2' ; '2015/08/26 k.i.,a.t.   ExTrig for GET, filename ctime @exp.end
  ver='2.21'  ; '2015/08/28 k.i   change wdf.MODE during obs
  ver='2.22'  ; '2015/08/31 k.i   filename ctime @exp.end for OBS_START
  ver='3.0'   ; '2016/03/23   k.o.,k.i.     from tf40obs.pro
  ver='3.1'   ; '2016/05/08   k.i.      orca_capture, scanget while ms1-ms0 lt 100
  ver='3.2'   ; '2016/08/02   k.i.      orca_capturemode_sequence
  ver='3.3' ; '2016/08/09 k.o.    subarray pixels must be multiple of 4
  ver='3.4' ; '2016/10/15 k.i.    wscan with boxcur1
  ver='3.5' ; '2016/10/27 k.o.    fast scanning observation
  ver='4.0' ; '2016/11/11 k.o.  refactoring'
  return,ver
end

;**************************************************************
pro make_dir,dir
  ;--------------------------------------------------------------
  if !version.arch eq 'x86_64' then $
    oscomdll='C:\Projects\cprog\VS2010\oscom64\x64\Debug\oscom64.dll' $
  else  oscomdll='C:\Projects\cprog\VS2008\oscom32\Debug\oscom32.dll'
  d=file_search(dir)
  if d[0] eq '' then dmy=call_external(oscomdll,'Dmkdirs',dir)

end

;**************************************************************
function wdevid, wd
  ;--------------------------------------------------------------
  ; return event ID's (long tags) in wd (widget) structure

  nt0=n_tags(wd)
  typ=lonarr(nt0)
  for i=0,nt0-1 do typ[i]=size(wd.(i),/type)
  ii=where(typ eq 3, nt)
  typ=typ[ii]
  wdid=lonarr(nt)
  for i=0,nt-1 do wdid[i]=wd.(ii[i])

  return,wdid
end

;**************************************************************
pro utfobs_event, ev
  ;--------------------------------------------------------------
  common utfobs, wd, wdp, wdf, evid_p, evid_f, o, p, f, img1, imgs, fh
  ; wd  - widget structure
  ; wdp - camera widget structure
  ; wdf - UTF widget structure
  ; o - obs control parameters
  ; f - UTF control parameters
  ; p - camera control parameters

  widget_control, ev.id, get_uvalue=value
  temp_ch=f.b.temp_ch
  tch=temp_ch[uniq(temp_ch,sort(temp_ch))]
  tmp=get_therm_temp(ch=tch,time=time)
  ctemp=strjoin(string(tmp,form='(f6.2)'),',')
  widget_control,wdf.TEMP,set_value=ctemp
  dbin=2048./wdp.p.wx/p.bin

  case (ev.id) of
    ;--- UTF handle ---
    wdf.WL0: begin
      wdf.p.iw=ev.index
      widget_control, ev.id, set_droplist_select=wdf.p.iw
      f.wl0=float(wdf.p.cwl0s[wdf.p.iw])
      set_lc_offset,f
      utf_set_wl,f,dwl=0.
      com=wdf.p.cwl0s[wdf.p.iw]+' selected'
      messagebox,com
    end
    wdf.DWTXT: begin
      f.dwl=float(gt_wdtxt(ev.id))
      i=(f.dwl-wdf.p.sld.dwmin)/wdf.p.sld.dwstep
      widget_control, wdf.DWSLD, set_value=i
      utf_set_wl,f
    end
    wdf.DWSLD: begin
      i=ev.value
      f.dwl=wdf.p.sld.dwmin+wdf.p.sld.dwstep*i
      widget_control, wdf.DWTXT, set_value=string(f.dwl,form='(f7.3)')
      utf_set_wl,f
    end
    wdf.DWmin: begin
      wdf.p.wscan.dwmin=float(gt_wdtxt(ev.id))
    end
    wdf.DWmax: begin
      wdf.p.wscan.dwmax=float(gt_wdtxt(ev.id))
    end
    wdf.DWstep: begin
      wdf.p.wscan.dwstep=float(gt_wdtxt(ev.id))
    end
    wdf.WSCAN: begin
      nstep=(wdf.p.wscan.dwmax-wdf.p.wscan.dwmin)/wdf.p.wscan.dwstep+1
      prof=dblarr(nstep)
      pdwl=dblarr(nstep)
      messagebox,'Please select the intensity averaging area.'
      box_cur1,x0, y0, nx, ny
      for i=0,nstep-1 do begin
        dwl=wdf.p.wscan.dwmin+i*wdf.p.wscan.dwstep
        utf_set_wl,f,dwl=dwl,wait=0.1
        print,f.b.volt
        j=(dwl-wdf.p.sld.dwmin)/wdf.p.sld.dwstep
        widget_control, wdf.DWSLD, set_value=j
        widget_control, wdf.DWTXT, set_value=string(f.dwl,form='(f7.3)')
        pdwl[i]=f.dwl
        img1=OrcaObs(nimg=1)
        img=rebin(img1,p.Width/dbin,p.Height/dbin)>0
        prof[i]=mean(img[x0:x0+nx-1,y0:y0+ny-1])
        dmin=wdp.p.min
        dmax=wdp.p.max
        if wdp.p.log_on then begin
          img=alog10(img>1)
          dmin=alog10(dmin>1)
          dmax=alog10(dmax>1)
        endif
        img=wdp.p.AUTOSCL_ON?bytscl(img):bytscl(img,min=dmin,max=dmax)
        tv,img,p.regionx/dbin,p.regiony/dbin
        ;tvscl,rebin(img1,p.Width/dbin,p.Height/dbin)>0,p.regionx/dbin,p.regiony/dbin
        print,f.dwl
      endfor

      window,4,xs=512,ys=512
      plot,pdwl,prof,$
        xtitle='wavelength [nm]',ytitle='intensity'
      ans=wdyesno('Change line center?',xpos=500,ypos=400)
      if ans then begin
        lcc=wdgetstr('Enter line center position [nm] =>',xpos=500,ypos=400)
        lc=float(lcc)
        print,'lc=',lc
        utf_set_wl,f,dwl=lc
        f.b.temp0=f.b.temp  ; 2015.6.21
        f.b.ret_offset=f.b.ret
        f.dwl=0.
      endif
      wdelete,4
    end
    wdf.ADJ: begin
      utf_adj,f
    end
    wdf.AUTOADJ: begin
      utf_autoadj,f,p,dbin,ev
    end
    wdf.MODE: begin
      widget_control, ev.id, get_value=value
      f.mode=wdf.p.modes[value]
    end

    ;--- ORCA handle ---
    wdp.PRV_START: begin
      nbins=128 & imax=2l^16 -1
      ii=findgen(nbins)/nbins*imax
      adj=0
      wscan=0
      orca_idle
      orca_capturemode_sequence,nimg=1      ;-- set capturemode sequence
      orca_capture    ;continuous capturing start

      set_plot,'z'
      device,set_resolution=[p.Width,p.Height]/dbin
      set_plot,'win'

      while ev.id ne wdp.PRV_STOP do begin
        img1=orca_getimg(nimg=1);,/nowait)
        if wscan eq 1 then begin
          prof1[is]=median(img1)
          is=is+1
        endif

        img=rebin(img1,p.Width/dbin,p.Height/dbin)>0
        dmin=wdp.p.min
        dmax=wdp.p.max
        if wdp.p.log_on then begin
          img=alog10(img>1)
          dmin=alog10(dmin>1)
          dmax=alog10(dmax>1)
        endif
        img=wdp.p.AUTOSCL_ON?bytscl(img):bytscl(img,min=dmin,max=dmax)

        set_plot,'z'
        tv,img
        if wdp.p.hist_on then begin
          h=histogram(img1,max=imax,min=0,nbins=nbins)
          plot,ii,h,psym=10, $
            /noerase,/xstyle,charsize=0.5,pos=[0.05,0.05,0.27,0.2],color=127
        endif
        if wdp.p.mmm_on then begin
          mmm=uint([min(img1,max=max),mean(img1),max])
          xyouts,0,1,/norm,'!C'+strjoin(['MIN ','MEAN','MAX ']+' '+string(mmm),'!C'),color=127
        endif
        tmp=tvrd()
        set_plot,'win'
        tvscl,tmp,p.regionx/dbin,p.regiony/dbin

        ;--- UTF ---
        wdscn=[wdf.DWmin,wdf.DWmax,wdf.DWstep]
        ev = widget_event(wdscn,/nowait)
        dmy=where(ev.id eq wdscn, count)
        if count ne 0 then utf_handle, ev, wdf, f
        ev = widget_event(wdf.DWTXT,/nowait)
        if ev.id eq wdf.DWTXT then begin
          f.dwl=float(gt_wdtxt(ev.id))
          i=(f.dwl-wdf.p.sld.dwmin)/wdf.p.sld.dwstep
          widget_control, wdf.DWSLD, set_value=i
          utf_set_wl,f
        endif
        ev = widget_event(wdf.DWSLD,/nowait)
        if ev.id eq wdf.DWSLD then begin
          i=ev.value
          f.dwl=wdf.p.sld.dwmin+wdf.p.sld.dwstep*i
          widget_control, wdf.DWTXT, set_value=string(f.dwl,form='(f7.3)')
          utf_set_wl,f
        endif
        ev = widget_event(wdf.ADJ,/nowait)
        if ev.id eq wdf.ADJ then begin
          utf_adj,f,wd=wda
          adj=1
          wdida=[wda.htemp,wda.hload,wda.hsave,$
            wda.hsld,wda.htxt,wda.hvolt,wda.hscan,wda.hwlsh,$
            wda.done]
        endif
        ev = widget_event(wdf.MODE,/nowait)
        if ev.id eq wdf.MODE then begin
          widget_control, ev.id, get_value=value
          f.mode=wdf.p.modes[value]
          utf_set_wl,f
        endif
        if adj eq 1 then begin
          ev = widget_event(wdida,/nowait)
          jjj=where(ev.id eq wdida, count)
          if count ne 0 then utf_adj_event, ev
          if ev.id eq wda.done then begin
            restore,f.tbldir+'\f.sav' ;--> f
            adj=0
          endif
        endif
        ev = widget_event(wdf.WSCAN,/nowait)
        if ev.id eq wdf.WSCAN then begin
          nstep=fix((wdf.p.wscan.dwmax-wdf.p.wscan.dwmin)/wdf.p.wscan.dwstep+1)
          wscan=1
          is=0
          prof1=fltarr(nstep)
          dws=wdf.p.wscan.dwmin+findgen(nstep)*wdf.p.wscan.dwstep
        endif
        if wscan eq 1 then begin
          dwl=wdf.p.wscan.dwmin+is*wdf.p.wscan.dwstep
          utf_set_wl,f,dwl=dwl
          print,f.b.volt
          j=(dwl-wdf.p.sld.dwmin)/wdf.p.sld.dwstep
          widget_control, wdf.DWSLD, set_value=j
          widget_control, wdf.DWTXT, set_value=string(f.dwl,form='(f7.3)')
          if is eq nstep then begin
            wscan=0
            utf_set_wl,f,dwl=0.
            j=(0-wdf.p.sld.dwmin)/wdf.p.sld.dwstep
            widget_control, wdf.DWSLD, set_value=j
            widget_control, wdf.DWTXT, set_value=string(f.dwl,form='(f7.3)')
            window,4,xs=512,ys=512
            plot,dws,prof1,$
              xtitle='wavelength [nm]',ytitle='intensity'
            ans=wdyesno('Change line center?',xpos=500,ypos=400)
            if ans then begin
              lcc=wdgetstr('Enter line center position [nm] =>',xpos=500,ypos=400)
              lc=float(lcc)
              print,'lc=',lc
              utf_set_wl,f,dwl=lc
              f.b.temp0=f.b.temp  ; 2015.6.21
              f.b.ret_offset=f.b.ret
              f.dwl=0.
            endif
            wdelete,4
          endif
        endif
        ev = widget_event(wdp.PRV_STOP,/nowait)
      endwhile
      orca_idle    ;capture stop
    end
    wdp.EXPO: begin
      p.expo=float(gt_wdtxt(ev.id))
      p=OrcaSetParam(expo=p.expo)
    end
    wdp.BIN: begin
      p.bin=fix(gt_wdtxt(ev.id))
      dbin=2048./wdp.p.wx/p.bin
      p=OrcaSetParam(bin=p.bin)
      set_wdroi,wdp,p
    end
    wdp.X0: begin
      p=OrcaSetParam(regionx=fix(gt_wdtxt(ev.id))/(4/p.bin)*(4/p.bin))
      set_wdroi,wdp,p
    end
    wdp.Y0: begin
      p=OrcaSetParam(regiony=fix(gt_wdtxt(ev.id))/(4/p.bin)*(4/p.bin))
      set_wdroi,wdp,p
    end
    wdp.WIDTH: begin
      p=OrcaSetParam(width=fix(gt_wdtxt(ev.id))/(4/p.bin)*(4/p.bin))
      set_wdroi,wdp,p
    end
    wdp.HEIGHT: begin
      p=OrcaSetParam(height=fix(gt_wdtxt(ev.id))/(4/p.bin)*(4/p.bin))
      set_wdroi,wdp,p
    end
    wdp.CBOX: begin
      box_cur1,x0, y0, nx, ny
      four=4/p.bin
      x0-=(x0 mod four)
      y0-=(y0 mod four)
      nx-=(nx mod four)
      ny-=(ny mod four)
      p=OrcaSetParam(regionx=x0*dbin,regiony=y0*dbin,width=nx*dbin,height=ny*dbin)
      set_wdroi,wdp,p
    end
    wdp.FULL: begin
      p=OrcaSetParam(regionx=0,regiony=0,width=2048/p.bin,height=2048/p.bin)
      set_wdroi,wdp,p
    end
    wdp.AUTOSCL:begin
      wdp.p.AUTOSCL_ON=widget_info(wdp.AUTOSCL,/button_set)
      widget_control,widget_info(wdp.MIN,/parent),sensitive=~wdp.p.AUTOSCL_ON
    end
    wdp.LOG:begin
      wdp.p.LOG_ON=widget_info(wdp.LOG,/button_set)
    end
    wdp.MIN:begin
      widget_control,wdp.MIN,get_value=tmp
      wdp.p.MIN=long(tmp)
    end
    wdp.MAX:begin
      widget_control,wdp.MAX,get_value=tmp
      wdp.p.MAX=long(tmp)
    end

    ;--- obs handle ---
    wd.NIMG: begin
      o.nimg=fix(gt_wdtxt(ev.id))
    end
    wd.SNAP: begin
      p.date_obs=get_systime(ctime=ctime)
      img1=OrcaObs(nimg=1)
      p.date_obs2=get_systime()

      img=rebin(img1,p.Width/dbin,p.Height/dbin)>0
      dmin=wdp.p.min
      dmax=wdp.p.max
      if wdp.p.log_on then begin
        img=alog10(img>1)
        dmin=alog10(dmin>1)
        dmax=alog10(dmax>1)
      endif
      img=wdp.p.AUTOSCL_ON?bytscl(img):bytscl(img,min=dmin,max=dmax)
      tv,img,p.regionx/dbin,p.regiony/dbin
      o.filename=o.fnam+ctime
      widget_control,wd.FILENAME,set_value=o.filename
    end
    wd.GET: begin
      if o.ExTrig then begin
        orca_idle    ;capture stop
        orca_capturemode_snap,nimg=o.nimg
        orca_settrigmode,'Start',polarity='Negative' ; for trigger test
      endif
      p.date_obs=get_systime()
      imgs=OrcaObs(nimg=o.nimg,/nowait)
      if o.ExTrig then begin
        caio_dout,0,1
        wait,0.001
        caio_dout,0,0
      endif
      imgs=orca_getimg(nimg=o.nimg)
      if o.ExTrig then orca_settrigmode,'Internal'
      p.date_obs2=get_systime(ctime=ctime)
      img1=imgs[*,*,o.nimg-1]
      img=rebin(img1,p.Width/dbin,p.Height/dbin)>0
      dmin=wdp.p.min
      dmax=wdp.p.max
      if wdp.p.log_on then begin
        img=alog10(img>1)
        dmin=alog10(dmin>1)
        dmax=alog10(dmax>1)
      endif
      img=wdp.p.AUTOSCL_ON?bytscl(img):bytscl(img,min=dmin,max=dmax)
      tv,img,p.regionx/dbin,p.regiony/dbin
      o.filename=o.fnam+ctime
      widget_control,wd.FILENAME,set_value=o.filename
    end
    wd.SCANGET: begin
      if o.ExTrig then orca_settrigmode,'Start',polarity='Negative' ; for trigger test
      nstep=(wdf.p.wscan.dwmax-wdf.p.wscan.dwmin)/wdf.p.wscan.dwstep+1
      pdwl=dblarr(nstep)
      dwl=wdf.p.wscan.dwmin+0*wdf.p.wscan.dwstep
      utf_set_wl,f,dwl=dwl
      j=(dwl-wdf.p.sld.dwmin)/wdf.p.sld.dwstep
      widget_control, wdf.DWSLD, set_value=j
      widget_control, wdf.DWTXT, set_value=string(f.dwl,form='(f7.3)')
      if o.ExTrig then begin
        caio_dout,0,1
        wait,0.001
        caio_dout,0,0
      endif
      orca_idle
      orca_capturemode_sequence,nimg=1      ;-- set capturemode sequence
      orca_capture    ;continuous capturing start
      imgs=uintarr(p.width,p.height,nstep)
      obstime=strarr(nstep)
      for i=0,nstep-1 do begin
        dwl=wdf.p.wscan.dwmin+i*wdf.p.wscan.dwstep
        utf_set_wl,f,dwl=dwl
        obstime[i]=get_systime(msec=ms0)
        img1=orca_getimg(nimg=1);,/nowait)
        imgs[*,*,i]=img1
        if 1 then begin
          img=rebin(img1,p.Width/dbin,p.Height/dbin)>0
          img=wdp.p.AUTOSCL_ON?bytscl(img):bytscl(img,min=dmin,max=dmax)
          tv,img,p.regionx/dbin,p.regiony/dbin
          xyouts,20,20,'dwl='+string(dwl,form='(f6.3)')+'nm',chars=1.5,/dev
        endif
        print,'dwl=',dwl
        j=(dwl-wdf.p.sld.dwmin)/wdf.p.sld.dwstep
        widget_control, wdf.DWSLD, set_value=j
        widget_control, wdf.DWTXT, set_value=string(f.dwl,form='(f7.3)')
      endfor
      p.date_obs=obstime[0]
      if o.ExTrig then orca_settrigmode,'Internal'
      p.date_obs2=get_systime(ctime=ctime)
      img1=imgs[*,*,nstep-1]
      img=rebin(img1,p.Width/dbin,p.Height/dbin)>0
      dmin=wdp.p.min
      dmax=wdp.p.max
      if wdp.p.log_on then begin
        img=alog10(img>1)
        dmin=alog10(dmin>1)
        dmax=alog10(dmax>1)
      endif
      img=wdp.p.AUTOSCL_ON?bytscl(img):bytscl(img,min=dmin,max=dmax)
      tv,img,p.regionx/dbin,p.regiony/dbin
      o.filename=o.fnam+ctime
      widget_control,wd.FILENAME,set_value=o.filename
      orca_idle
    end
    wd.PROFS: begin
      img1=imgs[*,*,0]
      profiles,rebin(img1,wdp.p.wx,wdp.p.wy)
    end
    wd.REV: begin
      imgsize,imgs,nx,ny,nim
      for i=0,nim-1 do begin
        img=rebin(imgs[*,*,i],p.Width/dbin,p.Height/dbin)>0
        dmin=wdp.p.min
        dmax=wdp.p.max
        if wdp.p.log_on then begin
          img=alog10(img>1)
          dmin=alog10(dmin>1)
          dmax=alog10(dmax>1)
        endif
        img=wdp.p.AUTOSCL_ON?bytscl(img):bytscl(img,min=dmin,max=dmax)
        tv,img,p.regionx/dbin,p.regiony/dbin
        xyouts,10,10,string(i,form='(i4.0)'),/dev
      endfor
    end
    wd.SAVE: begin
      outfile=o.outdir+o.filename+'.fits'
      savefits_pf,imgs,p,f,file=outfile
      print,'saved to '+outfile
    end
    wd.OBS_START: begin
      nbins=128 & imax=2l^16 -1
      ii=findgen(nbins)/nbins*imax
      count=0
      date_obs=get_systime(ctime=ctime,msec=msec)
      msec0=msec

      orca_idle
      orca_capturemode_sequence,nimg=o.nimg      ;-- set capturemode sequence
      orca_capture    ;continuous capturing start

      while ev.id ne wd.OBS_STOP do begin
        if o.ExTrig then begin
          orca_idle    ;capture stop
          orca_capturemode_snap,nimg=o.nimg
          orca_settrigmode,'Start',polarity='Negative' ; for trigger test
        endif
        ev = widget_event(wd.OBS_STOP,/nowait)
        dwl1=o.dvs[count mod o.ndw]/3e5*f.wl0
        f.mode=o.fmodes[count mod o.ndw]
        utf_set_wl,f,dwl=dwl1
        j=(dwl1-wdf.p.sld.dwmin)/wdf.p.sld.dwstep
        widget_control, wdf.DWSLD, set_value=j
        widget_control, wdf.DWTXT, set_value=string(f.dwl,form='(f7.3)')
        im=where(f.mode eq wdf.p.modes) & im=im[0]
        widget_control, wdf.MODE, set_value=im
        date_obs=get_systime(ctime=ctime,msec=msec)
        if (count gt 0) and ((count mod o.ndw) eq 0) then begin
          date_obs=get_systime(ctime=ctime,msec=msec)
          print,'waiting for '+string((o.dt-(msec-msec0)/1000.)>0,form='(f10.3)'),' sec'
          while msec lt msec0+o.dt*1000. do begin
            wait,0.001
            date_obs=get_systime(ctime=ctime,msec=msec)
          endwhile
          msec0=msec
        endif
        p.date_obs=date_obs
        imgs=OrcaObs(nimg=o.nimg,/nowait)
        if o.ExTrig then begin
          caio_dout,0,1
          wait,0.001
          caio_dout,0,0
        endif
        imgs=orca_getimg(nimg=o.nimg)
        if o.ExTrig then orca_settrigmode,'Internal'
        p.date_obs2=get_systime(ctime=ctime)
        img1=imgs[*,*,o.nimg-1]
        img=rebin(img1,p.Width/dbin,p.Height/dbin)>0
        dmin=wdp.p.min
        dmax=wdp.p.max
        if wdp.p.log_on then begin
          img=alog10(img>1)
          dmin=alog10(dmin>1)
          dmax=alog10(dmax>1)
        endif
        img=wdp.p.AUTOSCL_ON?bytscl(img):bytscl(img,min=dmin,max=dmax)
        tv,img,p.regionx/dbin,p.regiony/dbin
        if wdp.p.hist_on then begin
          h=histogram(img1,max=imax,min=0,nbins=nbins)
          plot,ii,h,psym=10, $
            /noerase,/xstyle,charsize=0.5,pos=[0.05,0.05,0.27,0.2],color=127
        endif
        if wdp.p.mmm_on then begin
          mmm=uint([min(img1,max=max),mean(img1),max])
          xyouts,p.regionx/dbin,(p.regiony+p.Height)/dbin,/dev,'!C'+strjoin(['MIN ','MEAN','MAX ']+' '+string(mmm),'!C'),color=127
        endif
        o.filename=o.fnam+ctime
        widget_control,wd.FILENAME,set_value=o.filename
        outfile=o.outdir+o.filename+'.fits'
        xyouts,10,p.Height/dbin-30,string(count,form='(i5)')+'  '+outfile+'   '+string(dwl1,form='(f6.3)')+'nm',/dev
        savefits_pf,imgs,p,f,file=outfile
        count=count+1
        widget_control,wdf.TEMP,set_value=string(f.b[0].temp,form='(f6.2)')
        ;wait,1 ; <=== 20160502 <======20161027 comment out
      endwhile
      orca_idle    ;capture stop
      print,'Obs. stop'
    end
    wd.ExTrig: begin
      widget_control, ev.id, get_value=value
      o.extrig=value
      if o.extrig then p.TrigMode='Start' else p.TrigMode='Internal'
      print,'ExTrigger=',o.extrig
    end
    wd.DT: begin
      o.dt=float(gt_wdtxt(ev.id))
    end
    wd.OUTDIR: begin
      o.outdir=gt_wdtxt(ev.id)
      make_dir,o.outdir
    end
    wd.FNAM: begin
      o.fnam=gt_wdtxt(ev.id)
    end
    wd.FILENAME: begin
      o.filename=gt_wdtxt(ev.id)
    end
    wd.LOAD: begin
      file=dialog_pickfile(path=o.outdir,title='select file')
      imgs=uint(rfits(file,head=fh))
      byteorder,imgs
      imgsize,imgs,nx,ny,nn
      o.nimg=nn
      widget_control,wd.NIMG,set_value=string(o.nimg,form='(i4)')
      RegionX=fits_keyval(fh,'X0',/fix)
      RegionY=fits_keyval(fh,'Y0',/fix)
      bin=fits_keyval(fh,'BIN',/fix)
      Width=nx
      Height=ny
      p=OrcaSetParam(bin=bin)
      p=OrcaSetParam(regionx=RegionX,regiony=RegionY,width=Width,height=Height)
      set_wdroi,wdp,p
      widget_control,wdp.BIN,set_value=string(p.bin,form='(i2)')
      widget_control,wd.NIMG,set_value=string(o.nimg,form='(i4)')
      dbin=2048./wdp.p.wx/p.bin
    end
    wd.FTSHED: begin
      edtfile='c:\tmp\hed.txt'
      editor='C:\Windows\notepad.exe'
      dllfile='C:\Projects\cprog\VS2010\oscom64\x64\Debug\oscom64.dll'
      openw,1,edtfile
      for i=0,n_elements(fh)-1 do printf,1,fh[i]
      close,1
      case !version.os_family of
        'Windows': begin
          retn = call_external(dllfile,'excom',editor+' '+edtfile, value = [0b])
        end
        'unix': begin
          spawn,editor+' '+edtfile+' &'
        end
      endcase
    end
    wd.DWEDT: begin
      oscomdll='C:\Projects\cprog\VS2010\oscom64\x64\Debug\oscom64.dll'
      dvset=f.tbldir+'\dvset.txt'
      dmy=call_external(oscomdll,'excom','c:\windows\notepad.exe '+dvset)
    end
    wd.DWset: begin
      dvset=f.tbldir+'\dvset.txt'
      str1=''
      count=0
      openr,1,dvset
      while not eof(1) do begin
        readf,1,str1
        if count eq 0 then strs=str1 else strs=[strs,str1]
        count=count+1
      endwhile
      close,1

      i=smenu(strs)
      str1=strs[i]

      sts=strsep(str1,sep=',')

      if (sts[0] eq 'A') or (sts[0] eq 'a') then begin
        sts=sts[1:n_elements(sts)-1]
        unit='nm'
        vfact=3e5/f.wl0
      endif else vfact=1.
      o.ndw=n_elements(sts)
      widget_control,wd.NDW,set_value=string(o.ndw,form='(i3)')
      print,'wl offset  V[km/s]   dwl[nm]  fmode'
      for i=0,o.ndw-1 do begin
        sts1=strcompress(sts[i],/remove_all)
        case strmid(sts1,0,1) of
          'a': begin
            o.fmodes[i]='a'
            sts1=strmid(sts1,1,strlen(sts1)-1)
          end
          'b': begin
            o.fmodes[i]='b'
            sts1=strmid(sts1,1,strlen(sts1)-1)
          end
          else: o.fmodes[i]='s'
        endcase
        o.dvs[i]=float(sts1)*vfact  ; km/s
        print,o.dvs[i],o.dvs[i]/3e5*f.wl0,o.fmodes[i],form='(f12.0,f10.3,a5)'
      endfor
    end
    wd.Exit: begin
      WIDGET_CONTROL, /destroy, ev.top
    end
    else:
  endcase


end

;************************************************************************
function utfobs_widget,base,o
  ;--------------------------------------------------------------

  NDWmax=n_elements(o.dvs)
  wd={wd_utfobs_V01,  $
    NIMG:   0l, $ ; integ # for 1 shot
    DT:   0l,   $
    NDW:  0l,   $ ; # of wavelength
    DWEDT:  0l,   $ ; set wavelength offsets by velocity (km/s)
    DWset:  0l,   $ ; set wavelength offsets by velocity (km/s)
    OBS_START:  0l,   $
    OBS_STOP: 0l,   $
    ExTRIG:   0l, $
    SNAP:   0l, $
    GET:    0l, $
    SCANGET:    0l, $
    PROFS:    0l, $
    REV:    0l, $
    OUTDIR:   0l, $
    FNAM:   0l, $
    FILENAME: 0l, $
    SAVE:   0l, $
    LOAD:   0l, $
    FTSHED:   0l, $
    Exit:   0l  $
  }

  b1=widget_base(base, /column, /frame)
  dmy = widget_label(b1, value='>>> Obs. <<<')
  b11=widget_base(b1, /row)
  dmy = widget_label(b11, value='nimg=')
  wd.NIMG = widget_text(b11,value=string(o.nimg,form='(i4)'), xsize=5, uvalue='NIMG',/edit)
  dmy = widget_label(b11, value='   dt:')
  wd.DT = widget_text(b11,value=string(o.dt,form='(f5.1)'), xsize=5, uvalue='DT',/edit)
  dmy = widget_label(b11, value='sec')
  b13=widget_base(b1, /row)
  dmy=widget_label(b13,value='ndwl')
  wd.NDW = widget_text(b13,value=string(o.ndw,form='(i3)'), xsize=5, uvalue='NDW')
  wd.DWEDT=widget_button(b13, value="Edit dvset", uvalue = "DWEDT")
  wd.DWset=widget_button(b13, value="Set dwl", uvalue = "DWset")

  b12=widget_base(b1, /row)
  wd.SNAP=widget_button(b12, value="Snap", uvalue = "SNAP")
  wd.GET=widget_button(b12, value="Get", uvalue = "GET")
  wd.SCANGET=widget_button(b12, value="ScanGet", uvalue = "ScanGET")
  b15=widget_base(b1, /row)
  wd.PROFS=widget_button(b15, value="Prof", uvalue = "PROFS")
  wd.REV=widget_button(b15, value="Rev", uvalue = "REV")
  wd.SAVE=widget_button(b15, value="Save", uvalue = "SAVE")
  b14=widget_base(b1, /row)
  wd_label = widget_label(b14,value='ExTrig:')
  setv=o.extrig
  wd.ExTrig  = cw_bgroup(b14,['Non','External'],/row, $
    uvalue="ExTrig",/no_release,set_value=setv,/exclusive,ysize=25,/frame)
  b13=widget_base(b1, /row)
  dmy = widget_label(b13, value='Obs. ')
  wd.OBS_START=widget_button(b13, value="Start", uvalue = "PRV_START")
  wd.OBS_STOP=widget_button(b13, value="Stop", uvalue = "PRV_STOP")

  b3=b1
  b31=widget_base(b3, /row )
  dmy = widget_label(b31, value='outdir: ')
  wd.OUTDIR=widget_text(b31,value=o.outdir, xsize=25, ysize=1,uvalue='outdir',/edit)
  b33=widget_base(b3, /row )
  dmy = widget_label(b33, value='filenam:')
  wd.FILENAME=widget_text(b33,value=o.filename, xsize=23, ysize=1,uvalue='filename',/edit)
  dmy = widget_label(b33, value='.fits')
  b32=widget_base(b3, /row )
  dmy = widget_label(b32, value='fnam: ')
  wd.FNAM=widget_text(b32,value=o.fnam, xsize=5, ysize=1,uvalue='fnam',/edit)
  ; b34=widget_base(b3, /row )
  wd.LOAD=widget_button(b32, value="Load", uvalue = "Load")
  wd.FTSHED=widget_button(b32, value="FitsHd", uvalue = "FTSHED")
  b4=widget_base(base, /row )
  wd.Exit=widget_button(b4, value="Exit", uvalue = "Exit")

  return,wd
end

;************************************************************************
;  main
;--------------------------------------------------------------
common utfobs, wd, wdp, wdf, evid_p, evid_f, o, p, f, img1, imgs, fh
; wd  - obs widget structure
; wdp - camera widget structure
; wdf - UTF widget structure
; o - obs control parameters
; f - UTF control parameters
; p - camera control parameters

;-----  initialize devices  --------------------
noDev=0 ; if 1, skip device control
utfdir='C:\Projects\data\UTF32\'
utftbl='utf32def.tbl'
f=utf_init(utftbl,tbldir=utfdir,noDev=noDev)
utf_set_wl,f,dwl=0.
p=orcainit(noDev=noDev)
p=OrcaSetParam(expo=0.003,bin=1)

;----- Observation control parameters  ---------
NDWmax=201
o={utf_obs_v0,    $  ; UTF obs. control params
  nimg:   1,      $ ; # of image for 1 shot
  dt:   0.,     $ ; time step, sec
  extrig:   0,    $ ; 0: free, 1: extrn.trig.
  ndw:  11,       $ ; # of wavelength
  dvs:  fltarr(NDWmax), $ ; velocity (km/s), dwl/wl0*c
  fmodes: strarr(NDWmax), $ ; filter mode, 's', 'a', 'b'
  date:   '',     $ ; 'yymmdd'
  outdir:   '',     $
  fnam:   'utf',  $
  filename: ''  $
}

dmy=get_systime(ctime=ctime)
o.date=strmid(ctime,0,8)
o.outdir='D:\data\'+o.date+'\'
make_dir,o.outdir

;------  create widgets  -------------------------------
base = WIDGET_BASE(title='UTFOBS:  Ver.'+version(), /column)
wdf=utf_widget(base,f)
wdp=orca_widget(base,p)
wd=utfobs_widget(base,o)
widget_control, base, /realize
window,xs=wdp.p.wx,ys=wdp.p.wy
XMANAGER, 'utfobs', base

orcafin

end

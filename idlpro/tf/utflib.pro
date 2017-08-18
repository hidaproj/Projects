;  utflib.pro

;  2013.05.20  m.h., k.i.
;  2014.05.16  k.i., m.h.
;  2014.05.18  k.i., m.h.  noDev keuword for utf_init
;  2014.05.22  k.i.    utf_set_wl, utf_widget, utf_handle
;  2014.05.30    m.h   add b[i].cthick=double(st[2,i]) in gt_utfst.pro
;  2014.06.10    m.h., k.i.  utf_wl_set, set_lc_offset, f.Offset_info, utf_adj
;  2014.06.11    k.i.    savefits_pf
;  2015.05.19    k.i.    include 632.8
;  2015.06.20    k.i.,m.h. savefits_pf,  TRIGMODE
;  2015.08.07    k.i.,a.t.,t.s.  f.mode
;  2015.08.26    k.i.,a.t. f.mode in FITS header
;  2015.09.01    k.i.,a.t. savefits_pf  DATE_OBS,OB2   a23
;  2016.03.11    k.i.,k.o. b.voff, ck(6,4,3)
;  2016.03.26    k.i.,k.o. b.temp_ch  from tf40def.tbl, b[*].temp0  in LCoffset.tbl
;  2016.04.06    k.o. limit the line set for TF40 (only He-Ne laser & H alpha)
;  2016.04.24    k.i,k.o temperature reading only once with nsample=100
;  2016.04.25    k.i,k.o, correction for IDL6.4
;  2016.05.01    k.i  bug in set_lc_offset
;  2016.05.06    k.i  BSCALE keys in fits header, (BZERO not)
;  2016.08.08    k.i, k.o.,   utf_set_wl  tback keyword, autoadj, f.offset_info
;  2016.08.09  k.o., bug in temp_back
;  2016.10.31  k.o., auto adjust for UTF32
;  2016.11.11  k.o., refactoring
;  2016.12.08  k.o., common bridge for paralell fits output in savefits_pf.pro

@mllclib_usb
@caiolib2

;*************************************************
function dret_calcite,wl0,l,t0,t1

  ; input parem
  ; wl : wavelength in nm
  ; l : calcite_thick in mm
  ; T0 : reference Temp in C
  ; T1 : current temp in C
  ;
  ;output
  ; ret : delta ret in wave

  wl=wl0*1e-3
  a1=0.16d-5
  c1=-1.044d-5
  c2=-0.00043d-5

  ret=l*( ((a1+c1/wl)*T1+(c2/wl)*(T1^2)) - ((a1+c1/wl)*T0+(c2/wl)*(T0^2)) )
  ret=ret*1e3

  return,ret

end

;*************************************************
function get_lc_volt_fit,wl,ck,ret,temp,voff

  ;wl: target wl in nm
  ;ck: [*,*,*]
  ;ret: target ret in wave
  ;temp: current temp in C

  back:
  sc=size(ck)

  bk=fltarr(sc[1],sc[2])  ;order of ak, oder of bk
  for j=0,sc[2]-1 do begin
    for i=0,sc[1]-1 do begin
      bk[i,j]=poly(temp,ck[i,j,*])
    endfor
  endfor

  sb=size(bk)
  ak=fltarr(sb[1])  ;wl,temp,nk
  for i=0,sb[1]-1 do begin
    ak[i]=poly(wl,bk[i,*])
  endfor

  b=poly(ret,ak)
  ;voff=0.5

  ;avoid v>9.5volt and v<0volt
  if b lt 1/(9.5-voff) then begin
    ret=ret+1.0
    goto,back
  endif

  v=1./b+voff

  return,v

end

;*************************************************
pro set_lc_offset,f
  ;  set f.b[*].ret_offset using the pre-determined offset table

  utftbl=f.tbldir+'\'+f.LCoffset_tbl
  st=rdcsv(utftbl,skip=0)
  s=size(st)
  hed=st[*,0]
  st=st[*,1:s[2]-1]
  s1=where(fix(st[0,*]) eq fix(f.wl0))

  st=st[*,s1]
  it=where(strpos(hed,'save time') ne -1)
  stt=strcompress(st[it[0],*])
  s2=reverse(sort(stt))
  st1=st[*,s2[0]]
  nb=n_elements(f.b)
  f.offset_info=strjoin(strtrim([st1[0],st1[nb+1],st1[it[0]]],2),', ') ; wl,temp,time

  for i=0,nb-1 do begin
    b1=f.b[i]
    b1.ret_offset=st1[i+1]/360.
    b1.temp0=st1[(i+1+nb)<(it[0]-1)]
    f.b[i]=b1
  endfor

end


;*************************************************
function gt_utfst,utftbl
  ;  return UTF definition structure

  ;block setting
  b1={block_info,$
    fsr:1d,$        ; fsr in nm
    temp:20.,$        ; from get temp in C
    temp_ch:0,$       ; therm channel
    bname:'', $       ; block name ex) 'block1'
    elem:'', $        ; block element ex) 'PL1,?@C16A, WP2, C16B, LC9, PL4'
    cthick: 0d, $       ; calcite thichness in mm
    LC_ID:'',$        ; LC id number 1-9,  ex. 'LC1'
    LC_ch:'',$        ; LC channel ex. '1-2' - controller-1, ch-2
    LC_table:'',$       ; LC table containing c[*] parameters, ex. '***'
    temp0:20.,$       ; reference temp for c_k
    ck: fltarr(6,4,3),$     ; delta -> volt fitting coefficient
    sign: 1,$       ; shift sign 1 or -1
    ret:0.,$        ; current retardation of LCVR in wave
    volt:0.,$       ; current volt of LCVR
    voff:0.,$       ; Voff in fitting
    ret_offset: 0.$       ; retardations in wave for target wavelength at T=temp0
  }

  st=rdcsv(utftbl,skip=2)
  s=size(st)
  nclm=s[1]
  nblk=s[2]
  str1='' & openr,1,utftbl &  readf,1,str1 &  close,1
  filt_name=strupcase((strsplit(str1,' ',/ex))[0])
  b=replicate(b1,nblk)
  filename_sep,utftbl,dir,fname
  for i=0,nblk-1 do begin
    b[i].fsr=float(st[1,i])
    b[i].LC_ID=strcompress(st[3,i],/remove_all)
    b[i].LC_ch=strcompress(st[4,i],/remove_all)+'-'+strcompress(st[5,i],/remove_all)
    b[i].sign=fix(st[6,i])
    restore,dir+'\ak\'+strcompress(st[7,i],/remove_all)
    sc=size(ck)
    b[i].ck[0:sc[1]-1,0:sc[2]-1,0:sc[3]-1]=ck
    b[i].elem=st[8,i]
    b[i].cthick=double(st[2,i]) ;<- Hagino 2014.05.30
    if nclm ge 10 then b[i].voff=float(st[9,i]) else b[i].voff=0.5
    if nclm ge 11 then b[i].temp_ch=fix(st[10,i])
  endfor

  ;filter setting
  f={tf_info,$
    version:  '0.1',        $ ; version of this structure
    filt_name:  filt_name,    $ ; filter name
    tbldir:   dir,          $ ; table dir
    b:    b,          $ ; block structures, b[7]
    wl0:    656.28d,        $ ; wavelength of target line in nm
    dwl:    0.00,       $ ; wavelength offset from wl0 in nm
    LCoffset_tbl: 'offset_table.txt', $ ; LC retardation offset table name, ex. 'UTF32_LCoffset.txt'
    Offset_info:  'wl0, T, date ',  $ ; selected offset value info, ex. '656.3, 20.22, 2014/06/09 10:00:00'
    mode:   's',          $ ; 's': single, 'a': double-a, 'b': double-b
    time:   '',         $ ; obs time yyyy/mm/dd hh:mm:ss JST
    bfilt:    'none'        $ ; blocking filter
  }

  return,f

end
;*************************************************
function utf_init,utftbl,tbldir=tbldir,noDev=noDev

  f=gt_utfst(tbldir+utftbl)
  if keyword_set(noDev) then noDev=1 else noDev=0
  tmp=get_therm_temp(ch=0,/init,time=time,noDev=noDev)
  lcdrv_Open,usbx='USB1',noDev=noDev
  lcdrv_Open,usbx='USB2',noDev=noDev

  ;check D3050 controller order and swap if necessary
  if f.filt_name eq 'TF40' then lcdrv_Swap

  set_lc_offset,f
  return,f
end

;*************************************************
pro utf_set_wl,f,dwl=dwl,wait=wt,tback=tback

  common tftemp, temp_back, time_back

  if n_elements(tback) eq 0 then tback=60


  if n_elements(wt) eq 0 then wt=0.1  ; sec
  if n_elements(dwl) ne 0 then f.dwl=dwl

  nblk=n_elements(f.b)

  tchs=f.b.temp_ch
  tchs=tchs[uniq(tchs,sort(tchs))]
  nch=n_elements(tchs)
  if keyword_set(nodev) then temps=replicate(30.,nch) else $
    temps=get_therm_temp(ch=tchs,time=time,noDev=noDev,nsample=100)
  tsec=systime(/sec)
  if n_elements(temp_back) eq 0 then begin
    temp_back=dblarr(100,nch)
    time_back=dblarr(100) ; systime(/sec)
  endif
  temp_back=[transpose([temps]),temp_back[0:98,*]]
  time_back=shift(time_back,1) &      time_back[0]=tsec
  ii=where((tsec-time_back) lt tback, count)
  for i=0,nch-1 do temps[i]=mean(temp_back[ii,i])


  for i=0,nblk-1 do begin
    b1=f.b[i]
    b1.temp=temps[where(tchs eq b1.temp_ch)]

    dret_c=-dret_calcite(f.wl0,b1.cthick,b1.temp0,b1.temp)
    dret_w=(f.dwl/2)/b1.fsr
    dret = b1.sign*(dret_c+dret_w)
    ret1=b1.ret_offset+dret
    if f.mode ne 's' then begin ; double mode
      if abs(b1.fsr - 0.05) lt 0.001 then ret1=ret1+0.5
      if abs(b1.fsr - 0.1) lt 0.001 then begin
        case f.mode of
          'a':  ret1=ret1+0.25
          'b':  ret1=ret1-0.25
        endcase
      endif
    endif
    while ret1 gt 1. do ret1=ret1-1.
    while ret1 lt 0. do ret1=ret1+1.
    b1.ret=ret1
    b1.volt=get_lc_volt_fit(f.wl0, b1.ck, ret1, b1.temp, b1.voff)
    lc_ch=str_sep(b1.lc_ch,'-')
    if not keyword_set(nodev) then lcvolt,lc_ch[1],b1.volt,USBx='USB'+strcompress(lc_ch[0],/remove_all)
    f.b[i]=b1
  endfor

  wait,wt

end

;*************************************************
pro utf_close

  lcdrv_zero
  lcdrv_close,usbx='USB1'
  lcdrv_close,usbx='USB2'

end


;**************************************************************
pro messagebox, com

  oscomdll='C:\Projects\cprog\VS2010\oscom64\x64\Debug\oscom64.dll'
  dmy=call_external(oscomdll,'Dmsgbox',com,value=[0d,1d],/PORTABLE,/cdecl )

end

;**************************************************************
pro utf_handle, ev, wd, f

  case (ev.id) of
    wd.WL0: begin
      wd.p.iw=ev.index
      widget_control, ev.id, set_droplist_select=wd.p.iw
      f.wl0=float(wd.p.cwl0s[wd.p.iw])
      set_lc_offset,f
      utf_set_wl,f,dwl=0.
      com=wd.p.cwl0s[wd.p.iw]+' selected'
      messagebox,com
    end
    wd.DWTXT: begin
      f.dwl=float(gt_wdtxt(ev.id))
      i=(f.dwl-wd.p.sld.dwmin)/wd.p.sld.dwstep
      widget_control, wd.DWSLD, set_value=i
      utf_set_wl,f
    end
    wd.DWSLD: begin
      i=ev.value
      f.dwl=wd.p.sld.dwmin+wd.p.sld.dwstep*i
      widget_control, wd.DWTXT, set_value=string(f.dwl,form='(f7.3)')
      utf_set_wl,f
    end
    wd.DWmin: begin
      wd.p.wscan.dwmin=float(gt_wdtxt(ev.id))
    end
    wd.DWmax: begin
      wd.p.wscan.dwmax=float(gt_wdtxt(ev.id))
    end
    wd.DWstep: begin
      wd.p.wscan.dwstep=float(gt_wdtxt(ev.id))
    end
    wd.WSCAN: begin
      nstep=(wd.p.wscan.dwmax-wd.p.wscan.dwmin)/wd.p.wscan.dwstep+1
      for i=0,nstep-1 do begin
        dwl=wd.p.wscan.dwmin+i*wd.p.wscan.dwstep
        utf_set_wl,f,dwl=dwl
        j=(dwl-wd.p.sld.dwmin)/wd.p.sld.dwstep
        widget_control, wd.DWSLD, set_value=j
        widget_control, wd.DWTXT, set_value=string(f.dwl,form='(f7.3)')
      endfor
    end
    wd.ADJ: begin
      messagebox,'run Hagino program'
      utf_adj,f
    end
    else:
  endcase

end

;**************************************************************
function utf_widget,base,f

  wd={wd_utf_v02, $
    p: {utf_gui, $
      cwl0s:  f.FILT_NAME eq 'TF40'?['632.8','656.3']:['517.2','632.8','656.3','854.2','1083.0'], $ ; available lines
      iw: 0,    $ ; wl selection (prelim.)
      modes:  ['s','a','b'],  $ ; 's': single, 'a': double-a, 'b': double-b
      sld: {slider,   $ ; wd slider range
        dwmin:  -1.,  $ ; min dwl, nm
        dwmax:  1.,   $ ; max dwl, nm
        dwstep: 0.005   $ ; dwl step, nm
      }, $
      wscan: {wl_scan, $ ; wl-scan range
        dwmin:  -0.15,  $ ; min dwl, nm
        dwmax:  0.15,   $ ; max dwl, nm
        dwstep: 0.01  $ ; dwl step, nm
      } $
    },  $
    WL0:  0l,   $
    WSCAN:  0l,   $
    DWmin:  0l,   $
    DWmax:  0l,   $
    DWstep: 0l,   $
    DWTXT:  0l,   $
    DWSLD:  0l,   $ ;
    ADJ:  0l,   $ ; adjust -> Hagino program
    AUTOADJ:  0l,   $ ; adjust using absorption line
    MODE: 0l, $ ;
    TEMP: 0l  $
  }

  wd.p.iw=where(wd.p.cwl0s eq '656.3')  ;default is H alpha

  b=widget_base(base, /colum, /frame)
  dmy=widget_label(b,value='>>> '+f.FILT_NAME+' <<<')
  b1=widget_base(b, /row)
  dmy=widget_label(b1,value='line:')
  wd.WL0 = widget_droplist(b1,value=wd.p.cwl0s)
  widget_control, wd.WL0, set_droplist_select=wd.p.iw

  dmy=widget_label(b1,value='nm, T=')
  temp_ch=f.b.temp_ch
  tch=temp_ch[uniq(temp_ch,sort(temp_ch))]
  tmp=get_therm_temp(ch=tch,time=time)
  ctemp=strjoin(string(tmp,form='(f6.2)'),',')
  wd.TEMP = widget_text(b1,value=ctemp, xsize=12, ysize=1, uvalue='TEMP')
  dmy=widget_label(b1,value='C')

  b2=widget_base(b, /row)
  dmy=widget_label(b2,value='dwl')
  wd.DWTXT = widget_text(b2,value=string(f.dwl,form='(f7.3)'), xsize=7, uvalue='DWL',/edit)
  dmy=widget_label(b2,value='nm     ')
  wd.ADJ=widget_button(b2, value="ADJ", uvalue = "ADJ")
  wd.AUTOADJ=widget_button(b2, value="AutoADJ", uvalue = "AutoADJ")
  b3=widget_base(b, /row)
  n=(wd.p.sld.dwmax-wd.p.sld.dwmin)/wd.p.sld.dwstep
  i=(f.dwl-wd.p.sld.dwmin)/wd.p.sld.dwstep
  wd.DWSLD=widget_slider(b3, value=i, uvalue='TFSld', $
  minimum=0, maximum=n, xsize=200,suppress=1, vertical=0, frame=7, /drag )
  b4=widget_base(b, /row)
  b41=widget_base(b, /row)
  dmy=widget_label(b4,value='   dwmin   dwmax   dwstep [nm]')
  wd.DWmin = widget_text(b41,value=string(wd.p.wscan.dwmin,form='(f7.3)'), xsize=7, uvalue='DWmin',/edit)
  wd.DWmax = widget_text(b41,value=string(wd.p.wscan.dwmax,form='(f7.3)'), xsize=7, uvalue='DWmax',/edit)
  wd.DWstep = widget_text(b41,value=string(wd.p.wscan.dwstep,form='(f7.3)'), xsize=7, uvalue='DWstep',/edit)
  wd.WSCAN=widget_button(b41, value="wscan", uvalue = "scan")
  b42=widget_base(b, /row)
  dmy=widget_label(b42,value='mode')
  wd.MODE= cw_bgroup(b42,wd.p.modes,/row, $
    uvalue="Mode",/no_release,set_value=0,/exclusive,ysize=25,/frame)

  return,wd

end



;******************************************
function get_tim
  tim=str_sep(systime(),' ')
  time=tim[4]+'/'+mmm2mm(tim[1])+'/'+tim[2]+' '+tim[3]
  return,time
end

;*************************************************************************************
pro LC_sld,hsld,htxt,hvolt,fa
  widget_control,hsld,get_value=value,get_uvalue=uvalue
  widget_control,htxt,set_value=string(value,format='(F5.1)')
  widget_control,hsld,get_uvalue=uv
  bi=fix(strmid(uv,3,1))-1
  b1=fa.b[bi]
  lc_ch=str_sep(b1.lc_ch,'-')
  usb='USB'+strcompress(lc_ch[0],/remove_all)
  ch=lc_ch[1]
  ret=value/360.
  b1.temp=get_therm_temp(ch=b1.temp_ch,time=time,nsample=100)
  v=get_lc_volt_fit(fa.wl0, b1.ck, ret, b1.temp, b1.voff)
  lcvolt,ch,v,USBx=usb
  fa.b[bi].ret_offset=ret
  fa.b[bi].volt=v
  fa.b[bi].temp=b1.temp
  widget_control,hvolt,set_value=string(v[0],form='(f5.3)')
end

;*************************************************************************************
pro LC_txt,hsld,htxt,hvolt,fa
  widget_control,htxt,get_value=value,get_uvalue=uvalue
  widget_control,hsld,set_value=fix(value)
  widget_control,htxt,get_uvalue=uv
  bi=fix(strmid(uv,3,1))-1
  b1=fa.b[bi]
  lc_ch=str_sep(b1.lc_ch,'-')
  usb='USB'+strcompress(lc_ch[0],/remove_all)
  ch=lc_ch[1]
  ret=value/360.
  b1.temp=get_therm_temp(ch=b1.temp_ch,time=time)
  v=get_lc_volt_fit(fa.wl0, b1.ck, ret, b1.temp, b1.voff)
  lcvolt,ch,v,USBx=usb
  fa.b[bi].ret_offset=ret
  fa.b[bi].volt=v
  fa.b[bi].temp=b1.temp
  widget_control,hvolt,set_value=string(v[0],form='(f5.3)')
end


;******************************************

pro LC_scan,hsld,htxt,hvolt,hscan
  common utf_adj,wd,fa

  step=10
  nstep=360/step+1
  ret=findgen(nstep)*step/360.

  widget_control,hscan,get_uvalue=uv
  bi=fix(strmid(uv,4,1))-1
  b1=fa.b[bi]
  lc_ch=str_sep(b1.lc_ch,'-')
  usb='USB'+strcompress(lc_ch[0],/remove_all)
  ch=lc_ch[1]
  for jjj=0,nstep-1 do begin
    widget_control,htxt,set_value=string(ret[jjj]*360,format='(F5.1)')
    widget_control,hsld,set_value=fix(ret[jjj]*360)
    b1.temp=get_therm_temp(ch=b1.temp_ch,time=time)
    v=get_lc_volt_fit(fa.wl0, b1.ck, ret[jjj], b1.temp, b1.voff)
    lcvolt,ch,v,USBx=usb
    wait,0.1
    widget_control,hvolt,set_value=string(v[0],form='(f5.3)')
  endfor

  stop
  ;not finished

end


;******************************************
function wd_create,base
  common utf_adj,wd,fa

  nb=n_elements(fa.b)
  hs=lonarr(nb)
  htx=lonarr(nb)
  hv=lonarr(nb)
  hsc=lonarr(nb)

  wd={htemp:0L,$
    ttemp:0L,$
    hload:0L,$
    hload_list:0L,$
    hsave:0L,$
    hsld:hs,$
    htxt:htx,$
    hvolt:hv,$
    hscan:hsc,$
    usld:'sld'+string(indgen(nb)+1,form='(i1)'),$
    utxt:'txt'+string(indgen(nb)+1,form='(i1)'),$
    vtxt:'volt'+string(indgen(nb)+1,form='(i1)'),$
    usca:'scan'+string(indgen(nb)+1,form='(i1)'),$
    hwlsh:0L,$
    done:0L}

  ww=500

  base_ctl=widget_base(base,/column,xsize=ww+10)

  ;----- wavelength and temp-----
  tbase=widget_base(base_ctl,/row,/align_center)
  wd.htemp=widget_button(tbase,value="GET temp",uvalue='temp',font='Arial')
  temp_ch=fa.b.temp_ch
  tch=temp_ch[uniq(temp_ch,sort(temp_ch))]
  tmp=get_therm_temp(ch=tch,time=time)
  ctemp=strjoin(string(tmp,form='(f6.2)'),',')
  wd.ttemp = widget_text(tbase,value=ctemp,uvalue='temp',$
    xsize=12, /edit,font='Arial')
  label = widget_label(tbase, value='C', font='Arial', /align_center)

  ;----- LC ctrl -----
  sldv1=0.
  volt1=0.
  base_LC=widget_base(base_ctl,/column ,/align_center,/frame,xsize=ww)
  label = widget_label(base_LC, value='LC ctl', font='Arial*28', /align_center)

  base_t=widget_base(base_LC,/row ,/align_center)
  dmy=widget_label(base_t,value='Offset Table',font='Arial')
  wd.hload=widget_button(base_t,value="Load",uvalue='load',font='Arial')
  ; wd.hload_list=widget_droplist(base_t, value="Load offset table",uvalue='load',font='Arial',xsize=350)

  wd.hsave=widget_button(base_t,value="Save",uvalue='stbl',font='Arial')
  base_t2=widget_base(base_LC,/row ,/align_center)
  wd.hload_list=widget_label(base_t2,value=fa.offset_info,font='Arial')

  base_label=widget_base(base_LC,/row)
  label = widget_label(base_label, value='                    ', font='Arial')
  label = widget_label(base_label, value='nm', font='Arial')
  label = widget_label(base_label, value='                              ', font='Arial')
  label = widget_label(base_label, value='deg', font='Arial')
  label = widget_label(base_label, value='                              ', font='Arial')
  label = widget_label(base_label, value='V', font='Arial')

  fa.b.ret-=(fa.b.ret gt 1.)

  for i=0,nb-1 do begin
    base1=widget_base(base_LC,/row ,/align_center)
    aa=string(fa.b[i].fsr,form='(f5.3)')
    label = widget_label(base1, value=string(i+1,form='(i1)')+'('+aa+')', font='Arial')
    wd.hsld[i] = widget_slider(base1, value=fa.b[i].ret*360, uvalue=wd.usld[i], minimum=0, maximum=360, xsize=150, suppress=1, frame=7, /drag)
    wd.htxt[i] = widget_text(base1,value=string(fa.b[i].ret*360,form='(f5.1)'),uvalue=wd.utxt[i],xsize=7, /edit,font='Arial')
    wd.hvolt[i] = widget_text(base1,value=string(fa.b[i].volt,form='(f5.3)'),uvalue=wd.vtxt[i],xsize=7,font='Arial')
    wd.hscan[i]=widget_button(base1,value="scan",uvalue=wd.usca[i],font='Arial')
  endfor

  ran=[-1.,1.]
  step=0.005
  n=(ran[1]-ran[0])/step+1
  i=(fa.dwl-ran[0])/step
  wd.hwlsh = widget_slider(base_ctl, value=i, uvalue=0, minimum=0, maximum=n, suppress=1, frame=7, /drag)

  ;----- Done -----
  wd.done=widget_button(base_ctl,value="Done",uvalue='Done',font='Arial')

  return,wd

end

;*************************************************************************************
pro utf_adj_event,ev
  common utf_adj,wd,fa

  case ev.id of

    ;----- TEMP -----
    wd.htemp: begin
      temp_ch=fa.b.temp_ch
      tch=temp_ch[uniq(temp_ch,sort(temp_ch))]
      tmp=get_therm_temp(ch=tch,time=time)
      widget_control,wd.ttemp,set_value=strjoin(string(tmp,form='(f6.2)'),',')
    end

    ;----- load table -----
    wd.hload:begin
      st=rdcsv(fa.tbldir+'\'+fa.LCoffset_tbl,skip=0)
      hed=st[*,0]
      it=where(strpos(hed,'save time') ne -1)
      st=st[*,1:*]
      nb=n_elements(fa.b)
      ll=(where(string(transpose(st[0,*]),form='(f5.1)') eq string(fa.wl0,form='(f5.1)')))
      menu=strjoin(st[[0,nb+1,it[0]],ll,0],', ')
      st=st[*,ll]
      l=smenu(menu,title='Offset Tables',pos=[500,500])
      Print, 'Current Selection: ', menu[l]
      st1=st[*,l]
      fa.b.ret_offset=float(st1[1:nb])/360.
      temp0s=float(st1[nb+1:(2*nb)<(it[0]-1)])
      if n_elements(temp0s) eq 1 then temp0s=temp0s[0]
      fa.b.temp0=temp0s
      widget_control,wd.hload_list,set_value=menu[l]
      for i=0,nb-1 do begin
        b1=fa.b[i]
        b1.temp=get_therm_temp(ch=b1.temp_ch)
        widget_control,wd.hsld[i],set_value=string(b1.ret_offset*360.,form='(f5.1)')
        widget_control,wd.htxt[i],set_value=string(b1.ret_offset*360.,form='(f5.1)')
        ret1=b1.ret_offset
        b1.ret=ret1
        b1.volt=get_lc_volt_fit(fa.wl0, b1.ck, ret1, b1.temp, b1.voff)
        lc_ch=str_sep(b1.lc_ch,'-')
        lcvolt,lc_ch[1],b1.volt,USBx='USB'+strcompress(lc_ch[0],/remove_all)
        fa.b[i]=b1
        widget_control,wd.hvolt[i],set_value=string(b1.volt,form='(f5.3)')
      endfor
      wait,0.1
      widget_control,wd.hload_list,set_value=menu[l]
    end

    ;----- save table -----
    wd.hsave:begin
      tfile=fa.tbldir+'\'+fa.LCoffset_tbl

      nb=n_elements(fa.b)
      for i=0,nb-1 do begin
        widget_control,wd.htxt[i],get_value=uv
        fa.b[i].ret=uv/360.
        print,uv
      endfor

      fa.b.ret-=(fa.b.ret gt 1.)

      openw,1,tfile,/append
      tbl=string(fa.wl0,form='(f8.2)')+', '$
        +strjoin(string((fa.b.ret*360),form='(f7.2)'),', ')+', '
      if fa.filt_name eq 'UTF32' then $
        tbl=tbl+string((rebin(fa.b.temp,1))[0],form='(f10.3)')+', '+get_tim() $
      else $
        tbl=tbl+strjoin(string((fa.b.temp),form='(f7.3)'),', ')+', '+get_tim()
      printf,1,tbl
      close,1
      print,'saved '+tfile+' as '+tbl
    end
    
    ;----- wl shift -----
    wd.hwlsh: begin
      j=ev.value
      ran=[-1.,1.]
      step=0.005
      fa.dwl=ran[0]+step*j
      nb=n_elements(fa.b)

      utf_set_wl,fa

      fa.b.ret-=(fa.b.ret gt 1.)

      print,fa.dwl,fa.b.ret

      for ii=0,nb-1 do begin
        widget_control,wd.htxt[ii],set_value=string(fa.b[ii].ret*360,format='(F5.1)')
        widget_control,wd.hsld[ii],set_value=fix(fa.b[ii].ret*360)
        widget_control,wd.hvolt[ii],set_value=string(fa.b[ii].volt,form='(f5.3)')
      endfor
    end

    ;----- done -----
    wd.done: begin
      widget_control,/destroy,ev.top
      f=fa
      save,f,file=f.tbldir+'\f.sav'
      set_lc_offset,f
    end
    else:
  endcase

  ;----- sld and txt-----
  for j=0,7-1 do begin
    if ev.id eq wd.hsld[j] then LC_sld,wd.hsld[j],wd.htxt[j],wd.hvolt[j],fa
    if ev.id eq wd.htxt[j] then LC_txt,wd.hsld[j],wd.htxt[j],wd.hvolt[j],fa
    if ev.id eq wd.hscan[j] then LC_scan,wd.hsld[j],wd.htxt[j],wd.hvolt[j],wd.hscan[j]
  endfor
end

;****************************************** main
pro utf_adj,f,wd=wdr
  common utf_adj,wd,fa

  fa=f
  nodev=0

  base=widget_base(title='utf_adj',/row)
  wd=wd_create(base)

  wdr=wd

  widget_control,base,/realize

  xmanager,'utf_adj',base

end

;******************************************
function utf_autoadj_sub,b,mm,dra,dd,fint=fint,fft=fft,volt0=volt0,abort=abort
  common utf_autoadj,fa,pa,dbina

  b1=b
  dbin=dbina

  print,float(b1.FSR)

  nd=round((dra[1]-dra[0])/dd)
  dret=interpol(dra-[0,dd],nd)/360.
  volt=fltarr(nd)
  int=fltarr(nd)
  std=fltarr(nd)

  b1.temp=get_therm_temp(ch=b1.temp_ch,noDev=noDev)
  lc_ch=str_sep(b1.lc_ch,'-')

  device,window_state=ws
  if (abort=ws[2] eq 0) then return,-1.
  wset,2
  plot,dret*360,int,/nodata,/yno,title=string(b1.FSR,form='(f5.3)')+' nm block', $
    xtit='retardartion offset [degree]',ytit='intensity [DN]',xs=3,xra=dra

  ;set capturemode sequence
  orca_capturemode_sequence,nimg=1

  ;continuous capturing start
  orca_capture

  for i=0,nd-1 do begin
    b1.ret=b1.ret_offset+dret[i]
    b1.volt=get_lc_volt_fit(fa.wl0,b1.ck,b1.ret,b1.temp,b1.voff)
    if not keyword_set(nodev) then lcvolt,lc_ch[1],b1.volt,USBx='USB'+strcompress(lc_ch[0],/remove_all)
    wait,0.1
    img1=orca_getimg(nimg=1)
    wset,0
    tvscl,rebin(img1,pa.Width/dbin,pa.Height/dbin)>0,pa.regionx/dbin,pa.regiony/dbin

    device,window_state=ws
    if (abort=ws[2] eq 0) then break
    wset,2

    volt[i]=b1.volt
    int[i]=mean(img1)
    std[i]=stddev(img1)

    device,window_state=ws
    if (abort=ws[2] eq 0) then break

    if int[i] ge !y.crange[0] and int[i] le !y.crange[1] then plots,dret[0:i]*360,int[0:i],psy=1 else $
      plot,dret[0:i]*360,int[0:i],/yno,title=string(b1.FSR,form='(f5.3)')+' nm block', $
      xtit='retardartion offset [degree]',ytit='intensity [DN]',psy=1,xs=3,xra=dra
  endfor

  ;capture stop
  orca_idle

  ;reset to original voltage
  b1.ret=b1.ret_offset
  b1.volt=get_lc_volt_fit(fa.wl0,b1.ck,b1.ret,b1.temp,b1.voff)
  if not keyword_set(nodev) then lcvolt,lc_ch[1],b1.volt,USBx='USB'+strcompress(lc_ch[0],/remove_all)

  device,window_state=ws
  if (abort=ws[2] eq 0) then return,-1.

  plot,dret*360,int,/yno,title=string(b1.FSR,form='(f5.3)')+' nm block', $
    xtit='retardartion offset [degree]',ytit='intensity [DN]',psy=1,xs=3,xra=dra,ys=8

  case keyword_set(fft) of
    0:begin
      ;peak/abiss detection
      aa=poly_fit(dret,int,2,yfit=yfit)

      device,window_state=ws
      if (abort=ws[2] eq 0) then return,-1.

      oplot,dret*360,yfit
      fret=-aa[1]/(2*aa[2])
      fint=aa[0]-aa[1]^2/(4*aa[2])
      plots,fret*360,fint,psy=2
    end
    1:begin
      ;FFT transform
      ff=fft(int)
      i0=abs(ff[0])
      p1=abs(ff[1])
      fret=(-atan(ff[1],/phase)/(2*!pi)-dra[0]/360.) mod 1 ;retardation offset value at final condition

      device,window_state=ws
      if (abort=ws[2] eq 0) then return,-1.

      oplot,dret*360,2*p1*cos((dret-fret)*2*!pi)+i0
      fret=(fret+0.5*(~mm)) mod 1
      fint=i0+([-2,+2])[mm]*p1  ;intensity at final condition
    end
  endcase

  device,window_state=ws
  if (abort=ws[2] eq 0) then return,-1.

  oplot,((fret*360*[1,1]+180) mod 360)-180,!y.crange
  xyouts,!x.crange[0],!y.crange[1],'!C dret= '+strtrim(fret,2),/data

  axis,/yaxis,/save,yra=minmax(volt),ytit='Volt'
  oplot,dret*360,volt
  volt0=interpol(volt,dret,((fret+0.5) mod 1)-0.5)
  oplot,!x.crange,volt0*[1,1]
  xyouts,!x.crange[1],!y.crange[1],'!C Volt= '+strtrim(volt0,2),/data,align=1.
  wset,0

  return,fret

end

;******************************************
function utf_autoadj_sub2,b,mm,wra,dw,fint=fint,fitw=fitw,abort=abort

  common utf_autoadj,fa,pa,dbina

  if not isvalid(fitw) then fitw=2*dw
  nfit=round(fitw/dw)+1

  b1=b  ;7 elements
  dbin=dbina

  nw=round((wra[1]-wra[0])/dw)/2*2
  woff=interpol(wra-[0,dw],nw)
  int=fltarr(nw)

  b1.temp=get_therm_temp(ch=b1.temp_ch,noDev=noDev)
  nb=n_elements(b1)
  lc_ch=strarr(2,nb)
  for i=0,nb-1 do lc_ch[*,i]=str_sep(b1[i].lc_ch,'-')

  device,window_state=ws
  if (abort=ws[2] eq 0) then return,-1.
  wset,2
  plot,woff,int,/nodata,/yno,title='Simultaneous tuning', $
    xtit='wavelength shift [nm]',ytit='intensity [DN]',xra=wra

  ;set capturemode sequence
  orca_capturemode_sequence,nimg=1

  ;continuous capturing start
  orca_capture

  for i=0,nw-1 do begin
    dret_w=(woff[i]/2)/b1.fsr
    dret=b1.sign*dret_w
    b1.ret=b1.ret_offset+dret
    for ib=0,nb-1 do begin
      b1[ib].volt=get_lc_volt_fit(fa.wl0,b1[ib].ck,b1[ib].ret,b1[ib].temp,b1[ib].voff)
      if not keyword_set(nodev) then lcvolt,lc_ch[1,ib],b1[ib].volt,USBx='USB'+strcompress(lc_ch[0,ib],/remove_all)
    endfor

    wait,0.1
    img1=orca_getimg(nimg=1)
    wset,0
    tvscl,rebin(img1,pa.Width/dbin,pa.Height/dbin)>0,pa.regionx/dbin,pa.regiony/dbin

    device,window_state=ws
    if (abort=ws[2] eq 0) then break
    wset,2

    int[i]=mean(img1)

    device,window_state=ws
    if (abort=ws[2] eq 0) then return,-1.

    if int[i] ge !y.crange[0] and int[i] le !y.crange[1] then plots,woff[0:i],int[0:i],psy=1 else $
      plot,woff[0:i],int[0:i],/yno,title='Simultaneous tuning', $
      xtit='wavelength shift [nm]',ytit='intensity [DN]',psy=1,xra=wra
    print,i,woff[i],int[i]

  endfor

  ;capture stop
  orca_idle

  ;reset to original voltage
  b1.ret=b1.ret_offset
  for ib=0,nb-1 do begin
    b1[ib].volt=get_lc_volt_fit(fa.wl0,b1[ib].ck,b1[ib].ret,b1[ib].temp,b1[ib].voff)
    if not keyword_set(nodev) then lcvolt,lc_ch[1,ib],b1[ib].volt,USBx='USB'+strcompress(lc_ch[0,ib],/remove_all)
  endfor

  device,window_state=ws
  if (abort=ws[2] eq 0) then return,-1.

  ;peak/abiss detection
  mint=mm?max(int,wm):min(int,wm)
  x=woff[wm-nfit/2>0:wm+nfit/2<(nw-1)]
  y=int[wm-nfit/2>0:wm+nfit/2<(nw-1)]
  a=poly_fit(x,y,2,yfit=yfit)
  p=-a[1]/(2*a[2])
  q=a[0]-a[1]^2/(4*a[2])

  fint=q

  device,window_state=ws
  if (abort=ws[2] eq 0) then return,-1.

  plots,x,yfit
  plots,p,fint,psy=2
  plots,[p,p],!y.crange

  xyouts,!x.crange[0],!y.crange[1],'!C dret= '+strtrim(p,2),/data
  wset,0

  return,p

end

;****************************************** main
pro utf_autoadj,f,p,dbin,ev
  common utf_autoadj,fa,pa,dbina

  mode=dialog_list(title='Select searching mode',['absorption line','emission line'])
  if mode eq '' then return

  fa=f
  dbina=dbin

  ;select roi
  messagebox,'Please select the intensity averaging area.'
  box_cur1,x0, y0, nx, ny
  x0=x0/8*8
  y0=y0/8*8
  nx=nx/8*8
  ny=ny/8*8

  pa=OrcaSetParam(width=nx*dbina,height=ny*dbina,regionx=x0*dbina,regiony=y0*dbina)

  b=fa.b
  fwhm=b.fsr

  case mode of
    'absorption line':begin
      case fa.FILT_NAME of
        'UTF32':mmlist=[0,0,0,0,0,0,0]  ;0->min, 1->max [16,8,4,2,1,0.5,0.25]
        'TF40' :mmlist=[1,0,0,0,0,0,0]  ;0->min, 1->max [16,8,4,2,1,0.5,0.25]
      endcase
      order=reverse(sort(fwhm))  ;large to small
    end
    'emission line':begin
      mmlist=[1,1,1,1,1,1,1]  ;0->min, 1->max [16,8,4,2,1,0.5,0.25]
      order=sort(fwhm)  ;small to large
    end
  endcase

  nb=n_elements(b)

  int=fltarr(2*nb+1)
  volt=fltarr(nb)

  window,2
  wset,0

  ;1st test
  dra=[-180.,+180.] ;[deg]
  dd=30.  ;[deg]

  for i=0,nb-1 do begin
    ib=order[i]
    b1=b[ib]
    dret=utf_autoadj_sub(b1,mmlist[ib],dra,dd,fint=fint,/fft,volt0=volt0,abort=abort)

    if keyword_set(abort) then goto,abort

    int[i]=fint
    volt[i]=volt0

    b1.temp=get_therm_temp(ch=b1.temp_ch,noDev=noDev)

    lc_ch=str_sep(b1.lc_ch,'-')

    b1.ret_offset+=dret
    b1.ret_offset mod=1
    b1.ret=b1.ret_offset
    b1.volt=get_lc_volt_fit(fa.wl0,b1.ck,b1.ret,b1.temp,b1.voff)

    if not keyword_set(nodev) then lcvolt,lc_ch[1],b1.volt,USBx='USB'+strcompress(lc_ch[0],/remove_all)

    b[ib]=b1  ;b revise
  endfor

  ;2nd test
  wra=[-0.2,+0.2] ;[nm]
  dw=0.01 ;[nm]
  mm=mode eq 'emission line'

  woff=utf_autoadj_sub2(b,mm,wra,dw,fint=fint,fitw=0.1,abort=abort)

  if keyword_set(abort) then goto,abort

  int[nb]=fint

  dret_w=(woff/2)/b.fsr mod 1
  dret=(b.sign*dret_w+1) mod 1

  ;b revise
  b.ret_offset+=dret
  b.ret_offset mod=1
  b.ret=b.ret_offset

  b.temp=get_therm_temp(ch=b.temp_ch,noDev=noDev)

  ;set to line center
  for ib=0,nb-1 do begin
    b[ib].volt=get_lc_volt_fit(fa.wl0,b[ib].ck,b[ib].ret,b[ib].temp,b[ib].voff)
    lc_ch=str_sep(b[ib].lc_ch,'-')
    if not keyword_set(nodev) then lcvolt,lc_ch[1],b[ib].volt,USBx='USB'+strcompress(lc_ch[0],/remove_all)
  endfor

  ;3rd test (fine tuning)
  dra=[-180.,+180.] ;[deg]
  dd=10.  ;[deg]

  fwmax=max(fwhm[order],imax)
  for i=0,nb-1 do begin ;except 16A block
    if i eq imax then continue
    ib=order[i]
    b1=b[ib]
    dret=utf_autoadj_sub(b1,mmlist[ib],dra,dd,fint=fint,/fft,volt0=volt0,abort=abort)

    if keyword_set(abort) then goto,abort

    int[i]=fint
    volt[i]=volt0

    b1.temp=get_therm_temp(ch=b1.temp_ch,noDev=noDev)

    lc_ch=str_sep(b1.lc_ch,'-')

    b1.ret_offset+=dret
    b1.ret_offset mod=1
    b1.ret=b1.ret_offset
    b1.volt=get_lc_volt_fit(fa.wl0,b1.ck,b1.ret,b1.temp,b1.voff)

    if not keyword_set(nodev) then lcvolt,lc_ch[1],b1.volt,USBx='USB'+strcompress(lc_ch[0],/remove_all)

    b[ib]=b1  ;b revise
  endfor

  ;4th test
  if mode eq 'absorption line' then begin  ;-0.4nm shift for absorption line
    woff=-0.4 ;[nm]
    dret_w=(woff/2)/b.fsr mod 1
    drets=(b.sign*dret_w+1) mod 1

    ;b revise
    b.ret+=drets
    b.ret mod=1

    b.temp=get_therm_temp(ch=b.temp_ch,noDev=noDev)

    for ib=0,nb-1 do begin
      b[ib].volt=get_lc_volt_fit(fa.wl0,b[ib].ck,b[ib].ret,b[ib].temp,b[ib].voff)
      lc_ch=str_sep(b[ib].lc_ch,'-')
      if not keyword_set(nodev) then lcvolt,lc_ch[1],b[ib].volt,USBx='USB'+strcompress(lc_ch[0],/remove_all)
    endfor
  endif else drets=fltarr(nb)

  ;for 16A block
  i=imax
  ib=order[i]
  b1=b[ib]
  b1.ret_offset=b1.ret
  dret=utf_autoadj_sub(b1,1,dra,dd,fint=fint,/fft,volt0=volt0,abort=abort)

  if keyword_set(abort) then goto,abort

  b1.ret_offset+=dret-drets[ib]+1
  b1.ret_offset mod=1
  b[ib]=b1  ;b revise fo 16A block

  b.ret=b.ret_offset

  wdelete,2

  ;fa revise
  fa.b=b
  f=fa

  abort:

  if max(f.b.ret lt 0) then stop

  f.b.temp=get_therm_temp(ch=f.b.temp_ch,noDev=noDev)

  for ib=0,nb-1 do begin
    f.b[ib].volt=get_lc_volt_fit(f.wl0,f.b[ib].ck,f.b[ib].ret,f.b[ib].temp,f.b[ib].voff)
    lc_ch=str_sep(f.b[ib].lc_ch,'-')
    if not keyword_set(nodev) then lcvolt,lc_ch[1],f.b[ib].volt,USBx='USB'+strcompress(lc_ch[0],/remove_all)
  endfor

  ;restore original setting
  p=OrcaSetParam(bin=p.bin,width=p.width,height=p.height,regionx=p.regionx,regiony=p.regiony)

end

;**************************************************************
pro savefits_pf,imgs,p,f,file=file

common bridge,bridge

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

  fh[20]='FILTER  = '+f.filt_name
  fh[21]=string('WAVE0   = ',f.wl0,format='(a10,f20.2," / nm")')
  fh[22]='LCOFFSET= '+f.Offset_info
  fh[23]=string('DWL     = ',f.dwl,format='(a10,f20.4," / nm")')
  fh[24]='TFMODE  = '+f.mode
  nb=n_elements(f.b)
  ctemp=string(f.b[0].temp,form='(f6.2)')
  for i=1,nb-1 do ctemp=ctemp+','+string(f.b[i].temp,form='(f6.2)')
  fh[25]='TEMP    = '+ctemp
  cret=string(f.b[0].ret*360,form='(i4)')
  for i=1,nb-1 do cret=cret+','+string(f.b[i].ret*360,form='(i4)')
  fh[26]='LCRET   = '+cret
  cvolt=string(f.b[0].volt,form='(f6.3)')
  for i=1,nb-1 do cvolt=cvolt+','+string(f.b[i].volt,form='(f6.3)')
  fh[27]='LCVOLT  = '+cvolt
  fh[28]='COMMENT = none'
  fh[29]='HISTORY = RAW'
  ;  fh[30]=string('BZERO   = ',si eq 12?2^15:0,format='(a10,i20," /")')
  fh[30]='BSCALE  =  1'
  fh[31:34]=string(' ',format='(a10)')
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

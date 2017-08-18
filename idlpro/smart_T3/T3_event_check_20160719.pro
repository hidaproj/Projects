;To make sav file, do below
;.FULL_RESET_SESSION
;CD,'C:\Projects\IDLPRO\smart_T3'
;.COMPILE T3_event_check
;RESOLVE_ALL,skip=['draw']
;SAVE, /ROUTINES, FILENAME='T3_event_check.sav'

; T3 event check
function version
  ver='0.1'  ;  2012.02.06 TTI based on T3_select
  ver='1.0'  ;  2016.07.06 K. Otsuji widgetaize
             ;             modify fname2msec, nn_file_log intarr->lonarr
  ver='1.1'  ;  2016.07.11 K. Otsuji fixed minor bug 
  ver='1.2'  ;  2016.07.19 K. Otsuji modify xmovie, value_locate error
  return,ver
end
;--------------------------------------------------------------------
function fname2msec,fname,ip

  tim=strmid(fname,ip+[0,2,4,7],[2,2,2,3])##transpose([3600000l,60000l,1000l,1l])
  ;tim=(strmid(fname,ip,2)*3600l+strmid(fname,ip+2,2)*60l $
  ;  +strmid(fname,ip+4,2))*1000l+strmid(fname,ip+7,3)
  return,tim

end

pro T3_event_check_event,ev

  widget_control,ev.top,get_uvalue=uval0

  case ev.id of
    uval0.dtdir:begin
      widget_control,uval0.dtdir,get_value=datedir & datedir=datedir[0]
      if ~file_test(datedir,/dir) then begin
        ans=dialog_message(I18N_UTF8TOMULTIBYTE('日付フォルダが存在しません。'), $
          title='T3 Event Check',/error,dialog_parent=ev.top)
        widget_control,uval0.startb,sensitive=0
      endif else begin
        if datedir ne uval0.datedir then begin
          day0=strmid(file_basename(datedir),0,8)
          uval0.day=day0
          uval0.datedir=datedir
          widget_control,ev.top,set_uvalue=uval0
        endif
        widget_control,uval0.startb,sensitive=1
      endelse
    end
    uval0.sldir:begin
      widget_control,uval0.sldir,get_value=seldir & seldir=seldir[0]
      uval0.seldir=seldir
      widget_control,ev.top,set_uvalue=uval0
    end
    uval0.canclb:begin
      WIDGET_CONTROL, ev.TOP, /DESTROY
      return
    end
    else:stop
  endcase

end


;--------------------------------------------------------------------
pro T3_event_check

  ver=version()

  ;dir='E:\T3\'
  ;dir='F:\T3\'

  ; selected/eventsの保存先
  ; 外付けHDDの電源を入れるのを忘れずに！
  dirout='D:\T3\'  ;default

  ; 観測日
  ;day='20160705'
  caldat,systime(0,/JULIAN),mm,dd,yyyy
  day=string([yyyy,mm,dd],form='(i4.4,2i2.2)')

  ; 観測データ（生データ）の保存先
  datedir=file_search('{E,F}:\T3\'+day,count=nday)
  if nday eq 0 then begin  ;before yesterday?
    ans=dialog_message(I18N_UTF8TOMULTIBYTE('本日('+day+')のフォルダがE:\T3またはF:\T3内に見つかりません。他の日の処理を行いますか？'),/question,title='T3 Event Check')
    if ans eq 'No' then return

    datelist=file_search('{E,F}:\T3\20??????*',count=nday)
    if nday eq 0 then begin
      ans=dialog_message(I18N_UTF8TOMULTIBYTE('日付フォルダがE:\T3またはF:\T3内に見つかりません。'),title='T3 Event Check')
      return
    endif

    datedir=dialog_list(datelist,title=I18N_UTF8TOMULTIBYTE('処理する日付フォルダを選択してください。'))
    if datedir eq '' then return

    day=strmid(file_basename(datedir),0,8)
  endif
  seldir=dirout+'selected\'+day

  ;widget

  base=widget_base(/COLUMN,title='T3 Event Check '+ver)
  tbase=widget_base(base,/column,/base_align_right)
  dtdir=cw_field(tbase,value=datedir,/string,title=I18N_UTF8TOMULTIBYTE('日付フォルダ: '),xsize=40,/return_event)
  sldir=cw_field(tbase,value=seldir,/string,title=I18N_UTF8TOMULTIBYTE('保存フォルダ: '),xsize=40,/return_event)
  textw=cw_field(tbase,/string,title='STATUS: ',xsize=40,ysize=4,/noedit)

  bbase=widget_base(base,/row,/align_right)
  startb=widget_button(bbase,value='start',event_pro='T3_event_check_process')
  canclb=widget_button(bbase,value='cancel')

  uval={base:base,tbase:tbase,dtdir:dtdir,sldir:sldir,textw:textw,bbase:bbase,startb:startb,canclb:canclb, $
    day:day,datedir:datedir,seldir:seldir}
  widget_control,base,set_uvalue=uval

  WIDGET_CONTROL, base, /REALIZE

  XMANAGER, 'T3_event_check', base

end

pro T3_event_check_process,ev

  device,decomp=0

  widget_control,ev.top,get_uvalue=uval0

  widget_control,uval0.dtdir,get_value=datedir & datedir=datedir[0]
  widget_control,uval0.sldir,get_value=seldir & seldir=seldir[0]
  if (day=strmid(file_basename(datedir),0,8)) ne (sldate=strmid(file_basename(seldir),0,8)) then begin
    ans=dialog_message(I18N_UTF8TOMULTIBYTE('日付フォルダと保存フォルダの日付が異なります('+day+' and '+sldate+')。実行してよろしいですか？'), $
      title='T3 Event Check',/question,dialog_parent=ev.top)
    if ans eq 'No' then return
  endif

  ;execute start
  widget_control,uval0.dtdir,sensitive=0
  widget_control,uval0.sldir,sensitive=0
  widget_control,uval0.startb,sensitive=0
  widget_control,uval0.canclb,set_value='abort'
  
  nn_file_log=lonarr(2,3)

  ;filename setting
  original_filename=seldir+'\'+day+'_original_files.sav'
  ql_movie_filename=seldir+'\'+day+'_Ha_ql.sav'
  pair_all_filename=seldir+'\'+day+'_pair_files_all.sav'
  nn_log_filename=seldir+'\'+day+'_files_log.sav'
  ;delta_t_c_filename=seldir+'\'+day+'_delta_t_c.sav'
  plot_filename=seldir+'\'+day+'_T3_plot.gif'

  if widget_info(uval0.canclb,/button_set) then goto,abort

  cd,datedir
  
  text='Loaded Co files: Counting....'
  widget_control,uval0.textw,set_value=text
  ;cfi=file_search('*\*\Co_*',count=nc)
  spawn,'dir /A-D /B /S Co_*.tif',cfi & nc=n_elements(cfi)
  if widget_info(uval0.canclb,/button_set) then goto,abort
  text='Loaded Co files: '+string(nc)+' files'
  widget_control,uval0.textw,set_value=text
  text=[text,'Loaded Ha files: Counting....']
  widget_control,uval0.textw,set_value=text
  ;hfi=file_search('*\*\Ha_*',count=nh)
  spawn,'dir /A-D /B /S Ha_*.tif',hfi & nh=n_elements(hfi)  
  if widget_info(uval0.canclb,/button_set) then goto,abort
  text=[text[0:-2],'Loaded Ha files: '+string(nh)+' files']
  widget_control,uval0.textw,set_value=text

  nn_file_log[0,0]=nc
  nn_file_log[1,0]=nh

  file_mkdir,seldir
  ;spawn,'mkdir '+seldir
  save,cfi,hfi,file=original_filename
  if widget_info(uval0.canclb,/button_set) then goto,abort

  ;-----  select simultaneous images ------
  ip=strpos(cfi[0],'.tif',/reverse_search)-10
  ctim=fname2msec(cfi,ip)
  htim=fname2msec(hfi,ip)
  
  start_time='' ;hhmmdd.mmm
  if keyword_set(start_time) then begin
    stim=fname2msec(start_time,0)
    ii=where(ctim gt stim,nc)
    ctim=ctim[ii]
    cfi=cfi[ii]
    ii=where(htim gt stim,nh)
    htim=htim[ii]
    hfi=hfi[ii]
  endif
    
  ;Pairing Ha to Co (dt < 30 msec)
  vl=value_locate(htim,ctim)
  dt1=ctim-htim[vl>0]
  dt2=ctim-htim[vl+1<(nh-1)]
  dt0=min([dt1,dt2],dim=1,/abs,ww)
  ii=where(abs(dt0) lt 30,nc2)

  cfi2=cfi[ii]
  hfi2=hfi[([vl>0,vl+1<(nh-1)])[ww[ii]]]

  ;hfi2=strarr(nc)
  ;for i=0l,nc-1 do begin
  ;  dti=abs(ctim[i]-htim)
  ;  dmy=min(dti,i0)
  ;  if dti[i0] lt 30 then hfi2[i]=hfi[i0]
  ;endfor

  ;ii=where(hfi2 ne '')
  ;if ii[0] ne -1 then begin
  ;  cfi2=cfi[ii]
  ;  hfi2=hfi2[ii]
  ;endif else begin
  ;  cfi2=cfi
  ;endelse

  ctim2=fname2msec(cfi2,ip)
  htim2=fname2msec(hfi2,ip)
  ;plot,ctim2-htim2
  ;ans=''
  ;read,'Ok? (y/n)',ans
  ;if ans eq 'n' then stop

  text=[text,string(nc2)+' pairs (dt<30msec) counted']
  widget_control,uval0.textw,set_value=text
  
  nn_file_log[0,1]=nc2
  nn_file_log[1,1]=nc2

  save,file=pair_all_filename,cfi2,hfi2,ctim2,htim2
  if widget_info(uval0.canclb,/button_set) then goto,abort

  ii=transpose(where(abs(htim2-ctim2) lt 6,nn))
  cfi3=cfi2[ii]
  hfi3=hfi2[ii]
  ctim3=ctim2[ii]
  htim3=htim2[ii]

  nn_file_log[0,2]=nn
  nn_file_log[1,2]=nn
  
  text=[text,string(nn)+' pairs (dt< 6msec) counted']
  widget_control,uval0.textw,set_value=text

  c_delta_t=[(ctim3[0,1:*]+ctim3)/2,ctim3[0,1:*]-ctim3]

  ;c_delta_t=fltarr(2,nn-1)
  ;for i=0d,nn-2 do begin
  ;  c_delta_t[0,i]=(ctim3[i+1]+ctim3[i])/2.
  ;  c_delta_t[1,i]=ctim3[i+1]-ctim3[i]
  ;endfor

  ;delta_t_c2=fltarr(4)
  ;delta_t_c2[0]=median(c_delta_t)
  ;delta_t_c2[1]=max(c_delta_t,index)
  ;delta_t_c2[2]=ctim3[index]
  ;delta_t_c2[3]=ctim3[index+1]
  ;
  ;print,'mean frame rate:',1000./delta_t_c2[0]
  ;print,'delta_t max:',delta_t_c2[1],'msec'

  set_plot,'z'
  device,set_resolution=[1200,900]
  !p.color=0
  !p.background=255
  !p.multi=[0,1,2]
  plot,ctim2/1000./3600.,ctim2-htim2,psym=1, $
    title=day,xrange=[6,18],yrange=[-30,30],/xstyle,/ystyle, $
    ytitle='Difference Cont/Ha [msec]',xtitle='Hour [JST]'
  plot,c_delta_t[0,*]/1000./3600.,c_delta_t[1,*]/1000.,/ylog,psym=1,$
    title=day,xrange=[6,18],yrange=[10,100000.]/1000.,/xstyle,/ystyle, $
    ytitle='Cadence (diff. < 6msec) [sec]',xtitle='Hour [JST]'
  write_gif,plot_filename,(image=tvrd())
  !p.multi=0
  
  set_plot,'win'
  window,xs=1200,ys=900
  !p.color=0
  !p.background=255
  tv,image
  
  if widget_info(uval0.canclb,/button_set) then goto,abort

  ;ans=''
  ;read,'Ok? (y/n)',ans
  ;if ans eq 'n' then stop

  save,nn_file_log,file=nn_log_filename
  if widget_info(uval0.canclb,/button_set) then goto,abort

  ;save,dleta_t_c2,file=delta_t_c_filename

  ;-----  select dt cadence ------
  
  dt=30*1000l  ; time interval for movie

  dark_level=800
  ;dark_level=10
  
  dark_check=intarr(nn)

  dur=ctim3[nn-1]-ctim3[0]
  nimg=round(dur/dt)

  reftim=ctim3[0]+dt*lindgen(nimg)  ;reference time
  vl=[value_locate(ctim3,reftim),nn-1]

  yadd=30
  xs=800
  ys=600
  img=bytarr(xs,ys+yadd,nimg)
  flg=bytarr(nimg)

  set_plot,'z'
  device,set_resolution=[xs,ys+yadd]
  !p.color=0
  !p.background=255

  k=0l
  for i=0,nimg-1 do begin
    for j=vl[i],vl[i+1]-1,100 do begin
      img1=read_tiff(hfi3[j],/unsigned)
      if mean(img1) gt dark_level then begin
        widget_control,uval0.textw,set_value='%%% Image OK. (#'+string(j-vl[i],form='(i4)')+' of '+strtrim(i,2)+'/'+strtrim(nimg,2)+')'
        img2=rotate(rebin(img1,xs,ys),-1)
        erase
        tvscl,img2,0,yadd
        xyouts,100,10,file_basename(hfi3[j]),/dev,chars=1.5
        img[0,0,k]=tvrd()
        flg[i]=1b
        k++
        break
      endif else begin
        widget_control,uval0.textw,set_value='%%% cloudy... (#'+string(j-vl[i],form='(i4)')+' of '+strtrim(i,2)+'/'+strtrim(nimg,2)+')'
      endelse
      if widget_info(uval0.canclb,/button_set) then goto,abort
    endfor
    if widget_info(uval0.canclb,/button_set) then goto,abort
  endfor
  
  set_plot,'win'
  !p.color=0
  !p.background=255
  
  ha_ql_img=img[*,*,0:k-1]
  wflg=where(flg)
  ctim4=ctim3[wflg]
  htim4=htim3[wflg]

  ;save,ha_ql_img,file=ql_movie_filename
  ;save,file=ql_movie_filename,ha_ql_img,htim2,ctim2; 2014.01.04 A.O.
  save,file=ql_movie_filename,ha_ql_img,htim4,ctim4; 2016.07.05 K. Otsuji
  if widget_info(uval0.canclb,/button_set) then goto,abort

  ;-------------------------------------------------------

  xmovie,ha_ql_img,/fit,/modal,group_leader=ev.top

  WIDGET_CONTROL, ev.TOP, /DESTROY

  if 1 then begin
    print,systime()
    dmy=dialog_message(I18N_UTF8TOMULTIBYTE('終了しました。'),title='T3 Event Check',/INFORMATION)
  endif else begin
    abort:
    print,systime()
    dmy=dialog_message(I18N_UTF8TOMULTIBYTE('中断しました。'),title='T3 Event Check',/INFORMATION,dialog_parent=ev.top)
    WIDGET_CONTROL, ev.TOP, /DESTROY
  endelse

end


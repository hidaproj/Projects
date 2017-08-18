;To make sav file, do below
;.FULL_RESET_SESSION
;CD,'C:\Projects\IDLPRO\smart_T3'
;.COMPILE T3_select_ps1d
;RESOLVE_ALL,skip=['draw']
;SAVE, /ROUTINES, FILENAME='T3_select_ps1d.sav'

function version
  ver='0.1'  ;  2011.09.25  k.i., t.k.,   frame selecton
  ver='0.2'  ;  2011.10.08 TTI cloud check, rms save
  ver='0.3'  ;  2012.01.24 TTI (separate selected/events)
  ver='0.4'  ;  2012.05.02 TTI, k.i  frame selection using 1D spatial psd
  ver='0.5'  ;  2013.10.15 TTI, k.i  spawn copy-> Dcolyfile
  ver='1.0'  ;  2016.07.14 K. Otsuji widgetize
  ver='1.1'  ;  2016.07.19 K. Otsuji modify xmovie, value_locate error
  ver='1.2'  ;  2016.07.20 K. Otsuji fixed event form in .sav file
  ver='1.3'  ;  2016.07.22 K. Otsuji fixed minor bug (eventlist order)
             ;                       skip already done process
  return,ver
end
;--------------------------------------------------------------------
function fname2msec,fname,ip

  tim=strmid(fname,ip+[0,2,4,7],[2,2,2,3])##transpose([3600000l,60000l,1000l,1l])

  return,tim
end

;--------------------------------------------------------------------
pro copyfile,file1,outdir,oscomdll=oscomdll
  if not keyword_set(oscomdll) then $
    oscomdll='C:\Projects\cprog\VS2010\oscom64\x64\Debug\oscom64.dll'
  ip=strpos(file1,'\',/reverse_search)
  file2=outdir+'\'+strmid(file1,ip+1,strlen(file1)-ip-1)

  dmy=call_external(oscomdll,'Dcopyfile',file1,file2)

end

;----------------------------------------------------------------
; imgpsd1.pro
;   1D spatial psd of image
;       2012.4.29       k.i.

function imgpsd1,data

  pix1=0.215      ; arcsec
  profs=ishft(data,-4)
  imgsize,profs,nx,ny

  profs=profs-mean(profs)

  n2=nx/2
  pow=fltarr(n2)
  for j=0,ny-1 do begin
    f=fft(profs[*,j],-1)
    ff=float(f*conj(f))
    pow=pow+ff[0:n2-1]
  endfor
  kx=findgen(nx)/(pix1*nx)
  ii=where(kx ge 0.3 and kx lt 0.6)
  pp=mean(pow[ii])        ; <-- seeing?

  return,pp

end


pro T3_select_ps1d_process,ev

  widget_control,ev.top,get_uvalue=uval0

  ;confirm event list
  ev2=ev
  ev2.id=uval0.evtxt
  T3_select_ps1d_ck_form,ev2,err=err
  widget_control,ev.top,get_uvalue=uval0
  if err then return

  widget_control,uval0.dtdir,get_value=datedir & datedir=datedir[0]
  widget_control,uval0.sldir,get_value=seldir & seldir=seldir[0]
  if (day=strmid(file_basename(datedir),0,8)) ne (sldate=strmid(file_basename(seldir),0,8)) then begin
    ans=dialog_message(I18N_UTF8TOMULTIBYTE('日付フォルダと保存フォルダの日付が異なります('+day+' and '+sldate+')。実行してよろしいですか？'), $
      title='T3 Select ps1d',/question,dialog_parent=ev.top)
    if ans eq 'No' then return
  endif

  dirout=uval0.dirout

  ;evlist
  tmp=strsplit(uval0.evlist,',',/ex)
  hh=strmid(tmp,0,2)
  eventlist=tmp[uniq(tmp,sort(tmp))]
  nev=total(eventlist ne '',/pres)
  event=nev ne 0?reform(string(strmid(eventlist,[0,2,5,7,10,12],2),form='(a2,":",a2)'),3,nev):''

  ans=dialog_message(I18N_UTF8TOMULTIBYTE('以下の内容でよろしいですか？'+string(13B)+ $
    'events (in JST):'+string(13B)+(nev ne 0?strjoin(strjoin(event,'-'),string(13B)):'none')),title='T3 Select ps1d',/question,dialog_parent=ev.top)

  if ans eq 'No' then return

  ;execute start
  widget_control,uval0.dtdir,sensitive=0
  widget_control,uval0.sldir,sensitive=0
  widget_control,uval0.movieb,sensitive=0
  widget_control,uval0.evtxt,sensitive=0
  widget_control,uval0.startb,sensitive=0
  widget_control,uval0.canclb,set_value='abort'

  ;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  dt1=1*1000l  ; time interval for selection, msec
  dt2=5*1000l

  frame_select=1
  ;frame_select=0

  dark_level=800
  ;dark_level=10
  
  ;filename set
  nn_log_filename=seldir+'\'+day+'_files_log.sav'         ;restore
  pair_all_filename=seldir+'\'+day+'_pair_files_all.sav'  ;restore
  restore,pair_all_filename                               ;cfi2,hfi2,ctim2,htim2

  events_log=seldir+'\'+day+'_events_log.sav'                   ;save
  save,event,file=events_log
  pair_nodark_filename=seldir+'\'+day+'_pair_files_nodark.sav'  ;save
  pair_1s_filename=seldir+'\'+day+'_pair_files_1s.sav'          ;save
  selected_filename=seldir+'\'+day+'_selected_files.sav'        ;save

  cd,datedir

  if file_test(pair_1s_filename) then begin
    ans=dialog_message(I18N_UTF8TOMULTIBYTE('すでに'+pair_1s_filename+'が存在します。このファイルを使用しますか？'), $
      title='T3 Select ps1d',/question,dialog_parent=ev.top)
    if ans eq 'Yes' then case file_test(selected_filename) of
      0:goto,jump1
      1:goto,jump2
    endcase
  endif

  ii=transpose(where(abs(htim2-ctim2) lt 6,nn))
  cfi3=cfi2[ii]
  hfi3=hfi2[ii]
  ctim3=ctim2[ii]
  htim3=htim2[ii]

  if widget_info(uval0.canclb,/button_set) then goto,abort

  ;-----  select dt cadence ------
  
  widget_control,uval0.textw,set_value=string(nn)+' pairs (dt< 6msec) counted'

  dark_check=bytarr(nn)
  dt=dt1

  dur=ceil(ctim3[nn-1]/1000.)*1000.-fix(ctim3[0]/1000.)*1000.
  ns=ceil(dur/dt)
  iis=lonarr(ns)
  pp1_arr=fltarr(ns)
  pp2_arr=fltarr(ns)
  pp3_arr=fltarr(ns)
  br1_arr=fltarr(ns)
  br3_arr=fltarr(ns)
  tt1_arr=fltarr(ns)

  reftim=ctim3[0]+dt*lindgen(ns)  ;reference time
  vl=[value_locate(ctim3,reftim),nn-1]

  k=0l
  for i=0,ns-1 do begin
    count=vl[i+1]-vl[i]
    
    if count eq 0 then continue

    if frame_select then begin
      pp1=fltarr(count>1)
      br1=fltarr(count>1)
      for j=vl[i],vl[i+1]-1 do begin
        img1=read_tiff(cfi3[j],/unsigned,sub_rect=[0,600-4,1600,8])
        pp1[j-vl[i]]=imgpsd1(img1)
        br1[j-vl[i]]=mean(img1)
        dark_check[j]=br1[j-vl[i]] gt dark_level
        if ~dark_check[j] then dark_check[j]=mean(read_tiff(cfi3[ii[i]],/unsigned)) gt dark_level
      endfor
      dmy=max(pp1,i0)
      dmy2=min(pp1)
      dmy3=mean(pp1)
      dmy4=mean(br1)
    endif else begin
      i0=0
      dmy=-1.0 & dmy2=-1 & dmy3=-1
    endelse
    
    img1=read_tiff(cfi3[vl[i]+i0],/unsigned)
    imgsize,img1,nx,ny
    
    if mean(img1) gt dark_level then begin
      iis[k]=vl[i]+i0
      pp1_arr[k]=dmy
      pp2_arr[k]=dmy2
      pp3_arr[k]=dmy3
      br1_arr[k]=br1[i0]
      br3_arr[k]=dmy4
      tt1_arr[k]=ctim3[vl[i]]+dt/2.
      widget_control,uval0.textw,set_value=strtrim(i,2)+'/'+strtrim(ns-1,2)+' '+cfi3[iis[k]]+' '+strtrim(i0,2)+'/'+strtrim(count,2)
      ;print,cfi3[iis[k]],' /  ',i0,' / ',count
      k++
    endif else widget_control,uval0.textw,set_value=strtrim(i,2)+'/'+strtrim(ns-1,2)+' '+cfi3[vl[i]]+' cloudy...'
    
    if widget_info(uval0.canclb,/button_set) then goto,abort

  endfor
  
  iis=iis[0:k-1]
  pp1_arr=pp1_arr[0:k-1]
  pp2_arr=pp2_arr[0:k-1]
  pp3_arr=pp3_arr[0:k-1]
  br1_arr=br1_arr[0:k-1]
  br3_arr=br3_arr[0:k-1]
  tt1_arr=tt1_arr[0:k-1]

  cfi4=cfi3[iis]
  hfi4=hfi3[iis]
  ctim4=ctim3[iis]
  htim4=htim3[iis]

  dok=where(dark_check)
  cfi2=cfi3[dok]
  hfi2=hfi3[dok]

  restore,nn_log_filename

  save,cfi2,hfi2,file=pair_nodark_filename
  save,cfi4,hfi4,ctim4,htim4,pp1_arr,pp2_arr,pp3_arr,br1_arr,br3_arr,tt1_arr,nn_file_log, $
    file=pair_1s_filename

  if 0 then begin
    jump1:
    restore,pair_nodark_filename
    restore,pair_1s_filename
  endif

  if widget_info(uval0.canclb,/button_set) then goto,abort

  ;-------------------------------------------------------

  cfi3=cfi4
  hfi3=hfi4
  ctim3=ctim4
  htim3=htim4

  nn=n_elements(cfi3)
  dt=dt2

  dur=ceil(ctim3[nn-1]/1000.)*1000.-fix(ctim3[0]/1000.)*1000.
  ns0=ceil(dur/dt)
  
  reftim=ctim3[0]+dt*lindgen(ns0)  ;reference time
  vl=[value_locate(ctim3,reftim),nn-1]
  
  vl=vl[uniq(vl)]
  ns=n_elements(vl)-1

  iis=lonarr(ns)
  pp2_arr=fltarr(ns)

  for i=0,ns-1 do begin
    count=vl[i+1]-vl[i]
    
    if frame_select then dmy=max(pp1_arr[vl[i]:vl[i+1]],i0) else begin
      i0=0
      dmy=-1.0
    endelse
    iis[i]=vl[i]+i0
    pp2_arr[i]=dmy
    widget_control,uval0.textw,set_value=strtrim(i,2)+'/'+strtrim(ns-1,2)+' '+cfi3[iis[i]]+' '+strtrim(i0,2)+'/'+strtrim(count,2)
    ;print,cfi3[iis[i]],' /  ',i0,' /  ',count

    if widget_info(uval0.canclb,/button_set) then goto,abort

  endfor

  cfi4=cfi3[iis]
  hfi4=hfi3[iis]
  save,cfi4,hfi4,pp2_arr,file=selected_filename

  if 0 then begin
    jump2:
    restore,selected_filename
  endif

  if widget_info(uval0.canclb,/button_set) then goto,abort

  ;-----  copy selected files ------
  
  ns=n_elements(cfi4)
  text=[string(ns)+' files selected']
  widget_control,uval0.textw,set_value=text
  ;print,ns,' files selected'
  
  cok=file_test(seldir+'\'+file_basename(cfi4))
  hok=file_test(seldir+'\'+file_basename(hfi4))
  if min([cok,hok]) then begin
    ans=dialog_message(I18N_UTF8TOMULTIBYTE('全ファイルコピー済みです。上書きしますか？'), $
      title='T3 Select ps1d',/question,dialog_parent=ev.top)
    if ans eq 'No' then goto,jump3
  endif
  
  for i=0,ns-1 do begin
    widget_control,uval0.textw,set_value=[text,'Now copying selected files... ('+strtrim(i,2)+'/'+strtrim(ns-1,2)+')', $
      cfi4[i],hfi4[i]]
    ;spawn,'copy '+cfi4[i]+' '+seldir,/hide
    ;spawn,'copy '+hfi4[i]+' '+seldir,/hide
    copyfile,cfi4[i],seldir
    copyfile,hfi4[i],seldir
    ;print,i,' / ',ns,'    ',cfi4[i],'   ',hfi4[i]

    if widget_info(uval0.canclb,/button_set) then goto,abort

  endfor

  jump3:  ;skip copy

  ;-------------

  if nev ne 0 then begin
    restore,pair_1s_filename
    restore,pair_nodark_filename
    ip=strpos(cfi2[0],'.tif',/reverse_search)-10
    ctim4=fname2msec(cfi4,ip)
    htim4=fname2msec(hfi4,ip)
    ctim2=fname2msec(cfi2,ip)
    htim2=fname2msec(hfi2,ip)
    evdir=dirout+'events\'+day

    spawn,'mkdir '+evdir
    for k=0,nev-1 do begin
      event1=event[*,k]
      text=['event selection : '+strjoin(event[*,k],'-')]
      widget_control,uval0.textw,set_value=text
      etim1=(strmid(event1[0],0,2)*3600l+strmid(event1[0],3,2)*60l-360l)*1000l
      etim2=(strmid(event1[0],0,2)*3600l+strmid(event1[0],3,2)*60l-120l)*1000l
      etim3=(strmid(event1[1],0,2)*3600l+strmid(event1[1],3,2)*60l+60l)*1000l
      etim4=(strmid(event1[2],0,2)*3600l+strmid(event1[2],3,2)*60l+120l)*1000l
      iie=where(ctim4 gt etim1 and ctim4 lt etim2, numiie) ; 2012.04.16. SM
      IF numiie GT 0 THEN BEGIN ; 2012.04.16. SM
        cfi5=cfi4[iie]
        hfi5=hfi4[iie]
        cok=file_test(evdir+'\'+file_basename(cfi5))
        hok=file_test(evdir+'\'+file_basename(hfi5))
        
        nne=min([cok,hok])?0:n_elements(cfi5)
        ;print,nne,'  files in preflare phase'
        for i=0l,nne-1 do begin
          widget_control,uval0.textw,set_value=[text,string(nne)+'  files in preflare phase  ('+strtrim(i,2)+'/'+strtrim(nne-1,2)+')', $
            cfi5[i],hfi5[i]]
          ; spawn,'copy '+cfi5[i]+' '+evdir,/hide
          ; spawn,'copy '+hfi5[i]+' '+evdir,/hide
          copyfile,cfi5[i],evdir
          copyfile,hfi5[i],evdir
          ;print,i,' /',nne,'   ',cfi5[i],'   ',hfi5[i]
          
          if widget_info(uval0.canclb,/button_set) then goto,abort

        endfor
      ENDIF ; 2012.04.16.  SM
      iie=where(ctim2 gt etim2 and ctim2 lt etim3, numiie)
      IF numiie GT 0 THEN BEGIN
        cfi5=cfi2[iie]
        hfi5=hfi2[iie]
        cok=file_test(evdir+'\'+file_basename(cfi5))
        hok=file_test(evdir+'\'+file_basename(hfi5))

        nne=min([cok,hok])?0:n_elements(cfi5)
        ;print,nne,'  files in impulsive phase'
        for i=0l,nne-1 do begin
          widget_control,uval0.textw,set_value=[text,string(nne)+'  files in impulsive phase  ('+strtrim(i,2)+'/'+strtrim(nne-1,2)+')', $
            cfi5[i],hfi5[i]]
          ;spawn,'copy '+cfi5[i]+' '+evdir,/hide
          ;spawn,'copy '+hfi5[i]+' '+evdir,/hide
          copyfile,cfi5[i],evdir
          copyfile,hfi5[i],evdir
          ;print,i,' /',nne,'   ',cfi5[i],'   ',hfi5[i]
          
          if widget_info(uval0.canclb,/button_set) then goto,abort

        endfor
      ENDIF
      iie=where(ctim4 gt etim3 and ctim4 lt etim4, numiie)
      IF numiie GT 0 THEN BEGIN
        cfi5=cfi4[iie]
        hfi5=hfi4[iie]
        cok=file_test(evdir+'\'+file_basename(cfi5))
        hok=file_test(evdir+'\'+file_basename(hfi5))

        nne=min([cok,hok])?0:n_elements(cfi5)
        ;print,nne,'  files in decay phase'
        for i=0l,nne-1 do begin
          widget_control,uval0.textw,set_value=[text,string(nne)+'  files in decay phase  ('+strtrim(i,2)+'/'+strtrim(nne-1,2)+')', $
            cfi5[i],hfi5[i]]
          ;spawn,'copy '+cfi5[i]+' '+evdir,/hide
          ;spawn,'copy '+hfi5[i]+' '+evdir,/hide
          copyfile,cfi5[i],evdir
          copyfile,hfi5[i],evdir
          ;print,i,' /',nne,'   ',cfi5[i],'   ',hfi5[i]
          
          if widget_info(uval0.canclb,/button_set) then goto,abort

        endfor
      ENDIF
      
      if widget_info(uval0.canclb,/button_set) then goto,abort
      
    endfor
  endif

  ;print,'T3_select_ps1d.pro finished'
  
  if 1 then begin
    print,systime()
    dmy=dialog_message(I18N_UTF8TOMULTIBYTE('終了しました。'),title='T3 Select ps1d',/INFORMATION,dialog_parent=ev.top)
    WIDGET_CONTROL, ev.TOP, /DESTROY
  endif else begin
    abort:
    print,systime()
    dmy=dialog_message(I18N_UTF8TOMULTIBYTE('中断しました。'),title='T3 Select ps1d',/INFORMATION,dialog_parent=ev.top)
    WIDGET_CONTROL, ev.TOP, /DESTROY
  endelse

end



pro T3_select_ps1d_ck_form,ev,err=err

  widget_control,ev.top,get_uvalue=uval0
  widget_control,ev.id,get_value=evlist
  w=where(strtrim(evlist,2) ne '' and strtrim(evlist,2) ne 'hhmm-hhmm-hhmm (start-peak-end in JST, one event per one line)',n)
  err=0b
  if n eq 0 then uval0.evlist='' else begin
    tmp=strtrim(evlist[w],2)
    flg=strlen(tmp) eq 14 and min((hh=strmid(tmp,[0,5,10],2)) ge '00',dim=1) and min(hh le '23',dim=1) and $
      min((mm=strmid(tmp,[2,7,12],2)) ge '00',dim=1) and min(mm le '59',dim=1) and min(strmid(tmp,[4,9],1) eq '-')
    wng=where(~flg,nng)
    if nng ne 0 then begin
      dmy=dialog_message(I18N_UTF8TOMULTIBYTE(strjoin(tmp[wng],', ')+'の書式が不正です。'),title='T3 Select ps1d',/error,dialog_parent=ev.top)
      uval0.evlist=n eq nng?'':strjoin(tmp[where(flg)],',')
      err=1b
    endif else uval0.evlist=strjoin(tmp,',')
  endelse

  widget_control,ev.top,set_uvalue=uval0

end


pro T3_select_ps1d_event,ev

  widget_control,ev.top,get_uvalue=uval0

  case ev.id of
    uval0.dtdir:begin
      widget_control,uval0.dtdir,get_value=datedir & datedir=datedir[0]
      widget_control,uval0.sldir,get_value=seldir & seldir=seldir[0]
      if ~file_test(datedir,/dir) then begin
        ans=dialog_message(I18N_UTF8TOMULTIBYTE('日付フォルダが存在しません。'), $
          title='T3 Select ps1d',/error,dialog_parent=ev.top)
        widget_control,uval0.startb,sensitive=0
      endif else begin
        if datedir ne uval0.datedir then begin
          day0=strmid(file_basename(datedir),0,8)
          uval0.day=day0
          uval0.datedir=datedir
          widget_control,ev.top,set_uvalue=uval0
        endif
        widget_control,uval0.startb,sensitive=file_test(seldir,/dir)
      endelse
    end
    uval0.sldir:begin
      widget_control,uval0.dtdir,get_value=datedir & datedir=datedir[0]
      widget_control,uval0.sldir,get_value=seldir & seldir=seldir[0]
      if ~file_test(seldir,/dir) then begin
        ans=dialog_message(I18N_UTF8TOMULTIBYTE('保存フォルダが存在しません。'), $
          title='T3 Select ps1d',/error,dialog_parent=ev.top)
        widget_control,uval0.movieb,sensitive=0
        widget_control,uval0.startb,sensitive=0
      endif else begin
        uval0.seldir=seldir
        widget_control,uval0.movieb,sensitive=1
        widget_control,ev.top,set_uvalue=uval0
        widget_control,uval0.startb,sensitive=file_test(datedir,/dir)
      endelse
    end
    uval0.movieb:begin
      ql_movie_filename=uval0.seldir+'\'+uval0.day+'_Ha_ql.sav'
      restore,ql_movie_filename
      xmovie,ha_ql_img,/fit,group_leader=ev.top
    end
    uval0.evtxt:T3_select_ps1d_ck_form,ev
    uval0.canclb:begin
      WIDGET_CONTROL, ev.TOP, /DESTROY
      return
    end
    else:stop
  endcase

end


pro T3_select_ps1d

  ver=version()
  dirout='D:\T3\'  ;default

  ;datedir?
  caldat,systime(0,/JULIAN),mm,dd,yyyy
  day=string([yyyy,mm,dd],form='(i4.4,2i2.2)')

  ans1=dialog_message(I18N_UTF8TOMULTIBYTE('本日('+day+')のデータ処理を行いますか？'),/question,title='T3 Select ps1d')

  datedir=''
  ans2=''
  if ans1 eq 'Yes' then begin
    datedir=file_search('{E,F}:\T3\'+day,count=nday)
    if nday eq 0 then begin  ;before yesterday?
      ans2=dialog_message(I18N_UTF8TOMULTIBYTE('本日('+day+')のフォルダがE:\T3またはF:\T3内に見つかりません。他の日の処理を行いますか？'),/question,title='T3 Select ps1d')
      if ans2 eq 'No' then return
    endif
  endif
  if ans1 eq 'No' or ans2 eq 'Yes' then begin
    datelist=file_search('{E,F}:\T3\20??????*',count=nday)
    if nday eq 0 then begin
      ans=dialog_message(I18N_UTF8TOMULTIBYTE('日付フォルダがE:\T3またはF:\T3内に見つかりません。'),title='T3 Select ps1d')
      return
    endif

    datedir=dialog_list(datelist,title=I18N_UTF8TOMULTIBYTE('処理する日付フォルダを選択してください。'))
    if datedir eq '' then return

    day=strmid(file_basename(datedir),0,8)
  endif
  seldir=dirout+'selected\'+day

  ;widget

  base=widget_base(/COLUMN,title='T3 Select ps1d '+ver)
  tbase=widget_base(base,/column,/base_align_right)
  dtdir=cw_field(tbase,value=datedir,/string,title=I18N_UTF8TOMULTIBYTE('日付フォルダ: '),xsize=64,/return_event)
  sldir=cw_field(tbase,value=seldir,/string,title=I18N_UTF8TOMULTIBYTE('保存フォルダ: '),xsize=64,/return_event)
  movieb=widget_button(tbase,value='view movie')
  evtxt=cw_field(tbase,value='hhmm-hhmm-hhmm (start-peak-end in JST, one event per one line)',/string,title='Events time range:',xsize=64,ysize=10,/return_event)
  textw=cw_field(tbase,/string,title='STATUS: ',xsize=64,ysize=4,/noedit)

  bbase=widget_base(base,/row,/align_right)
  startb=widget_button(bbase,value='start',event_pro='T3_select_ps1d_process')
  canclb=widget_button(bbase,value='cancel')

  evlist=''
  uval={base:base,tbase:tbase,dtdir:dtdir,sldir:sldir,movieb:movieb,evtxt:evtxt,textw:textw,bbase:bbase,startb:startb,canclb:canclb, $
    dirout:dirout,day:day,datedir:datedir,seldir:seldir,evlist:evlist}
  widget_control,base,set_uvalue=uval

  WIDGET_CONTROL, base, /REALIZE

  XMANAGER, 'T3_select_ps1d', base
  
  print,'done'
  
end



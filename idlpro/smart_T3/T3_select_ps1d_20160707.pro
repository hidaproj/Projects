;To make sav file, do below
;.FULL_RESET_SESSION
;CD,'C:\Projects\IDLPRO\smart_T3'
;.COMPILE T3_select_ps1d
;RESOLVE_ALL
;SAVE, /ROUTINES, FILENAME='T3_select_ps1d.sav'

; T3 select images
function version
  ver='0.1'  ;  2011.9.25  k.i., t.k.,   frame selecton
  ver='0.2'  ;  2011.10.08 TTI cloud check, rms save
  ver='0.3'  ;  2012.01.24 TTI (separate selected/events)
  ver='0.4'  ;  2012.05.02 TTI, k.i  frame selection using 1D spatial psd
  ver='0.5'  ;  2013.10.15 TTI, k.i  spawn copy-> Dcolyfile
  ver='1.0'  ;  2016.07.07 K. Otsuji widgetize
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

  widget_control,uval0.dtdir,get_value=datedir & datedir=datedir[0]
  widget_control,uval0.sldir,get_value=seldir & seldir=seldir[0]
  if (day=strmid(file_basename(datedir),0,8)) ne (sldate=strmid(file_basename(seldir),0,8)) then begin
    ans=dialog_message(I18N_UTF8TOMULTIBYTE('日付フォルダと保存フォルダの日付が異なります('+day+' and '+sldate+')。実行してよろしいですか？'), $
      title='T3 Select ps1d',/question,dialog_parent=ev.top)
    if ans eq 'No' then return
  endif

  ;evlist
  tmp=strsplit(uval0.evlist,',',/ex)
  hh=strmid(tmp,0,2)
  eventlist=tmp[uniq(tmp,sort((['0','1'])[hh le '12']+tmp))]
  nev=total(eventlist ne '',/pres)
  events=nev ne 0?reform(string(strmid(eventlist,[0,2,5,7],2),form='(a2,":",a2)'),2,nev):''

  ans=dialog_message(I18N_UTF8TOMULTIBYTE('以下の内容でよろしいですか？'+string(13B)+ $
    'events:'+string(13B)+(nev ne 0?strjoin(strjoin(events,'-'),string(13B)):'none')),title='T3 Select ps1d',/question,dialog_parent=ev.top)

  if ans eq 'No' then return

  ;execute start
  widget_control,uval0.dtdir,sensitive=0
  widget_control,uval0.sldir,sensitive=0
  widget_control,uval0.evtxt,sensitive=0
  widget_control,uval0.startb,sensitive=0
  widget_control,uval0.canclb,set_value='abort'


  ;--------------------------------------------------------------------
  ; こけたとき、dirout, dir, day を適宜編集、コメントアウト解除
  ;dirout = 'D:\T3\'
  ;dir = 'E:\T3\'
  ;dir = 'F:\T3\'
  ;Smart2003
  ;day = '20160104'

  ; event無いとき
  ;event=''
  evtmp=strmid(events,[0,3],[2,2])
  evtmp=reform(fix((['','1'])[evtmp[0,*,*] le '12']+evtmp[0,*,*]+evtmp[1,*,*]))
  nev=total(events ne '',/pres)/2

  ;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  dt1=1*1000l  ; time interval for selection, msec
  dt2=5*1000l

  frame_select=1
  ;frame_select=0

  dark_level=800
  ;dark_level=10
  
  ;filename set
  selected_filename=seldir+'\'+day+'_selected_files.sav'
  pair_1s_filename=seldir+'\'+day+'_pair_files_1s.sav'

  events_log=seldir+'\'+day+'_events_log.sav'
  save,event,file=events_log


  pair_all_filename=seldir+'\'+day+'_pair_files_all.sav'
  pair_nodark_filename=seldir+'\'+day+'_pair_files_nodark.sav'

  restore,pair_all_filename
  cfi2=cfi99
  hfi2=hfi99

  cd,dir+day

  ii=where(abs(htim2-ctim2) lt 6)
  cfi3=cfi2[ii]
  hfi3=hfi2[ii]
  ctim3=fname2msec(cfi3,ip)
  htim3=fname2msec(hfi3,ip)

  ;-----  select dt cadence ------
  nn=n_elements(cfi3)
  print,nn,' pairs (dt<6msec) counted'
  dark_check=intarr(nn)
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

  k=0l
  t1=fix(ctim3[0]/dt)*dt &  t2=t1+dt
  while t2 le ctim3[nn-1] do begin
    ii=where(ctim3 ge t1 and ctim3 lt t2, count)
    if count ne 0 then begin
      if frame_select then begin
        pp1=fltarr(count)
        br1=fltarr(count)
        for i=0,count-1 do begin
          ;print,cfi3[ii[i]]   ; add 2012.09.14 sm
          img1=read_tiff(cfi3[ii[i]],sub_rect=[0,600-4,1600,8])
          br1[i]=mean(img1)
          if br1[i] gt dark_level then begin
            dark_check[ii[i]]=1
          endif else begin
            img0=read_tiff(cfi3[ii[i]],/unsigned)
            if mean(img0) gt dark_level then dark_check[ii[i]]=1
          endelse
          pp1[i]=imgpsd1(img1)
        endfor
        dmy=max(pp1,i0)
        dmy2=min(pp1)
        dmy3=mean(pp1)
        dmy4=mean(br1)
      endif else begin
        i0=0
        dmy=-1.0 & dmy2=-1 & dmy3=-1
      endelse
      img1=read_tiff(cfi3[ii[i0]],/unsigned)
      imgsize,img1,nx,ny
      ;      print,cfi3[ii[i0]],mean(img1),dark_level
      if mean(img1) gt dark_level then begin
        iis[k]=ii[i0]
        pp1_arr[k]=dmy
        pp2_arr[k]=dmy2
        pp3_arr[k]=dmy3
        br1_arr[k]=br1[i0]
        br3_arr[k]=dmy4
        tt1_arr[k]=t1+dt/2.
        print,cfi3[iis[k]],' /  ',count
        ;      wset,0
        ;     tvscl,rebin(img1,nx/2,ny/2)
        ;     xyouts,10,10,cfi3[iis[k]],/dev
        k=k+1
      endif
    endif
    ;;    stop
    t1=t2 & t2=t1+dt
  endwhile
  iis=iis[0:k-1]
  pp1_arr=pp1_arr[0:k-1]
  pp2_arr=pp2_arr[0:k-1]
  pp3_arr=pp3_arr[0:k-1]
  br1_arr=br1_arr[0:k-1]
  br3_arr=br3_arr[0:k-1]
  tt1_arr=tt1_arr[0:k-1]


  ns=n_elements(iis)
  cfi4=cfi3[iis]
  hfi4=hfi3[iis]
  ctim4=fname2msec(cfi4,ip)
  htim4=fname2msec(hfi4,ip)

  dok=where(dark_check eq 1)
  cfi2=cfi3[dok]
  hfi2=hfi3[dok]

  save,cfi2,hfi2,file=pair_nodark_filename
  save,cfi4,hfi4,pp1_arr,pp2_arr,pp3_arr,br1_arr,br3_arr,tt1_arr,nn_file_log, $
    file=pair_1s_filename

  ;-------------------------------------------------------

  cfi3=cfi4
  hfi3=hfi4
  ctim3=fname2msec(cfi3,ip)
  htim3=fname2msec(hfi3,ip)

  nn=n_elements(cfi3)
  dt=dt2

  dur=ceil(ctim3[nn-1]/1000.)*1000.-fix(ctim3[0]/1000.)*1000.
  ns=ceil(dur/dt)
  iis=lonarr(ns)
  pp2_arr=fltarr(ns)

  k=0l
  t1=fix(ctim3[0]/dt)*dt &  t2=t1+dt
  while t2 le ctim3[nn-1] do begin
    ii=where(ctim3 ge t1 and ctim3 lt t2, count)
    if count ne 0 then begin
      if frame_select then begin
        pp1=fltarr(count)
        for i=0,count-1 do begin
          pp1[i]=pp1_arr[ii[i]]
        endfor
        dmy=max(pp1,i0)
      endif else begin
        i0=0
        dmy=-1.0
      endelse
      iis[k]=ii[i0]
      pp2_arr[k]=dmy
      print,cfi3[iis[k]],' /  ',count
      k=k+1
    endif
    ;;    stop
    t1=t2 & t2=t1+dt
  endwhile
  iis=iis[0:k-1]
  pp2_arr=pp2_arr[0:k-1]

  ns=n_elements(iis)
  cfi4=cfi3[iis]
  hfi4=hfi3[iis]

  print,ns,' files selected'
  save,cfi4,hfi4,pp2_arr,file=selected_filename

  ;-----  copy selected files ------
  for i=0,ns-1 do begin
    ;spawn,'copy '+cfi4[i]+' '+seldir,/hide
    ;spawn,'copy '+hfi4[i]+' '+seldir,/hide
    copyfile,cfi4[i],seldir
    copyfile,hfi4[i],seldir
    print,i,' / ',ns,'    ',cfi4[i],'   ',hfi4[i]
  endfor


  ;-------------

  if keyword_set(event) then begin


    restore,pair_1s_filename
    restore,pair_nodark_filename
    ip=strpos(cfi2[0],'.tif',/reverse_search)-10
    ctim4=fname2msec(cfi4,ip)
    htim4=fname2msec(hfi4,ip)
    ctim2=fname2msec(cfi2,ip)
    htim2=fname2msec(hfi2,ip)
    evdir=dirout+'events\'+day


    spawn,'mkdir '+evdir
    imgsize,event,n2,nev
    for k=0,nev-1 do begin
      event1=event[*,k]
      etim1=(strmid(event1[0],0,2)*3600l+strmid(event1[0],3,2)*60l-360l)*1000l
      etim2=(strmid(event1[0],0,2)*3600l+strmid(event1[0],3,2)*60l-120l)*1000l
      ;    etim3=(strmid(event1[1],0,2)*3600l+strmid(event1[1],3,2)*60l+360l)*1000l ; 2013.6.21 AO
      etim3=(strmid(event1[1],0,2)*3600l+strmid(event1[1],3,2)*60l+60l)*1000l
      etim4=(strmid(event1[2],0,2)*3600l+strmid(event1[2],3,2)*60l+120l)*1000l
      iie=where(ctim4 gt etim1 and ctim4 lt etim2, numiie) ; 2012.04.16. SM
      IF numiie GT 0 THEN BEGIN ; 2012.04.16. SM
        cfi5=cfi4[iie]
        hfi5=hfi4[iie]
        nne=n_elements(cfi5)
        print,nne,'  files in preflare phase'
        for i=0l,nne-1 do begin
          ; spawn,'copy '+cfi5[i]+' '+evdir,/hide
          ; spawn,'copy '+hfi5[i]+' '+evdir,/hide
          copyfile,cfi5[i],evdir
          copyfile,hfi5[i],evdir
          print,i,' /',nne,'   ',cfi5[i],'   ',hfi5[i]
        endfor
      ENDIF ; 2012.04.16.  SM
      iie=where(ctim2 gt etim2 and ctim2 lt etim3, numiie)
      IF numiie GT 0 THEN BEGIN
        cfi5=cfi2[iie]
        hfi5=hfi2[iie]
        nne=n_elements(cfi5)
        print,nne,'  files in impulsive phase'
        for i=0l,nne-1 do begin
          ;spawn,'copy '+cfi5[i]+' '+evdir,/hide
          ;spawn,'copy '+hfi5[i]+' '+evdir,/hide
          copyfile,cfi5[i],evdir
          copyfile,hfi5[i],evdir
          print,i,' /',nne,'   ',cfi5[i],'   ',hfi5[i]
        endfor
      ENDIF
      iie=where(ctim4 gt etim3 and ctim4 lt etim4, numiie)
      IF numiie GT 0 THEN BEGIN
        cfi5=cfi4[iie]
        hfi5=hfi4[iie]
        nne=n_elements(cfi5)
        print,nne,'  files in decay phase'
        for i=0l,nne-1 do begin
          ;spawn,'copy '+cfi5[i]+' '+evdir,/hide
          ;spawn,'copy '+hfi5[i]+' '+evdir,/hide
          copyfile,cfi5[i],evdir
          copyfile,hfi5[i],evdir
          print,i,' /',nne,'   ',cfi5[i],'   ',hfi5[i]
        endfor
      ENDIF
    endfor
  endif

  print,'T3_select_ps1d.pro finished'

end



pro T3_select_ps1d_ck_form,ev

  widget_control,ev.top,get_uvalue=uval0
  widget_control,ev.id,get_value=evlist
  w=where(strtrim(evlist,2) ne '' and strtrim(evlist,2) ne 'hhmm-hhmm-hhmm (start-peak-end, one event per one line)',n)
  if n eq 0 then uval0.evlist='' else begin
    tmp=strtrim(evlist[w],2)
    flg=strlen(tmp) eq 14 and min((hh=strmid(tmp,[0,5,10],2)) ge '00',dim=1) and min(hh le '23',dim=1) and $
      min((mm=strmid(tmp,[2,7,12],2)) ge '00',dim=1) and min(mm le '59',dim=1) and min(strmid(tmp,[4,9],1) eq '-')
    wng=where(~flg,nng)
    if nng ne 0 then begin
      dmy=dialog_message(I18N_UTF8TOMULTIBYTE(strjoin(tmp[wng],', ')+'の書式が不正です。'),title='T3 Select ps1d',/error,dialog_parent=ev.top)
      uval0.evlist=n eq nng?'':strjoin(tmp[where(flg)],',')
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
        widget_control,uval0.startb,sensitive=0
      endif else begin
        uval0.seldir=seldir
        widget_control,ev.top,set_uvalue=uval0
      endelse
      widget_control,uval0.startb,sensitive=file_test(datedir,/dir)
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

  base=widget_base(/COLUMN,title='T3 Event Check '+ver)
  tbase=widget_base(base,/column,/base_align_right)
  dtdir=cw_field(tbase,value=datedir,/string,title=I18N_UTF8TOMULTIBYTE('日付フォルダ: '),xsize=40,/return_event)
  sldir=cw_field(tbase,value=seldir,/string,title=I18N_UTF8TOMULTIBYTE('保存フォルダ: '),xsize=40,/return_event)
  evtxt=cw_field(tbase,value='hhmm-hhmm-hhmm (start-peak-end, one event per one line)',/string,title='Events time range:',xsize=40,ysize=10,/return_event)
  textw=cw_field(tbase,/string,title='STATUS: ',xsize=40,ysize=4,/noedit)

  bbase=widget_base(base,/row,/align_right)
  startb=widget_button(bbase,value='start',event_pro='T3_select_ps1d_process')
  canclb=widget_button(bbase,value='cancel')

  evlist=''
  uval={base:base,tbase:tbase,dtdir:dtdir,sldir:sldir,evtxt:evtxt,textw:textw,bbase:bbase,startb:startb,canclb:canclb, $
    day:day,datedir:datedir,seldir:seldir,evlist:evlist}
  widget_control,base,set_uvalue=uval

  WIDGET_CONTROL, base, /REALIZE

  XMANAGER, 'T3_select_ps1d', base

end



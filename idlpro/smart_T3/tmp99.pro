; T3 select images
function version
  ver=0.1  ;  2011.9.25  k.i., t.k.,   frame selecton
  ver=0.2  ;  2011.10.08 TTI cloud check, rms save 
  ver=0.3  ;  2012.01.24 TTI (separate selected/events)
  ver=0.4  ;  2012.05.02 TTI, k.i  frame selection using 1D spatial psd
  return,ver
end
;--------------------------------------------------------------------   
function fname2msec,fname,ip

tim=(strmid(fname,ip,2)*3600l+strmid(fname,ip+2,2)*60l $
    +strmid(fname,ip+4,2))*1000l+strmid(fname,ip+7,3)
return,tim
end


;--------------------------------------------------------------------   
dir='E:\T3\'
day='20120521'

event=['06:19','06:20','08:59']

start_time=''

seldir=dir+'selected\'+day


cd,dir+day

dark_level=800
selected_filename=dir+'selected\'+day+'\'+day+'_selected_files.sav'
pair_1s_filename=dir+'selected\'+day+'\'+day+'_pair_files_1s.sav'



pair_all_filename=dir+'selected\'+day+'\'+day+'_pair_files_all.sav'
pair_nodark_filename=dir+'selected\'+day+'\'+day+'_pair_files_nodark.sav'

if keyword_set(event) then begin


restore,pair_1s_filename
restore,pair_nodark_filename
ip=strpos(cfi2[0],'.tif',/reverse_search)-10
ctim4=fname2msec(cfi4,ip)
htim4=fname2msec(hfi4,ip)
ctim2=fname2msec(cfi2,ip)
htim2=fname2msec(hfi2,ip)
evdir=dir+'events\'+day


  spawn,'mkdir '+evdir
  imgsize,event,n2,nev
  for k=0,nev-1 do begin
    event1=event[*,k]
    etim1=(strmid(event1[0],0,2)*3600l+strmid(event1[0],3,2)*60l-60l)*1000l
    etim2=(strmid(event1[0],0,2)*3600l+strmid(event1[0],3,2)*60l-60l)*1000l
    etim3=(strmid(event1[1],0,2)*3600l+strmid(event1[1],3,2)*60l+60l)*1000l
    etim4=(strmid(event1[2],0,2)*3600l+strmid(event1[2],3,2)*60l+60l)*1000l
    iie=where(ctim4 gt etim1 and ctim4 lt etim2, numiie) ; 2012.04.16. SM
    IF numiie GT 0 THEN BEGIN ; 2012.04.16. SM
      cfi5=cfi4[iie]
      hfi5=hfi4[iie]
      nne=n_elements(cfi5)
      print,nne,'  files in preflare phase'
      for i=0l,nne-1 do begin
        spawn,'copy '+cfi5[i]+' '+evdir,/hide
        spawn,'copy '+hfi5[i]+' '+evdir,/hide
        print,i,' /',nne,'   ',cfi5[i],'   ',hfi5[i]
      endfor
    ENDIF ; 2012.04.16.  SM
    iie=where(ctim2 gt etim2 and ctim2 lt etim3)
    cfi5=cfi2[iie]
    hfi5=hfi2[iie]
    nne=n_elements(cfi5)
    print,nne,'  files in impulsive phase'
    for i=0l,nne-1 do begin
      spawn,'copy '+cfi5[i]+' '+evdir,/hide
      spawn,'copy '+hfi5[i]+' '+evdir,/hide
      print,i,' /',nne,'   ',cfi5[i],'   ',hfi5[i]
    endfor
    iie=where(ctim4 gt etim3 and ctim4 lt etim4)
    cfi5=cfi4[iie]
    hfi5=hfi4[iie]
    nne=n_elements(cfi5)
    print,nne,'  files in decay phase'
    for i=0l,nne-1 do begin
      spawn,'copy '+cfi5[i]+' '+evdir,/hide
      spawn,'copy '+hfi5[i]+' '+evdir,/hide
      print,i,' /',nne,'   ',cfi5[i],'   ',hfi5[i]
    endfor
  endfor
endif


  

  


end
  

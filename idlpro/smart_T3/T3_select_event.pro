; T3 select images
function version
  ver=0.1  ;  2011.9.25  k.i., t.k.,   frame selecton
  ver=0.2  ;  2011.10.08 TTI cloud check, rms save 
  ver=0.3  ;  2012.01.24 TTI (separate selected/events)
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
day='20120128'
event=['10:11','10:30','10:50']
;event=[['06:29','06:43'],['06:54','07:15'],['16:01','16:08']]

;event=''

pair_all_filename=dir+'selected\'+day+'\'+day+'_pair_files_all.sav'
pair_1s_filename=dir+'selected\'+day+'\'+day+'_pair_files_1s.sav'

restore,pair_all_filename
restore,pair_1s_filename
ip=strpos(cfi2[0],'.tif',/reverse_search)-10
ctim4=fname2msec(cfi4,ip)
htim4=fname2msec(hfi4,ip)
ctim2=fname2msec(cfi2,ip)
htim2=fname2msec(hfi2,ip)
evdir=dir+'events\'+day

cd,dir+day

if keyword_set(event) then begin
  spawn,'mkdir '+evdir
  imgsize,event,n2,nev
  for k=0,nev-1 do begin
    event1=event[*,k]
    etim1=(strmid(event1[0],0,2)*3600l+strmid(event1[0],3,2)*60l-300l)*1000l
    etim2=(strmid(event1[0],0,2)*3600l+strmid(event1[0],3,2)*60l)*1000l
    etim3=(strmid(event1[1],0,2)*3600l+strmid(event1[1],3,2)*60l)*1000l
    etim4=(strmid(event1[2],0,2)*3600l+strmid(event1[2],3,2)*60l)*1000l
    iie=where(ctim4 gt etim1 and ctim4 lt etim2)
    if iie[0] ne -1 then begin
    cfi5=cfi4[iie]
    hfi5=hfi4[iie]
    nne=n_elements(cfi5)
    print,nne,'  files in preflare phase'
    for i=0l,nne-1 do begin
      spawn,'copy '+cfi5[i]+' '+evdir,/hide
      spawn,'copy '+hfi5[i]+' '+evdir,/hide
      print,i,' /',nne,'   ',cfi5[i],'   ',hfi5[i]
    endfor
    endif
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
  

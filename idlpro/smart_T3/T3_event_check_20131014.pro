@'C:\Projects\IDLPRO\smart_T3\wd_sldbx.pro'
@'C:\Projects\IDLPRO\smart_T3\xmovie.pro'

; T3 event check
function version
  ver=0.1  ;  2012.02.06 TTI based on T3_select
  return,ver
end
;--------------------------------------------------------------------   
function fname2msec,fname,ip

tim=(strmid(fname,ip,2)*3600l+strmid(fname,ip+2,2)*60l $
    +strmid(fname,ip+4,2))*1000l+strmid(fname,ip+7,3)
return,tim

end


;--------------------------------------------------------------------   

;観測データ（生データ）の保存先


;dir='F:\T3\'
dir='E:\T3\'

day='20131013'

;selected/eventsの保存先
dirout='D:\T3\'  ;外付けHDDの電源を入れるのを忘れずに！

;dirout=dir

start_time=''

nn_file_log=intarr(2,3)
seldir=dirout+'selected\'+day

dt1=30*1000l  ; time interval for movie

dark_level=800

original_filename=dirout+'selected\'+day+'\'+day+'_original_files.sav'
ql_movie_filename=dirout+'selected\'+day+'\'+day+'_Ha_ql.sav'
pair_all_filename=dirout+'selected\'+day+'\'+day+'_pair_files_all.sav'
nn_log_filename=dirout+'selected\'+day+'\'+day+'_files_log.sav'
;delta_t_c_filename=dirout+'selected\'+day+'\'+day+'_delta_t_c.sav'
plot_filename=dirout+'selected\'+day+'\'+day+'_T3_plot.gif'

cd,dir+day
cfi=file_search('*\*\Co_*')
print,'loaded Co files, ',n_elements(cfi),' files'
hfi=file_search('*\*\Ha_*')
print,'loaded Ha files, ',n_elements(hfi),' files'



nn_file_log[0,0]=n_elements(cfi)
nn_file_log[1,0]=n_elements(hfi)



spawn,'mkdir '+seldir
save,cfi,hfi,file=original_filename
;-----  select simultaneous images ------
ip=strpos(cfi[0],'.tif',/reverse_search)-10
ctim=fname2msec(cfi,ip)
htim=fname2msec(hfi,ip)
if keyword_set(start_time) then begin
  stim=fname2msec(start_time,0)
  ii=where(ctim gt stim)
  ctim=ctim[ii]
  cfi=cfi[ii]
  ii=where(htim gt stim)
  htim=htim[ii]
  hfi=hfi[ii]
endif
nc=n_elements(cfi)
nh=n_elements(hfi)
hfi2=strarr(nc)
for i=0l,nc-1 do begin
  dti=abs(ctim[i]-htim)
  dmy=min(dti,i0)
  if dti[i0] lt 30 then hfi2[i]=hfi[i0]
endfor

ii=where(hfi2 ne '')
if ii[0] ne -1 then begin
  cfi2=cfi[ii]
  hfi2=hfi2[ii]
endif else begin
  cfi2=cfi
endelse

print,n_elements(cfi2),' pairs (dt<30msec) counted'

ctim2=fname2msec(cfi2,ip)
htim2=fname2msec(hfi2,ip)
;plot,ctim2-htim2
;ans=''
;read,'Ok? (y/n)',ans
;if ans eq 'n' then stop 

nn_file_log[0,1]=n_elements(cfi2)
nn_file_log[1,1]=n_elements(hfi2)


cfi99=cfi2
hfi99=hfi2

ii=where(abs(htim2-ctim2) lt 6)
cfi3=cfi2[ii]
hfi3=hfi2[ii]
ctim3=fname2msec(cfi3,ip)
htim3=fname2msec(hfi3,ip)

nn_file_log[0,2]=n_elements(cfi3)
nn_file_log[1,2]=n_elements(hfi3)

nn=n_elements(cfi3)

c_delta_t=fltarr(2,nn-1)
for i=0d,nn-2 do begin
c_delta_t[0,i]=(ctim3[i+1]+ctim3[i])/2.
c_delta_t[1,i]=ctim3[i+1]-ctim3[i]
endfor

;delta_t_c2=fltarr(4)
;delta_t_c2[0]=median(c_delta_t)
;delta_t_c2[1]=max(c_delta_t,index)
;delta_t_c2[2]=ctim3[index]
;delta_t_c2[3]=ctim3[index+1]
;
;print,'mean frame rate:',1000./delta_t_c2[0]
;print,'delta_t max:',delta_t_c2[1],'msec'


window,xs=1200,ys=900
!p.multi=[0,1,2]
plot,ctim2/1000./3600.,ctim2-htim2,psym=1, $
  title=day,xrange=[6,18],yrange=[-30,30],/xstyle,/ystyle, $
    ytitle='Difference Cont/Ha [msec]',xtitle='Hour [JST]'
plot,c_delta_t[0,*]/1000./3600.,c_delta_t[1,*]/1000.,/ylog,psym=1,$
  title=day,xrange=[6,18],yrange=[10,100000.]/1000.,/xstyle,/ystyle, $
    ytitle='Cadence (diff. < 6msec) [sec]',xtitle='Hour [JST]'
tmp=tvrd()
tmp2=abs(tmp-255)
write_gif,plot_filename,bytscl(tmp2)

;ans=''
;read,'Ok? (y/n)',ans
;if ans eq 'n' then stop 

!p.multi=[0,1,1]

print,nn,' pairs (dt<6msec) counted'
save,nn_file_log,file=nn_log_filename
;save,dleta_t_c2,file=delta_t_c_filename

;stop


;-----  select dt cadence ------
dark_check=intarr(nn)
dt=dt1

dur=ctim3[nn-1]-ctim3[0]

yadd=30
xs=800
ys=600
bg=bytarr(xs,yadd)
img=bytarr(xs,ys+yadd,round(dur/dt))
window,0,xs=xs,ys=ys+yadd

k=0l
t1=ctim3[0] &  t2=t1+dt
while t2 le ctim3[nn-1] do begin
    ii=where(ctim3 ge t1 and ctim3 lt t2, count)
    if count ne 0 then begin
    jj=0
    ck_jj=1
    while (jj lt count) and (ck_jj) do begin
      img1=read_tiff(hfi3[ii[jj]],/unsigned)
      if mean(img1) gt dark_level then begin
	wset,0 & wshow
	img2=rebin(img1,xs,ys)
	img2=rotate(img2,-1)
	tv,bg
        tvscl,img2,0,yadd
        xyouts,100,10,hfi3[ii[jj]],/dev,chars=1.5
	img3=tvrd()
	img(*,*,k)=img3
        ck_jj=0
	k=k+1
      endif else begin
      print,'%%% cloudy.....'
      jj=jj+100
      endelse
    endwhile
    endif
    t1=t2 & t2=t1+dt
endwhile

ha_ql_img=img(*,*,0:k-1)

save,ha_ql_img,file=ql_movie_filename

;-------------------------------------------------------
xmovie,ha_ql_img,/fit



end
  

;PRO ObsLib
;+
;
;  ObsLib.pro
;
;Functions for observation
;based on DST Poralization scripts by T. Anan
;
;20100512  T.K.
;20100815 T.K.
;
;
;========================headder==============================;
;pro pro_preview,img
;function gettime
;function NormalObs
;function FinishObs
;pro PrevObs
;function timestamp1,file
;
;
;-

;========================include==============================;
;;@CameraLib
;=========================main================================;



;**************************************************************
function gettime
;--------------------------------------------------------------

caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
second=string(seco,form='(i2.2)') & second0=second

caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
yyyymmdd_hhmmss=string(year,form='(i4.4)')+string(mon,form='(i2.2)')+string(day,form='(i2.2)') $
  +'_'+string(hour,form='(i2.2)')+string(minu,form='(i2.2)')+string(seco,form='(i2.2)')
time=yyyymmdd_hhmmss
return,time

end


;**************************************************************
function NormalObs,wp
;--------------------------------------------------------------

;CamInit
sttime=gettime()
imgs=Gigobs(wp.nimg)
entime=gettime()
;GrubImg,wp.nimg
;savefits,'NORMAL',sttime,entime,wp
;savestmpfits,sttime,wp
;CamFin
;filename=wp.svdir+wp.fname+strcompress(sttime,/remove_all)+'.fits'

filename=wp.svdir+wp.fname+strcompress(sttime,/remove_all)+'.sav'
save,sttime,entime,wp,imgs,file=filename

return,filename

end

;**************************************************************
function FinishObs
;--------------------------------------------------------------

entime=gettime()
;CamFin
return,entime

end

;**************************************************************
pro PrevObs,wp
;--------------------------------------------------------------
CamInit
CamSetParam,wp
if wp.Width gt 1000 then  window,0,xs=wp.Width/2,ys=wp.Height/2 $
  else  window,0,xs=wp.Width,ys=wp.Height
    img=intarr(wp.Width,wp.Height)
WSET, 0
  
PrevImg,img
CamFin
return
end


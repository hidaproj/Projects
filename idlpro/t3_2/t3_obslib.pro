;PRO T3_ObsLib
;+
;
;  T3_ObsLib.pro
;
;Functions for observation
;based on DST Poralization scripts by T. Anan
;
;20100512  T.K.
;
;
;
;========================headder==============================;
;pro pro_preview,img
;function gettime
;function NormalObs
;function FinishObs
;pro PrevObs
;pro MessageBox,msg
;function timestamp1,file
;
;
;-

;========================include==============================;
@T3_CameraLib
;=========================main================================;



;**************************************************************
function gettime
;--------------------------------------------------------------
;T3_ParamLib,prodll,wpp


caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
second=string(seco,form='(i2.2)') & second0=second
;while (second eq second0) do begin
;  caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
;  second=string(seco,form='(i2.2)')
;endwhile

;msec0=call_external(prodll,'getime',/all_value,/cdecl)


caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
yyyymmdd_hhmmss=string(year,form='(i4.4)')+string(mon,form='(i2.2)')+string(day,form='(i2.2)') $
  +'_'+string(hour,form='(i2.2)')+string(minu,form='(i2.2)')+string(seco,form='(i2.2)')
;msec=call_external(prodll,'getime',/all_value,/cdecl)

;time0=msec-msec0
;length=strlen(string(time0))
;msec=strmid(string(time0),(length-3),3)

;time=yyyymmdd_hhmmss+msec
time=yyyymmdd_hhmmss
return,time

end


;**************************************************************
function NormalObs,wp
;--------------------------------------------------------------

T3_CamInit
T3_CamSetParam,wp
sttime=gettime()
T3_GrubImg,wp
entime=gettime()
savefits,'NORMAL',sttime,entime,wp
;savestmpfits,sttime
T3_CamFin

filename=wp.svdir+wp.fname+strcompress(sttime,/remove_all)+'.fits'
return,filename

end

;**************************************************************
function FinishObs
;--------------------------------------------------------------

entime=gettime()
T3_CamFin
return,entime

end

;**************************************************************
pro PrevObs,wp
;--------------------------------------------------------------
  T3_CamInit
  T3_CamSetParam,wp
  if wp.Width gt 1000 then  window,0,xs=wp.Width/2,ys=wp.Height/2 $
    else  window,0,xs=wp.Width,ys=wp.Height
      img=intarr(wp.Width,wp.Height)
  WSET, 0
  
  T3_PrevImg,img
  T3_CamFin
return
end



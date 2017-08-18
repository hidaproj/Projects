;PRO T3_CameraLib
;+
;
;  T3_CameraLib
;
;Functions for camera (prosillica)
;based on DST Poralization scripts by T. Anan
;
;20100512  T.K.
;
;========================headder==============================;
;pro T3_CamInit
;pro T3_CamFin
;pro T3_CamSetParam,wp
;pro T3_GrubImg,wp
;PRO T3_PrevImg,img
;pro savefits,obs,pol,sttime,entime,wp
;pro savestmpfits,sttime
;
;-

;========================include==============================;
@T3_ParamLib
;==========================main===============================;

;**************************************************************
PRO T3_CamInit
;--------------------------------------------------------------
T3_ParamLib,prodll,wpp

err=call_external(prodll,'CamInit',/all_value,/cdecl)


END

;**************************************************************
PRO T3_CamFin
;--------------------------------------------------------------
T3_ParamLib,prodll,wpp

err=call_external(prodll,'CamFin',/all_value,/cdecl)

END

;**************************************************************
PRO T3_CamSetParam,wp
;--------------------------------------------------------------
T3_ParamLib,prodll,wpp


p={prosilica_param,   $
   expo:         wp.expo,          $   ; exposure time [usec]
   framerate:    wp.framerate,     $   ; frame rate [frame/sec]
   gain:         wp.gain,          $   ; gain 0-28
   nimg:         wp.nimg,          $   ;  # of images
   binx:         wp.binx,          $   ; Binning X 1-8
   biny:         wp.biny,          $   ; Binning Y 1-200
   Height:       wp.Height,        $   ; Height  max=1200 (biny=1)
   Width:        wp.Width,         $   ; Width  max=1600 (binx=1)
   RegionX:      wp.RegionX,       $   ; start of region read out,pixel,left edge
   RegionY:      wp.RegionY,       $   ; start of region read out,pixel,top edge
   clock:        wp.clock,         $   ; TimeStanmpFrequency [Hz]
   timelo:       wp.timelo,        $   ; Time stamp, lower 32-bits
   timehi:       wp.timehi        $   ; Time stamp, upper 32-bits
}

help,p,/st

err=call_external(prodll,'SetParam',    $
                            wp.expo,      $
                            wp.framerate, $
                            wp.gain,      $
                            wp.binx,      $
                            wp.biny,      $
                            wp.Width,     $
                            wp.Height,    $
                            wp.RegionX,   $
                            wp.RegionY,   $
                            /all_value,/cdecl)

END

;**************************************************************
PRO T3_GrubImg,wp
;--------------------------------------------------------------
T3_ParamLib,prodll,wpp

err=call_external(prodll,'GrabImg',wp.nimg,/all_value,/cdecl)

END


;**************************************************************
PRO T3_PrevImg,img
;--------------------------------------------------------------
T3_ParamLib,prodll,wpp


r=call_external(prodll,'GrabImg',1,/all_value,/cdecl)
r=call_external(prodll,'DivBuf',0,/all_value,/cdecl)
r=call_external(prodll,'GoIdl',img,/cdecl)

if n_elements(img[*,0]) gt 1000 then  $
  tvscl,congrid(img,n_elements(img[*,0])/2,n_elements(img[0,*])/2) $
  else tvscl,img
plot,findgen(50)/50*4096,histogram(img,max=4096,min=0,nbins=50), $
    /noerase,/xstyle,charsize=0.5,position=[0.05,0.05,0.25,0.2],color=0

end


;**************************************************************
PRO savefits,obs,sttime,entime,wp
;--------------------------------------------------------------
T3_ParamLib,prodll,wpp


outfile=wp.svdir+wp.fname+'_'+strcompress(sttime,/remove_all)+'.fits'
print,outfile 

;if (obs ne 'NORMAL') then dstangl,hangl,zd
;  dstangl,hangl,zd
;hangle=strcompress(string(hangle),/remove_all) & zd=strcompress(string(zd),/remove_all)
ra=wp.ra_h+'h'+wp.ra_m+'m'+wp.ra_s+'s'
dec=wp.dec_d+'d'+wp.dec_m+'m'+wp.dec_s+'s'
kusabi=wp.kusabi_f+'f'+wp.kusabi_r+'r'

r=call_external(prodll,'openfits',outfile,wp.svdir,/all_value,/cdecl)

r=call_external(prodll,'WriteImage',wp.nimg,/all_value,/cdecl)
r=call_external(prodll,'addkeywords_prosilica',     $
          obs,    $
          wp.wavelength,  $
          sttime,   $
          entime,   $
          ra,   $
          dec,    $
          kusabi,    $
           'T3_continuum',  $
          /all_value,/cdecl)
r=call_external(prodll,'closefits',/all_value,/cdecl)

END

;**************************************************************
PRO savestmpfits,sttime
;--------------------------------------------------------------
T3_ParamLib,prodll,wpp


outfile=wp.svdir+'stmp'+sttime+'.fits'
print,outfile 

r=call_external(prodll,'TimeStamp',outfile,wp.nimg,/all_value,/cdecl)

END


;**************************************************************
pro MessageBox,msg
;--------------------------------------------------------------
T3_ParamLib,prodll,wpp

r=call_external(prodll,'MSGBox', msg, /PORTABLE )

end




;PRO CameraLib
;+
;
;  CameraLib
;
;Functions for camera (prosillica)
;based on DST Poralization scripts by T. Anan
;
;20100512  T.K.
;20100815 T.K.
;
;========================headder==============================;
;pro CamInit
;pro CamFin
;pro CamSetParam,wp
;pro GrubImg,wp
;PRO PrevImg,img
;pro savefits,obs,pol,sttime,entime,wp
;pro savestmpfits,sttime
;pro MessageBox,msg
;-

;========================include==============================;
@ParamLib
;==========================main===============================;

;**************************************************************
PRO CamInit
;--------------------------------------------------------------
ParamLib,prodll,wpp

err=call_external(prodll,'CamInit',/all_value,/cdecl)


END

;**************************************************************
PRO CamFin
;--------------------------------------------------------------
ParamLib,prodll,wpp

err=call_external(prodll,'CamFin',/all_value,/cdecl)

END

;**************************************************************
PRO CamSetParam,wp
;--------------------------------------------------------------
ParamLib,prodll,wpp


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
PRO GrubImg,wp
;--------------------------------------------------------------
ParamLib,prodll,wpp

err=call_external(prodll,'GrabImg',wp.nimg,/all_value,/cdecl)

END


;**************************************************************
PRO PrevImg,img
;--------------------------------------------------------------
ParamLib,prodll,wpp


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
ParamLib,prodll,wpp


outfile=wp.svdir+wp.fname+'_'+strcompress(sttime,/remove_all)+'.fits'
print,outfile 

;if (obs ne 'NORMAL') then dstangl,hangl,zd
;  dstangl,hangl,zd
;hangle=strcompress(string(hangle),/remove_all) & zd=strcompress(string(zd),/remove_all)

r=call_external(prodll,'openfits',outfile,wp.svdir,/all_value,/cdecl)

r=call_external(prodll,'WriteImage',wp.nimg,/all_value,/cdecl)
r=call_external(prodll,'addkeywords_prosilica',     $
          obs,    $
          wp.wavelength,  $
          sttime,   $
          entime,   $
          wp.observatory,   $
          wp.telescope,    $
          wp.program,    $
          /all_value,/cdecl)
r=call_external(prodll,'closefits',/all_value,/cdecl)

END

;**************************************************************
PRO savestmpfits,sttime,wp
;--------------------------------------------------------------
ParamLib,prodll,wpp


outfile=wp.svdir+'stmp_'+wp.fname+'_'+strcompress(sttime,/remove_all)+'.fits'
print,outfile 

r=call_external(prodll,'TimeStamp',outfile,wp.nimg,/all_value,/cdecl)

END

;**************************************************************
pro MessageBox,msg
;--------------------------------------------------------------
ParamLib,prodll,wpp

r=call_external(prodll,'MSGBox', msg, /PORTABLE )

end


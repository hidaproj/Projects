PRO T3_ParamLib,prodll,wp

;+
;
;  T3_ParamLib.pro
;
;Global (common) variables and parameters difinition for SMART T3 observation
;based on DST Poralization scripts by T. Anan
;
;20100512  T.K.
;
;
;-

;*************************************************************
;address of prosilica DLL
cd,'C:\Projects\cprog\VS2008\prosilica'

prodll='C:\Projects\cprog\VS2008\prosilica\Debug\prosilica.dll'


;*************************************************************
;default parameters for observation

  wp={widget_param, $
      wavelength: '6405',       $   ; wave length observed [A]
      framerate:   30,          $   ; frame rate [frame/sec]
      expo:        300l,         $   ; exposure time [usec]
      gain:        0,           $   ; gain 0�`28
      nimg:        100,         $   ;  # of image 
      binx:        1,           $   ; Binning X 1�`8
      biny:        1,           $   ; Binning Y 1�`1200
      Height:      1200,         $   ; Height  max=1200 (biny=1)
      Width:       1600,         $   ; Width  max=1600 (binx=1)
      RegionX:     0,           $   ; start of region read out,pixel,left edge
      RegionY:     0,           $   ; start of region read out,pixel,top edge
      clock:       79861111l,  $   ; TimeStanmpFrequency [Hz]
      timelo:      0,           $   ; Time stamp, lower 32-bits
      timehi:      0,           $   ; Time stamp, upper 32-bits
      svdir:       'D:\test_wl\20100605\', $   ; save directory
      fname:       'ss',     $   ; head of file name 
      nf:          1,           $   ; number of files
      ra_h:         '00',         $  
      ra_m:         '00',         $  
      ra_s:         '00',         $
      dec_d:        '000',         $ 
      dec_m:        '00',         $ 
      dec_s:        '00',         $ 
      kusabi_f:     '000',         $
      kusabi_r:     '000'         $ 
     }

RETURN
END
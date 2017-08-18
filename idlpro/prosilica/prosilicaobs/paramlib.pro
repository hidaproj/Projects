PRO ParamLib,prodll,wp
;+
;
;  ParamLib.pro
;
;Global (common) variables and parameters difinition
;based on DST Poralization scripts by T. Anan
;
;20100512  T.K.
;20100815  T.K.
;
;-

;*************************************************************
;address of prosilica DLL

;cd,'C:\Projects\cprog\VS2008\prosilica_cprog'

;prodll='C:\Projects\cprog\VS2008\prosilica_cprog\Debug\prosilica.dll'
prodll='C:\Projects\cprog\VS2010\prosilica_64\x64\Debug\prosilica_64.dll'

caldat,systime(/JULIAN), mon , day , year
yyyymmdd=string(year,format='(i4.4)')+string(mon,format='(i2.2)')+string(day,format='(i2.2)')
;*************************************************************
;default parameters for observation

  wp={widget_param, $
      wavelength:  '6563',       $   ; wave length observed [A]
      framerate:   30,          $   ; frame rate [frame/sec]
      expo:        50l,         $   ; exposure time [usec]
      gain:        0,           $   ; gain 0-28
      nimg:        100,         $   ;  # of image 
      binx:        1,           $   ; Binning X 1-8
      biny:        1,           $   ; Binning Y 1-1200
      Height:      1200,         $   ; Height  max=1200 (biny=1)
      Width:       1600,         $   ; Width  max=1600 (binx=1)
      RegionX:     0,           $   ; start of region read out,pixel,left edge
      RegionY:     0,           $   ; start of region read out,pixel,top edge
      SeqInt:      0,           $   ; Sequence Interval
      clock:       79861111l,  $   ; TimeStanmpFrequency [Hz]
      timelo:      0,           $   ; Time stamp, lower 32-bits
      timehi:      0,           $   ; Time stamp, upper 32-bits
      svdir:       'D:\test\'+yyyymmdd+'\', $   ; save directory
      fname:       'ss',     $   ; head of file name 
      nf:          1,           $   ; number of files
      observatory: 'Hida Observatory',         $   ; Observatory name
      telescope:   'DST',         $   ;Telescope name
      program:      ' '        $    ;Observation Program
     }
RETURN
END
;+
; NAME       : dskfind_smt (procedure)
; PURPOSE    :
;	find center of occulting disk & its radius
; CATEGORY :
;	idl/lib/nkr
; CALLING SEQUENCE :
;	dskfind_smt, img, mask, RSunPix, ox, oy, r, x=x, y=y, robimg=robimg
;
; INPUTS :
; 	img     -- full sun data
;       mask    -- mask (2D array, [1: valid area, 0:masked area]).
;       RSunPix -- expected solar radius in pixel.
; OPTIONAL INPUT PARAMETERS : 
;	none
; KEYWORD PARAMETERS :
;	x,y	 -- return edge position
;	robimg	 -- return edge image
; OUTPUTS :
;	ox,oy,r  -- center and radius
; COMMON BLOCKS : 	none
; RESTRICTIONS :	none
; PROCEDURE :
; MODIFICATION HISTORY :
;	98/03/11	K.I.
;	98/07/05	K.I.    side cut robimg
;	99/01/07	K.I.    ox,oy +1
;	03/03/10	K.I.    r_deriv keyword
;	09/08/10	K.I.    cut keyword
;	10/03/25	K.I.    ox,oy +1 -> 0
;	11/10/12	S.M.    add mask, and cleaning, and remove some block for
;	                        SMART T1 Obs soft.
;-

;*************************************************************************
PRO dskfind_smt, img, mask, RSunPix, ox, oy, r, x=x, y=y, robimg=robimg

;### Set some parameters ###
coef_stddev      = 3.
smooth_num       = 8

;min_num_edgepos  = 1e+5 ; [pix] from an experiment
min_num_edgepos  = 1e+4 ; [pix] from an experiment

RSunPix_margin_1 = 0.2   ; = 20  [%]; for 1st cleaning.
RSunPix_margin_2 = 0.1   ; = 10  [%]; for 2nd cleaning.  
RSunPix_margin_3 = 0.05  ; =  5  [%]; for 3rd cleaning.  
RSunPix_margin_4 = 0.025 ; =  2.5[%]; for 4th cleaning.  
;###########################

start = SYSTIME(1)

  ;+ *** calc some parameters
  IMGSIZE, img, nx, ny

  ss_use  = WHERE(mask EQ 1)
  ss_mask = WHERE(mask NE 1)

  RSunPixdiff1 = RSunPix * RSunPix_margin_1
  RSunPixdiff2 = RSunPix * RSunPix_margin_2
  RSunPixdiff3 = RSunPix * RSunPix_margin_3
  RSunPixdiff4 = RSunPix * RSunPix_margin_4
  ;- *** calc some parameters

  ;+ **** smoothing image for avoiding noize
  uimg   = SMOOTH(img, smooth_num)
  ;- **** smoothing image for avoiding noize

  ;+ **** make an edge image
  robimg = ROBERTS(uimg)
  ;- **** make an edge image

  ;+ **** apply mask
  robimg[mask] = 0
  ;- **** apply mask

  ;+ **** make an initial guess
  ii  = WHERE(robimg GT ( MEAN(robimg[ss_use]) + $
                          coef_stddev * STDDEV(robimg[ss_use]) ) )
    x = ii - ii/nx*nx
    y = ii/nx

  CIRCLFIT, x, y, ox, oy, r
  ;- **** make an initial guess

  ;+ **** Do cleaning
  rr = SQRT( (x - ox)^2 + (y - oy)^2 )
  ss = WHERE( ABS(rr-RSunPix) LT RSunPixdiff1, sn)

  IF sn GT min_num_edgepos THEN BEGIN
    x = x[ss]
    y = y[ss]

    CIRCLFIT, x, y, ox, oy, r

    ;+=== cleaning 2nd ===
    rr = SQRT( (x - ox)^2 + (y - oy)^2 )
    ss = WHERE( ABS(rr-RSunPix) LT RSunPixdiff2, sn)

    IF sn GT min_num_edgepos THEN BEGIN
      x = x[ss]
      y = y[ss]

      CIRCLFIT, x, y, ox, oy, r

      ;+=== cleaning 3rd ===
      rr = SQRT( (x - ox)^2 + (y - oy)^2 )
      ss = WHERE( ABS(rr-RSunPix) LT RSunPixdiff3, sn)

      IF sn GT min_num_edgepos THEN BEGIN
        x = x[ss]
        y = y[ss]

        CIRCLFIT, x, y, ox, oy, r

        ;+=== cleaning 4th ===
        rr = SQRT( (x - ox)^2 + (y - oy)^2 )
        ss = WHERE( ABS(rr-RSunPix) LT RSunPixdiff4, sn)

        IF sn GT min_num_edgepos THEN BEGIN
          x = x[ss]
          y = y[ss]

          CIRCLFIT, x, y, ox, oy, r
        ENDIF
        ;-=== cleaning 4th ===
      ENDIF
      ;-=== cleaning 3rd ===
    ENDIF
    ;-=== cleaning 2nd ===

  ENDIF
  ;- **** Do cleaning

PRINT, SYSTIME(1) - start, '[sec]'

END

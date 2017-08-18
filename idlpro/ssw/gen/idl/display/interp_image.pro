;---------------------------------------------------------------------------
; Document name: interp_image.pro
; Time-stamp: <Thu Mar 17 2005 10:35:47 csillag darksun>
;---------------------------------------------------------------------------
;
;+
; PROJECT:
;       HESSI
;
; NAME:
;       INTERP_IMAGE()
;
; PURPOSE:
;       Function to congrid an image within a specific coordinate system.
;       Used primarily with the specplot__define utility, more
;       precisely spectro_plot_obj, but it could probably
;       be used elsewhere too. In fact it's a kind of generalization
;       of congrid, where we assiume 
;
; CATEGORY:
;       gen/display
;
;
; CALLING SEQUENCE:
;       result =  interp_image( image, xaxis, yaxis, nx, ny, $
;                 [, /YLOG] [, /SMOOTH] , $
;                 [xrange = xrange] [, yrange = yrange]
; INPUTS:
;       image- a two dimentional axis containing the image to map
;       xaxis, yaxis- the vectors containing the axis values
;       nx, ny- then number of pixels of the resulting array
;
;       you need to pass the range too -- check x/y range keywords
;
; OUTPUTS:
;       result - a 2d array with the dimensions nx, ny
;
; KEYWORDS:
;       smooth - tells that the image result shoudl be interpolated
;       xrange, yrange- the range on which the values should be mapped
;                       to. this helps to take care of the fact that
;                       the axis passed contain mean bin values and
;                       the plot nneds to extend them in case of small
;                       numbero of channels, for instance.
;       ylog - tells that the y axis will be displayed in log scale
;
; HISTORY:
;       july 2004 - added the xrange / yrange kwds to deal correctly
;                   with the case of small number of elements.
;       april 2004:  based on a program of davin
;---------------------------------------------------------------------------

FUNCTION interp_axis, axis, npixel, crange, SMOOTH=smooth
; returns the axis values needed by the function interpolate
; the new axis contains a vector of npixel elements with
; values from 0...n_elements(axis)-1 in the number of pixels

pixel_point = findgen( npixel ) * ( crange[1]-crange[0] ) / (npixel-1)  + crange[0]
new_axis = interpol( Findgen( N_Elements( axis ) ), axis, pixel_point )

IF not smooth THEN new_axis = round( new_axis )

return, new_axis

END

;---------------------------------------------------------------------------

FUNCTION  interp_image, image, xaxis, yaxis, nx, ny, YLOG=ylog, SMOOTH=smooth, $
  xrange = xrange, yrange = yrange, xlog = xlog

;checkvar, xrange, !x.crange
;checkvar, yrange, !y.crange

IF Keyword_Set( YLOG ) THEN BEGIN
    yy_before = alog10( yaxis )
    yrange = alog10( yrange )
ENDIF ELSE BEGIN
    yy_before = yaxis
ENDELSE
IF Keyword_Set( XLOG ) THEN BEGIN
    xx_before = alog10( xaxis )
    xrange = alog10( xrange )
ENDIF ELSE BEGIN
    xx_before = xaxis
ENDELSE

yy = interp_axis( yy_before, ny,  yrange,  smooth= smooth ) $
  > 0 <  (N_Elements( yy_before ) -1)
xx = interp_axis( xx_before, nx, xrange,  smooth= smooth ) $
  > 0 <  (N_Elements( xx_before ) -1)

; the interpolation needs to be done in float and reconverted in 
; bytes after. otherwise we get an arithmetic error all the time
if size( image, /type ) eq 1 then image = float( image )
ret = interpolate( image, xx, yy, /grid, missing=!values.f_nan)

return, ret

END

;---------------------------------------------------------------------------
; End of 'interp_image.pro'.
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; Document name: spectro_plot2
; Time-stamp: <Fri Jul 15 2005 15:56:22 csillag auriga.ethz.ch>
;---------------------------------------------------------------------------
;+
;NAME:
;   spectro_plot2
;
;PROJECT:
;   Generic Display utilities
;
;CATEGORY:
;   /ssw/gen/display
;
;PURPOSE:
;       This routine is the core spectrogram display tool which is either
;       called by the object specplot__define, or can also be used from the
;       command line via spectro_plot. Please use one of those programs and
;       not directly thsi one (although it shoudl be quite possible)
;
;       This program merges functionalities of the programs tplot, rapp_plot_spectrogram,
;       and show_image. This is a complete rewrite of the spectro_plot
;       utility that was built on show_image uniquely.
;
;
;HISTORY:
;       acs jul 2005 - just some changes in the doc and adding a test for 2d
;                      when image passed directly. I realize that cba and
;                      invert do not work correctly, I'll have to correct this.
;       acs mar 2005 - minor changes to make it work correctly with
;                      spectrogram__define and plotman.
;       acs mar 2005 - removed the object dependancy and made it a standalone
;                      routine by popular demand. renamed to spectro_plot2,
;                      the cli user interface is spectro_plot
;       acs feb 2005 - activated the no_ut and xlog options, some significant
;                      corrections on the x axis time display (in fact i throw
;                      away the axis calls and replace with another utplot call)
;       acs july 2004- some changes related to the problems of
;                      displaying correctly only a few channels
;       acs april 2004: release as spectro_plot_obj
;       acs july 2003: corrected way of displaying spectrograms with
;                      only few channels
;       acs june 2003: now fully featured xstyle and ystyle keywords
;       acs may 2003: fixing the x/y range problem and refactoring session
;       ACS April 2003: including feedback from kim & paolo
;       ACS March 2003: after meeting with AOBenz, PSG and PG extended version
;                       with YINTEGRATE, NO_INTERPOLATE, YRANGE
;                       PS capabilities checked
;       ACS Feb 2003   : make sure it works with other plot routines
;       ETH Zurich       for integration into the plotman utilities
;                        (i.e. zooming etc )
;                        csillag@ssl.berkeley.edu. This is basically a
;                        merge between the functions of Pascal
;                        Saint-Hilaire, Paolo Grigis and Peter Messmer
;                        with the show_image
;                        - ps option modified to be able to set the ps
;                        option outside of the routine
;                        - replacement of the call to utplot with the
;                        set_utaxis and then axis
;                        - interpolation taken from Davin (tplot)
;                        - compatibility with tplot
;                        - utplot is now default
;                        - full logarithm implemented
;       ACS Jan 2003: merged several version of the same program into
;                     the first prototy of this generic
;                     routines. Contributions from all people listed above.
;
;--------------------------------------------------------------------------------

pro spectro_plot2_test

spectro_plot2, dist(100)

spectro_plot2, dist(100), /interp

spectro_plot2, dist(100), /no_ut

spectro_plot2, dist(100), /no_ut, /xlog

spectro_plot2, dist(100), /no_ut, /xlog, /ylog

spectro_plot2, dist(100), findgen(100), /no_ut

spectro_plot2, dist(100), findgen(100)+10, /no_ut

spectro_plot2, dist(100), findgen(100)+10, /no_ut, /xlog

spectro_plot2, dist(100), findgen(100)+10, /no_ut, /xlog, xs = 1

spectro_plot2, dist(100), findgen(100)+11, /no_ut, /xlog, xs = 1

spectro_plot2, dist(100), findgen(100)+9, /no_ut, /xlog, xs = 1

spectro_plot2, dist(100), findgen(100)+9, /no_ut, /xlog, xs = 3

spectro_plot2, dist(100), findgen(100)+9, findgen(100) + 1, /no_ut, /xlog, xs = 3

spectro_plot2, dist(100), findgen(100)+9, findgen(100) + 1, /no_ut, /xlog, xs = 3, ys = 1

spectro_plot2, dist(100), findgen(100)+9, findgen(100) + 1, /no_ut, /xlog, xs = 3, ys = 3

spectro_plot2, dist(100), findgen(100)+9, findgen(100) + 1, /no_ut, /xlog, xs = 3, ys = 3, /ylog

spectro_plot2, dist(100), findgen(100)+1, findgen(100) + 1, /no_ut, /xlog, xs = 3, ys = 3, /ylog

spectro_plot2, dist(100), findgen(100)+100, xrange = [150, 160]

spectro_plot2, dist(100), findgen(100)+100, xrange = [150, 160], /no_ut, xs = 3

spectro_plot2, dist(100), findgen(100)+100, xrange = [150, 160], /no_ut, xs = 3, /interp

spectro_plot2, dist(100), findgen(100)+100, xrange = [150, 160], /no_ut, xs = 3, /interp, yrange = [45, 65]

spectro_plot2, dist(100), findgen(100)+100, xrange = [150, 160], /no_ut, xs = 3, /interp, yrange = [45, 65], ys = 3

time_axis = anytim( '2002/02/04 11:02:45' ) + findgen(100)
spectro_plot2, dist(100), time_axis
spectro_plot2, dist(100), time_axis, xs = 3, xrange = time_axis[0] + [10,20]


end

;--------------------------------------------------------------------------------

PRO spectro_plot2, image_param, xaxis_param, yaxis_param, $
                   verbose  =verbose, debug = debug, $
                   bottom =bottom, $
                   cbar=cbar, $
                   drange = drange, $
                   EXP_scale=exp_scale, $
                      hist_equal_percent = hist_equal_percent, $
                      INTERPOLATE=interpolate, $
                      INVERT=invert, $
                      log_scale = log_scale, $
                      ncolors=ncolors, $
                   no_interpolate = no_interpolate, $
                      no_ut = no_ut, $
                      POSTSCRIPT=POSTSCRIPT, $
                   reverse = reverse, $
                      TIMERANGE=timerange, $
                      TITLE=title, $
                      xcharsize = xcharsize, $
                      xlog = xlog, $
                      XRANGE=xrange, $
                      xstyle  = xstyle, $
                      XTITLE=xtitle, $
;                      YINTEGRATE=yintegrate, $
                      YLOG=ylog, $
                      YRANGE=yrange, $
                      ystyle = ystyle, $
                      YTITLE=ytitle, $
                      zexp = zexp, $
                      zlog = zlog, $
                   yes_nextone = yes_nextone, $
                      _EXTRA=_extra

tpp = size( image_param, /type )
if tpp eq 11 then begin
    o = image_param
    image = reform( o->get( /ydata ) )
    yes_obj = 1B
endif else begin
    if size( image_param, /n_dim ) ne 2 then begin 
        message, 'Image must be 2-dimensional', /cont
        return
    endif
    image = image_param
    yes_obj = 0B
endelse

no_ut = exist( no_ut ) ? no_ut : yes_obj ? o->get( /no_ut ) : 0

; during the development:
checkvar, verbose, 1
checkvar, debug, 1

image_size = Size( image, /structure )
IF image_size.n_elements LE 1 THEN BEGIN
    message, 'Cannot display a single element -- returning', /CONTINUE
    RETURN
ENDIF
IF image_size.n_dimensions GT 2 THEN BEGIN
    MESSAGE, 'Cannot display a data set with more than two dimensions -- returning', /CONTINUE
    RETURN
ENDIF

nx = image_size.dimensions[0]
ny = image_size.dimensions[1]

if not exist( xaxis_param ) then begin
    if yes_obj then begin
        xaxis = o->getprop( /xdata )
    endif else xaxis = lindgen(nx)
endif else xaxis = xaxis_param
if not exist( yaxis_param ) then begin
    if yes_obj then begin
        yaxis = o->getprop( /dim1_vals )
    endif else yaxis = lindgen(ny)
endif else yaxis = yaxis_param
if keyword_set( reverse ) then  begin
    yaxis = reverse( yaxis )
    image = reverse( image, 2 )
endif

; deal with utbase
;if not no_ut then begin
    utbase =  anytim( yes_obj ? o->get(/utbase) : 0 )
    dprint,'% UTBASE: ',utbase
;endif

; deal with xrange_local. If this is not the first plot to be drawn, then we
; have to use the !crange stuff, otherwise it will be wrong.
if keyword_set( yes_nextone ) then begin
    xrange_local = !x.crange + getutbase()
endif else begin
    if valid_range( xrange ) then begin
        xrange_local = xrange
        if not no_ut then begin
            xrange_local_temp = anytim( xrange_local  )
; without the float it interprets it as a single number, that's a problem
; because anytim has a format that has 2 ints (that is really never used as
; far as I know)
            if n_elements( xrange_local ) ne n_elements( xrange_local_temp ) then  $
              xrange_local_temp = anytim( float( xrange_local )  )
            xrange_local = xrange_local_temp
        endif
    endif else if valid_range( timerange ) then begin
        if size( timerange, /type ) eq 2 then timerange = double( timerange )
        xrange_local = no_ut ? timerange : anytim( timerange )
    endif  else if not valid_range( xrange_local ) then begin
        if yes_obj then begin
            xrange_local =  o->get( /xrange )
        endif else xrange_local = [0,0]
    endif else xrange_local = [0,0]
endelse

if valid_range( yrange ) then begin
    yrange_local = yrange
endif else if valid_range( spectrum_range ) then begin
    yrange_local = spectrum_range
endif else if yes_obj then begin
    yrange_local = o->get( /yrange )
endif else yrange_local = [0,0]

title_local = exist( title ) ? title : yes_obj ? o->get(/title) :  ''

if not exist( log_scale ) then begin
    if not exist( zlog ) then zlog =  yes_obj ? o->get(/log_scale) : 0
endif else zlog = log_scale
if not exist( exp_scale ) then begin
    if not exist( zexp ) then zexp =  yes_obj ? o->get(/exp_scale) : 0
endif else zexp = exp_scale
if zlog and not is_string( title_local ) then title_local = title_local + ' (Log)'
if zexp and not is_string( title_local ) then title_local = title_local + ' (Exp)'

if not exist( ylog ) then ylog = yes_obj ? o->get( /ylog ) : 0
if not exist( xlog ) then xlog = yes_obj ? o->get( /xlog ) : 0
if not no_ut and xlog then xlog = 0

; perhaps needs to send a message for this ?
if ylog then yrange_local = yrange_local > 1

if not exist( xtitle ) then begin
    xtitle_tmp = yes_obj ? o->get( /xtitle ) : ''
; dont set this var if it xtitle is '' otherwise utplot does not
; behave as expected
    if is_string( xtitle_tmp ) then xtitle_local = xtitle_tmp
endif else if xtitle ne '' then xtitle_local = xtitle

if not exist( xstyle ) then xstyle = yes_obj ? o->get( /xstyle ) : !x.style
if keyword_set( yes_nextone ) then xstyle = 1

if not exist( ystyle ) then ystyle = yes_obj ? o->get( /ystyle ) : !y.style

if nx eq 1 then begin
    message, 'Cannot display line plots with only one x value -- returning', /continue
    return
endif
;kim -

minmax_image_orig = minmax(image)
IF zlog THEN begin
    image = Alog( image  + abs( min(image) ) + 1 )
endif
if zexp then begin
    image = Exp(image )
endif
IF exist( invert ) ? invert : yes_obj ? o->get(/INVERT) : 0 THEN image = max(image) - image
minmax_image = minmax( image )

nodata = 1
;    ystyle = 7  ; kim changed from 4 to 7
;    IF NOT keyword_Set( ystyle ) THEN ystyle = 4 ELSE ystyle = ystyle + 4
;    xstyle = 3
;    normal case
IF NOT valid_range(yrange_local) THEN BEGIN ; kim moved this block from just before axis call
    yrange_local = minmax( axis_get_edges( yaxis ) )
ENDIF
IF yaxis[0] GT last_item(yaxis) and yrange_local[0] lt last_item(yrange_local) THEN $
                    yrange_local = yrange_local[[1, 0]]


if not is_number( charsize ) then charsize = !p.charsize
;if charsize eq 0 then charsize = 0.8
; mmmm this dies not work if there has not been any plots before...
; charsize = ch_scale(charsize, /xy)

if not is_number( xcharsize ) then  begin
    xcharsize = charsize* yes_obj ? o->getprop( /xcharsize ) : !x.charsize
endif

IF xcharsize NE 0 AND xcharsize LT 0.05 THEN ymargin = [0, !y.margin[1]]

this_xstyle = xstyle ;MOD 4 + 4
this_ystyle = ystyle ;MOD 4 + 4

cbar = exist(cbar)? cbar : (yes_obj ? o->get(/cbar) : 1 )
if cbar then begin
    position=[.1,.1,.9,.88]
endif

if exist( ytitle ) then begin
    ytitle_local = ytitle
endif else begin
    ytitle_local = yes_obj ? o->getprop( /ytitle ) : ''
endelse

if total(xrange_local ne 0.) then xrange_local = xrange_local - utbase

IF NOT no_ut THEN BEGIN
; we're trying to use only utbase 0
    utplot, xaxis, yaxis[*, 0], utbase, /nodata, $
;        TIMERANGE=xrange_local + utbase, $
        xrange = xrange_local, $
        title = title_local, $
        YRANGE=yrange_local, $
        charsize = charsize, $
        YLOG=ylog, XCHARSIZE=xcharsize, $
        XTITLE=xtitle_local, position = position, $
        YMARGIN=ymargin, xstyle = this_xstyle, $
        ystyle = this_ystyle, $
        ytitle = ytitle_local, _extra = _extra

ENDIF ELSE BEGIN

    plot, xaxis, yaxis[*, 0], /nodata, $
      XRANGE=xrange_local, $
        title = title_local, $
        YRANGE=yrange_local, $
        charsize = charsize, $
        YLOG=ylog, XCHARSIZE=xcharsize, xlog = xlog, $
        XTITLE=xtitle_local, position = position, $
        YMARGIN=ymargin, xstyle = this_xstyle, $
        ystyle = this_ystyle, $
        ytitle = ytitle_local, _extra = _extra

ENDELSE

if total( !p.multi ) ne 0 then !p.multi[0] = !p.multi[0]+1

; ok now plot the image

; get the index of the first and last axis element within the range
;x_limit = xaxis_obj->get_axis_limits( )
;y_limit = yaxis_obj->get_axis_limits( )
x_limit = axis_get_limits( xaxis, xrange_local )
y_limit = axis_get_limits( yaxis, yrange_local )

; reduce the image/axes to these limits
image = image[x_limit[0]:x_limit[1], $
              y_limit[0]:y_limit[1]]
xaxis = xaxis[x_limit[0]:x_limit[1]]
yaxis = yaxis[y_limit[0]:y_limit[1]]

IF n_elements(xaxis) lt 2 or n_elements(yaxis) lt 2 THEN BEGIN
    message, 'Not enough points to plot -- returning', /CONTINUE
    RETURN
ENDIF

x_edges = axis_get_edges( xaxis )
y_edges = axis_get_edges( yaxis )

; go tired of trying to avoid this if
xcrange = xlog? 10^!x.crange : !x.crange
if x_edges[0] le last_item( x_edges ) then begin
    x_expanded_range = [x_edges[0], last_item(x_edges)] > xcrange[0] < xcrange[1]
endif else begin
    x_expanded_range = [x_edges[0], last_item(x_edges)] < xcrange[0] > xcrange[1]
endelse

ycrange = ylog? 10^!y.crange : !y.crange
if y_edges[0] le last_item( y_edges ) then begin
    y_expanded_range = [y_edges[0], last_item(y_edges)] > ycrange[0] < ycrange[1]
endif else begin
    y_expanded_range = [y_edges[0], last_item(y_edges)] < ycrange[0] > ycrange[1]
endelse

im_position = convert_coord( x_expanded_range,  $
                             y_expanded_range, /data, /to_normal )

xpos = im_position[0,*]
ypos = im_position[1,*]
if ypos[1] lt ypos[0] then ypos = ypos[[1,0]]
if xpos[1] lt xpos[0] then xpos = xpos[[1,0]]

;kim+
checkvar, bottom, 0
checkvar, top, !d.table_size-1

;ncolors = o->getprop( /ncolors )
if exist(ncolors) then top = bottom + ncolors
; scale image to range of colors between top and bottom
if not exist( hist_equal_percent ) then begin
    hist_equal_percent_local = yes_obj ? o->getprop( /hist_equal_percent ) : 0
endif else begin
    hist_equal_percent_local = hist_equal_percent
endelse

if hist_equal_percent_local ne 0 then begin
    image = hist_equal( image, percent = hist_equal_percent_local )
endif


;kim-

im_size = convert_coord(xpos, ypos, /normal, /to_device)
npixx = round(im_size[0, 1] - im_size[0, 0])
npixy = round(im_size[1, 1] - im_size[1, 0])

; take the value first form interpolate kwd, then from no_interpolate
; kwd (for plotman) then from the value stored in specplot then
; default to 0
interpolate =  exist( interpolate ) ? $
                    interpolate : ( exist( no_interpolate ) ? $
                                    (not no_interpolate) : (yes_obj ? $
                                                          o->get( /interpolate ) : 0 ))

IF !d.flags AND 1 THEN BEGIN

    image = interp_image( image, xaxis, yaxis, $
                          !d.x_size/!d.x_px_cm*50, $
                          !d.y_size/!d.y_px_cm*50, $
                             xrange = x_expanded_range, $
                              yrange = y_expanded_range, $
                           YLOG=ylog, xlog=xlog, smooth=interpolate)
; to be refactored real soon
    nans = where( finite( image, /NAN ), count )
    if count gt 0 then image[nans] = min(image)
    if not valid_range( drange ) then begin
        if yes_obj then begin
            drange = o->getprop( /drange )
            if not valid_range( drange ) then drange = minmax_image
        endif else   drange = minmax_image
    endif
    image = cscale( image, bottom=bottom, top=top, drange=drange )

    tv, image, xpos[0], ypos[0], /normal, $ ;kim  - tvscl -> tv
        xsize=xpos[1]-xpos[0], $
        ysize=ypos[1]-ypos[0]

ENDIF ELSE BEGIN

; adapt image to the axis:
    IF npixx GT 0 AND npixy GT 0 THEN BEGIN
        image = Interp_image( image, xaxis, yaxis, $
                              npixx, npixy, $
                              xrange = x_expanded_range, $
                              yrange = y_expanded_range, $
                              YLOG=ylog, xlog = xlog, smooth=interpolate )

; acs moved this here from above since interpolate in interp_image
; needs floats
; seems missing in cscale does not work so elim nans by hand
        nans = where( finite( image, /NAN ), count, complement = not_nans )
; adds not_nans acs 2004-07-20
        if count gt 0 then image[nans] = min(image[not_nans])
;        if not exist( drange ) then drange = yes_obj ? o->getprop( /drange ) : minmax_image

        if not valid_range( drange ) then begin
            if yes_obj then begin
                drange = o->getprop( /drange )
                if not valid_range( drange ) then drange = minmax_image
            endif else   drange = minmax_image
        endif

        image = cscale( image, bottom=bottom, top=top, drange=drange )

;kim+  changed tvscl to tv since already scaled image to bytes
        tv, image, xpos[0], ypos[0], /normal, /NAN

    ENDIF
;kim-
ENDELSE

; do the axes again, this time with noerase, but pass same kwds.
IF NOT no_ut THEN BEGIN
    ;; why is this one not set back by utplot?
    old_noerase = !p.noerase

    utplot, xaxis, yaxis[*, 0], /nodata, /noerase, $
            xrange = xrange_local, $
            title = title_local, $
            YRANGE=yrange_local, $
            charsize = charsize, $
            YLOG=ylog, XCHARSIZE=xcharsize, $
            XTITLE=xtitle_local, position = position, $
            YMARGIN=ymargin, xstyle = this_xstyle, $
            ystyle = this_ystyle, $
            ytitle = ytitle_local, _extra = _extra

    !p.noerase = old_noerase

ENDIF ELSE BEGIN

    plot, xaxis, yaxis[*, 0], /nodata, /noerase, $
      XRANGE=xrange_local, $
        title = title_local, $
        YRANGE=yrange_local, $
        charsize = charsize, $
        YLOG=ylog, XCHARSIZE=xcharsize, xlog = xlog, $
        XTITLE=xtitle_local, position = position, $
        YMARGIN=ymargin, xstyle = this_xstyle, $
        ystyle = this_ystyle, $
        ytitle = ytitle_local, _extra = _extra

ENDELSE

if total( !p.multi ) ne 0 then !p.multi[0] = !p.multi[0]-1

if since_version('5.2') and cbar then plot_map_colorbar, minmax_image_orig, $
  bottom, ncolors, _extra=_extra, log = zlog


END


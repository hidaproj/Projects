;+
; Project     : SOHO_CDS
;
; Name        : PLOT_MAP2 [WARNING-TEMPORARY FIX ONLY FOR 16 bit Z-Buffer support]
;
; Purpose     : Plot an image map 
;
; Category    : imaging
;
; Explanation : 
;
; Syntax      : plot_map,map
;
; Examples    :
;
; Inputs      : MAP = image structure map created by MAKE_MAP
;
; Opt. Inputs : None;
;
; Outputs     : None
;
; Opt. Outputs: None
;
; Keywords    : 
;     /OVER = overlay on previous image
;     /CONT = contour the image
;     /SMOOTH = smooth the image
;     FOV = [fx,fy] = field of view to be plotted 
;     GRID = grid spacing (deg) for latitude-longitude grid [def= 0, no grid]
;     GLABEL = label grid with coordinate values [def = 0, no labels]
;     GSTYLE = grid linestyle [def=0]
;     CENTER = [xc,yc] = center coordinates (arcsec) of FOV [def = center of image]
;            (if center is a valid map, then use its center)
;     DMIN,DMAX = min, max data for plot [def = data min,max]
;     BORDER = draw border around image [def = no]
;     DRAW = alerts PLOT_MAP that window is a DRAW widget
;     /TAIL = allows user to tailor contours
;     /LOG  = log_10 scale image
;     WINDOW = window index to send plot to
;     /NOAXES = inhibit plotting axes
;     /NOTITLE = inhibit printing title
;     /NOLABELS = inhibit axis labels
;     /NOXTICKS = inhibit X-tick labels
;     /NOYTICKS = inhibit Y-tick labels
;     /DROTATE  = solar rotate image contour
;     LEVELS  = user specified contour levels
;     /POSITIVE = plot positive data
;     /NEGATIVE = plot negative data
;     XRANGE,YRANGE = cartesian plot limits 
;     /INTERLACE = interlace two images when overlaying
;     /COMPOSITE = simultaneously plot two images when overlaying
;                = type of compositing:
;                  1: original , 2: latest(new), 3: max(new/old) 4: min(new/old)
;     /AVERAGE   = average two images when using /COMPOSITE
;     BOTTOM = lowermost color index to use in color scaling [def=0]
;     LAST_SCALE = use MIN/MAX from previous plot, or if LAST is a valid map
;                  use scaling from it
;     LIMB = overplot solar limb
;     ROLL = roll image contour
;     BTHICK = border thickness
;     CTHICK = contour thickness
;     LCOLOR = contour line color [0 = black]
;     VIEW_ADJUST = adjust for different spacecraft views when overlaying
;     MULTI = set for multiple plots per page, e.g. mult=[2,2] (or 2) for 4 
;             plots per page (!p.multi remains at this value until cleared)
;     NOERASE = don't erase previous plot
;     SQUARE = force equal aspect ratio (by adjusting position)
;
; Restrictions: None
;      - do not set /GRID unless absolute coordinates of image are known
;      - do not set /OVER unless a plot exists on the current device
;
; Side effects: None
;
; History     : Written 22 December 1996, D. Zarro, SAC/GSFC
;             : S.L.Freeland - let COMPOSITE have different interpretations
;             : Major modifications 15 Feb 1999, Zarro (SM&A/GSFC) 
;                -- reorganized
;                -- sped-up sub-field extraction
;                -- fixed potential bug in contour levels scaling                 
;                -- fixed potential bug in image color scaling
;                -- fixed roll correction
;             : Zarro (SM&A/GSFC) 28 April 1999
;                -- fixed roll_center interpretation
;                (heaven help me for onlining this during Gopal's CDAW)
;             : Zarro (SM&A/GSFC) 3 May 1999
;                -- allowed overlaying images with different roll centers.
;             : Zarro (SM&A/GSFC) 5 Aug 1999
;                -- Made VIEW_ADJUST=1 the default
;             : Zarro (SM&A/GSFC) 25 Sep 1999
;                -- Made VIEW_ADJUST=0 the default
;             : Zarro (SM&A/GSFC) 1 Nov 1999
;                -- Added GSTYLE grid style keyword
;             : Freeland (LMSAL) - KLUDGE for Z-buffer, 16 bit support
;               (Called from 'ssw_build_mosaic.pro')
;
; Contact     : dzarro@solar.stanford.edu
;-

pro plot_map2,map,cont=cont,over=over,smooth=smooth,border=border,$
 fov=fov,grid=grid,glabel=glabel,gstyle=gstyle,center=center,$
 draw=draw,tail=tail,log=log,notitle=notitle,title=title,$
 lcolor=lcolor,window=window,noaxes=noaxes,nolabels=nolabels,$
 xsize=xsize,ysize=ysize,new=new,rotate=rotate,levels=levels,$
 missing=missing,dmin=dmin,dmax=dmax,$
 top=top,quiet=quiet,square=square,$
 tag_id=tag_id,trans=trans,positive=positive,negative=negative,$
 offset=offset,time=time,bottom=bottom,$
 cstyle=cstyle,cthick=cthick,date_only=date_only,nodate=nodate,$
 gfont=gfont,last_scale=last_scale,composite=composite,$
 interlace=interlace,xrange=xrange,yrange=yrange,$
 average=average,ncolors=ncolors,drange=drange,$
 err=err,limb=limb,roll=roll,rcenter=rcenter,truncate=truncate,$
 duration=duration,bthick=bthick,drotate=drotate,$
 view_adjust=view_adjust,multi=multi,noerase=noerase,_extra=extra

;-- some variables saved in memory for overlay

@plot_map_com
         
on_error,1
err=''

;-- color controls

zbuff=!d.name eq 'Z'
wbuff=(!d.name eq 'X') or (!d.name eq 'WIN')
n_colors=!d.table_size 
white=n_colors-1L & black=0L
if not exist(lcolor) then begin
  if wbuff or zbuff then lcolor=white else lcolor=black
endif
if not exist(bottom) then bottom=0 else bottom=bottom > 0
if not exist(top) then begin
 if exist(ncolors) then top=(bottom+ncolors-1L) else top=n_colors-1L
endif
top=top < (n_colors-1L)

;-- examine keywords

quiet=keyword_set(quiet)
loud=1-quiet
if not exist(tag_id) then tag_id='DATA'
over=keyword_set(over)          ;-- overlay image
cont=keyword_set(cont)          ;-- contour image
smo=keyword_set(smooth)         ;-- smooth image
bord=exist(border)              ;-- plot image border
dlog=keyword_set(log)           ;-- log scale image
droll=exist(roll)               ;-- roll image
dtrans=n_elements(trans) eq 2   ;-- translate image

if exist(view_adjust) then view=(1b < view_adjust > 0b) else view=0b
if keyword_set(rotate) and (1-keyword_set(drotate)) then $
 message,'in future, please use /DROTATE instead of /ROTATE',/cont
drotate=keyword_set(rotate) or keyword_set(drotate)

;-- always overlay as a contour unless /interlace or /composite are set

take_average=keyword_set(average)
if not keyword_set(noerase) then noerase=0 else noerase=1 
interlace=keyword_set(interlace)
if keyword_set(composite) then comptype=composite   ; intercept COMPTYPE
if keyword_set(composite) or interlace then over=1
if over and (not keyword_set(composite)) and (not interlace) then cont=1 

;-- open a new window if one doesn't exist
;-- else get viewport from previous plot

if (not over) then begin
 get_xwin,window,xsize=xsize,ysize=ysize,draw=draw,new=new
; if (!d.name eq 'X') then wshow,window,0
endif else begin
 if exist(window) and exist(last_window) then begin
  if window ne last_window then begin
   err='WARNING - overlaying on other than last plot window. Cannot guarantee coalignment'
   print,'---------------------------------------------------------------------'
   message,err,/cont
   print,'% window, last_window: ',window,last_window
   print,'---------------------------------------------------------------------'
  endif
 endif
 if not exist(last_window) then begin
  err='No previous window on which to overlay'
  message,err,/cont
  return
 endif
 smart_window,last_window,status=status,draw=draw
 if not status then begin
  if exist(last_window) then app=trim(string(last_window))
  err='Could not open window '
  if exist(app) then err=err+app
  message,err,/cont
  return
 endif
endelse

;-- overlay limb and/or grid

if over and keyword_set(limb) then begin
 if not exist(last_time) then begin
  err='No previous image on which to overlay limb/grid'
  message,err,/cont
  if not valid_map(map) then return
 endif
 if not exist(grid) then grid=30. else if grid ne 0. then grid=grid > 10.
 plot_helio,last_time,roll=last_roll,grid=grid,glabel=glabel,gstyle=gstyle,$
  /over,color=lcolor,font=gfont,soho=last_view,center=last_rcenter
 return
endif        

;-- check input map

if not exist(map) then begin
 pr_syntax,'plot_map,map'
 return
endif

if not valid_map(map) then begin
 err='Invalid input image map'
 message,err,/cont
 return
endif

if n_elements(map) gt 1 then begin
 message,'Cannot handle more than one image map at this time',/cont
 return
endif

;-- plot labels

mtime=get_map_time(map,/tai,/original)
mtitle=get_map_prop(map,/id,def='')
if (not over) then begin
 err=''
 date_obs=anytim2utc(mtime,/vms,err=err,time_only=keyword_set(nodate),$
  date_only=date_only,truncate=truncate)
 if (1-keyword_set(date_only)) then field=' UT' else field=''
 if err eq '' then mtitle=mtitle+' '+date_obs+field
endif
mtitle=trim(mtitle)
if datatype(title) eq 'STR' then mtitle=title
if keyword_set(notitle) then mtitle=''     
units=get_map_prop(map,/units,def='arcsecs')
units='('+units+')'
xunits=units & yunits=units
if tag_exist(map,'xunits') then xunits='('+map.xunits+')'
if tag_exist(map,'yunits') then yunits='('+map.yunits+')'
xtitle='X '+xunits & ytitle='Y '+yunits
if keyword_set(nolabels) then begin
 xtitle='' & ytitle=''
endif

;-- keep track of plot location for multi-page plots
;-- clear page if !p.multi changed

if exist(multi) then begin
 pmulti=[multi(0),multi(n_elements(multi)-1)]
 !p.multi([1,2])=pmulti
endif
pnx=!p.multi(1)
pny=!p.multi(2)
if n_elements(last_multi) lt 3 then last_multi=!p.multi
if (last_multi(1) ne pnx) or (last_multi(2) ne pny) then begin
 erase & !p.multi(0)=0
endif

;-- go to previous image if an overlay

sp=!p.multi(0)
if over then begin
 !p.multi(0)=(!p.multi(0)+1)
 if !p.multi(0) gt pnx*pny then !p.multi(0)=0 
 sp=!p.multi(0)
endif

;-- deduce image center 
;   (start with that of image, then FOV keyword, then CENTER keyword)

icenter=get_map_prop(map,/center) 
dcenter=icenter 
if valid_map(fov) then dcenter=get_map_prop(fov,/center) 
if exist(center) then begin
 if valid_map(center) then dcenter=get_map_prop(center,/center) else begin 
  if n_elements(center) eq 2 then dcenter=float(center) 
 endelse 
endif

;-- deduce actual ranges 
;   (start with image, then FOV, then XRANGE/YRANGE keywords, then last
;    ranges used if /OVER)

ixrange=get_map_prop(map,/xr)
iyrange=get_map_prop(map,/yr)

dxrange=ixrange & dyrange=iyrange
if exist(fov) then begin
 if valid_map(fov) then dfov=get_map_fov(fov) else begin
  nfov=n_elements(fov)
  dfov=60.*float([fov(0),fov(nfov-1)])
 endelse
endif else dfov=[max(dxrange)-min(dxrange),max(dyrange)-min(dyrange)]
half_fov=dfov/2.     
dxrange=[dcenter(0)-half_fov(0),dcenter(0)+half_fov(0)]
dyrange=[dcenter(1)-half_fov(1),dcenter(1)+half_fov(1)]

if n_elements(xrange) eq 2 then dxrange=float(xrange)
if n_elements(yrange) eq 2 then dyrange=float(yrange)
if over then begin
 if exist(last_xrange) then dxrange=last_xrange
 if exist(last_yrange) then dyrange=last_yrange
endif

;-- viewport

xmin=min(dxrange) & xmax=max(dxrange)
ymin=min(dyrange) & ymax=max(dyrange)
dxrange=[xmin,xmax]
dyrange=[ymin,ymax]

if (xmin eq xmax) or (ymin eq ymax) then begin
 err='Plot scale MIN/MAX must differ'
 message,err,/cont
 goto,clean_up
endif

;-- get some map properties

curr_view=get_map_prop(map,/soho,def=0b)
curr_roll=get_map_prop(map,/roll,def=0.)
curr_rcenter=get_map_prop(map,/roll_center,def=icenter)
if exist(rcenter) then begin
 if valid_map(rcenter) then roll_center=get_map_prop(rcenter,/center) else $
  if n_elements(rcenter) eq 2 then roll_center=float(rcenter)
endif else roll_center=icenter
if curr_view then dprint,'% using SOHO view'
if exist(time) then rtime=time else $
 if exist(last_time) and over then rtime=last_time
if not exist(last_view) then last_view=curr_view

;-- if solar rotating, first de-rotate viewport and extract only
;   pixels that will be rotated into field (faster this way)

mxrange=dxrange & myrange=dyrange
xt=[mxrange(0),mxrange(1),mxrange(1),mxrange(0)]
yt=[myrange(0),myrange(0),myrange(1),myrange(1)]

if drotate and cont then begin
 dur=get_drot_dur(map,duration,time=rtime)
 if not exist(rtime) then rtime=mtime+dur
 if dur eq 0. then drotate=0b else begin
  drot_xy,xt,yt,mtime,mtime-dur,xtm,ytm,$
   roll=curr_roll,rcenter=curr_rcenter,view=curr_view
  mxrange=[min(xtm),max(xtm)]
  myrange=[min(ytm),max(ytm)]
  rcheck=sqrt(mxrange^2 + myrange^2)
  pr=pb0r(mtime,soho=curr_view,/arcsec)
  radius=float(pr(2))
  if rcheck(0) ge radius then begin
   mxrange(0)=ixrange(0) & myrange(0)=iyrange(0)
  endif
  if rcheck(1) ge radius then begin
   mxrange(1)=ixrange(1) & myrange(1)=iyrange(1)
  endif 
 endelse
 xt=[mxrange(0),mxrange(1),mxrange(1),mxrange(0)]
 yt=[myrange(0),myrange(0),myrange(1),myrange(1)]
endif
 
;-- if overlaying and roll is not specifically requested, then we need to 
;    roll second image to same roll angle and center as first image

if droll then roll=float(roll)
need_to_roll=0b
if over and cont and (not droll) and exist(last_roll) then begin
 need_to_roll=(abs(last_roll-curr_roll) mod 360.) ne 0.
 if need_to_roll then begin
  dprint,'% PLOT_MAP: base and overlay image have different rolls -- will correct'
  dprint,last_roll,curr_roll
  droll=1b & roll=last_roll-curr_roll 
  if exist(last_rcenter) and ((last_roll mod 360.) ne 0.) then $
   roll_center=last_rcenter else roll_center=curr_rcenter
 endif
endif

;-- if rolling, roll only pixels in viewport

if droll then droll=(roll mod 360.) ne 0.

need_to_rcenter=0b
if droll and cont and over then begin
 dprint,'% roll angle , roll center: ',roll,roll_center
 if need_to_roll then $
  need_to_rcenter=(roll_center(0) ne curr_rcenter(0)) or $
                  (roll_center(1) ne curr_rcenter(1))
 if need_to_rcenter then begin
   dprint,'% PLOT_MAP: base and overlay image have different roll_centers -- will correct'
   dprint,roll_center,curr_rcenter
   roll_xy,xt,yt,-last_roll,xtm,ytm,center=roll_center
   roll_xy,xtm,ytm,curr_roll,xtm,ytm,center=curr_rcenter
 endif else roll_xy,xt,yt,-roll,xtm,ytm,center=roll_center
 mxrange=[min(xtm),max(xtm)]
 myrange=[min(ytm),max(ytm)]
endif

;-- unpack data and pixel arrays

pic=get_map_sub(map,xrange=mxrange,yrange=myrange,arange=brange,$
                irange=irange,count=count,tag_id=tag_id,err=err)
    
if (err ne '') then goto,clean_up

;-- check if translating 

xoff=0. & yoff=0.
if dtrans then begin
 xoff=trans(0) & yoff=trans(1)
endif

brange(0:1)=brange(0:1)+xoff
brange(2:3)=brange(2:3)+yoff
if cont then begin
 xp=get_map_prop(map,/xp)
 yp=get_map_prop(map,/yp)
 xp=temporary(xp(irange(0):irange(1),irange(2):irange(3)))+xoff
 yp=temporary(yp(irange(0):irange(1),irange(2):irange(3)))+yoff
endif

if (drotate or droll) and (not cont) then begin
 err='Can only solar rotate/roll in overlay or contour mode'
 message,err,/cont
 goto,clean_up
endif

;-- solar rotate

if drotate then begin
 drot_xy,xp,yp,mtime,mtime+dur,xp,yp,err=err,/verbose,/no_copy,roll=curr_roll,$
  rcenter=curr_rcenter,view=curr_view
 if err ne '' then goto,clean_up
endif

;-- if overlaying images with different platform views (EARTH vs SOHO),
;   then adjust for parallax

if view and cont and over then begin
 if last_view ne curr_view then begin
  message,'adjusting for difference between spacecraft views. Use VIEW=0 to inhibit',/cont
  if (curr_view eq 0) then to_view=',/soho' else to_view=',/earth'
  stat='lview_xy,xp,yp,mtime,xp,yp,/verb,roll=curr_roll,rcenter=curr_rcenter,err=err'
  status=execute(stat+to_view)
  curr_view=last_view
  if err ne '' then goto,clean_up
 endif
endif

;-- apply roll

if droll then begin
 if need_to_rcenter then begin
  roll_xy,xp,yp,-curr_roll,xp,yp,center=curr_rcenter
  roll_xy,xp,yp,last_roll,xp,yp,center=roll_center
 endif else roll_xy,xp,yp,roll,xp,yp,center=roll_center
endif

;-- get data limits

if cont then begin
 min_x=min(xp) & max_x=max(xp)
 min_y=min(yp) & max_y=max(yp)
endif else begin
 min_x=brange(0) & max_x=brange(1)
 min_y=brange(2) & max_y=brange(3)
endelse

xlim = [min_x,max_x,max_x,min_x,min_x]
ylim = [min_y,min_y,max_y,max_y,min_y]
dx=get_map_prop(map,/dx)
dy=get_map_prop(map,/dy)
dx2=dx/2. & dy2=dy/2.
xedge=[xlim(0)-dx2,xlim(1)+dx2,xlim(2)+dx2,xlim(3)-dx2,xlim(4)-dx2]
yedge=[ylim(0)-dy2,ylim(1)-dy2,ylim(2)+dy2,ylim(3)+dy2,ylim(4)-dy2]
nx=data_chk(pic,/nx)
ny=data_chk(pic,/ny)

dprint,'% xc,yc,dx,dy,nx,ny',dcenter(0),dcenter(1),dx,dy,nx,ny

if exist(offset) then pic=temporary(pic)+offset
if n_elements(positive) eq 1 then if (positive ne 0) then pic=temporary(pic) > 0
if keyword_set(negative) then pic=temporary(pic) < 0

;-- get data value limits
;-- start with actual data, then dmin/dmax keywords, then last scale, and
;   finally drange.

npic=n_elements(pic)
pmin=min(pic,max=pmax)
prange=[pmin,pmax]

if exist(dmin) then prange(0)=dmin
if exist(dmax) then prange(1)=dmax
if exist(last_scale) then begin
 if valid_map(last_scale) then prange=get_map_prop(last_scale,/dr) else begin
  if n_elements(last_drange) eq 2 then prange=last_drange
 endelse
endif

if exist(drange) then begin 
 if valid_map(drange) then prange=get_map_prop(drange,/dr) else begin
  if n_elements(drange) eq 2 then prange=float(drange)
 endelse
endif

;-- set up contours

@plot_map_contour

;-- plot axes & viewport
;-- try to preserve aspect ratio (won't work if multi is set)

@plot_map_aspect

if (not over) then begin
 if have_tag(extra,'noxt') then begin
  xticks=replicate(' ',n_elements(!x.tickname)-1)
  extra=rep_tag_value(extra,xticks,'xtickname')
 endif
 if have_tag(extra,'noyt') then begin
  yticks=replicate(' ',n_elements(!y.tickname)-1)
  extra=rep_tag_value(extra,yticks,'ytickname')
 endif
 !p.multi(0)=sp
 plot,[xmin,xmax,xmax,xmin,xmin],[ymin,ymin,ymax,ymax,ymin],/data, $
  xstyle=5,ystyle=5,noeras=noerase,/nodata,xrange=dxrange,yrange=dyrange,$
  _extra=extra
 smart_window,window,/set,draw=draw
endif 
 
dprint,'*******************' 
dprint,!x.crange
dprint,!y.crange 
dprint,dxrange 
dprint,dyrange 
dprint,'*******************'

;-- plot contour

if cont then begin 
 cs=uniq([clev],sort([clev]))
 !p.multi(0)=sp
 if drotate or view then begin
  off_limb=where_off_limb(xp,yp,mtime,count=count,view=curr_view)
  if (count gt 0) then pic(off_limb)=2.*nmax
 endif
 
 contour,pic,xp,yp,/data,xstyle=5,ystyle=5,c_linestyle=cstyle(cs),$
  levels=clev(cs),c_labels=clabel(cs),c_thick=cthick(cs),c_colors=colors(cs),$
  max_value=corange(1),min_value=corange(0),over=over,$
  spline=smo,/noeras,noclip=0,/follow,xrange=dxrange,yrange=dyrange,$
  clip=[dxrange(0),dyrange(0),dxrange(1),dyrange(1)],_extra=extra

;-- plot image

endif else begin
 xb_dev=!d.x_vsize*(!x.s(0)+!x.s(1)*xedge)
 yb_dev=!d.y_vsize*(!y.s(0)+!y.s(1)*yedge)
 sx=abs(max(xb_dev)-min(xb_dev)) > 1.
 sy=abs(max(yb_dev)-min(yb_dev)) > 1.

;-- rebin image for X-windows or Z-buffer (!d.name = 'X' or 'Z')
;-- or plot in Postscript using scalable pixels (!d.name = 'PS')

 dprint,'% min, max pic: ',prange
 dprint,'% bottom, top: ',bottom,top

 scaleit=0
 
 if scaleit then pic=cscale(pic,top=top,bottom=bottom,$
      max=prange(1),min=prange(0),/no_copy, missing=missing,err=err,log=dlog)

 if err ne '' then goto,done

;-- check if composite/interlace is requested

 dprint,'% over, sx, sy:',over, sx,sy
 dprint,'% xscale, yscale:',(max_x-min_x+dx)/sx, (max_y-min_y+dy)/sy
 
 if zbuff or wbuff then begin
  words=data_chk(map.data,/type) ne 1         ; boolean for tv/tvrd
  true=!d.n_colors gt 256

  pic=congrid(temporary(pic),sx,sy,interp=smo)
  if (keyword_set(composite) or interlace) and over then begin
   ok=(xb_dev(0) gt 1) and (xb_dev(0) lt dxsize) and $
      (yb_dev(0) gt 1) and (yb_dev(0) lt dysize) and $
      (xb_dev(0)+sx lt dxsize) and $
      (yb_dev(0)+sy lt dysize)

   ;-- just create a blank bytarr if underlying image cannot be read
ok=1   
   if ok then begin
    if zbuff then $
     base=tvrd(xb_dev(0),yb_dev(0),sx,sy,/channel,/words) else $
      base=tvrd(xb_dev(0),yb_dev(0),sx,sy)
   endif else  base=bytarr(sx,sy)

;   base=cscale(base,top=last_top,bottom=last_bottom,/no_copy)

;-- combine underlying and overlaying images 

   if interlace then begin
    base=swiss_cheese(base,last_bottom,/shift,/no_copy)
    pic=swiss_cheese(pic,bottom,/no_copy)
    pic=temporary(base)+temporary(pic)
   endif else begin                             ; COMPOSITE set
     case comptype of                            ; setable option
       1: begin                                 ; backwardly compatible
	   bfac=([1,2])(take_average)
           pic=temporary(base)/bfac+temporary(pic)/bfac
          end
       3: begin
	    pic=temporary(base) > temporary(pic)        ; 'largest'  pixel
          endcase
       4: pic=temporary(base) < temporary(pic)        ; 'smallest' pixel
       else: do_nothing=1
    endcase
   endelse
  endif
 endif

 !p.multi(0)=sp
 case 1 of  
  zbuff: tv,pic,xb_dev(0),yb_dev(0),xsize=sx,ysize=sy, /channel, /words
  else:  tv,pic,xb_dev(0),yb_dev(0),xsize=sx,ysize=sy
 endcase

endelse

done:

noaxes=keyword_set(noaxes)
if (not over) then begin
 if noaxes then begin
  xstyle=5 & ystyle=5
 endif else begin
  xstyle=1 & ystyle=1
 endelse
 !p.multi(0)=sp
 plot,[xmin,xmax,xmax,xmin,xmin],[ymin,ymin,ymax,ymax,ymin],/data,noclip=0, $
  xstyle=xstyle,ystyle=ystyle,/noeras,/nodata,xrange=dxrange,$
  yrange=dyrange,xtitle=xtitle,ytitle=ytitle,title=mtitle,$
  _extra=extra,$
  clip=[dxrange(0),dyrange(0),dxrange(1),dyrange(1)]
endif

;-- overlay a solar latitude-longitude grid

if not exist(grid) then if keyword_set(limb) then grid=30. else grid=0. else $
 if grid ne 0. then grid=grid > 10.
if (grid gt 0.) then begin
 if (not over) or composite or interlace then begin
  !p.multi(0)=sp
  plot_helio,mtime,roll=curr_roll,grid=grid,glabel=glabel,gstyle=gstyle,$
   /over,color=lcolor,font=gfont,soho=curr_view,center=curr_rcenter
 endif
endif

;-- plot border edges

if not exist(bthick) then bthick=1
if bord then begin
 if (border eq 2) and cont then begin
  edge=replicate(1.,nx,ny) 
  if ((nx gt 2) and (ny gt 2)) then edge(1:nx-2,1:ny-2) = 0
  !p.multi(0)=sp
  contour,edge,xp,yp,xstyle=5,ystyle=5,levels=[0,1],$
   /data,noclip=0,/noeras,/follow,c_linestyle=[0,0],c_thick=bthick,$
   xrange=dxrange,yrange=dyrange,c_color=[lcolor,lcolor],over=over,$
   clip=[dxrange(0),dyrange(0),dxrange(1),dyrange(1)],_extra=extra
 endif else begin
  !p.multi(0)=sp
  oplot,xedge,yedge,thick=bthick,color=white
 endelse
endif
    
;-- save last settings

if not over then begin
 if exist(window) then last_window=window
 if exist(prange) then last_drange=prange
 if exist(dxrange) then last_xrange=dxrange
 if exist(dyrange) then last_yrange=dyrange
 if drotate then last_time=rtime else last_time=get_map_time(map)
 if exist(top) then last_top=top
 if exist(bottom) then last_bottom=bottom
 if exist(curr_view) then last_view=curr_view
 if exist(curr_roll) then last_roll=curr_roll 
 if exist(curr_rcenter) then last_rcenter=curr_rcenter 
endif

!p.multi(0)=(!p.multi(0)-1)
if !p.multi(0) lt 0 then !p.multi(0)=(pnx*pny-1)
last_multi=!p.multi

clean_up:
delvarx,xp,yp,pic


return & end



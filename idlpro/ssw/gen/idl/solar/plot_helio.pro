;+
; Project     : SMM-XRP
;
; Name        : PLOT_HELIO
;
; Purpose     : Plot solar heliographic grid and limb
;
; Category    : Plotting
;
; Explanation : Uses heliographic formulae from:
;               "Textbook on Spherical Astronomy",
;               by W.M. Smart (see page 174).
;
; Syntax      : plot_helio,date,grid=grid,glabel=glabel
;
; Inputs      : DATE = date/time
;
; Opt. Outputs: LAT,LON = latitude and longitude arrays
;
; Keywords    : GRID_SPACING = spacing (deg) of lat-long  [def = 10]
;               GLABEL = label lat-long grid with coordinate values (def = no labels)
;               OVER = control erasing of previously drawn plot. Setting this
;                        to 1 will cause grid to be overlaid on a drawn plot
;               GCOLOR  = color index for grid
;               GFONT   = gfont index for grid label
;               ROLL = solar roll (deg) clockwise from solar north
;               CENTER = roll center [def = 0,0]
;               GSTYLE = grid linestyle [def=0]
;               GTHICK = grid thickness
;               XRANGE,YRANGE = data ranges in arcsecs
;               LIMB_PLOT =  plot limb
;               LMCOLOR = color index for limb
;               LMTHICK = thickness of limb
;
; History     : Written, 22 December 1991, D. Zarro, ARC/GSFC
;               Modified,30 November 1999, Zarro (SM&A/GSFC) - added RANGE
;                keywords
;               Modified,17 April 2000, Zarro (SM&A/GSFC) - added /LIMB
;               and made SOHO/Yohkoh cross-compatible
;               Modified, 1-Sept-2001, Zarro (EITI/GSFC) - ensure PSYM=0
;               when plotting limb
;               Modified, 16-Sept-2001, Zarro (EITI/GSFC) - added
;               check for ROLL_CENTER within image
;               Modified, 10-Oct-2001, Zarro (EITI/GSFC) - fixed bug in
;               limb calculation when roll is applied
;               Modified, 10-Jan-2002, Zarro (EITI/GSFC) - fixed bug in
;               use of /SOHO
;               Modified, 4-Nov-2002, Zarro (EER/GSFC) - added GCOLOR
;               Modified, 23-Feb-2004, Kim Tolbert - added lmthick
;               Modified, 24-Feb-2004, Zarro (L-3Com/GSFC) - changed
;               lcolor to lmcolor for consistency with lmthick
;               Modified, 26-Oct-2004, Yurow (L-3Com/GSFC) - added
;               _EXTRA keyword that is passed to the plot command.
;               Modified 11-Jan-2004, Zarro (L-3Com/GSFC) - added b0 keyword
;               Modified 17-Jul-2005, Zarro (L-3Com/GSFC) - added GTHICK keyword
;
; Contact     : dzarro@solar.stanford.edu
;-

pro plot_helio,date,grid_spacing=grid_spacing,glabel=glabel,_extra=extra,$
           gcolor=gcolor,gfont=gfont,soho=soho,roll=roll,center=center,$
           gstyle=gstyle,over=over,xrange=xrange,yrange=yrange,$
           limb_plot=limb_plot,lmcolor=lmcolor,lmthick=lmthick, $
           position = position,b0=b0,gthick=gthick

dprint,'% lmcolor: ',lmcolor
dprint,'% lmthick: ',lmthick

;-- plot limb?

do_limb=keyword_set(limb_plot)

;-- grid defaults

if not exist(gstyle) then gstyle=1
;if not exist(gthick) then gthick=1
if not exist(grid_spacing) then grid_spacing=10. else grid_spacing=abs(grid_spacing)

;-- default to current date

err=''
tdate=anytim2utc(date,err=err)
if err ne '' then get_utc,tdate

save_clip=!p.noclip
!p.noclip=0
if not exist(gfont) then gfont=-1
;if not exist(gcolor) then gcolor=!d.table_size-1
;if not exist(lmcolor) then lmcolor=!d.table_size-1

;-- get solar radius and B-angle

pvec=float(pb0r(tdate,/arcsec,soho=soho))
radius=pvec[2]
if (1-is_number(b0)) then b0=pvec[1]

;-- check for non-zero roll

rcenter=[0.,0.]
do_roll=0
if exist(roll) then begin
 do_roll=(roll mod 360.) ne 0.
 have_center=(n_elements(center) eq 2)
 if have_center then begin
  on_disk=(center[0]^2+center[1]^2) lt radius^2
  if do_roll and on_disk then rcenter=center
 endif
endif
dprint,'% rcenter: ',rcenter

if do_limb then begin
 ang=findgen(361)/!radeg
 xlimb=radius*cos(ang)
 ylimb=radius*sin(ang)
 if do_roll then roll_xy,xlimb,ylimb,roll,xlimb,ylimb,center=rcenter
endif

;-- define latitude-longitude matrices (in degrees) at desired grid spacing

do_grid=grid_spacing gt 0.

if do_grid then begin
 gv=-90.+grid_spacing*findgen(180./grid_spacing+1) & ng=n_elements(gv)
 tgrid=.5
 v=-90.+tgrid*findgen(180./tgrid+1) & np=n_elements(v)
 lon=rebin(v,np,np) & lat=rotate(lon,1)

;-- compute cartesian coordinates of grid points

 xy=hel2arcmin(lat,lon,date=tdate,soho=soho,_extra=extra,b0=b0)*60.
 xcor=reform(xy[0,*],np,np)
 ycor=reform(xy[1,*],np,np)
 if do_roll then roll_xy,xcor,ycor,roll,xcor,ycor,center=rcenter

endif

if (1-keyword_set(over)) then begin
 if not is_wopen() then wdef
 drange=[-1300,1300]
 if n_elements(xrange) ne 2 then xrange=drange
 if n_elements(yrange) ne 2 then yrange=drange
 plot,xrange,yrange,xrange=xrange,yrange=yrange,xstyle=5,ystyle=5,$
       /nodata,_extra=extra
endif

if do_limb then oplot,xlimb,ylimb,noclip=0,color=lmcolor,psym=0,linestyle=0,thick=lmthick

;-- plot latitude-longitude lines

if do_grid then begin
 for i=0,np-1 do begin
  ok=where(v[i] eq gv,count)
  if count gt 0 then $
   oplot,xcor[i,*],ycor[i,*],linestyle=gstyle,noclip=0,color=gcolor,$
    psym=0,thick=gthick
 endfor
 for j=0,np-1 do begin
  ok=where(v[j] eq gv,count)
  if count gt 0 then $
   oplot,xcor[*,j],ycor[*,j],linestyle=gstyle,noclip=0,color=gcolor,$
    thick=gthick,psym=0
 endfor

;-- label grid coordinates within current viewport

 if keyword_set(glabel) then begin
  glon=rebin(gv,ng,ng) & glat=rotate(glon,1)
  xy=hel2arcmin(glat,glon,date=tdate,soho=soho,_extra=extra)*60.
  xcor=reform(xy[0,*],ng,ng)
  ycor=reform(xy[1,*],ng,ng)
  if do_roll then roll_xy,xcor,ycor,roll,xcor,ycor,center=rcenter
  within = where( (abs(glon) lt 90.) and (abs(glat) lt 90.)  $
       and (!x.crange[0] le xcor) and (xcor le !x.crange[1]) $
           and (!y.crange[0] le ycor) and (ycor le !y.crange[1]))
  if within[0] ne -1 then begin
   for k=0,n_elements(within)-1 do begin
    wlon=glon(within[k])
    wlat=glat(within[k])
    clon = strcompress(nint(wlon),/remove_all)
    clat = strcompress(nint(wlat),/remove_all)
    xyouts, xcor(within[k]), ycor(within[k]), '!3 '+clon+','+clat,noclip=0,$
           font=gfont
   endfor
  endif
 endif
endif

!p.noclip=save_clip
dummy=is_wopen(/show)
return & end

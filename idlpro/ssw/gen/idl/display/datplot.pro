;+
; PROJECT:
;       SDAC
; Name:
;	DATPLOT
;
; CALLING SEQUENCE:
;	datplot,xl,xh,y [,xnew,ynew , STAIRS=STAIRS, color=color [,/OUTPLOT] ]
;
;	or
;	datplot,dummy1, dummy2, xs=xs, y [, xnew,ynew , $
;	STAIRS=STAIRS, color=color [,/OUTPLOT] ]
;
; PURPOSE:
;	This procedure is used to OVERplot histograms between channel edges.
; CATEGORY:
;	1-D Graphics.
; INPUTS:
;	xl - Low edges of abscissae
;	xh - High edges of abscissae
;	y  - Ordinates
;
; Optional Inputs:
;	xs - xl and xhi are packed into xs(0,*) and xs(1,*)
;	if xs is passed, then y is still the third argument,
;	  the first two arguments are not used but required
;
; Optional Outputs:
;	xnew - If /stairs, then array used for abscissae bins
;	ynew - If /stairs, then ordinate array
;
; KEYWORDS:
;	OUTPLOT - if set, interpret abscissae as time values for OUTPLOT
;		If the abscissae is datatype structure, it is interpreted
;		using Anytim().  Xnew  will have Utbase removed.
;
;	/stairs - connect all the bins together, looks like stairs
;	/nolegs - leave the ends floating under stairs option
;	color   - plotting color to use, as with linecolors
;	psym  - symbol to plot at center of horizontal bar, def=no effect
;	sysmsize - size of symbol, def=0.1
;	All remaining keywords available to OPLOT
; CALLS:
;	FCOLOR
;
; RESTRICTIONS:
;       Initial call to plot must be made to establish scaling. For
;	overplotting only.
;
; MODIFICATION HISTORY:
;	91/12/06, FOR VERSION2
;       	18-oct-93, ras, nolegs and color
;	19-may-94, ras, indicate gaps between intervals using stairs
;	Version 4, RAS, 17-Jun-1997, protect case of a single data interval to plot.
;	Version 5, RAS, 29-mar-2001, direct support for Utplot through OUTPLOT
;   Kim, 17-Apr-2001, Add keyword nsum so nsum will not be in oplotextra, and can
;     be excluded from call to oplot.  Changed default for psym to !p.psym, and don't
;     oplot the symbols if psym=0 OR psym=10.  Also use abs(psym).
; CONTACT:
;	richard.schwartz@gsfc.nasa.gov
;-

pro datplot,xl,xh,y,xnew,ynew, xs=xs, _extra=oplotextra, $
    OUTPLOT=outplot, $
    STAIRS=STAIRS, color=color, nolegs=nolegs, psym=psym, symsize=symsize, nsum=nsum

xtype = size(/tname, xs)
if xtype eq 'UNDEFINED' then begin
	xtype = size(/tname, xl)

	xl1 = anytim(xl)
	xh1 = anytim(xh)
	endif

use_utbase = xtype EQ 'STRUCT'
if n_elements(outplot) eq 1 then use_utbase = outplot

if n_elements(xs) ge 2 then begin
    xs1 = anytim( xs)
    xl1  = xs1(0,*)
    xh1  = xs1(1,*)
    endif

f_color = fcheck( color, !p.color )
if !d.name eq 'TEK' then f_color = ([0,intarr(15)+1])(f_color)

nel=n_elements(xl1)


utbase    = ([ 0.0, getutbase()])(use_utbase)
xh1       = anytim(xh1) - utbase
xl1       = anytim(xl1) - utbase

;PLOT BARS
if NOT KEYWORD_SET(STAIRS) $
then for i=0L,nel-1L do $
  oplot, [xl1(i),xh1(i)], [y(i),y(i)], psy=0,$
  color=f_color,  _extra=oplotextra $
    else begin

    ;OR PLOT STAIRS
    ;find where data bins are not contiguous
    if nel gt 1 then $
	wncont = where( abs(xh1 - xl1(1:*)) ge .01 *abs(xh1-xl1) , nncont) $
	else nncont = 0

    xnew = transpose( reform( [xl1(*),xh1(*)],nel,2) )
    ynew = transpose( reform( [y(*),y(*)], nel, 2) )
    if not keyword_set(nolegs) then begin
        xnew = [ xnew(0), xnew(*), xnew(nel*2-1) ]
        yrange = crange('y')
        ynew = [ yrange(0), ynew(*), yrange(0) ]
        endif
    oplot,xnew, $
	ynew,PSYM=0,color=f_color, _extra=oplotextra
    if nncont gt 0 then $ ;overplot blanks during gaps
    for i=0L, nncont-1L do $
    oplot, [xh1(wncont(i)),xl1(wncont(i)+1)], [y(wncont(i)),y(wncont(i)+1)], $
    psym=0, color = !p.background, _extra=oplotextra


    endelse
checkvar, psym, !p.psym
checkvar, symsize,0.1
if psym ne 0 and psym ne 10 then begin
	if !x.type eq 1 then xm = sqrt(xl1*xh1) else xm=(xl1+xh1)/2
	oplot, xm, y, psym=abs(psym), symsize=symsize, color = f_color, _extra=oplotextra
	endif

end


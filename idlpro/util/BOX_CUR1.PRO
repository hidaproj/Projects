; $Id: box_cur1.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

pro box_cur1, x0, y0, nx, ny
;+
; NAME:
;	BOX_CUR1
;
; PURPOSE:
;	Emulate the operation of a variable-sized box cursor (also known as
;	a "marquee" selector).
;	with single mouse button
;
; CATEGORY:
;	Interactive graphics.
;
; CALLING SEQUENCE:
;	BOX_CURSOR, x0, y0, nx, ny
;
; INPUTS:
;	No required input parameters.
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;	x0:  X value of lower left corner of box.
;	y0:  Y value of lower left corner of box.
;	nx:  width of box in pixels.
;	ny:  height of box in pixels. 
;
;	The box is also constrained to lie entirely within the window.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	A box is drawn in the currently active window.  It is erased
;	on exit.
;
; RESTRICTIONS:
;	Works only with window system drivers.
;
; PROCEDURE:
;	The graphics function is set to 6 for eXclusive OR.  This
;	allows the box to be drawn and erased without disturbing the
;	contents of the window.
;
; MODIFICATION HISTORY:
;	DMS, April, 1990.
;	DMS, April, 1992.  Made dragging more intutitive.
;	k.i., '96/09/24	  from box_cursor
;-

device, get_graphics = old, set_graphics = 6  ;Set xor
col = !d.n_colors -1
if not keyword_set(line) then line=0

cursor,x0,y0,1,/dev	; push and set 1 corner
;print,x0,y0,!err
x=x0 &	y=y0
px = [x0, x, x, x0, x0] ;X points
py = [y0, y0, y, y, y0] ;Y values

while !err eq 1 do begin
	cursor, x, y, 2, /dev	;Wait for a button
	plots,px, py, col=col, /dev, thick=1, lines=0  ;Draw the box

	px = [x0, x, x, x0, x0] ;X points
	py = [y0, y0, y, y, y0] ;Y values

	plots,px, py, col=col, /dev, thick=1, lines=0  ;Draw the box
	;wait, .1		;Dont hog it all
endwhile

device,set_graphics = old
nx=max([x0,x])-min([x0,x]) &	x0=min([x0,x])
ny=max([y0,y])-min([y0,y]) &	y0=min([y0,y])

end

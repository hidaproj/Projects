;+
; Project     : SOHO - CDS
;
; Name        : ARCMIN2HEL2()
;
; Purpose     : Convert arcmin from sun centre to heliographic coords,
;		using the correct (perspective) transformation.
;		Derived from arcmin2hel (which uses an orthographic
;		transformation); meant as a drop-in replacement.
;
; Explanation : Converts an (x,y) position given in arcmins relative to the
;               solar disk centre to heliographic coordinates for the date
;               supplied (default date = today).  The formula used is
;		valid for any viewpoint -- though results can be "funny"
;		from viewpoints inside the Sun.
;
; Use         : IDL> helio = arcmin2hel(xx,yy,date=date)
;
; Inputs      : xx  -  E/W coordinate in arc minutes relative to sun center
;                      (West is positive); can be a vector
;               yy  -  S/N coordinate in arc minutes relative to sun center
;                      (North is positive); can be a vector
;
; Opt. Inputs : None
;
; Outputs     : Function returns a 2xN element vector: [lat, long] in
;               degrees, where N is number of elements in XX and YY
;
; Opt. Outputs: None
;
; Keywords    : date      -  date/time in any CDS format
;
;               soho      -  if set uses the SOHO view point rather than
;                            the Earth.  Note this functionality is
;                            duplicated by the system variable SC_VIEW,
;                            which in turn is set by the procedures
;                            USE_EARTH_VIEW or USE_SOHO_VIEW.
;
;               off_limb  - flag which is true if the coordinates are beyond
;                           the solar limb.
;               error     - Output keyword containing error message;
;                           a null string is returned if no error occurs
;               radius    - solar radius value [output in arcmin units]
;               no_copy   - input xx and yy arrays are destroyed
;		p	  - If specified, override pb0r for p angle
;		b0 	  - If specified, override pb0r for b angle
;		r0	  - If specified, override pb0r for solar apparent
;			    dia:  r0 is the observer's distance from the
;			    Sun, in solar radii (one may use "zunits" to
;			    convert fron solar radii to, say, kilometers).
;		l0	  - If specified, this is the longitude (relative
;			    to Earth- or SOHO- central meridian) of the 
;			    sub-observer meridian.  Useful for reprojections
;			    and locations far from Earth.
;		sphere    - If set, return longitude over the whole sphere
;			    rather than mirror reflecting around the 
;			    lon=90 meridian.  (Useful for nonzero B angle)
;		backside  - If set, return the farthest, rather than nearest,
;			    matching point.
;
; Calls       : PB0R
;               ANYTIM2UTC
;
; Restrictions: If the supplied coordinates are outside the solar disk, the
;               region is projected onto the limb.
;
; Prev. Hist. : Original by J P Wuelser.
;
; Written     : CDS version by C D Pike, RAL, 6 Sept 93
;
; Modified    : Converted to use CDS time and pb0r routines, CDP, 17-May-94
;		Version 3, William Thompson, GSFC, 14 November 1994
;                  Modified .DAY to .MJD
;               Version 4, July 31, 1995, Liyun Wang, GSFC/ARC
;                  Vectorized the input parameters (i.e., the two
;                     input parameters can be vectors)
;                  Added ERROR keyword
;               Version 5, 26-Feb-96, CDP
;                  Added SOHO keyword.
;               Version 6, March 11, 1996, Liyun Wang, GSFC/ARC
;                  Modified such that point of view can be changed to
;                     SOHO if the env variable SC_VIEW is set to 1
;               Version 7, March 13, 1996, Liyun Wang, GSFC/ARC
;                  Allow scalar and vector input.
;               Version 8, August 2, 1996, Liyun Wang, NASA/GSFC
;                  Error from calling PB0R is ignored
;               Version 9, June 10, 1997, Liyun Wang, NASA/GSFC
;                  Fixed a bug that occurs when trying to avoid off-limb points
;               Version 10, Sept 10, 1997, Zarro SAC/GSFC
;                  Added RADIUS output keyword
;               Version 11, Oct 7, 1997, Zarro SAC/GSFC
;                  Added some calls to TEMPORARY
;		Version 12, November 4, 1997 Giulio Del Zanna, UCLAN, UK
;		   corrected the fact that the DATE format was changed
;		   inside the routine (e.g. string to a structure)
;		Version 13, February 20, 1998, Zarro (SAC/GSC)
;		   - Added more error checking and improved memory management
;                  - Added /NO_COPY (use with care)
;		Version 14, Dec 17, 1998, DeForest (Stanford/GSFC)
;		   - Added PB0R overrides on a keyword-by-keyword basis
;		Version 15, 6-Jan-1999, DeForest (Stanford/GSFC)
;		   - Added keyword override to remove mirror-image behavior
;		     around the azimuthal meridian.
;               Version 16, 7-Jan-99, Zarro (SMA/GSFC)
;                  - Made use of SOHO keyword more logical
;		Version 17, 8-Apr-99, DeForest (Stanford/GSFC)
;		   - Added L0 keyword; renamed R -> R0 to avoid conflict with
;			"radius" keyword (FDWIM).
;		Version 17.5, 8-Apr-99, DeForest (Stanford/GSFC)
;		   - Modified interpretation of R0 keyword to agree with 
;		 	hel2arcmin:  observer distance from Sun.
;		Version 17.6, 9-Apr-99, Andretta (CUA/GSFC)
;                  - Corrected a bug in the distance-radius conversion
;	        Version 18, 23-Feb-2000, DeForest (SWRI)
;		   - Changed to the correct (non-orthographic)
;		     transformation.
;               Version 19, 22-Aug-2001, Zarro (EITI/GSFC)
;                    Added ANGLES keyword
;		Version 20, 29-Nov-2001, William Thompson, GSFC
;		     Corrected bug with (0,0) as input
;		Version 21, 15-Mar-2002, Zarro/Andretta.
;                    Correct bugs in use of ANGLES and SOHO keywords
;		Version 22, 12-July-2004, Zarro (L-3Com/GSFC)
;                    Added check for negative square roots
;		Version 23, 10-Jan-2005, Zarro (L-3Com/GSFC)
;                    Added /DEBUG and converted () to []
;               14-March-05, Zarro (L-3Com/GSFC) - added more
;                    calls to TEMPORARY
;-

FUNCTION arcmin2hel, xx_in, yy_in, date=this_date, off_limb=offlimb, $
                     soho=soho, error=error,radius=sunr,no_copy=no_copy, $
			p=p_kw,b0=b0_kw,r0=r_kw,l0=l_kw,sphere=sphere, $
		 	backside=backside,angles=angles,debug=debug

   error = ''

   dummy=[-99., -99.]

   IF N_ELEMENTS(xx_in) NE N_ELEMENTS(yy_in) THEN BEGIN
      error = 'Two input parameter not compatible'
      MESSAGE, error, /cont
      RETURN, dummy
   ENDIF

;-- use temporary to save memory I/O 

   if keyword_set(no_copy) then begin
    xxa = temporary(xx_in)
    yya = temporary(yy_in)
   endif else begin
    xxa=xx_in
    yya=yy_in
   endelse

   xs = SIZE(xxa)
   ys = SIZE(yya)

   offlimb = INTARR(n_elements(xxa))
   if n_elements(xxa) eq 1 then offlimb=0.

   IF xs[0] GT 1 THEN BEGIN
      IF xs[1] NE 1 THEN BEGIN
         error = 'Parameter must be a column or row vector.'
         MESSAGE, error, /cont       
         RETURN,dummy
      ENDIF ELSE xxa = TRANSPOSE(temporary(xxa))
   ENDIF

   IF ys[0] GT 1 THEN BEGIN
      IF ys[1] NE 1 THEN BEGIN
         error = 'Parameter must be a column or row vector.'
         MESSAGE, error, /cont
         RETURN, dummy
      ENDIF ELSE yya = TRANSPOSE(temporary(yya))
   ENDIF


;----------------------------------------------------------------------
; Deal with P, B0, and R.  If all three are specified, don't bother
; finding out where the Earth was...
;----------------------------------------------------------------------
   if (1-is_number(p_kw)) or (1-is_number(b0_kw)) or (1-is_number(r_kw)) then begin
	;---------------------------------------------------------------------
	;  set up date required in format required by PB0R
	;---------------------------------------------------------------------
	   IF N_ELEMENTS(this_date) EQ 0 THEN get_utc, dat ELSE $
	   dat = anytim2utc(this_date)
	   IF dat.mjd EQ 0 THEN BEGIN
	      error = 'Wrong time format specification.'
	      MESSAGE, error, /cont
	      RETURN, dummy
	   ENDIF
	
	;--------------------------------------------------------------------
	;  get B0 and solar radius; convert to distance from sun
	;--------------------------------------------------------------------
	   angles = float(pb0r(dat, soho=soho, error=error,debug=debug))
	   Robs = 1.0/tan(angles(2)/60/!radeg)

	;--------------------------------------------------------------------
	;  normalize, check if off limb, project back to limb
	;--------------------------------------------------------------------
	if is_number(p_kw) then angles[0] = p_kw
	if is_number(b0_kw) then angles[1] = b0_kw
	if is_number(r_kw) then Robs = r_kw

  endif else begin
	angles = [double(p_kw),double(b0_kw),double(r_kw)]
	Robs = r_kw
  endelse

   Robs = double(Robs)
   b0 = angles[1]/!radeg ; Convert B0 to radians

   if keyword_set(debug) then begin
    message,'used b0 = '+trim(b0*!radeg)+' at '+anytim2utc(dat,/vms),/cont
   endif

   xxat = tan(temporary(xxa)/60./!radeg) ;(Convert to radians & tanify)
   yyat = tan(temporary(yya)/60./!radeg) ;(Convert to radians & tanify)


; Convert to cylindrical angular coordinates and azimuth -- makes
; the final transformation easier.  Here, ra is the angle out from
; centerline; phi is the azimuth.  This reduces the problem to 2-D
; geometry in the observer -- Sun-center -- viewpoint plane.

;; Load phi with atan(xxat,yyat).
   rat2 = (xxat*xxat+yyat*yyat)
   phi = 0*rat2
   w_rat2 = where(rat2 ne 0,n_rat2)
   if n_rat2 gt 0 then phi[w_rat2] = atan(xxat(w_rat2),yyat(w_rat2))

   max_ra = asin(1.0/Robs)
   max_rat2 = tan(max_ra)*tan(max_ra)

   ii = where(rat2 gt max_rat2)
   IF ii[0] GE 0 THEN begin
	rat2[ii] = max_rat2
        offlimb[ii] = 1
   ENDIF

;
; Solving for the intersection of the line of sight with the sphere
; gives a z-coordinate (toward the observer) of
;   z = R * (sin(ra))^2 +/- sqrt( Ro^2 - (sin(ra))^2 * R^2 )
; with Ro = the solar radius, ra the angular displacement from disk 
; center, and R the viewpoint distance from Sun center.
;
; We normally want the positive branch, which represents the front
; side of the Sun; but who knows? Someone may want the opposite.

   ras2 = 0*rat2
   if n_rat2 gt 0 then ras2[w_rat2] = 1/(1+1/rat2[w_rat2])
   d1=(1-ras2) > 0.
   d2=(1-(Robs*Robs)*ras2) > 0.
   if not keyword_set(backside) then $
	   x = ras2*Robs + sqrt(d1) * sqrt(d2) $
   else $  ; This branch is for the far side of the sun
	   x = ras2*Robs - sqrt(d1) * sqrt(d2) 

   rr = sqrt(temporary(rat2) > 0. ) * (Robs - x) 

; Now we can finally convert back to xyz coords and do the 
; helioraphic conversion.  x: towards obs., y: west, z: North
   xyz = transpose([[temporary(x)],[sin(phi)*rr],[cos(phi)*rr]])
;---------------------------------------------------------------------------
;  rotate around y axis to correct for B0 angle (B0: hel. lat. of diskcenter)
;---------------------------------------------------------------------------
   rotmx = [[COS(b0), 0.0, SIN(b0)], [0.0, 1.0, 0.0], [-SIN(b0), 0.0, COS(b0)]]
   xyz = rotmx # temporary(xyz)
   xyz = TRANSPOSE(temporary(xyz))

;---------------------------------------------------------------------------
;  calc. latitude and longitude.
;---------------------------------------------------------------------------
   latitude = ASIN(xyz(*,2))
   latitude = temporary(latitude) < (89.99/!radeg) ; force lat. smaller 89.99 deg.
   latitude = temporary(latitude) > (-89.99/!radeg) ; force lat. bigger -89.99 deg.
   longitude = ATAN(xyz[*,1], xyz[*,0]) ; longitude


if not keyword_set(sphere) then begin
;---------------------------------------------------------------------------
;  longitude may be larger than 90 degrees due to nonzero B0: get proper value
;---------------------------------------------------------------------------
   ii = WHERE(xyz[*, 0] LT 0.0)
   IF ii[0] GE 0 THEN BEGIN
      tmp = xyz[ii, *]
      tmp_l = longitude[ii]
      jj = WHERE(tmp[*, 1] GE 0.0)
      IF jj[0] GE 0 THEN BEGIN
         tmp_l[jj] = !pi-tmp_l[jj]
         longitude[ii] = tmp_l
      ENDIF
      tmp_l = longitude[ii]
      jj = WHERE(tmp[*, 1] LT 0.0)
      IF jj[0] GE 0 THEN BEGIN
         tmp_l[jj] = -!pi-tmp_l[jj]
         longitude[ii] = tmp_l
      ENDIF
   ENDIF
end
;---------------------------------------------------------------------------
;  convert to degrees.  If we have an L0 offset, make sure the output is 
;  between +/- 180 degrees!
;---------------------------------------------------------------------------

   delvarx,xyz,inr,xx,yy

   if not is_number(l_kw) then $
	   RETURN, !radeg*TRANSPOSE([[temporary(latitude)], [temporary(longitude)]]) $
   else begin
	   RETURN, !radeg*TRANSPOSE([[temporary(latitude)], $
		[(((5*!pi) + temporary(longitude)+((l_kw mod 360)/!radeg)) mod (2*!pi))-!pi]])
	end


END

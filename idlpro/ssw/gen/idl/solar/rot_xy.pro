FUNCTION rot_xy, xx, yy, interval, tstart=tstart, tend=tend, date=date, $
            error=error, soho=soho, keep=keep, offlimb=offlimb, index=index,$
            back_index=back_index,_extra=extra,radius=radius,sphere=sphere, $
            p=p_kw,b0=b0_kw,r0=r_kw,angles=angles,before=before,after=after

;+
; PROJECT:
;       SOHO - CDS/SUMER
;
; NAME:
;       ROT_XY()
;
; PURPOSE:
;       Get a solar rotated position for a given time interval.
;
; CATEGORY:
;       Utility, coordinates
;
; SYNTAX:
;       Result = rot_xy(xx, yy, interval [, date=date])
;       Result = rot_xy(xx, yy, tstart='96/4/19' [, tend=tend])
;
; INPUTS:
;       XX       - Solar X position (arcsecs) of the point; can be a vector
;                  (in this case YY must be a vector with same elements)
;       YY       - Solar Y position (arcsecs) of the point; can be a vector
;                  (in this case XX must be a vector with same elements)
; OPTIONAL INPUTS:
;       INTERVAL - Time interval in seconds; positive (negative) value
;                  leads to forward (backward) rotation. If INTERVAL
;                  is not given, a beginning time must be given via
;                  the TSTART keyword
;
; OUTPUTS:
;       RESULT - A (Mx2) array representing rotated positions in arcsecs,
;                RESULT(*,0) being X position and RESULT(*,1) Y position;
;                where M is number of elements in XX (and in YY).
;                If an error occurs, [-9999,-9999] will be returned.
;
;                If OFFLIMB = 1 after the call, there must be some
;                points rotated to the back of the sun. The points remain
;                visible can be determined by RESULT(INDEX,*), and
;                off-limb points will have the value of (-9999, -9999).
;
; OPTIONAL OUTPUTS:
;       None.
;
; KEYWORDS:
;       DATE    - Date/time at which the sun position is calculated; can
;                 be in any UTC format. If missing, current date/time is
;                 assumed.
;       TSTART  - Date/time to which XX and YY are referred; can be in
;                 any acceptable time format. Must be supplied if
;                 INTERVAL is not passed
;       TEND    - Date/time at which XX and YY will be rotated to; can be
;                 in any acceptable time format. If needed but missing,
;                 current time is assumed
;       SOHO    - Set this keyword to use SOHO view. If this keyword is
;                 missing, the point of view will depend on whether the
;                 env variable SC_VIEW is set 1 or not
;       ERROR   - Error message returned; if there is no error, a null
;                 string is returned
;       OFFLIMB - A named variable indicating whether any rotated
;                 point is off the limb (1) or not (0). When OFFLIMB
;                 is 1, the points still remaining visible (inside the limb)
;                 will be those whose indices are INDEX (below)
;       INDEX   - Indices of XX/YY which remain inside the limb after
;                 rotation. When OFFLIMB becomes 1, the number of
;                 INDEX will be smaller than that of XX or YY. If no
;                 point remains inside the limb, INDEX is set to -1
;       BACK_INDEX  - Indices of XX/YY which were on the disk at TSTART, 
;                 but are no longer visible (i.e. are behind the visible 
;                 solar disk) after rotation. 
;       KEEP    - keep same epoch DATE when rotating; use same P,B0,R values 
;                 both for DATE and DATE+INTERVAL
;       
;       RADIUS  - solar radius value [output in arcmin units]
;
;       P       - If specified, override PB0R for P angle
;       B0      - If specified, override PB0R for B0 angle
;       R0      - If specified, override PB0R for solar apparent diameter:
;                 r0 is the observer's distance from the Sun, in solar radii
;       
;       SPHERE  - keyword passed to ARCMIN2HEL
;
;       BEFORE/AFTER - set to when to override P/B0/R0 with input values.
;
; HISTORY:
;       Version 1, March 18, 1996, Liyun Wang, NASA/GSFC. Written
;       Version 2, April 16, 1996, Liyun Wang, NASA/GSFC
;          Added SOHO keyword
;       Version 3, April 18, 1996, Liyun Wang, NASA/GSFC
;          Added keywords TSTART and TEND
;          Made INTERVAL an optional input parameter if TSTART is set
;       Version 4, July 15, 1996, Zarro, NASA/GSFC
;          Added KEEP keyword
;       Version 5, July 31, 1996, Liyun Wang, NASA/GSFC
;          Modified such that no calculation is done if interval is zero
;       Version 6, March 11, 1997, Liyun Wang, NASA/GSFC
;          Added OFFLIMB keyword
;       Version 7, May 16, 1997, Liyun Wang, NASA/GSFC
;          Added INDEX keyword
;          Fixed a bug feeding off-limb points to HEL2ARCMIN
;       Version 8, June 10, 1997, Liyun Wang, NASA/GSFC
;          Properly handled off-limb points
;       Version 9, June 10, 1997, Liyun Wang, NASA/GSFC
;          Properly handled off-limb points
;       Version 10, July 1 1997 , Zarro SAC/GSFC -- added _EXTRA
;       Version 11, Sept 10, 1997, Zarro SAC/GSFC 
;          Added RADIUS output keyword
;       Version 12, September 19, 1997, Liyun Wang, NASA/GSFC
;          Fixed a bug that caused INDEX undefined when no rotation is needed
;       Version 13, November 20, 1998, Zarro (SM&A) - returned scalar INDEX
;          in place of INDEX[1]
;       Version 14, January 9, 1998, Zarro (SMA/GSFC) - made use of /SOHO
;          more consistent
;       Version 15, April 10, 1999, Andretta (CUA/GSFC)
;          - Added keywords P,B0,R0 (see ARCMIN2HEL and HEL2ARCMIN).
;          - Keyword KEEP has now the effect of saving one call to PB0R 
;          (useful when it can be assumed that during the rotation interval 
;          the P, B0, R parameters do no change significantly). 
;          - Added kewyord BACK_INDEX to keep track of points no longer 
;          visible because they have been rotated beyond the limb.
;       Version 16, 22-Aug-2001, Zarro (EITI/GSFC)
;          -Added ANGLES keyword
;       Version 17, 11-Mar-2002, Andretta/Zarro (GSFC)
;          - fixed time interval & SOHO keyword bug
;       Version 18, 10-Jan-2005, Zarro (L-3Com/GSFC)
;          - added /BEFORE, /AFTER. If P, B0, & R0 are entered via
;            keywords, then you can choose to apply them before or after
;            differential rotation. The default is to apply them at the
;            start and end of the rotation period, even though it doesn't
;            make sense to apply them at both times since at least one of 
;            them will change.

; CONTACT:
;       Zarro (dzarro@solar.stanford.edu)
;-
   error = ''
   offlimb = 0
   dum=[-9999,-9999]
   IF N_PARAMS() LT 2 THEN BEGIN
      error = 'Syntax: a = rot_xy(x, y, interval)'
      MESSAGE, error, /cont
      RETURN, dum
   ENDIF
   IF N_ELEMENTS(xx) NE N_ELEMENTS(yy) THEN BEGIN
      error = 'XX and YY must be scalar or vector with the same elements'
      MESSAGE, error, /cont
      RETURN,dum
   ENDIF

   IF N_ELEMENTS(interval) EQ 0 AND N_ELEMENTS(tstart) EQ 0 THEN BEGIN
      error = 'You need to specify a time interval'
      MESSAGE, error, /cont
      RETURN,dum
   ENDIF

;---------------------------------------------------------------------------
;  Determine time interval
;---------------------------------------------------------------------------
   IF N_ELEMENTS(interval) EQ 0 THEN BEGIN
      date = anytim2utc(tstart)
      IF N_ELEMENTS(tend) EQ 0 THEN get_utc, tend
      interval = anytim2tai(tend)-anytim2tai(date)
   ENDIF

;----------------------------------------------------------------------
; Deal with P, B0, and R.  If all three are specified, don't bother
; finding out where the Earth was...
;----------------------------------------------------------------------
   if keyword_set(keep) then begin
     if (1-is_number(p_kw)) or (1-is_number(b0_kw)) or (1-is_number(r_kw)) then begin
        ;--------------------------------------------------------------------
        ;  get B0 and solar radius
        ;--------------------------------------------------------------------
          angles = pb0r(date, soho=soho, error=error)

        ;--------------------------------------------------------------------
        ;  normalize, check if off limb, project back to limb
        ;--------------------------------------------------------------------
          if 1-is_number(p_kw) then p_kw = angles[0]
          if 1-is_number(b0_kw) then b0_kw = angles[1]
          if 1-is_number(r_kw) then r_kw = 1./tan(angles[2]/(60.*!radeg))
     endif
    endif

;---------------------------------------------------------------------------
;  Compute heliographic coordinates
;---------------------------------------------------------------------------

   before=keyword_set(before)  
   after=keyword_set(after)
   before_1=before and (1-after)
   after_1=after and (1-before)

   if after_1 then begin
    temp = TRANSPOSE(arcmin2hel(xx/60.0,yy/60.0,date=date,off=off,soho=soho,$
                               radius=radius,/no_copy,sphere=sphere, $
                               _extra=extra))
   endif else begin
    temp = TRANSPOSE(arcmin2hel(xx/60.0,yy/60.0,date=date,off=off,soho=soho,$
                               radius=radius,/no_copy,sphere=sphere, $
                               p=p_kw,b0=b0_kw,r0=r_kw,_extra=extra))
   endelse
   
;---------------------------------------------------------------------------
;  If time interval is zero, bail out unless b0_kw is entered
;---------------------------------------------------------------------------

   b0_1=is_number(b0_kw)
   IF (interval EQ 0.d0) and (1-b0_1) THEN BEGIN
      MESSAGE, 'Zero time interval. No need to rotate.', /cont
      index = WHERE(temp[*, 0] NE -9999.0)
      if n_elements(index) eq 1 then index=index[0]
      back_index=-1
      RETURN, [[xx], [yy]]
   ENDIF

   IF N_ELEMENTS(date) EQ 0 THEN get_utc, date
   tmp = anytim2utc(date, err=error)
   IF error NE '' THEN BEGIN
      MESSAGE, error, /cont
      RETURN,dum
   ENDIF

   out = WHERE(off EQ 1, count)
   IF count EQ N_ELEMENTS(xx)  THEN BEGIN
      error = 'All initial points are off the limb; cannot rotate!'
      temp[*, *] = -9999.0
      offlimb = 1
      index = -1
      back_index=-1
      RETURN, temp
   ENDIF
   in = WHERE(off EQ 0)

   IF count NE 0 THEN BEGIN
      temp[out, 0] = 1.0
      temp[out, 1] = -9999.0
      offlimb = 1
   ENDIF

   ddays =float(interval)/86400.0
   temp[*, 1] = temp[*, 1]+diff_rot(ddays, temp[*, 0], /synodic,_extra=extra)

   IF NOT KEYWORD_SET(keep) THEN $
    tmp= anytim2utc( anytim2tai(tmp)+double(interval) )

   back_index=-1
   IF offlimb EQ 0 THEN BEGIN
      if before_1 then begin
       temp = 60.0*TRANSPOSE(hel2arcmin([temp[*, 0]], [temp[*, 1]], $
               front,soho=soho,date=tmp,_extra=extra))
      endif else begin
       temp = 60.0*TRANSPOSE(hel2arcmin([temp[*, 0]], [temp[*, 1]], $
               front,soho=soho,date=tmp,p=p_kw,b0=b0_kw,r0=r_kw,_extra=extra))
      endelse
      back_index = WHERE(front EQ 0)
      IF back_index(0) ge 0 THEN BEGIN
        temp[back_index,*] = -9999.0
        offlimb = 1
      ENDIF
   ENDIF ELSE BEGIN
      IF in[0] GE 0 THEN BEGIN
         subtemp = temp[in, *]
         if before_1 then begin
          subtemp = 60.0*TRANSPOSE(hel2arcmin([subtemp[*, 0]], [subtemp[*, 1]],$
                    subfront, _extra=extra,soho=soho,date=tmp))
         endif else begin
          subtemp = 60.0*TRANSPOSE(hel2arcmin([subtemp[*, 0]], [subtemp[*, 1]],$
                        subfront, _extra=extra,$
                        soho=soho,date=tmp,p=p_kw,b0=b0_kw,r0=r_kw))
         endelse
         temp[in, *] = subtemp
         temp[out, *] = -9999.0
         bck_index = WHERE(subfront EQ 0)
         IF bck_index[0] ge 0 THEN BEGIN
           back_index = in[bck_index]
           temp[back_index, *] = -9999.0
         ENDIF
      ENDIF ELSE temp[*,*] = -9999.0
   ENDELSE

   index = WHERE(temp[*,0] NE -9999.0)
   if n_elements(index) eq 1 then index=index[0]
   RETURN, temp
END


	FUNCTION SIGRANGE,ARRAY,FRACTION=FRACTION,MISSING=MISSING,RANGE=RANGE
;+
; Project     : SOHO - CDS
;
; Name        : 
;	SIGRANGE()
; Purpose     : 
;	Selects the most significant data range in an image.
; Explanation : 
;	Selects out the most significant range in the data to be used in 
;	displaying images.  The histogram of ARRAY is used to select the most
;	significant range.
; Use         : 
;	OUTPUT = SIGRANGE( ARRAY )
; Inputs      : 
;	ARRAY	 = Array to take most significant range of.
; Opt. Inputs : 
;	None.
; Outputs     : 
;	The function returns an array where values above and below the
;	selected range are set equal to the maximum and minimum of the
;	range respectively.
; Opt. Outputs: 
;	None.
; Keywords    : 
;	FRACTION = Fraction of data to consider most significant.
;		   Defaults to 0.99
;	MISSING	 = Value used to flag missing points.  Data points with this
;		   value are not considered or changed.
;       RANGE    = Return the range used
;
; Calls       : 
;	GET_IM_KEYWORD
; Common      : 
;	None.
; Restrictions: 
;	ARRAY must have more than two points.  Fraction must be greater than 0 
;	and less than 1.
;
;	In general, the SERTS image display routines use several non-standard
;	system variables.  These system variables are defined in the procedure
;	IMAGELIB.  It is suggested that the command IMAGELIB be placed in the
;	user's IDL_STARTUP file.
;
;	Some routines also require the SERTS graphics devices software,
;	generally found in a parallel directory at the site where this software
;	was obtained.  Those routines have their own special system variables.
;
; Side effects: 
;	None.
; Category    : 
;	None.
; Prev. Hist. : 
;	W.T.T., Oct. 1987.
;	W.T.T., Jan. 1991.  Changed FRACTION to keyword, and added keyword
;			    BADPIXEL.
;	W.T.T., Mar. 1992.  Rewrote to apply histograms to only a fraction of
;			    the array to speed up the process.
;	William Thompson, August 1992, renamed BADPIXEL to MISSING.
; Written     : 
;	William Thompson, GSFC, October 1987.
; Modified    : 
;	Version 1, William Thompson, GSFC, 12 May 1993.
;		Incorporated into CDS library.
;	Version 2, William Thompson, GSFC, 25 May 1993.
;		Changed call to HISTOGRAM to be compatible with OpenVMS/ALPHA
;       Version 3, CDP, RAL, Add RANGE keyword.  16-Apr-96
;	Version 4, William Thompson, GSFC, 17 April 1996
;		Corrected some problems when range is too high.
;	Version 5, 13-Jan-1998, William Thompson, GSFC
;		Use random numbers to improve statistics when only using a
;		fraction of the array.
;	Version 6, 06-Mar-1998, William Thompson, GSFC
;		Change default to 0.99
;       Version 7, 28-Mar-2006, William Thompson, GSFC
;               Made more robust
;
; Version     : 
;	Version 6, 06-Mar-1998
;-
;
	GET_IM_KEYWORD, MISSING, !IMAGE.MISSING
;
	IF N_ELEMENTS(FRACTION) NE 1 THEN FRACTION = 0.99
	IF N_ELEMENTS(ARRAY) LE 2 THEN BEGIN
	    MESSAGE, /CONTINUE, 'Not enough points to form histogram'
	    RETURN, ARRAY
	END ELSE IF (FRACTION LE 0) OR (FRACTION GE 1) THEN BEGIN
	    MESSAGE, /CONTINUE, 'Fraction must be GT 0 and LT 1'
	    RETURN, ARRAY
	ENDIF
;
;  To speed up the process, work on a reduced version of ARRAY.
;
	IF N_ELEMENTS(ARRAY) LT 10000 THEN ATEMP0 = ARRAY ELSE BEGIN
	    NN = 1000 > (N_ELEMENTS(ARRAY) / 25) < 100000
	    ATEMP0 = ARRAY(N_ELEMENTS(ARRAY)*RANDOMU(SEED,NN))
	ENDELSE
;
;  Get the total range of the data, excluding any missing points.
;
	ATEMP0 = GOOD_PIXELS(ATEMP0, MISSING=MISSING)
	N_TOTAL = N_ELEMENTS(ATEMP0)
        BMAX = MAX(ATEMP0, MIN=BMIN)
	AMAX = 1.*BMAX
	AMIN = 1.*BMIN
	IF AMIN EQ AMAX THEN GOTO, EXIT_POINT
;
;  Set up some initial parameters for the reiteration.
;
	ATEMP = ATEMP0
	DELTA = 0
;
;  Form the histogram, and calculate an array expressing the fraction of points
;  that fall within or below the given bin.
;
FIND_RANGE:
	LAST_DELTA = DELTA
	X = AMIN  +  FINDGEN(1001) * (AMAX - AMIN) / 1000.
	H = HISTOGRAM(LONG((ATEMP-AMIN)*1000./(AMAX - AMIN)))
	FOR I = 1,N_ELEMENTS(H)-1 DO H(I) = H(I) + H(I-1)
	H = H / FLOAT(N_TOTAL)
;
;  Estimate the endpoints corresponding to the specified range, and calculate
;  the values at these endpoints.  Limit the array to be within these values.
;
	IMIN = (MIN( WHERE( H GT ((1. - FRACTION) / 2.) )) - 1) > 0
	IMAX =  MIN( WHERE( H GT ((1. + FRACTION) / 2.) ))
	IF IMAX LT 0 THEN IMAX = 1000
	AMIN = MIN(ATEMP0(WHERE(ATEMP0 GE X(IMIN))))
	AMAX = MAX(ATEMP0(WHERE(ATEMP0 LE X(IMAX))))
;
;  If the calculated range is zero, then use 2% of the full range of the data.
;
        IF AMAX EQ AMIN THEN BEGIN
            W = WHERE(ATEMP0 GE (AMAX + 0.01*(BMAX-BMIN)), COUNT)
            IF COUNT GT 0 THEN AMAX = MIN(ATEMP0(W))
            W = WHERE(ATEMP0 LE (AMIN - 0.01*(BMAX-BMIN)), COUNT)
            IF COUNT GT 0 THEN AMIN = MAX(ATEMP0(W))
	ENDIF
;
;  If the range calculated has changed by more than 5% from the last iteration,
;  the reiterate.
;
	ATEMP = AMIN > ATEMP0 < AMAX
	DELTA = AMAX - AMIN
	RATIO = (DELTA - LAST_DELTA) / (DELTA + LAST_DELTA)
	IF ABS(RATIO) GT 0.05 THEN GOTO, FIND_RANGE
;
;  If a missing pixel flag value was passed, then reset those points to the
;  flag value.  Return the adjusted array.
;
EXIT_POINT:
	ATEMP = AMIN > ARRAY < AMAX
	IF N_ELEMENTS(MISSING) EQ 1 THEN BEGIN
		WW = WHERE(ARRAY EQ MISSING,N_MISSING)
		IF N_MISSING GT 0 THEN ATEMP(WW) = MISSING
	ENDIF
        RANGE = [AMIN,AMAX]
	RETURN, ATEMP
	END

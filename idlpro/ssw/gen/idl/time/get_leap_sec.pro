	PRO GET_LEAP_SEC, MJD, ERRMSG=ERRMSG
;+
; Project     :	SOHO - CDS
;
; Name        :	GET_LEAP_SEC
;
; Purpose     :	Returns the dates of all known leap seconds.
;
; Explanation :	The routine returns the Modified Julian Day number of all known
;		leap seconds.  The dates are taken from the file
;		'leap_seconds.dat' in the directory given by the environment
;		variable TIME_CONV.  That file is read in the first time that
;		GET_LEAP_SEC is called.  These values are then stored in an
;		internal common block for subsequent calls.  The file is also
;		reread whenever the current day number changes.
;
; Use         :	GET_LEAP_SEC, MJD
;
; Inputs      :	None.
;
; Opt. Inputs :	None.
;
; Outputs     :	MJD	= An array containing the Modified Julian Day numbers
;			  for all dates on which a leap second was inserted,
;			  starting with 31 December 1971.
;
; Opt. Outputs:	None.
;
; Keywords    :	ERRMSG	=  If defined and passed, then any error messages 
;			   will be returned to the user in this parameter 
;			   rather than using the IDL MESSAGE utility.  If no
;			   errors are encountered, then a null string is
;			   returned.  In order to use this feature, the 
;			   string ERRMSG must be defined first, e.g.,
;
;				ERRMSG = ''
;				GET_LEAP_SEC, MJD, ERRMSG=ERRMSG
;				IF ERRMSG NE '' THEN ...
;
; Calls       :	FIND_WITH_DEF, GET_UTC
;
; Common      :	LEAP_SECONDS is an internal common block.
;
; Restrictions:	This procedure requires a file containing the dates of all leap
;		second insertions starting with 31 December 1971.  This file
;		must have the name 'leap_seconds.dat', and must be in the
;		directory given by the environment variable TIME_CONV.  It must
;		be properly updated as new leap seconds are announced.
;
; Side effects:	None.
;
; Category    :	Utilities, time.
;
; Prev. Hist. :	None.
;
; Written     :	William Thompson, GSFC, 21 September 1993.
;
; Modified    :	Version 1, William Thompson, GSFC, 21 September 1993.
;		Version 2, William Thompson, GSFC, 14 November 1994
;			Changed .DAY to .MJD
;		Version 3, Donald G. Luttermoser, GSFC/ARC, 20 December 1994
;			Added the keyword ERRMSG.
;		Version 4, Donald G. Luttermoser, GSFC/ARC, 30 January 1995
;			Added ERRMSG keyword to internally called procedures.
;			Made the error handling procedures more robust.
;		Version 5, William Thompson, GSFC, 15 March 1995
;			Changed CDS_TIME to TIME_CONV
;               Version 6, Zarro, SM&A/GSFC 6 May 1999
;                       Added /CONT to MESSAGE
;
; Version     :	Version 6
;-
;
	ON_ERROR, 2  ; Return to the caller of this procedure if error occurs.
	MESSAGE=''   ; Error message that is returned if ERRMSG keyword set.
	COMMON LEAP_SECONDS, LAST_READ, LEAP_MJD
;
;  Check the number of parameters.
;
	IF N_PARAMS() NE 1 THEN BEGIN
		MESSAGE = 'Syntax:  GET_LEAP_SEC, MJD'
		GOTO, HANDLE_ERROR
	ENDIF
;
;  Get the current date as a Modified Julian Day number.
;
	GET_UTC, UTC, ERRMSG=ERRMSG
	IF N_ELEMENTS(ERRMSG) NE 0 THEN $
		IF ERRMSG(0) NE '' THEN RETURN
	TODAY = UTC.MJD
;
;  If the common block hasn't been initialized yet, then read in the data.
;
	IF N_ELEMENTS(LAST_READ) EQ 0 THEN GOTO, READ_FILE
;
;  If the current date is larger than the date stamp for the last time the file
;  was read, then reread the file.
;
	IF TODAY GT LAST_READ THEN GOTO, READ_FILE
;
;  Otherwise, simply return what is in the common block.
;
	GOTO, EXIT_POINT
;
;  Read the data file into the common block, and time-tag the common block with
;  today's date.
;
READ_FILE:
	FILENAME = FIND_WITH_DEF('leap_seconds.dat','TIME_CONV','')
	IF FILENAME EQ '' THEN BEGIN
		MESSAGE = 'Unable to open "leap_seconds.dat".'
		GOTO, HANDLE_ERROR
	ENDIF
	OPENR,UNIT,FILENAME,/GET_LUN
	MJD0 = 0L
	READF,UNIT,MJD0
	LEAP_MJD = MJD0
	WHILE NOT EOF(UNIT) DO BEGIN
		READF,UNIT,MJD0
		LEAP_MJD = [LEAP_MJD, MJD0]
	ENDWHILE
	FREE_LUN, UNIT
	LAST_READ = TODAY
;
;  Return the array from the common block.
;
EXIT_POINT:
	MJD = LEAP_MJD
	IF N_ELEMENTS(ERRMSG) NE 0 THEN ERRMSG = MESSAGE
	RETURN
;
;  Error handling point.
;
HANDLE_ERROR:
	IF N_ELEMENTS(ERRMSG) EQ 0 THEN MESSAGE, MESSAGE,/CONT ELSE $
         ERRMSG = MESSAGE
        LEAP_MJD=0l
        MJD=LEAP_MJD
        LAST_READ = TODAY
	RETURN
;
	END


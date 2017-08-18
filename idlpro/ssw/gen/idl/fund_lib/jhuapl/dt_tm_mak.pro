;-------------------------------------------------------------
;+
; NAME:
;       DT_TM_MAK
; PURPOSE:
;       Set up a time and date string from reference JD and offset.
; CATEGORY:
; CALLING SEQUENCE:
;       s = dt_tm_mak(jd0, [sec])
; INPUTS:
;       jd0 = Julian Date of a reference date (0:00 hr).  in
;       sec = Seconds since JD0 at 0:00.                  in
; KEYWORD PARAMETERS:
;       Keywords:
;         FORMAT = format string.  Allows output date to be customized.
;         The default format string is 'Y$ n$ d$ h$:m$:s$ w$'
;            The following substitutions take place in the format string:
;         Y$ = 4 digit year.
;         y$ = 2 digit year.
;         N$ = full month name.
;         n$ = 3 letter month name.
;         0n$= month as a 2 digit number.
;         d$ = day of month number.
;         0d$= 2 digit day of month number.
;         doy$= 3 digit day of year.
;         W$ = full weekday name.
;         w$ = 3 letter week day name.
;         h$ = hour.
;         m$ = minute.
;         s$ = second.
;         f$ = fraction of second (see DECIMAL, DENOMINATOR below).
;         I$ = time interval in days to 2 decimal places.
;         i$ = time interval in days as an integer.
;         H$ = time interval in integer hours.
;         @  = Carriage Return.
;         !  = Line feed.
;        DECIMAL=dp  Number of decimal places to use for fraction of
;          second (def=3) for f$ in format.  f$ will include dec pt.
;        DENOMINATOR=den If given then fraction is listed as nnn/ddd
;          ddd is given by den.  Over-rides DECIMAL keyword.  Ex:
;          DENOM=1000 might give 087/1000 for f$ in format.
; OUTPUTS:
;       S = resulting string.                             out
; COMMON BLOCKS:
; NOTES:
;       Notes: Some examples: 'h$:m$:s$' -> 09:12:04,
;         'd$ n$ Y$' -> 12 Jan 1991, 'd$D h$h' -> 3D 2h, ...
; MODIFICATION HISTORY:
;       R. Sterner.  17 Nov, 1988.
;       Johns Hopkins University Applied Physics Laboratory.
;       RES  20 Apr, 1989 --- 2 digit year.
;       R. Sterner, 26 Feb, 1991 --- Renamed from time_date_str.pro
;       R. Sterner, 27 Feb, 1991 --- Renamed from tm_dt_str.pro
;       R. Sterner, 28 Feb, 1991 --- changed format.
;       R. Sterner, 17 Jun, 1992 --- fixed a bug for large sec.
;       R. Sterner, 27 Sep, 1993 --- Modified to handle arrays.
;       R. Sterner,  2 Dec, 1993 --- Slightly modified def format.
;       R. Sterner, 1994 Jun 15 --- Added fraction of second.
;       R. Sterner, 1995 Mar  8 --- Added i$ format.
;       R. Sterner, 1995 Jul  6 --- Added 0d$ format.
;       R. Sterner, 1997 Feb  3 --- Added new keywords 0n$ and doy$ to
;       give month as 2 digit number and day of year.
;       Matthew Savoie, Systems Technology Associates --- 1997 Feb 5
;       fixed a bug by adding floor to: days = long(floor(idays)).
;       R. Sterner, 1997 Dec 18 --- Added DDECIMAL=ddec for number of
;       decimal places in Day of Year.
;
; Copyright (C) 1988, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------
 
 
	function dt_tm_mak, jd0, sec, format=frmt, decimal=dec, $
	  denominator=den, ddecimal=ddec, help=hlp
 
	if (n_params(0) lt 1) or (keyword_set(hlp)) then begin
	  print,' Set up a time and date string from reference JD and offset.
	  print,' s = dt_tm_mak(jd0, [sec])
	  print,'   jd0 = Julian Date of a reference date (0:00 hr).  in'
	  print,'   sec = Seconds since JD0 at 0:00.                  in'
	  print,'   S = resulting string.                             out'
          print,' Keywords:'
          print,'   FORMAT = format string.  Allows output date to be '+$
            'customized.'
          print,"   The default format string is 'Y$ n$ d$ h$:m$:s$ w$'"
          print,'      The following substitutions take place in the '+$
            'format string:'
          print,'   Y$ = 4 digit year.'
          print,'   y$ = 2 digit year.'
          print,'   N$ = full month name.'
          print,'   n$ = 3 letter month name.'
          print,'   0n$= month as a 2 digit number.'
          print,'   d$ = day of month number.'
          print,'   0d$= 2 digit day of month number.'
	  print,'   doy$= 3 digit day of year.'
          print,'   W$ = full weekday name.'
          print,'   w$ = 3 letter week day name.'
	  print,'   h$ = hour.'
	  print,'   m$ = minute.'
	  print,'   s$ = second.'
	  print,'   f$ = fraction of second (see DECIMAL, DENOMINATOR below).'
	  print,'   I$ = time interval in days to 2 decimal places.'
	  print,'   i$ = time interval in days as an integer.'
	  print,'   H$ = time interval in integer hours.'
	  print,'   @  = Carriage Return.'
	  print,'   !  = Line feed.'
	  print,'  DECIMAL=dp  Number of decimal places to use for fraction of'
	  print,'    second (def=3) for f$ in format.  f$ will include dec pt.'
	  print,'  DENOMINATOR=den If given then fraction is listed as nnn/ddd'
	  print,'    ddd is given by den.  Over-rides DECIMAL keyword.  Ex:'
	  print,'    DENOM=1000 might give 087/1000 for f$ in format.'
	  print," Notes: Some examples: 'h$:m$:s$' -> 09:12:04,"
	  print,"   'd$ n$ Y$' -> 12 Jan 1991, 'd$D h$h' -> 3D 2h, ..."
	  return, -1
	endif
 
	if n_params(0) lt 2 then sec = 0.	; Default seconds are 0.
	num = n_elements(sec)			; Number of strings to make.
 
        ;-----  format string  ------
        fmt = 'Y$ n$ d$ h$:m$:s$ w$'		; Default format.
        if keyword_set(frmt) then fmt = frmt	; Use given format.
 
        ;-----  Get all the allowed parts  -----
	idays = sec/86400d0			; Seconds to interval in days
	days = long(floor(idays))		; Interval to integer days
	rem = sec - days*86400d0		;   and left over seconds.
	jd2ymd, jd0+days, y, m, d		; Find Yr, Mon, Day.
	if n_elements(ddec) eq 0 then ddec=0	; Default for doy is integer.
	doy = string(ymd2dn(y,m,d),form='(I3.3)')  ; Day of year.
	if ddec gt 0 then begin			; If DOY fraction requested.
	  doyfrm = '(F'+strtrim(2+ddec,2)+'.'+strtrim(ddec,2)+')'
	  doyfrac = string(sec/86400d0,form=doyfrm)	; Find fraction of day.
	  doy = doy + strmid(doyfrac,1,99)	; Concatenate on fraction.
	endif
        yu = strtrim(y,2)			; 4 Digit year.
        yl = strtrim(fix(y-100*fix(y/100)),2)	; 2 digit year.
        mnames = monthnames()			; List of names.
        mu = mnames(m)				; Long month name.
        ml = strmid(mu,0,3)			; 3 letter month name.
	mn = string(m,form='(I2.2)')		; Month as a 2-digit number.
        dl = strtrim(d,2)			; Day of month.
	dl0 = dl				;   Leading 0 form.
	w = where(d lt 10, cnt)			; Look for day<10.
	if cnt gt 0 then begin
	  dl(w)=' '+dl(w)			;   Leading space form.
	  dl0(w) = '0'+dl0(w)			;   Leading 0 form.
	endif
        wu = weekday(y,m,d)			; Long weekday name.
        wl = strmid(wu,0,3)			; 3 letter weekday name.
 
	sechms, rem, h, m, s, hh, mm, ss	; Find Hr, Min, Sec.
	ii = strtrim(string(idays,format='(f20.2)'),2)
	ii2 = strtrim(days,2)
	hh2 = string(idays*24.,format='(I0)')
 
	;-------  Handle fraction of second  ---------
	frac = s - floor(s)
	if keyword_set(den) then begin		; nnn/ddd
	  wid = strtrim(ceil(alog10(den)),2)
	  dn = strtrim(den,2)
	  fm2 = '(I'+wid+'.'+wid+')'
	endif else begin			; .fff
	  dp = 3
	  if keyword_set(dec) then dp = dec
	  fm1 = '(f'+strtrim(dp+2,2)+'.'+strtrim(dp,2)+')'
	endelse
	ff = strarr(num)
	for i = 0, num-1 do begin
	  if keyword_set(den) then begin
	    ff(i) = string(den*frac(i),form=fm2)+'/'+dn    ; Add denominator.
	  endif else begin
	    ff(i) = strmid(string(frac(i),form=fm1),1,99)  ; Keep dec. point.
	  endelse
	endfor
 
	;------  Replacements  -------
	out = strarr(num)
	for i = 0, num-1 do begin
	  tmp = fmt
	  tmp = stress(tmp, 'R', 0, 'I$', ii(i))   ; Interval in days (2 dp).
	  tmp = stress(tmp, 'R', 0, 'i$', ii2(i))  ; Interval in days (int).
	  tmp = stress(tmp, 'R', 0, 'H$', hh2(i))  ; Interval in integer hours.
	  tmp = stress(tmp, 'R', 0, 'doy$',doy(i)) ; 3 digit day of year.
	  tmp = stress(tmp, 'R', 0, 'Y$', yu(i))   ; 4 digit year.
	  tmp = stress(tmp, 'R', 0, 'y$', yl(i))   ; 2 digit year.
	  tmp = stress(tmp, 'R', 0, 'N$', mu(i))   ; Long month name.
	  tmp = stress(tmp, 'R', 0, '0n$',mn(i))   ; Month as 2-digit number.
	  tmp = stress(tmp, 'R', 0, 'n$', ml(i))   ; 3 letter month name.
	  tmp = stress(tmp, 'R', 0, 'W$', wu(i))   ; Long weekday name.
	  tmp = stress(tmp, 'R', 0, 'w$', wl(i))   ; 3 letter weekday name.
	  tmp = stress(tmp, 'R', 0, '0d$',dl0(i))  ; Day of month (leading 0).
	  tmp = stress(tmp, 'R', 0, 'd$', dl(i))   ; Day of month.
	  tmp = stress(tmp, 'R', 0, 'h$', hh(i))   ; Hour.
	  tmp = stress(tmp, 'R', 0, 'm$', mm(i))   ; Minute.
	  tmp = stress(tmp, 'R', 0, 's$', ss(i))   ; Second.
	  tmp = stress(tmp, 'R', 0, 'f$', ff(i))   ; Fraction of second.
	  tmp = repchr(tmp, '@', string(13B))	   ; <CR>
	  tmp = repchr(tmp, '!', string(10B))	   ; <LF>
	  out(i) = tmp
	endfor
 
	if n_elements(out) eq 1 then return, out(0)
	return, out
	end

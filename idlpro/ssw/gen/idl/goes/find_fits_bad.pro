;+ FIND_FITS_BAD
; Finds the element numbers in tarray and yarray that have bad points that
; need to be eliminated and interpolated over.  Bad points means any of the
; following are true:
;   1. flux value is -99999.
;   2. the moon is eclipsing the Sun
;   3. the detector is off
;   4. the detector is being calibrated
;   5. the channel is going through a gain change
; The bad elements numbers are returned for channels 0 and 1 in bad0 and bad1.
;
; Kim Tolbert 7/13/93
; Modifications:
; 9-Dec-2005 - stop printing the whoops message if no times match status times. With
;   new goes obj, could have status from a large time interval, but tarray from a subset.
;
;-
pro find_fits_bad, tarray, yarray, bad0, bad1, numstat, tstat, stat

;for i=0,n_elements(tstat)-1 do print,atime(tstat(i)),' ',stat(i,0),stat(i,1),$
;   form='(2a,2o10)'

; Convert status words from float to long words so we can examine bits
; using AND operator.
lstat = long(stat)

; Set bit patterns for eclipse, detector off, calibration mode, and gain
; change for two channels.
eclipse = '1000'o
detoff = '1'o
calib = '2'o
gainch = ['20'o, '40'o]

for ich = 0,1 do begin
   bad = where (yarray(*,ich) eq -99999.)
   ;print,'for channel ', ich, ' points = -99999.: ', bad
   if numstat eq 0 then goto, nextchan

   qbad = where((lstat(*,0) and eclipse) or (lstat(*,1) and detoff) or $
                (lstat(*,1) and calib) or (lstat(*,1) and gainch(ich)), nbad)
   for i = 0,nbad-1 do begin
      q = where(tarray eq tstat(qbad(i)), kq)
;      if kq eq 0 then print, 'Whoops.  No times matching status times.' $
;         else if (where(bad eq q(0)))(0) eq -1 then bad = [bad, q]
      if kq ne 0 then if (where(bad eq q(0)))(0) eq -1 then bad = [bad, q]
   endfor

   nextchan:

   if bad(0) eq -1 and n_elements(bad) gt 1 then bad = bad(1:*)

   if ich eq 0 then bad0 = bad else bad1 = bad
   ;print,'in find_fits_bad, chan=',ich,'  bad=',bad
endfor

return&end

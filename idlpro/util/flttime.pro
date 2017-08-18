;*************************************************************************
function flttime,timestr,sec=sec 
;  timestring (hh:mm:ss) --> float value (h)
;    97/09/29	k.i.	sec keyword
hh=float(strmid(timestr,0,2))
mm=float(strmid(timestr,3,2))
ss=float(strmid(timestr,6,2))
if not keyword_set(sec) then return,hh + mm/60. + ss/3600. $
else return,long(hh)*3600 + long(mm)*60 + long(ss)
end

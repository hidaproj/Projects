function today,ut=ut

tt=strtrim(str_sep(systime(),' '),2)

tt[1]=mmm2mm(tt[1])
time1=tt[3]
date1=strmid(tt[4],2,2)+'/'+tt[1]+'/'+tt[2]
yy0=strmid(tt[4],0,2)

if keyword_set(ut) then jst2ut,date1,time1

date=yy0+strjoin(str_sep(date1,'/'))

return,date

end
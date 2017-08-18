; sqrem1.pro
; read SQREM ASCII file and plot temperatures
;	99/02/15	k.i.
@sqremlib

sqdir='c:\system\sqrem'
ff=findfile(sqdir,count=count)
if count eq 0 then sqdir='c:\usr\sqrem'
datadir	=sqdir+'\datfiles\orig\'
files='nkr_00'+['16','17','18','19']+'.l01'
files='nkr_00'+['18','19']+'.l01'
;psyoko &	mono=1
set_plot,'win' &	mono=0

nf=n_elements(files)
rdascdat,datadir+files(0),year,doy,ltim,temp

ltime=doy*24l*3600+ltim
for i=1,nf-1 do begin
	rdascdat,datadir+files(i),year1,doy1,ltim1,temp1
	ltime1=doy1*24l*3600+ltim1
	year=[year,year1]
	ltime=[ltime,ltime1]
	temp=[[temp],[temp1]]
endfor
fd=fday1998(year,ltime)
ii=sort(fd)
year=year(ii) &	ltime=ltime(ii) &	temp=temp(*,ii)

pltsqrem1,year,ltime,temp,mono=mono

if keyword_set(giffile) then win2gif,giffile


end

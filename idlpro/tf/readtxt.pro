pro readtxt,file1,magc,fcount
;file1----->data file
close,1
i=0L
openr,1,file1
while not EOF(1) do begin
	magc1=''
	readf,1,magc1
i=long(i+1)
endwhile
close,1
openr,1,file1
fcount=i
magc=strarr(fcount)
for j=0L,fcount-1 do begin
	magc1=''
	readf,1,magc1
	magc(j)=magc1
endfor
close,1
end
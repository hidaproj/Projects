;+
; function rdcsv,file,skip=skip,nelem=nelem,sep=sep
;
; read CSV file and return strarr(*,*)
;  skip	-	# of lines to skip
;  nelem -	# of element for 1 line
;  sep	-	separator
;
;  02.06.23	k.i.
;  03.02.05	k.i.  skip keyword
;  05.11.30	k.i.  get count first
;  06.03.16	k.i.  nelem keyword
;  10.05.21	k.i.  sep keyword
;  12.09.14	k.i.  sep keyword bug fix
;  14.05.16	k.i.  pad blank line
;-
function rdcsv,file,skip=skip,nelem=nelem,sep=sep

str1=''
openr,1,file
if keyword_set(skip) then for i=0,skip-1 do readf,1,str1
if not keyword_set(sep) then sep=','
count=0
while not eof(1) do begin
	readf,1,str1
	count=count+1
endwhile
strs=strsep(str1,sep=sep)
if not keyword_set(nelem) then ns=n_elements(strs) else ns=nelem

point_lun,1,0
if keyword_set(skip) then for i=0,skip-1 do readf,1,str1
strs=strarr(ns,count)
for i=0,count-1 do begin
	readf,1,str1
	strs1=strsep(str1,sep=sep)
	ns1=n_elements(strs1)
	nst=min([ns1,ns])
	strs[0:nst-1,i]=strs1[0:nst-1]
endfor
close,1
return,strs

end

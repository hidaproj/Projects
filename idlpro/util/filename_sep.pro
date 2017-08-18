;*************************************************************************
;NAME       : filename_sep (function)
;FUNCTION   : separate file name to dir, fname, ext
;PROGRAMER  : 2005.01.30 k.i.
;	      2006.12.13  "  unix
;=========================================================================
pro filename_sep,file,dir,fname,ext,file=filename

case !version.os_family of
  'Windows': 	div='\'
  'unix': 	div='/'
endcase
len=strlen(file)
ip=len
while ip ge 0 and strmid(file,ip,1) ne '.' do ip=ip-1
if ip ge 0 then begin
	ext=strmid(file,ip+1,len-ip-1)
endif else begin
	ext=''
	ip=len+1
endelse
id=len
while id ge 0 and strmid(file,id,1) ne div do id=id-1
if id ge 0 then begin
	dir=strmid(file,0,id)
	fname=strmid(file,id+1,ip-id-1)
endif else begin
	dir=''
	fname=strmid(file,0,ip-1)
endelse
filename=fname+'.'+ext

end

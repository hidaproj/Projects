; psout.pro
;  view/printout a PostScript file
;  by using gswin software (installed in 'c:\usr\gs261\')
;	98/01/23	k.i.
;	98/08/13	k.i.  convert to absolute path
pro psout,psfile0,printout=printout

if !version.os eq 'Win32' then 	dllfile='c:\nkrprj\cprog\oscom32.dll' $
else				dllfile='c:\nkrprj\cprog\oscomdll.dll'
gsdir='c:\usr\gs261\'
ff=findfile(gsdir+'gswin.exe',count=count)
if count eq 0 then begin
	print,'You need gswin software installed in '+gsdir+'...'
	return
endif

cdir=''
if not keyword_set(psfile0) then begin
	psfile0=!dir+'\idl.ps' 
endif else begin
	if not (strpos(psfile0,':') ne -1 or strmid(psfile0,0,1) eq '\') then begin
		cd,current=cdir
		cdir=cdir+'\'
	endif
endelse

ff=findfile(psfile0,count=count)
if count eq 0 then begin
	print,psfile0+' not found...'
	return
endif

psfile=cdir+psfile0
print,psfile
ip=strpos(psfile,'\')
while ip ne -1 do begin
	strput,psfile,'/',ip
	ip=strpos(psfile,'\')
endwhile

;----------  create tempgs.lst file  -------
cd,gsdir,current=old_dir
gslist='tempgs.lst'
openw,1,gslist
writeu,1,'('+psfile+') run'

if keyword_set(printout) then begin
	writeu,1,' quit'
	com='gswin.exe -sDEVICE=mswinprn -dNOPAUSE -f'+gslist
	print,'printing out '+psfile0+'.....'
endif else begin
	com='gswin.exe -f'+gslist
	print,'enter <ret> and "quit" in ghostview..'
endelse
close,1

retn = call_external(dllfile,'excom',com, value = [0b]) 

cd,old_dir

end

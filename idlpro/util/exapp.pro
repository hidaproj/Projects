; exapp.pro
;  execute application
;	98/01/25	k.i.
;	02/10/14	k.i.   32bit
pro exapp,com,dir=dir

if !version.os eq 'Win32' then 	dllfile='\nkrprj\cprog\oscom32.dll' $
else				dllfile='c:\nkrprj\cprog\oscomdll.dll'

if keyword_set(dir) then cd,dir,current=old_dir
retn = call_external(dllfile,'excom',com, value = [0b]) 
if keyword_set(dir) then cd,old_dir

end

; xinetics.pro

dllfile='C:\Projects\cprog\VS2005\RS232C\Debug\RS232C.dll'
COMx='COM1'
retn = call_external(dllfile,'CommOpen', COMx, $
	' baud=115200 parity=N data=8 stop=1', /PORTABLE )



retn = call_external(dllfile,'CommWrite','1')
retn = call_external(dllfile,'CommWrite','0')

;retn = call_external(dllfile,'CommClose')

;dllfile2='C:\Projects\cprog\VS2008\DM97lib.sln'

;retn = call_external(dllfile2,'DM97Open)

stop
end

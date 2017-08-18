; LUCEO quartz Lyot filter
;*****************************************************
pro comcom,com
;-----------------------------------------------------
; send command string to COMx
common qlflib,dllfile,PNs,Vss,Vms,Acs,Unknown,Dev_exist
retn = call_external(dllfile,'CommWrite',com)

end




;*****************************************************
common qlflib,dllfile,PNs,Vss,Vms,Acs,Unknown,Dev_exist
common hr2000_com, wd, p, sp1

@hr2000lib

value=1	;exposure [ms]

	p.expo=fix(value)
	print,'expo=',p.expo
	hr_setexpo,p.expo

sp1=hr_getsp1()

dllfile='C:\Projects\cprog\VS2010\RS232C_64\x64\Debug\RS232C_64.dll'

COMx='COM4'
if not keyword_set(COMx) then COMx='COM1'

retn = call_external(dllfile,'CommOpen', COMx, $
		' baud=19200 parity=N data=8 stop=1', /PORTABLE )

comcom,"C1O\r\n"

comcom,"M1O\r\n"

comcom,"M1C\r\n"

comcom,"M1S\r\n"

comcom,"M1+002000\r\n"

comcom,"C1F\r\n"

stop
retn = call_external(dllfile,'CommClose')

end

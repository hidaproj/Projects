; test_oscom.pro
;  testing dll function using oscom32.dll
;	2009.1.29	k.i.
oscomdll='C:\Projects\cprog\VS2005\oscom32\Debug\oscom32.dll'
oscomdll='C:\nkrprj\cprog\oscom32.dll'
;oscomdll='C:\Projects\cprog\oscom32\Debug\oscom32.dll'
;;oscomdll='C:\nkrprj\cprog\oscom32\Debug\oscom32.dll'
oscomdll='C:\Projects\cprog\VS2010\oscom64\x64\Debug\oscom64.dll'

com='Test_oscom'
dmy=call_external(oscomdll,'Dmsgbox',com,value=[0d,1d],/PORTABLE,/cdecl )

n=5303;
;dmy=call_external(oscomdll,'Dmsgboxl',n,value=[0d,0d],/PORTABLE,/cdecl )


;date1=call_external(oscomdll,'getdatestr',value=[0d],/S_VALUE)
;time1=call_external(oscomdll,'gettimestr',value=[0d],/S_VALUE)
;print,date1,'   ',time1

dmy=call_external(oscomdll,'Dmkdirs','c:\tmp\bb')

dmy=call_external(oscomdll,'excom','c:\windows\notepad.exe')

;dmy=call_external(oscomdll,'Dcopyfile','c:\tmp\a.gif','c:\tmp\b.gg2')


end
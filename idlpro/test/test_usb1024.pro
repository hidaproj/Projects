; test_oscom.pro
;  testing dll function using oscom32.dll
;	2009.1.29	k.i.
usbiodll='C:\Projects\cprog\USB1024LS\Debug\USB1024LS.dll'

com='Test_usb1024'
val=call_external(usbiodll,'init_usb1024',value=[0d])
print,val
stop


nn=100
vals=intarr(nn)
for i=0,nn-1 do begin
	vals[i]=call_external(usbiodll,'usb_digin_a',value=[0d])
	print,i,'  ',vals[i]
endfor

stop

val=call_external(usbiodll,'usb_dscan',value=[0d])

stop

;time1=call_external(usbiodll,'usb_trig_a0',value=[0d],/S_VALUE)

;==test0==;
;time2=call_external(usbiodll,'test',value=[1d],/I_VALUE)
;==test1==;
;time=intarr(50)
;for i=0,49 do begin
;  print,i
;  time[i]=call_external(usbiodll,'test1',value=[1d],/I_VALUE)
;endfor
;time2=intarr(49)
;for i=0,48 do time2[i]=time[i+1]-time[i]
;plot,time2,yrange=[990,1010],ytitle='[msec]',title='using C to look for trigger (1rev./sec)'
;write_png,'C:\Documents and Settings\corona\�f�X�N�g�b�v\anan\cwhile_1.png',tvrd()
;==test2==;
;time2=call_external(usbiodll,'test2',value=[1d],/I_VALUE)
;time3=time2/100
;==test3==;
usbiodll='C:\Projects\cprog\USB1024LS\Debug\USB1024LS.dll'
cval=call_external(usbiodll,'test3',val,value=[1d],/I_VALUE)



end
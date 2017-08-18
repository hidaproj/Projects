;PRO CDIO_Lib
;+
;
;  CDIO_Lib.pro
;
;Functions for CDIO8_8
;based on DST Poralization script by T. Anan
;
;20100911  T.A.
;
;
;
;========================headder==============================;
;PRO	  cdio_init
;PRO	  cdio_exit
;FUNCTION cdio_input
;FUNCTION cdio_outstate
;PRO	  cdio_o45
;PRO	  cdio_o90
;PRO	  cdio_o135
;PRO	  cdio_o180
;PRO	  cdio_o225
;PRO	  cdio_o270
;PRO	  cdio_o315
;PRO	  cdio_o360
;PRO	  cdio_op25
;PRO	  cdio_opjg
;PRO	  cdio_omjg
;PRO	  cdio_ostop
;PRO	  cdio_o0
;PRO	  cdio_o0set
;PRO	  cdio_ow0ad
;
;-

;========================include==============================;
;=========================main================================;


;**************************************************************
PRO cdio_init
;--------------------------------------------------------------
common cdiolib,diodll

diodll='C:\Projects\cprog\VS2005\DIO8\Debug\DIO8.dll'

r=call_external(diodll,'Cdio_Init',/all_value,/cdecl)
	if r ne 0 then print,'Error at CDIO8_8 initialize (r=',r,')'

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
	if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

END
;**************************************************************
PRO cdio_exit
;--------------------------------------------------------------
common cdiolib

r=call_external(diodll,'Cdio_Exit',/all_value,/cdecl)
	if r ne 0 then print,'Error at CDIO8_8 exit (r=',r,')'

END
;**************************************************************
FUNCTION cdio_input
;--------------------------------------------------------------
common cdiolib

InByte=call_external(diodll,'Cdio_InByte',/all_value,/cdecl)

Case InByte of
	0:	wpin='Non'
	1:	wpin='JOGing(+)'
	33:	wpin='JOGing(+)'
	2:	wpin='JOGing(-)'
	34:	wpin='JOGing(-)'
	3:	wpin='moving to destination'
	35:	wpin='moving to destination'
	4:	wpin='moving to origin'
	36:	wpin='moving to origin'
	32:	wpin='origin'
	6:	wpin='not yet find origin'
	7:	wpin='not yet find origin'
	8:	wpin='not yet find origin'
	9:	wpin='not yet find origin'
	10:	wpin='not yet find origin'
	12:	wpin='not yet find origin'
	13:	wpin='not yet find origin'
	14:	wpin='not yet find origin'
	15:	wpin='not yet find origin'
	38:	wpin='45°'
	39:	wpin='90°'
	40:	wpin='135°'
	41:	wpin='180°'
	42:	wpin='225°'
	44:	wpin='270°'
	45:	wpin='315°'
	46:	wpin='360°'
	47:	wpin='+22.5°'
	17:	wpin='Error'
	49:	wpin='Error'
	18:	wpin='abnormality of pulse'
	50:	wpin='abnormality of pulse'
	else:	wpin='?'
endcase

return,wpin

END
;**************************************************************
FUNCTION cdio_outstate
;--------------------------------------------------------------
common cdiolib

outstate=call_external(diodll,'Cdio_OutState',/all_value,/cdecl)

Case outstate of
	0:	wpout='Non'
	1:	wpout='Rotate 45°'
	2:	wpout='Rotate 90°'
	3:	wpout='Rotate 135°'
	4:	wpout='Rotate 180°'
	5:	wpout='Rotate 225°'
	6:	wpout='Rotate 270°'
	7:	wpout='Rotate 315°'
	8:	wpout='Rotate 360°'
	16:	wpout='Origin'
	32:	wpout='JOG(+)'
	48:	wpout='JOG(-)'
	64:	wpout='Set origin'
	96:	wpout='Stop'
	80:	wpout='Rotate +22.5°'
	112:	wpout='Write adress of origin'	;装置原点アドレス書き込み'
endcase

return,wpout

END
;**************************************************************
PRO cdio_o45,wp,wd
;--------------------------------------------------------------
common cdiolib

OutByte=1
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
	if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

for i=0,9 do begin
	wait,0.2
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output
endfor

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
	if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

END
;**************************************************************
PRO cdio_o90,wp,wd
;--------------------------------------------------------------
common cdiolib

OutByte=2
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
	if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

for i=0,9 do begin
	wait,0.2
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output
endfor

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
	if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

END
;**************************************************************
PRO cdio_o135,wp,wd
;--------------------------------------------------------------
common cdiolib

OutByte=3
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
		if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

for i=0,9 do begin
	wait,0.2
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output
endfor

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
		if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

END
;**************************************************************
PRO cdio_o180,wp,wd
;--------------------------------------------------------------
common cdiolib

OutByte=4
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
		if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

for i=0,9 do begin
	wait,0.2
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output
endfor

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
		if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

END
;**************************************************************
PRO cdio_o225,wp,wd
;--------------------------------------------------------------
common cdiolib

OutByte=5
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
		if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

for i=0,9 do begin
	wait,0.2
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output
endfor

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
		if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

END
;**************************************************************
PRO cdio_o270,wp,wd
;--------------------------------------------------------------
common cdiolib

OutByte=6
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
		if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

for i=0,9 do begin
	wait,0.2
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output
endfor

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
		if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

END
;**************************************************************
PRO cdio_o315,wp,wd
;--------------------------------------------------------------
common cdiolib

OutByte=7
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
		if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

for i=0,9 do begin
	wait,0.2
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output
endfor

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
		if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

END
;**************************************************************
PRO cdio_o360,wp,wd
;--------------------------------------------------------------
common cdiolib

OutByte=8
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
		if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

for i=0,9 do begin
	wait,0.2
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output
endfor

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
		if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

END
;**************************************************************
PRO cdio_op25,wp,wd
;--------------------------------------------------------------
common cdiolib

OutByte=80
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
		if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

for i=0,9 do begin
	wait,0.2
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output
endfor

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
		if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

END
;**************************************************************
PRO cdio_opjg,wp,wd
;--------------------------------------------------------------
common cdiolib

OutByte=32
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
		if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'
wp.input=cdio_input()
  widget_CONTROL,wd.in,set_value=wp.input
wp.output=cdio_outstate()
  widget_CONTROL,wd.out,set_value=wp.output

END
;**************************************************************
PRO cdio_omjg,wp,wd
;--------------------------------------------------------------
common cdiolib

OutByte=48
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
	if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'
wp.input=cdio_input()
  widget_CONTROL,wd.in,set_value=wp.input
wp.output=cdio_outstate()
  widget_CONTROL,wd.out,set_value=wp.output

END
;**************************************************************
PRO cdio_ostop,wp,wd
;--------------------------------------------------------------
common cdiolib

OutByte=96
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
		if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

for i=0,9 do begin
	wait,0.2
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output
endfor

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
		if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

END
;**************************************************************
PRO cdio_o0,wp,wd
;--------------------------------------------------------------
common cdiolib

OutByte=16
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
		if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

for i=0,9 do begin
	wait,0.2
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output
endfor

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
		if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

END
;**************************************************************
PRO cdio_o0set,wp,wd
;--------------------------------------------------------------
common cdiolib

OutByte=64
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
		if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

for i=0,9 do begin
	wait,0.2
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output
endfor

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
		if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

END
;**************************************************************
PRO cdio_ow0ad,wp,wd
;--------------------------------------------------------------
common cdiolib

OutByte=112
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
		if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

for i=0,9 do begin
	wait,0.2
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output
endfor

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)
		if r ne 0 then print,'Error at CDIO8_8 outbyte (r=',r,')'

END
;============================================================================================

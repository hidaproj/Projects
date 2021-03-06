; DIO8lib.pro
;  handle Contec DIO-0808LY-USB for pro_obs.pro
;   2009.11.3  T.A.
;**************************************************************
function cdio_init
;--------------------------------------------------------------
common Contec_dio,dllfile
dllfile='C:\Projects\cprog\VS2005\DIO8\Debug\DIO8.dll'

r=call_external(dllfile,'Cdio_Init',/all_value,/cdecl)
return,r

end

;**************************************************************
pro cdio_exit
;--------------------------------------------------------------
common Contec_dio,dllfile
r=call_external(dllfile,'Cdio_Exit',/all_value,/cdecl)

end
;**************************************************************
function cdio_input
;--------------------------------------------------------------
common Contec_dio,dllfile

InByte=call_external(dllfile,'Cdio_InByte',/all_value,/cdecl)

Case InByte of
	0:	wpin='Non'
	1:	wpin='JOG(+)   	JOGing(+)'
	2:	wpin='JOG(-)   	JOGing(-)'
	3:	wpin='ÚIpxÚ®	moving to destination'
	4:	wpin='@B´_Ú®	moving to origin'
	32:	wpin='u´_Ú®®¹  origin'
	6:	wpin='45(Ú®®¹)'
	7:	wpin='90(Ú®®¹)'
	8:	wpin='135(Ú®®¹)'
	9:	wpin='180(Ú®®¹)'
	10:	wpin='225(Ú®®¹)'
	12:	wpin='270(Ú®®¹)'
	13:	wpin='315(Ú®®¹)'
	14:	wpin='360(Ú®®¹)'
	15:	wpin='+22.5(Ú®®¹)'
	16:	wpin='ud¹ON	Power ON'
	17:	wpin='Error'
	18:	wpin='pXoÍÙí	abnormality of pulse'
endcase

return,wpin

end

;**************************************************************
function cdio_outstate
;--------------------------------------------------------------
common Contec_dio,dllfile

outstate=call_external(dllfile,'Cdio_OutState',/all_value,/cdecl)

Case outstate of
	0:	wpout='Non'
	1:	wpout='45Ú®		go 45'
	2:	wpout='90Ú®		go 90'
	3:	wpout='135Ú®		go 135'
	4:	wpout='180Ú®		go 180'
	5:	wpout='225Ú®		go 225'
	6:	wpout='270Ú®		go 270'
	7:	wpout='315Ú®		go 315'
	8:	wpout='360Ú®		go 360'
	16:	wpout='´_Ú®		go origin'
	32:	wpout='JOG(+)'
	48:	wpout='JOG(-)'
	64:	wpout='[Zbg	set origin'
	96:	wpout='â~		Stop'
	80:	wpout='+22.5Ú®	Move +22.5'
	112:	wpout='u´_AhX«Ý'
endcase

return,wpout

end

;**************************************************************
pro cdio_o45
;--------------------------------------------------------------
common Contec_dio,dllfile

OutByte=1
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

wait,2
OutByte=0
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_o90
;--------------------------------------------------------------
common Contec_dio,dllfile

OutByte=2
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

wait,2.
OutByte=0
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_o135
;--------------------------------------------------------------
common Contec_dio,dllfile

OutByte=3
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

wait,2.
OutByte=0
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_o180
;--------------------------------------------------------------
common Contec_dio,dllfile

OutByte=4
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

wait,2.
OutByte=0
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_o225
;--------------------------------------------------------------
common Contec_dio,dllfile

OutByte=5
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

wait,2.
OutByte=0
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_o270
;--------------------------------------------------------------
common Contec_dio,dllfile

OutByte=6
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

wait,2.
OutByte=0
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_o315
;--------------------------------------------------------------
common Contec_dio,dllfile

OutByte=7
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

wait,2.
OutByte=0
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_o360
;--------------------------------------------------------------
common Contec_dio,dllfile

OutByte=8
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

wait,2.
OutByte=0
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_op25
;--------------------------------------------------------------
common Contec_dio,dllfile

OutByte=80
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

wait,2.
OutByte=0
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_opjg
;--------------------------------------------------------------
common Contec_dio,dllfile

OutByte=32
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_omjg
;--------------------------------------------------------------
common Contec_dio,dllfile

OutByte=48
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_ostop
;--------------------------------------------------------------
common Contec_dio,dllfile

OutByte=96
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

wait,2.
OutByte=0
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_o0
;--------------------------------------------------------------
common Contec_dio,dllfile

OutByte=16
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

wait,2.
OutByte=0
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_o0set
;--------------------------------------------------------------
common Contec_dio,dllfile

OutByte=64
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

wait,2.
OutByte=0
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_ow0ad
;--------------------------------------------------------------
common Contec_dio,dllfile

OutByte=112
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

wait,2.
OutByte=0
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end


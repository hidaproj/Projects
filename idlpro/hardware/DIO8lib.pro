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
	1:	wpin='JOG(+)中   	JOGing(+)'
	2:	wpin='JOG(-)中   	JOGing(-)'
	3:	wpin='目的角度移動中	moving to destination'
	4:	wpin='機械原点移動中	moving to origin'
	32:	wpin='装置原点移動完了  origin'
	6:	wpin='45°(移動完了)'
	7:	wpin='90°(移動完了)'
	8:	wpin='135°(移動完了)'
	9:	wpin='180°(移動完了)'
	10:	wpin='225°(移動完了)'
	12:	wpin='270°(移動完了)'
	13:	wpin='315°(移動完了)'
	14:	wpin='360°(移動完了)'
	15:	wpin='+22.5°(移動完了)'
	16:	wpin='装置電源ON	Power ON'
	17:	wpin='Error'
	18:	wpin='パルス出力異常	abnormality of pulse'
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
	1:	wpout='45°移動		go 45°'
	2:	wpout='90°移動		go 90°'
	3:	wpout='135°移動		go 135°'
	4:	wpout='180°移動		go 180°'
	5:	wpout='225°移動		go 225°'
	6:	wpout='270°移動		go 270°'
	7:	wpout='315°移動		go 315°'
	8:	wpout='360°移動		go 360°'
	16:	wpout='原点移動		go origin'
	32:	wpout='JOG(+)'
	48:	wpout='JOG(-)'
	64:	wpout='ゼロセット	set origin'
	96:	wpout='停止		Stop'
	80:	wpout='+22.5°移動	Move +22.5°'
	112:	wpout='装置原点アドレス書き込み'
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


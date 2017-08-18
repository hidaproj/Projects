;*****************************************************
function con_tmp2,volt0,volt_r
;-----------------------------------------------------
; convert to temp for volt at circuit of thermistor
;   20120815, by K.S @naoj
;   2012.09.28 M.H.
;   volt0  -- voltage of power souce 電源電圧（V)
;   volt_r  -- voltage of resist　固定抵抗電圧（V)
;   fix_res -- 固定抵抗値（6550Ω）
;   th_res -- サーミスター抵抗値（Ω）
;   low_res --　10℃以下となるサーミスター抵抗値：19900Ω 
;   hig_res -- 　40℃以上となるサーミスター抵抗値：5329Ω
;   rtmp --  温度（℃）

volt0 = float(volt0)
volt_r = float(volt_r)

restore,'C:\Projects\data\temp\R_T.dat'
fix_res = 6200.000
th_res = (float((volt0-volt_r)/volt_r)*fix_res)/10000.  ; サーミスタ抵抗値計算 
;coeff=[0.00924445,0.0179656,-0.00307837,0.000909775]
rtmp=1./poly(th_res,coef)-15.

goto,pass

fix_res = 6550.000
low_res = 19900.000
hig_res = 5329.000

th_res = float((volt0-volt_r)/volt_r)*fix_res  ; サーミスタ抵抗値計算 

;th_res = float(5328)
;print,'thermistor:',th_res

;---------------10℃未満の場合
if  th_res  gt low_res then begin
	rtmp = (3.16761e-22)*th_res^5-(2.03666e-17)*th_res^4-(5.05435e-13)*th_res^3+(8.27686e-8)*th_res^2-0.00334543*th_res+49.9846
endif

;--------------40℃より大きい場合
if th_res lt hig_res then begin
	rtmp = (4.49202e-21)*th_res^6-(1.09599e-16)*th_res^5+(1.1501e-12)*th_res^4-(6.78862e-9)*th_res^3+(2.48588e-5)*th_res^2-0.0612315*th_res+128.333
endif

;--------------10℃以上40℃以下の場合
if ( th_res le low_res )  and ( th_res ge hig_res ) then begin
	rtmp = (-2.04641e-28)*(th_res^7)+(2.05823e-23)*(th_res^6)-(8.85323e-19)*(th_res^5)+(2.13082e-14)*(th_res^4)-(3.15939e-10)*(th_res^3)+(3.01790e-6)*(th_res^2)-(0.0197420*th_res)+93.4849
endif
pass:
return,rtmp
end


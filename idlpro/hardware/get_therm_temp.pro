@caiolib2
;; function get_therm_temp,ch=ch,vv=vv,p=p0,init=init,therm=therm,time=time

; 2013.1.21	ki, mh
; 2014.5.18	ki, mh nsample & noDev keywords
; 2014.6.10	ki, mh coef=[***]
; 2015.4.5	ki, c.lose,keyword

;*****************************************************
function con_tmp2,volt0,volt_r	; Nikkisyo
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

;restore,'C:\Projects\data\temp\R_T.dat'	; -> coef[*]
fix_res = 6200.000
th_res = (float((volt0-volt_r)/volt_r)*fix_res)/10000.  ; サーミスタ抵抗値計算 
coef=[0.00924445, 0.0179656, -0.00307837, 0.000909775]

rtmp=1./poly(th_res,coef)-15.

return,rtmp
end


;*****************************************************
function con_tmp3,volt0,volt_r	; Murata
;-----------------------------------------------------
; convert to temp for volt at circuit of Murata thermistor
;   20120815, by K.S @naoj
;   2012.09.28 M.H.
;   volt0  -- voltage of power souce 電源電圧（V)
;   volt_r  -- voltage of resist　固定抵抗電圧（V)
;   fix_res -- 固定抵抗値（6550Ω）      <---------------現在　6.2kΩ@25C
;   th_res -- サーミスター抵抗値（Ω）
;   low_res --　10℃以下となるサーミスター抵抗値：19900Ω 
;   hig_res -- 　40℃以上となるサーミスター抵抗値：5329Ω
;   rtmp --  温度（℃）

volt0 = float(volt0)
volt_r = float(volt_r)

;restore,'C:\Projects\data\temp\R_T_murata2.dat'	; -> coef[*]
fix_res = 6200.000
th_res = (float((volt0-volt_r)/volt_r)*fix_res)/10000.  ; サーミスタ抵抗値計算 
;coeff=[0.00924445,0.0179656,-0.00307837,0.000909775]
coef=[0.00626456, 0.0639358, -0.108417, 0.151428, -0.0940115, 0.0226565]

rtmp=1./poly(th_res,coef)-15.

return,rtmp
end



;************************************************************************
function get_therm_temp,ch=ch,vv=vv,p=p0,init=init,therm=therm,time=time, $
	nsample=nsample,noDev=noDev,close=close

common get_therm_temp,p

;  get Temperature from contec AD

	if keyword_set(init) then begin	; init contex AD
		p=caio_ctl()
 		if keyword_set(noDev) then p.Dev_exist=0 else p.Dev_exist=1
		caio_init,p=p,st=st,dllfile=caiodll
		p.rate=5000	; sampling, Hz
		if n_elements(nsample) eq 0 then nsample=20l
		p.n_rec1=nsample	; length of one record
		p.nch=8
		p=caio_pSet(p)
		p0=p
	endif
	if keyword_set(close) then begin	; close contec  AD
		caio_exit
		return,0
	endif

	if p.Dev_exist eq 0 then return,999
	if keyword_set(nsample) then begin
		if nsample ne p.n_rec1 then begin
			p.n_rec1=nsample
			p=caio_pSet(p)
		endif
	endif

	caio_start,n_sample=p.n_rec1
	ndat=0
	while ndat lt p.n_rec1 do begin
		ndat=caio_count()
		;st=caio_status(BUSY=busy,START_TRG=START_TRG,DATA_NUM=data_num,OFERR=oferr)
	endwhile
	temps=caio_read(p.nch,p.n_rec1)
	T=reform(rebin(temps,p.nch,1),p.nch)
	vv=t
	tt=fltarr(7)
	if not keyword_set(therm) then begin
		for i=0,6 do tt[i]=con_tmp2(t[7],t[i])
	endif else begin
		if therm eq 'murata' then begin
			for i=0,6 do tt[i]=con_tmp3(t[7],t[i])
		endif else begin
			print,'no thermister defined!'
			tt=-99
		endelse
	endelse
	if n_elements(ch) ne 0 then tt=tt[ch]
	tim=str_sep(systime(),' ')
	time=tim[4]+'/'+mmm2mm(tim[1])+'/'+tim[2]+' '+tim[3]

; 電源電圧（V)     --- ch7
; 固定抵抗電圧（V) --- ch0-6
return,tt

end

;************************************************************************
function get_advolt,ch=ch,nsample=nsample,div=div

common get_therm_temp,p

;  get volt from contec AD

	if p.Dev_exist eq 0 then return,999
	if keyword_set(nsample) then begin
		if nsample ne p.n_rec1 then begin
			p.n_rec1=nsample
			p=caio_pSet(p)
		endif
	endif

	caio_start,n_sample=p.n_rec1
	ndat=0
	while ndat lt p.n_rec1 do begin
		ndat=caio_count()
		;st=caio_status(BUSY=busy,START_TRG=START_TRG,DATA_NUM=data_num,OFERR=oferr)
	endwhile
	temps=caio_read(p.nch,p.n_rec1)
	vv=reform(rebin(temps,p.nch,1),p.nch)	; vv[8,nsample] -> vv[8]
	if n_elements(div) ne 0 then vv=float(vv)/vv[div]
	if n_elements(ch) ne 0 then vv=vv[ch]

return,vv

end


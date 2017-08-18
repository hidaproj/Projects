pro get_lctemp,tt,vv,p,time,init=init

;caiolib2
;  get Temperature from contec AD

	if keyword_set(init) then begin	; init contex AD
		p=caio_ctl()
		caio_init,p=p,st=st,dllfile=caiodll
		p.rate=5000	; sampling, Hz
		p.n_rec1=10l	; length of one record
		p.nch=8
		p=caio_pSet(p)
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
	tt=con_tmp2(t[7],t[0])
	tim=str_sep(systime(),' ')
	time=tim[4]+'/'+mmm2mm(tim[1])+'/'+tim[2]+' '+tim[3]

; 電源電圧（V)     --- ch7
; 固定抵抗電圧（V) --- ch0-6

end
pro mk_tf_head,f,p

;  2013.5.20	m.h., k.i.

;ブロック用ヘッダー
b1={block_info,$
	fsr:1d,$				; fsr in nm
	temp:20.,$				; from get temp in C
	temp_ch:0,$				; therm channel
	bname:'', $				; block name ex) 'block1'
	elem:'', $				; block element ex) 'PL1,　C16A, WP2, C16B, LC9, PL4'  
	cthick: 0d, $				; calcite thichness in mm
	LC_ID:'',$				; LC id number 1-9,  ex. 'LC1'
	LC_ch:'',$				; LC channel ex. '1-2' - controller-1, ch-2
	LC_table:'',$				; LC table containing c[*] parameters, ex. '***'
	sign: 1,$				; shift sign 1 or -1
	ret:0.,$				; current retardation of LCVR in wave
	volt:0.,$				; current volt of LCVR
	ret_offset: 0.$				; retardations in wave for target wavelength at T=20C
	}

b=replicate(b1,7)

;フィルター用ヘッダー
f={tf_info,$
	version: '0.1',$			; version of this structure
	filt_name: 'UTF32',$			; filter name
	b: b,$					; block structures, b[7]
	wl0: 656.28d,$				; wavelength of target line in nm
	dwl: 0.00,$				; wavelength offset from wl0 in nm
	LCoffset_tbl:'',$			; LC retardation offset table name, ex. 'UTF32_LCoffset.txt'
	time:'',$				; obs time yyyy/mm/dd hh:mm:ss JST
	bfilt:'none'$				; blocking filter
	}

;観測ヘッダー
p={obs_info,$
	filename: '', $				; saved filename
	sdir:'', $				; saved dir
	exp: 0.1, $				; exp in sec (Orca-flash)
	gain: 0, $				; gain 0-5 (Orca-flash)
	framerate: 30, $  			; framerate (Orca-flash)
	binx: 1, $				; binx (Orca-flash)
	biny: 1, $				; biny (Orca-flash)
	imgsize: [2048,2048], $			; image size (Orca-flash)
	odr:1, $				; order
	gpos: '', $				; grating position of DST 2F
	slit: 0.1, $				; slit of DST 2F
	cam_pos: 37.5 $				; camera position of DST 2F in cm	
	}

end



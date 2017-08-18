function mmm2mm,mmm,mm

case strtrim(mmm,2) of
	'Jan':mm='01'
	'Feb':mm='02'
	'Mar':mm='03'
	'Apr':mm='04'
	'May':mm='05'
	'Jun':mm='06'
	'Jul':mm='07'
	'Aug':mm='08'
	'Sep':mm='09'
	'Oct':mm='10'
	'Nov':mm='11'
	'Dec':mm='12'
endcase

return,mm
end
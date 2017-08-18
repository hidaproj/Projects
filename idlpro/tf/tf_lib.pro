;------------------------------------------------------------------ time
function get_tim,time
	tim=str_sep(systime(),' ')
	time=tim[4]+'/'+mmm2mm(tim[1])+'/'+tim[2]+' '+tim[3]
	return,time
end

;------------------------------------------------------------------ element
function block_elem7,bl

case bl of
	'block1':elem=['PL1','C16A','C16B','LC9','PL4']
	'block2':elem=['PL4','C8A','LC6','PL5']
	'block3':elem=['PL5','C4A','2012C-1-2','C4B','LC5','PL6']
	'block4':elem=['PL6','C2A','2012C-2-1','C2B','LC4','PL8']
	'block5':elem=['PL8','C05A','2012C-2-2','C05B','LC2','PL9']
	'block6':elem=['PL9','C025A','2012C-2-3','C025B','LC1','PL7']
	'block7':elem=['PL7','C1A','2012A','C1B','LC3','PL10']
endcase

return,elem

end

;------------------------------------------------------------------ element
pro block7,bl,$
	   fsr,elem,cthick,lc,ch,usb,str=str

restore,'C:\Projects\IDLPRO\TF\calcite_thick.sav'

case bl of
	'block1':begin
			fsr=16.
			elem='PL1, C16A, C16B, LC9, PL4'
			cthick=c16a+c16b
			lc='LC9'
			ch=1
			usb='USB1'
		 end
	'block2':begin
			fsr=8.
			elem='PL4, C8A, LC6, PL5'
			cthick=c8a
			lc='LC6'
			ch=2
			usb='USB1'
		 end
	'block3':begin
			fsr=4.
			elem='PL5, C4A, 2012C-1-2, C4B, LC5, PL6'
			cthick=c4a+c4b
			lc='LC5'
			ch=3
			usb='USB1'
		 end
	'block4':begin
			fsr=2.
			elem='PL6, C2A, 2012C-2-1, C2B, LC4, PL8'
			cthick=c2a+c2b
			lc='LC4'
			ch=4
			usb='USB1'
		 end
	'block5':begin
			fsr=0.5
			elem='PL8, C05A, 2012C-2-2, C05B, LC2, PL9'
			cthick=c05a+c05b
			lc='LC2'
			ch=1
			usb='USB2'
		 end
	'block6':begin
			fsr=0.25
			elem='PL9, C025A, 2012C-2-3, C025B, LC1, PL7'
			cthick=c025a+c025b
			lc='LC1'
			ch=2
			usb='USB2'
		 end
	'block7':begin
			fsr=1.
			elem='PL7, C1A, 2012A, C1B, LC3, PL10'
			cthick=c1a+c1b
			lc='LC3'
			ch=3
			usb='USB2'
		 end
endcase

end



pro save_data,f,g,img0,sdir,dark=dark
			time=get_tim()
			stim=strjoin(str_sep((str_sep(time,' '))[0],'/'))+'_'+strjoin(str_sep((str_sep(time,' '))[1],':'))
			if keyword_set(dark) then begin
				ofile=sdir+'dark_'+stim+'.dat'
				g.filename=ofile
				f.time=time
;				save,filename=ofile,img0
			endif else begin
				ofile=sdir+'block7_'+stim+'.dat'
				g.filename=ofile
				f.time=time
;				save,filename=ofile,f,g,img0
			endelse
				save,filename=ofile,f,g,img0
end

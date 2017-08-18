; initmcd.pro
;	'97/09/10	k.i.

@mcdlib
;******************************************************************
pro mobs,expo=expo,filename=filename,bin=nbin,dark=dark,h=h,img=img

img=mcdobs(expo*1000.,bin=nbin,h=h,dark=dark)
wshow
tvscl,img
if keyword_set(filename) then begin
	nkrsave,filename,h,img
	print,'image was saved in ',filename
endif

end

;******************************************************************
mcdinit
mcdcooler,/on
window,xsize=1024,ysize=1024

end

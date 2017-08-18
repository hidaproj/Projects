;+
; win2gif.pro
; clip figure on window and save it to GIF file
; USAGE:
;	win2gif,'Gif-file',inv=inv
; HISTORY:
;   '98/10/19  k.i.
;   '99/02/16  k.i.	r,g,b
;   '99/10/06  k.i.	updown keyword
;   '00/01/10  k.i.	wdgetstr
;   '09/04/11  k.i.	bmp keyword
;-
pro win2gif,file,inv=inv,updown=updown,bmp=bmp

if not keyword_set(file) then begin
	;file=''
	;read,'enter gif filename : ',file
	file=wdgetstr('Enter Gif file name => ',xpos=300,ypos=300)
endif
tvlct,r,g,b,/get
rr=tvrd()
if keyword_set(inv) then rr=byte(255-rr)
if keyword_set(updown) then begin
	imgsize,rr,nx,ny
	rr1=rr
	for j=0,ny-1 do rr1(*,j)=rr(*,ny-1-j)
	rr=rr1
endif
;tv,rr
help,rr
if keyword_set(bmp) then write_bmp,file,rr,r,g,b $
else write_gif,file,rr,r,g,b


end

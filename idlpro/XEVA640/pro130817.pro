dir='C:\data\20130817\dark0040\'
files=FINDFILE(dir+'dark0040_*.bin')
nf=(SIZE(files,/dim))[0]

FOR i=0,nf-1 DO BEGIN

	data=read_bin(file=files[i])
	IF i EQ 0 THEN dark=data $
	ELSE dark=dark+data/nf

ENDFOR


dir='C:\data\20130817\scan4\'
files=FINDFILE(dir+'scan4_*.bin')
nf=(SIZE(files,/dim))[0]
img1=FLTARR(512,nf)
img2=FLTARR(512,nf)

FOR i=0,nf-1 DO BEGIN

	ii=STRTRIM(i,2)
	fname=dir+'scan4_'+ii+'.bin'
	data0=read_bin(file=fname)
	data1=data0-dark
	img1[*,i]=REBIN(data1[315:317,*],1,512)
	img2[*,i]=REBIN(data1[350:352,*],1,512)
;	PRINT,fname

ENDFOR

img1=ROTATE(TRANSPOSE(img1),2)
img2=ROTATE(TRANSPOSE(img2),2)

WINDOW,0,xs=750,ys=512

TV,BYTSCL(CONGRID(img1-img2,750,512),min=-1000,max=1000)
WRITE_PNG,'C:\chrm130817.png',tvrd(/true)

TV,BYTSCL(CONGRID(img2,750,512),min=0,max=10000)
WRITE_PNG,'C:\cont130817.png',tvrd(/true)

END

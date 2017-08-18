dir='E:\data\20130818\dark\'
files=FINDFILE(dir+'dark_*.bin')
nf=(SIZE(files,/dim))[0]

FOR i=0,nf-1 DO BEGIN

	data=read_bin(file=files[i])
	IF i EQ 0 THEN dark=data $
	ELSE dark=dark+data/nf

ENDFOR

FOR j=0,50-1 DO BEGIN
jj=string(j+1,form='(i2.2)')

sq='scan'+jj


dir='E:\data\20130818\'+sq+'\'
IF jj eq '22' then files=FINDFILE(dir+'scan21_*.bin') $
			else files=FINDFILE(dir+sq+'_*.bin')
nf=(SIZE(files,/dim))[0]
img1=FLTARR(640,nf)
img2=FLTARR(640,nf)
;window,0,xs=640,ys=512

FOR i=0,nf-1 DO BEGIN

	ii=STRTRIM(i,2)
IF jj eq '22' then fname=dir+'scan21_'+ii+'.bin' $
			else fname=dir+sq+'_'+ii+'.bin'
	data0=read_bin(file=fname)
	data1=data0-dark
;tvscl,data1
;plots,[0,640-1],[1,1]*260,/dev
;plots,[0,640-1],[1,1]*264,/dev
;plots,[0,640-1],[1,1]*229,/dev
;plots,[0,640-1],[1,1]*231,/dev
	img1[*,i]=REBIN(data1[*,260:264],640,1)
	img2[*,i]=REBIN(data1[*,229:231],640,1)
;	PRINT,fname

ENDFOR


img1=ROTATE(TRANSPOSE(img1),2)
img2=ROTATE(TRANSPOSE(img2),2)

WINDOW,0,xs=800,ys=640

TV,BYTSCL(CONGRID(img1,800,640),min=1000,max=8000)
WRITE_PNG,'C:\mouri\chromo1_'+sq+'.png',tvrd(/true)

TV,BYTSCL(CONGRID(img1,800,640),min=-1500,max=1000)
WRITE_PNG,'C:\mouri\chromo2_'+sq+'.png',tvrd(/true)

TV,BYTSCL(CONGRID(img2,800,640),min=0,max=10000)
WRITE_PNG,'C:\mouri\cont_'+sq+'.png',tvrd(/true)

save,filename='C:\mouri\'+sq+'.dat',img1,img2

ENDFOR

END

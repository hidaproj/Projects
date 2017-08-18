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
		ELSE files=FINDFILE(dir+sq+'_*.bin')
	nf=(SIZE(files,/dim))[0]
	img1=FLTARR(640,nf)
	img2=FLTARR(640,nf)

	window,0,xs=640,ys=512
	ans='' & kk=200
	WHILE ans NE 'y' DO BEGIN

		IF jj EQ 22 THEN sq='scan21'
		fname=dir+sq+'_'+STRTRIM(kk,2)+'.bin'
		PRINT,fname
		data0=read_bin(file=fname)
		data1=data0-dark
		TV,BYTSCL(data1,min=0,max=max(data1)*0.7)
		kk=kk+1
		READ,ans,prompt='This one? : '

	ENDWHILE

	PRINT,'Click the LEFT of the Helium line.'
	CURSOR,x,y,/dev,/down & x1=x & y1=y
	PLOTS,x,y,/dev,psym=1,color=255
	PRINT,'Click the RHIGT of the Helium line.'
	CURSOR,x,y,/dev,/down & x2=x & y2=y
	PLOTS,x,y,/dev,psym=1,color=255
	PLOTS,[x1,x2],[y1,y2],/dev,color=255
;	READ,ans,prompt='Is this the Helium line? (y/n): '

	y0=(y1+y2)*.5
	XYOUTS,10,10,/dev,fname,charsize=1.5,color=255
	WRITE_PNG,'C:\mouri\prof_'+sq+'.png',tvrd(/true)
	PRINT,'NOW '+sq+' ...'

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
		img1[*,i]=REBIN(data1[*,y0-2:y0+2],640,1)
		img2[*,i]=REBIN(data1[*,y0-32:y0-30],640,1)
;		PRINT,fname

	ENDFOR


	img1=ROTATE(TRANSPOSE(img1),2)
	img2=ROTATE(TRANSPOSE(img2),2)

	WINDOW,0,xs=800,ys=640

	TV,BYTSCL(CONGRID(img1-img2,800,640),min=-1000,max=1000)
	WRITE_PNG,'C:\mouri\chrm_'+sq+'.png',tvrd(/true)

	TV,BYTSCL(CONGRID(img2,800,640),min=0,max=10000)
	WRITE_PNG,'C:\mouri\cont_'+sq+'.png',tvrd(/true)

	save,filename='C:\mouri\'+sq+'.dat',img1,img2

ENDFOR

END

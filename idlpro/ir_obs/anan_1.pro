;;;;データ整理;;;;


;;;;波長ごとのスキャン;;;;

	file=findfile('C:\anan\10_19\hige*i1_12????.dat',count=n)
	dfile=findfile('C:\anan\10_19\hige*i1_12dark????.dat',count=n)
	ffile=findfile('C:\anan\10_19\hige*i1_12flat????.dat',count=n)
	fdfile=findfile('C:\anan\10_19\hige*i1_12flatdark????.dat',count=n);
	
	data=fltarr(200,512,n)
	
	i=0
	         readdatfile, dfile(i), 200, 512, 1, 1, dark2, time
		 dark=float(dark2)
		 readdatfile, ffile(i), 200, 512, 1, 1, flat2, time
		 flat1=float(flat2)
		 readdatfile, fdfile(i), 200, 512, 1, 1, fd2, time
		 fd1=float(fd2)
	for i=1,n-1 do begin
	    print,file(i)
	         readdatfile, dfile(i), 200, 512, 1, 1, dark2, time
		 dark=dark+float(dark2)
		 readdatfile, ffile(i), 200, 512, 1, 1, flat2, time
		 flat1=flat1+float(flat2)
		 readdatfile, fdfile(i), 200, 512, 1, 1, fd2, time
		 fd1=fd1+float(fd2)
	endfor
	
	dark=dark/n
	flat1=flat1/n
	fd1=fd1/n

		;dark=rebin(dark1,200,512,1)
		;flat3=rebin(flat1,200,512,1)-rebin(fd1,200,512,1)
	flat3=flat1-fd1
	flat=flat3/mean(flat3[*,*])

	for i=0,n-1 do begin
		 readdatfile, file(i), 200, 512, 1, 1, data2, time
		 data1=float(data2)
		 data[*,*,i]=(data1-dark)/flat
	endfor

	;save,data,file='C:\anan\10_19\scan_12.sav'

;２つの目立つ吸収線の位置を測る（だいたい）
;data[*,*,120]を用いて
;da=rebin(data[*,20:511,120],200,1)
;print,where(da[*] eq min(da[0:100]))
;	46
;print,where(da[*] eq min(da[100:199]))
;	160
;
;   
;a=		;46
;b=10827.		;160

;read,r
;x=(160.-46.)/(b-a)*(r-a)+46.
;tvscl,data[x,512,n]
;write_gif,'\\kiezu\usbdisk5\25cm\ir_cam\10_19\scan_12.gif',tvrd()

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;データを太陽っぽく
;r10830=rebin(data[84:86,*,*],1,512,700)
;img=fltarr(700,512)
;for i=0,511 do img[*,i]=r10830[0,i,*]
; drk=fltarr(512)
; for i=0,511 do drk[i]=mean(img1[500:549,*])
;drk=rebin(img1[414:780,*],1,512)
;drk1=drk/mean(drk[30:470])
;drk2=fltarr(1400,512)
;for i=0,1399 do drk2[i,*]=drk1[*]
;tvscl,(img1/drk2)>5000<10000
;write_gif,'C:\anan\10_19\scan_12.gif',tvrd()


end
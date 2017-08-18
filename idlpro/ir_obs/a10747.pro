a='7'
file=findfile('C:\anan\10_19\hige*i*_'+a+'????.dat',count=n)
;dfile=findfile('C:\anan\10_19\hige*i*_'+a+'dark????.dat',count=n)
	;file=findfile('C:\anan\10_18\hige*i1????.dat',count=n)
	;dfile=findfile('C:\anan\10_18\hige*i1dark????.dat',count=n)
;ffile=findfile('C:\anan\10_19\hige*i1_'+a+'flat????.dat',count=n)
;fdfile=findfile('C:\anan\10_19\hige*i1_'+a+'flatdark????.dat',count=n)

data2=fltarr(640,512,n)
;dark2=fltarr(640,512,n)
;flat2=fltarr(200,512,n)
;fd2=fltarr(200,512,n)
for i=0,n-1 do begin
    print,file(i)
	 readdatfile, file(i), 640, 512, 1, 1, data, time
	 data2(*,*,i)=float(data)
	 
;      readdatfile, dfile(i), 640, 512, 1, 1, dark, time
;	 dark2(*,*,i)=float(dark)
	
	; readdatfile, ffile(i), 640, 512, 1, 1, flat3, time
	; flat2(*,*,i)=float(flat3)
	
	; readdatfile, fdfile(i), 640, 512, 1, 1, fd, time
	; fd2(*,*,i)=float(fd)


endfor

;data1=rebin(data2,640,512,1)
;dark1=rebin(dark2,640,512,1)
;flat1=rebin(flat2,640,512,1)
;fd1=rebin(fd2,640,512,1)

;flat=(flat1-fd1)/mean(flat1-fd1)
;data=(data1-dark1)/flat

;test1=fltarr(640,512,n)
;for i=0,n-1 do test1[*,*,i]=data2[*,*,i]-dark2[*,*,i]
;test2=rebin(test1,640,512,1)
end
;  mmip_cal.pro
;  
@mmsplib
;** MODIFICATION HISTORY **
function version
ver='0.1'	; 2011.01.26   a.n., k.i.
ver='0.2'	; 2011.03.09   k.i.
ver='0.3'	; 2011.04.05   k.i.,  DRet, DDia, Rot MM, save MM
ver='0.31'	; 2011.06.20   k.i.,  save MM  + s
ver='0.4'	; 2011.07.09   k.i.,  flip button
ver='0.5'	; 2011.07.30   k.i.,  wmin,wmax, wlp
ver='0.6'	; 2012.04.08   k.i.,  Dia>0.97 -> Ret=0, Profs
ver='0.7'	; 2012.04.25   k.i.,  flip90deg
ver='0.71'	; 2013.03.04   k.i.,  DRet ret_mn, ret_mx
ver='0.72'	; 2014.05.24   k.i.,  initial dir
ver='0.73'	; 2014.11.08   k.i.,  accept no p.temp (pltMM)
ver='0.8'	; 2015.03.21   k.i.,  save wl,ret,dia,ang in csv
ver='0.9'	; 2015.04.22   t.a.,  mmip_cal
return,ver
end

;********************************************************************
pro flip90deg,ang_r,ang,ret

	ang_r=ang_r[0]
	ii=where(ang gt ang_r+70, count)
	if count gt 0 then begin
		ang[ii]=ang[ii]-90.
		ret[ii]=360.-ret[ii]
	endif
	ii=where(ang lt ang_r-70, count)
	if count gt 0 then begin
		ang[ii]=ang[ii]+90.
		ret[ii]=360.-ret[ii]
	endif

end

;********************************************************************
pro mmsp_cal_event, ev1
common mmsp_cal_com, wd, p, pp, cdat, dat, c, o, th1, th2, MM0, MM, s0, s, $
	ret,dia,ang

rad=!pi/180.

blank=replicate(' ',10)
wl_ref=500.	; reference wavelength for flip axis

case (ev1.id) of
     wd.Dirb: begin	; select dir
	o.dir=dialog_pickfile(path=o.dir0,title='select dir',/dir)
	cd,o.dir
	widget_control,wd.Dir,set_value=o.dir
	end
     wd.Cfileb: begin	; select clear file
	cfile=dialog_pickfile(path=o.dir,filter='ic*.sav',title='select clear file')
	filename_sep,cfile,dir,file=cfile
	o.cfile=cfile
	widget_control,wd.Cfile,set_value=o.cfile
	restore,o.dir+o.cfile	;p,pp,dat
	cdat=dat
	imgsize,cdat,nx,ny,nn
	th1=dindgen(nn)*p.dth1
	th2=th1/5.
	;----  waveplate calibration  ----
	mmip_calwps,th1,th2,cdat,c,verb=0,thresh=1000.
	iiv=where(c.valid eq 1)
	iix=iiv mod nx
	iiy=iiv/nx
	stretch,255,0
	;loadct,0
	ofm1=mean(c[iix,iiy].offset1)
	ofm2=mean(c[iix,iiy].offset2)
	if 1 then begin
		window,xs=700,ys=700/float(nx)*float(ny)
		!p.font=-1
		mr=([c[iix,iiy].ret1,c[iix,iiy].ret2])
		rmax=(fix(3.*stddev(mr))+mean(mr))	;<140.
		rmin=(fix(-3.*stddev(mr))+mean(mr))	;>100.
		omax=(fix(3.*stddev([c[iix,iiy].offset1-ofm1,c[iix,iiy].offset2-ofm2])))	;<2.
		omin=(fix(-3.*stddev([c[iix,iiy].offset1-ofm1,c[iix,iiy].offset2-ofm2])))	;>(-2.)
		brank=replicate(' ',10)
		chs=1.
		xs=0.02
		xd=0.4
		xdd=0.02
		ys=0.05
		yd=0.4;xd/float(nx)*float(ny)
		ydd=0.1
		!p.multi=[0,2,2]
			arr=c.ret1
			ix=0
			iy=1
			arr[0:1,0]=[rmax,rmin]
			plot_image,arr<rmax>rmin,min=rmin,max=rmax,charsize=chs,	$
				xtickname=brank,ytickname=brank,title='MMP waveplates, ret 1',	$
				norm=1,noerase=0,pos=[xs+ix*(xd+xdd),ys+iy*(yd+ydd),xs+ix*(xd+xdd)+xd,ys+iy*(yd+ydd)+yd],	$
				;color=0,background=255,	$
				ticklen=-0.02

			arr=c.ret2
			ix=1
			iy=1
			arr[0:1,0]=[rmax,rmin]
			plot_image,arr<rmax>rmin,min=rmin,max=rmax,charsize=chs,	$
				xtickname=brank,ytickname=brank,title='MMP waveplates, ret 2',	$
				norm=1,noerase=1,pos=[xs+ix*(xd+xdd),ys+iy*(yd+ydd),xs+ix*(xd+xdd)+xd,ys+iy*(yd+ydd)+yd],	$
				;color=0,background=255,	$]
				ticklen=-0.02
			color_bar,norm=1,0.92,0.95,ys+yd+ydd,ys+yd+ydd+yd,min=rmin,max=rmax,	$
				title='retardation [deg.]';,color=0

			arr=c.offset1-ofm1
			ix=0
			iy=0
			arr[0:1,0]=[omax,omin]
			plot_image,arr<omax>omin,min=omin,max=omax,charsize=chs,	$
				xtickname=brank,ytickname=brank,title='MMP waveplates, offset 1',	$
				norm=1,noerase=1,pos=[xs+ix*(xd+xdd),ys+iy*(yd+ydd),xs+ix*(xd+xdd)+xd,ys+iy*(yd+ydd)+yd],	$
				;color=0,background=255,	$
				ticklen=-0.02

			arr=c.offset2-ofm2
			ix=1
			iy=0
			arr[0:1,0]=[omax,omin]
			plot_image,arr<omax>omin,min=omin,max=omax,charsize=chs,	$
				xtickname=brank,ytickname=brank,title='MMP waveplates, offset 2',	$
				norm=1,noerase=1,pos=[xs+ix*(xd+xdd),ys+iy*(yd+ydd),xs+ix*(xd+xdd)+xd,ys+iy*(yd+ydd)+yd],	$
				;color=0,background=255,	$
				ticklen=-0.02
			color_bar,norm=1,0.92,0.95,ys,ys+yd,min=omin,max=omax,	$
				title='axis [deg.]';,color=0
		!p.multi=0
	endif
	end
     wd.Fileb: begin
	file=dialog_pickfile(path=o.dir,filter='*.sav',title='select file')
	filename_sep,file,dir,file=file
	o.file=file
	widget_control,wd.File,set_value=o.file
	restore,o.dir+o.file
	imgsize,dat,nx,ny,nn
	;--------------  Mueller matrix  ----
	mmip_mueller,th1,th2,dat,c,MM,fit=fitv
	iiv=where(c.valid eq 1)
	;s=mmdecmp2(MM,iiv=where(c.valid eq 1),flip=wd.flip)	; 2015.4.20	T.A. comment out
	s=mmdecmp2(MM,iiv=iiv,flip=wd.flip,/ip)			; 2015.4.20	T.A. 
	ii=where(s.Dia gt 0.97, count)
	if count ne 0 then begin & s[ii mod nx,ii/nx].Ret=0. &	s[ii mod nx,ii/nx].Vr=[1.,0,0] & endif
	MM0=MM
	s0=s
	dia=s.dia &	ret=s.ret
	print,'complete of calcularion MM'
	end
     wd.PltMM: begin
	imgsize,dat,nx,ny,nn
	window,0,xs=700,ys=700/float(nx)*float(ny)
	;xrange=[o.wmin,o.wmax]
	dmy=where(tag_names(p) eq 'temp', count)
	title=o.file+', filter: '+pp.filter
	iiv=where(c.valid eq 1)
	if count eq 1 then begin
		tt=strsep(p.temp,sep=',')
			if n_elements(tt) ge 2 then temp=(float(tt[0])+float(tt[1]))/2. $
		else temp=float(tt[0])
		title=title+',  T='+string(temp,form='(f5.2)')+'C'
	endif
	mmplot_ip,mm,xtitle=0,title=title,chs=1.2, $
		yrange=1.05*[-1,1],norm=wd.mmnorm,iiv=iiv
	end
     wd.PltRet: begin
	imgsize,dat,nx,ny,nn
	ret=s.ret/!pi*180.
	ang=atan(s.vr[1],s.vr[0])/2/!pi*180.
	Vr2=s.vr^2
	th=atan(sqrt(Vr2[2,*,*]),sqrt(Vr2[0,*,*]+Vr2[1,*,*]))/!pi*180
	;ang_r=interpol(ang,wl,wl_ref) &	ang_r=ang_r[0]
	;flip90deg,ang_r,ang,ret
	;------------------------------------------------------------
	window,1,xs=700/float(ny)*float(nx),ys=700
	erase
	x0=0.18 & 	x1=0.95
	y0=0.12 &	y1=0.95
	xrange=[o.wmin,o.wmax]
	nn=4
	dy1=(y1-y0)/nn
	yp=y0+dy1*3
	chars=1.5

	xs=0.01 & xd=0.37 & xdd=0.12 & ys=0.08 & yd=0.38 & ydd=0.1
	!p.multi=0
	loadct,0
	brank=replicate(' ',10)
	arr=s.tu & pmax=1. & pmin=0. & & ix=0 & iy=1
	arr[0:1,0]=[pmax,pmin]
	plot_image,arr<pmax>pmin,max=pmax,min=pmin,title='transmission',	$
		charsize=chars,xtickname=brank,ytickname=brank,			$
		noerase=1,norm=1,pos=[xs+ix*(xd+xdd),ys+iy*(yd+ydd),xs+ix*(xd+xdd)+xd,ys+iy*(yd+ydd)+yd];,	$
		;color=0,background=255
	color_bar,norm=1,xs+ix*(xd+xdd)+xd+0.08,xs+ix*(xd+xdd)+xd+0.11,	$
		ys+iy*(yd+ydd),ys+iy*(yd+ydd)+yd,min=pmin,max=pmax,title='';,color=0
	

	arr=ret & pmax=mean(ret)+3.*stddev(ret) & pmin=mean(ret)-3.*stddev(ret) & ix=1 & iy=1
	arr[0:1,0]=[pmax,pmin]
	plot_image,arr<pmax>pmin,max=pmax,min=pmin,title='retardation [deg.]',	$
		charsize=chars,xtickname=brank,ytickname=brank,			$
		noerase=1,norm=1,pos=[xs+ix*(xd+xdd),ys+iy*(yd+ydd),xs+ix*(xd+xdd)+xd,ys+iy*(yd+ydd)+yd];,	$
		;color=0,background=255
	color_bar,norm=1,xs+ix*(xd+xdd)+xd+0.08,xs+ix*(xd+xdd)+xd+0.11,	$
		ys+iy*(yd+ydd),ys+iy*(yd+ydd)+yd,min=pmin,max=pmax,title='';,color=0

	arr=ang & pmax=mean(arr)+3.*stddev(arr) & pmin=mean(arr)-3.*stddev(arr) & ix=0 &iy=0
	arr[0:1,0]=[pmax,pmin]
	plot_image,arr<pmax>pmin,max=pmax,min=pmin,title='r-ax [deg.]',	$
		charsize=chars,xtickname=brank,ytickname=brank,		$
		noerase=1,norm=1,pos=[xs+ix*(xd+xdd),ys+iy*(yd+ydd),xs+ix*(xd+xdd)+xd,ys+iy*(yd+ydd)+yd];,	$
		;color=0,background=255
	color_bar,norm=1,xs+ix*(xd+xdd)+xd+0.08,xs+ix*(xd+xdd)+xd+0.11,	$
		ys+iy*(yd+ydd),ys+iy*(yd+ydd)+yd,min=pmin,max=pmax,title='';,color=0

	arr=s.dia & pmax=1. & pmin=0. &ix=1 & iy=0
	arr[0:1,0]=[pmax,pmin]
	plot_image,arr<pmax>pmin,max=pmax,min=pmin,title='diattenuation',	$
		charsize=chars,xtickname=brank,ytickname=brank,			$
		noerase=1,norm=1,pos=[xs+ix*(xd+xdd),ys+iy*(yd+ydd),xs+ix*(xd+xdd)+xd,ys+iy*(yd+ydd)+yd];,	$
		;color=0,background=255
	color_bar,norm=1,xs+ix*(xd+xdd)+xd+0.08,xs+ix*(xd+xdd)+xd+0.11,	$
		ys+iy*(yd+ydd),ys+iy*(yd+ydd)+yd,min=pmin,max=pmax,title='';,color=0
	end
     wd.PltDia: begin
	ret=s.ret/!pi*180.
	ang=atan(s.vd[1],s.vd[0])/2/!pi*180.
	Vd2=s.vd^2
	th=atan(sqrt(Vd2[2,*]),sqrt(Vd2[0,*]+Vd2[1,*]))/!pi*180
	;------------------------------------------------------------
	imgsize,dat,nx,ny,nn
	window,1,xs=700/float(ny)*float(nx),ys=700
	erase
	x0=0.18 & 	x1=0.95
	y0=0.12 &	y1=0.95
	xrange=[o.wmin,o.wmax]
	nn=4
	dy1=(y1-y0)/nn
	yp=y0+dy1*3
	chars=1.5

	xs=0.01 & xd=0.37 & xdd=0.12 & ys=0.08 & yd=0.38 & ydd=0.1
	!p.multi=0
	loadct,0
	brank=replicate(' ',10)
	arr=s.tu & pmax=1. & pmin=0. & & ix=0 & iy=1
	arr[0:1,0]=[pmax,pmin]
	plot_image,arr<pmax>pmin,max=pmax,min=pmin,title='transmission',	$
		charsize=chars,xtickname=brank,ytickname=brank,			$
		noerase=1,norm=1,pos=[xs+ix*(xd+xdd),ys+iy*(yd+ydd),xs+ix*(xd+xdd)+xd,ys+iy*(yd+ydd)+yd];,	$
		;color=0,background=255
	color_bar,norm=1,xs+ix*(xd+xdd)+xd+0.08,xs+ix*(xd+xdd)+xd+0.11,	$
		ys+iy*(yd+ydd),ys+iy*(yd+ydd)+yd,min=pmin,max=pmax,title='';,color=0
	

	arr=s.dia & pmax=1. & pmin=0. &ix=1 & iy=1
	arr[0:1,0]=[pmax,pmin]
	plot_image,arr<pmax>pmin,max=pmax,min=pmin,title='diattenuation',	$
		charsize=chars,xtickname=brank,ytickname=brank,			$
		noerase=1,norm=1,pos=[xs+ix*(xd+xdd),ys+iy*(yd+ydd),xs+ix*(xd+xdd)+xd,ys+iy*(yd+ydd)+yd];,	$
		;color=0,background=255
	color_bar,norm=1,xs+ix*(xd+xdd)+xd+0.08,xs+ix*(xd+xdd)+xd+0.11,	$
		ys+iy*(yd+ydd),ys+iy*(yd+ydd)+yd,min=pmin,max=pmax,title='';,color=0

	arr=ang & pmax=mean(arr)+3.*stddev(arr) & pmin=mean(arr)-3.*stddev(arr) & ix=0 &iy=0
	arr[0:1,0]=[pmax,pmin]
	plot_image,arr<pmax>pmin,max=pmax,min=pmin,title='d-ax [deg.]',	$
		charsize=chars,xtickname=brank,ytickname=brank,		$
		noerase=1,norm=1,pos=[xs+ix*(xd+xdd),ys+iy*(yd+ydd),xs+ix*(xd+xdd)+xd,ys+iy*(yd+ydd)+yd];,	$
		;color=0,background=255
	color_bar,norm=1,xs+ix*(xd+xdd)+xd+0.08,xs+ix*(xd+xdd)+xd+0.11,	$
		ys+iy*(yd+ydd),ys+iy*(yd+ydd)+yd,min=pmin,max=pmax,title='';,color=0

	arr=ret & pmax=mean(ret)+3.*stddev(ret) & pmin=mean(ret)-3.*stddev(ret) & ix=1 & iy=0
	arr[0:1,0]=[pmax,pmin]
	plot_image,arr<pmax>pmin,max=pmax,min=pmin,title='retardation [deg.]',	$
		charsize=chars,xtickname=brank,ytickname=brank,			$
		noerase=1,norm=1,pos=[xs+ix*(xd+xdd),ys+iy*(yd+ydd),xs+ix*(xd+xdd)+xd,ys+iy*(yd+ydd)+yd];,	$
		;color=0,background=255
	color_bar,norm=1,xs+ix*(xd+xdd)+xd+0.08,xs+ix*(xd+xdd)+xd+0.11,	$
		ys+iy*(yd+ydd),ys+iy*(yd+ydd)+yd,min=pmin,max=pmax,title='';,color=0
	end
     wd.DDia: begin
	ret=s.ret/!pi*180.
	ang=atan(s.vd[1],s.vd[0])/2/!pi*180.
	Vd2=s.vd^2
	th=atan(sqrt(Vd2[2,*]),sqrt(Vd2[0,*]+Vd2[1,*]))/!pi*180
	dia0=1. &	drange=0.1
	;------------------------------------------------------------
	imgsize,mm,n4,n4,nx,ny
	wx=400. & wy=wx/float(nx)*float(ny*2)
	window,1,xs=wx,ys=wy
	chars=1.5
	xs=0.01 & xd=0.70 & xdd=0.12 & ys1=0.08 & ys2=0.58 & yd=0.70/2. & ydd=0.1
	!p.multi=0
	loadct,0
	brank=replicate(' ',10)

	arr=s.dia & pmax=1. & pmin=0. &ix=0 & iy=1
	arr[0:1,0]=[pmax,pmin]
	plot_image,arr<pmax>pmin,max=pmax,min=pmin,title='diattenuation',	$
		charsize=chars,xtickname=brank,ytickname=brank,			$
		noerase=1,norm=1,pos=[xs+ix*(xd+xdd),ys2,xs+ix*(xd+xdd)+xd,ys2+yd];,	$
		;color=0,background=255
	color_bar,norm=1,xs+ix*(xd+xdd)+xd+0.1,xs+ix*(xd+xdd)+xd+0.14,	$
		ys2,ys2+yd,min=pmin,max=pmax,title='';,color=0


	arr=ang & arr=arr-mean(ang[where(c.valid eq 1)]) & pmax=10. & pmin=-10. & ix=0 &iy=0
	arr[0:1,0]=[pmax,pmin]
	plot_image,arr<pmax>pmin,max=pmax,min=pmin,title='d-ax [deg.]',	$
		charsize=chars,xtickname=brank,ytickname=brank,		$
		noerase=1,norm=1,pos=[xs+ix*(xd+xdd),ys1,xs+ix*(xd+xdd)+xd,ys1+yd];,	$
		;color=0,background=255
	color_bar,norm=1,xs+ix*(xd+xdd)+xd+0.10,xs+ix*(xd+xdd)+xd+0.14,	$
		ys1,ys1+yd,min=pmin,max=pmax,title='';,color=0
	end
     wd.DRet: begin
	ret=s.ret/!pi*180.
	ang=atan(s.vr[1],s.vr[0])/2/!pi*180.
	Vr2=s.vr^2
	th=atan(sqrt(Vr2[2,*]),sqrt(Vr2[0,*]+Vr2[1,*]))/!pi*180
	ret0=127. &	rrange=30.
	;ret0=180. &	rrange=20.
	;ret_mn=0.2*360 &	ret_mx=0.5*360
	;ang_r=interpol(ang,wl,wl_ref) &	ang_r=ang_r[0]
	;flip90deg,ang_r,ang,ret
	;------------------------------------------------------------
	imgsize,mm,n4,n4,nx,ny
	wx=400. & wy=700
	window,1,xs=wx,ys=wy
	chars=1.5
	xs=0.01 & xd=0.70 & xdd=0.12 & ys1=0.08 & ys2=0.58 & yd=0.70/2. & ydd=0.1
	!p.multi=0
	loadct,0
	brank=replicate(' ',10)

	;arr=s.dia & pmax=1. & pmin=0. &ix=0 & iy=1
	arr=ret & pmax=ret0+rrange & pmin=ret0-rrange & ix=0 & iy=1
	arr[0:1,0]=[pmax,pmin]
	plot_image,arr<pmax>pmin,max=pmax,min=pmin,title='retardation [deg.]',	$
		charsize=chars,xtickname=brank,ytickname=brank,			$
		noerase=1,norm=1,pos=[xs+ix*(xd+xdd),ys2,xs+ix*(xd+xdd)+xd,ys2+yd];,	$
		;color=0,background=255
	color_bar,norm=1,xs+ix*(xd+xdd)+xd+0.10,xs+ix*(xd+xdd)+xd+0.14,	$
		ys2,ys2+yd,min=pmin,max=pmax,title='';,color=0


	arr=ang & arr=arr-mean(ang[where(c.valid eq 1)]) & pmax=10. & pmin=-10. & ix=0 &iy=0
	arr[0:1,0]=[pmax,pmin]
	plot_image,arr<pmax>pmin,max=pmax,min=pmin,title='r-ax [deg.]',	$
		charsize=chars,xtickname=brank,ytickname=brank,		$
		noerase=1,norm=1,pos=[xs+ix*(xd+xdd),ys1,xs+ix*(xd+xdd)+xd,ys1+yd];,	$
		;color=0,background=255
	color_bar,norm=1,xs+ix*(xd+xdd)+xd+0.10,xs+ix*(xd+xdd)+xd+0.14,	$
		ys1,ys1+yd,min=pmin,max=pmax,title='';,color=0
	end
     wd.Wmin: begin
	o.wmin=float(gt_wdtxt(ev1.id))
	end
     wd.Wmax: begin
	o.wmax=float(gt_wdtxt(ev1.id))
	end
     wd.Wlp: begin
	o.wlp=float(gt_wdtxt(ev1.id))
	print,'wl   ='+string(o.wlp,form='(f6.2)')
	tu=interpol(s.tu,wl,o.wlp)
	ii=where(wl gt o.wlp-2. and wl lt o.wlp+2.)
	ret=interpol(s[ii].ret/!pi*180.,wl[ii],o.wlp)
	ang=atan(interpol(s[ii].vr[1],wl[ii],o.wlp),interpol(s[ii].vr[0],wl[ii],o.wlp))/2/!pi*180.
	print,'Thru ='+string(tu,form='(f6.3)')+' '
	print,'Ret  ='+string(ret,form='(f6.1)')+' deg.'
	print,'RetAx='+string(ang,form='(f6.1)')+' deg.'
	end
     wd.Ax: begin
	o.ax=float(gt_wdtxt(ev1.id))
	end
     wd.RotAng: begin
	o.rotang=float(gt_wdtxt(ev1.id))
	end
     wd.GetAng: begin
	ret=s.ret/!pi*180.
	ang=atan(s.vr[1],s.vr[0])/2/!pi*180.
	Vr2=s.vr^2
	th=atan(sqrt(Vr2[2,*]),sqrt(Vr2[0,*]+Vr2[1,*]))/!pi*180
	;------------------------------------------------------------
	imgsize,dat,nx,ny,nn
	window,3,xs=500,ys=500
	x0=0.18 & 	x1=0.95
	y0=0.12 &	y1=0.95
	nn=3
	dy1=(y1-y0)/nn
	yp=y0+dy1*2
	chars=1.2

	plot_image,s.tu,pos=[x0,yp,x1,yp+dy1-0.005],xtickname=blank, $
		ytitle='transmission',charsize=chars,title=o.file

	yp=y0+dy1*1
	plot_image,ret,/noerase,pos=[x0,yp,x1,yp+dy1-0.005],xtickname=blank, $
		ytitle='retardation',charsize=chars

	yp=y0+dy1*0
	dmax=1.1
	plot_image,s.dia,/noerase,pos=[x0,yp,x1,yp+dy1-0.005],xtitle='X [pix]', $
		ytitle='diattenuation',charsize=chars
	print,'click reference point...'
	cursor,xc,yc,1,/data
	;dmy=min(abs(wl-xc),ic)
	s1=s0[xc,yc mod ny]
	if yc lt ny then begin	; diattenuation
		print,'Diattenuation'	;, wl=',wl[ic]
		ang=atan(s1.vd[1],s1.vd[0])/2/rad 
	endif else begin
		print,'Retardation'	;, wl=',wl[ic]
		ang=atan(s1.vr[1],s1.vr[0])/2/rad
	endelse
	o.rotang=o.ax-ang
	cang=string(o.rotang,form='(f7.2)')
	widget_control,wd.RotAng,set_value=cang
	end
     wd.MMRot: begin
	MM=rotmm(MM0,o.rotang*rad)
	s=mmdecmp2(MM,iiv=where(c.valid eq 1),/ip)
	end
     wd.MMSv: begin
	filename_sep,o.file,dmy,outf
	outf=outf+'_MM.sav'
	outfile=dialog_pickfile(path=o.mmdir,file=outf,title='out MM file')
	save,p,pp,c,o,MM,s,file=outfile
	end 
     wd.MMRd: begin
	filename_sep,o.file,dmy,outf
	outf=outf+'_MM.sav'
	mmfile=dialog_pickfile(path=o.mmdir,filter='*MM.sav',title='select MM file')
	restore,mmfile	; p,c,o,wl,MM,s
	s=mmdecmp2(MM,iiv=where(c.valid eq 1),/ip)
	MM0=MM
	s0=s
	end 
     wd.MMNm: begin
	if wd.mmnorm eq 0 then wd.mmnorm=1 else wd.mmnorm=0
	end 
     wd.Rflip: begin
	if wd.flip eq 0 then wd.flip=1 else wd.flip=0
	s=mmdecmp2(MM,iiv=where(c.valid eq 1),flip=wd.flip,/ip)
	MM0=MM
	s0=s
	end 
     wd.ProfMv: begin
	imgsize,dat,nx,ny,nn
	window,3,xs=700,ys=700./2./float(nx)*float(ny)
	!p.multi=[0,2,1]
	yrange=[0,max([cdat,dat])]
	for i=0,nn-1 do begin
		arr=cdat[*,*,i]
		arr[0:1,0]=yrange
		plot_image,arr,min=yrange[0],max=yrange[1],xtitle='X (pix)',title='clear, '+string(i,form='(i3)')
		arr=dat[*,*,i]
		arr[0:1,0]=yrange
		plot_image,arr,min=yrange[0],max=yrange[1],xtitle='X (pix)',title=o.file+', filter: '+pp.filter
		wait,0.1
	endfor
	!p.multi=0
	end 
     wd.Gif: begin
	win2gif,'c:\tmp\a.gif'
	end
     wd.OUTCSV: begin
	csvfile='c:\tmp\mmip_cal.csv'
	openw,1,csvfile
	printf,1,'x, y, ret, dia, ang'
	imgsize,dat,nx,ny,nn
	for i=0,nx-1 do begin
	for j=0,ny-1 do begin
		printf,1,i,j,ret[i,j],dia[i,j],ang[i,j],form='(f10.2,",  ",f10.4,", ",f10.4," ",f10.4)'
	endfor
	endfor
	close,1
	print,'x, y, ret, dia, ang  saven in '+csvfile
	end
     wd.Exit: begin
	WIDGET_CONTROL, /destroy, ev1.top
	return
	end
    else: 
endcase


end

;********************************************************************
common mmsp_cal_com, wd, p, pp, cdat, dat, c, o, th1, th2, MM0, MM, s0, s, $
	ret,dia,ang

; p  -  MMSP measurement parmeters
; c  -  MMSP calib parameters
; o  -  parameters for this program
;loadct,0
stretch,255,0

o = mmsp_calctl()

dir0='C:\data\MMSP\'
o.dir0=dir0
dirs=findfile(dir0+'20*')
o.dir=dirs(n_elements(dirs)-1)
o.mmdir=dir0+'mm\tf_bbmelm'
				o.mmdir='/Users/Tetsu/Desktop/'
o.wmin=400. &	o.wmax=1100.
o.wbin=1
;-----------------------------------------------------------------

wd = {wd_mmsp_cal, $
	Dirb:		0l, $
	Dir:		0l, $
	Cfileb:		0l, $
	Cfile:		0l, $
	Fileb:		0l, $
	File:		0l, $
	PltMm:		0l, $
	PltRet:		0l, $
	PltDia:		0l, $
	DRet:		0l, $
	DDia:		0l, $
	Ax:		0l, $
	GetAng:		0l, $
	RotAng:		0l, $
	Wmin:		0l, $
	Wmax:		0l, $
	Wlp:		0l, $
	MMRot:		0l, $
	MMSv:		0l, $
	MMRd:		0l, $
	MMNm:		0l, $
	mmnorm:		0, $	; <-- 1, normalize Muller matrix
	RFLIP:		0l, $
	flip:		0, $
	Gif:		0l, $
	ProfMv:		0l, $
	OUTCSV:		0l, $
	Exit:		0l $
	}


base = WIDGET_BASE(title='MMIP_CAL (Ver.'+version()+')', /column) 
b1=widget_base(base, /column, /frame )
b1a=widget_base(b1, /row )
   	wd.Dirb = widget_button(b1a, value="Dir", uvalue = "Dirb",xsize=40)
	wd.Dir = widget_text(b1a,value=o.dir,uvalue='Dir',xsize=30,/edit)
b1b=widget_base(b1, /row )
   	wd.Cfileb = widget_button(b1b, value="Cfile", uvalue = "Cfileb",xsize=40)
	wd.Cfile = widget_text(b1b,value=o.cfile,uvalue='Cfile',xsize=30,/edit)
b1c=widget_base(b1, /row )
   	wd.Fileb = widget_button(b1c, value="File", uvalue = "Fileb",xsize=40)
	wd.File = widget_text(b1c,value=o.file,uvalue='File',xsize=30,/edit)

b1=widget_base(base, /column )
b3a=widget_base(b1, /row )
	dmy = widget_label(b3a,value='Plot: ')
   	wd.PltMm = widget_button(b3a, value="MM", uvalue = "Plot-MM")
   	wd.PltRet = widget_button(b3a, value="Ret", uvalue = "Plot-Ret")
   	wd.PltDia = widget_button(b3a, value="Dia", uvalue = "Plot-Dia")
   	wd.DRet = widget_button(b3a, value="DRet", uvalue = "DRet")
   	wd.DDia = widget_button(b3a, value="DDia", uvalue = "DDia")
b3a2=widget_base(b1, /row )
   	wd.MMNm = widget_button(b3a2, value="norm", uvalue = "MMNm")
   	wd.Rflip = widget_button(b3a2, value="Rflip", uvalue = "Rflip")
   	wd.ProfMv = widget_button(b3a2, value="Profs", uvalue = "Profs")
   	wd.OUTCSV = widget_button(b3a2, value="CSV", uvalue = "CSV")
;b3a3=widget_base(b1, /row )
;	dmy = widget_label(b3a3,value='wmin ')
;	cwmin=string(o.wmin,form='(f6.1)')
;	wd.Wmin = widget_text(b3a3,value=cwmin,uvalue='Wmin',xsize=5,/edit)
;	dmy = widget_label(b3a3,value='  wmax ')
;	cwmax=string(o.wmax,form='(f6.1)')
;	wd.Wmax = widget_text(b3a3,value=cwmax,uvalue='Wmax',xsize=5,/edit)
;	dmy = widget_label(b3a3,value=' wlp ')
;	cwp=string(o.wlp,form='(f6.1)')
;	wd.Wlp = widget_text(b3a3,value=cwp,uvalue='Wlp',xsize=5,/edit)
b3b=widget_base(b1, /row )
	dmy = widget_label(b3b,value='Rot: ')
	dmy = widget_label(b3b,value='ax')
	cang=string(o.ax,form='(f5.1)')
	wd.Ax = widget_text(b3b,value=cang,uvalue='Ax',xsize=5,/edit)
	dmy = widget_label(b3b,value=' ang')
	cang=string(o.rotang,form='(f7.2)')
	wd.RotAng = widget_text(b3b,value=cang,uvalue='RotAng',xsize=7,/edit)
	dmy = widget_label(b3b,value='deg.')
   	wd.GetAng = widget_button(b3b, value="GetAng", uvalue = "GetAng")
b3c=widget_base(b1, /row )
   	wd.MMRot = widget_button(b3c, value="MMRot", uvalue = "MMRot")
   	wd.MMSv = widget_button(b3c, value="MMSv", uvalue = "MMSv")
   	wd.MMRd = widget_button(b3c, value="MMRd", uvalue = "MMRd")
   	wd.Gif = widget_button(b3c, value="Gif", uvalue = "Gif")
   	wd.Exit = widget_button(b3c, value="Exit", uvalue = "Exit")

widget_control, base, /realize
XMANAGER, 'mmsp_cal', base


stop
window,3
plot,dat[i0,*],psym=2
oplot,fitv[i0,*]

stop

;------  misc plot  ------
surface,dat,wl,th1,charsize=2,xtitle='wavelength [nm]',ytitle='!7h!31'


end

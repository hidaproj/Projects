
@/ssw/stereo/secchi/idl/euvi/despike_gen.pro
@~anan/lib/ta_sinfit_mpfit.pro
@~anan/lib/ta_ct_kuhn_0.pro
@~anan/lib/DSTPOL//polarimeterlib_v2.pro
pro update_cal_dstsp,date

;ffo='2013b'
;ffo='2014a'



ref_month2=['January','February','March','April','May','June',$
                'July','August','September','October','November','December']
year=strmid(date,0,4)
ck_mon=fix(strmid(date,4,2))
;if ck_mon gt 6 then ffo=year+'b' else ffo=year+'a'
mon2=ref_month2(ck_mon-1)
day=string(float(strmid(date,6,2)),format="(i0)")

org=read_file('/sp_pub/DSTSP_'+year+'.html',/str)
spawn,'\cp /sp_pub/DSTSP_'+year+'.html /sp_pub/DSTSP_'+year+'_backup.html'

print,mon2
ckmon='<caption><h3>'+mon2+'</h3></caption>'
ck1=where(org eq ckmon)
print,ck1
ck2=where(org(ck1:*) eq '<td align=center height=30 width=30 style="background: #aaffaa"><a href="./'+date+'/'+date+'.html">'+day+'</a></td>')

if ck2(0) eq -1 then begin

ck3=where(org(ck1:*) eq '<td align=center height=30 width=30>'+day+'</td>')
res=org
res(ck1+ck3)='<td align=center height=30 width=30 style="background: #aaffaa"><a href="./'+date+'/'+date+'.html">'+day+'</a></td>'


openw,1,'/sp_pub/DSTSP_'+year+'.html'
printf,1,res
close,1

;spawn,'scp /home/smart/public_html/T3/T3_'+ffo+'.html 130.54.111.2:/smart3/SMART/T3/'

endif

return

end
;**************************************************************
pro dstvssp_cal_event, ev
;--------------------------------------------------------------
common widgetlib,wparam,windex,date,dir,iquvs,hds,lun,iobs,svdir

widget_control, ev.id, get_uvalue=uvalue,get_value=value
print,'uvalue=',uvalue

dir0='/sp_pub/'
dir1='/sp_pub/save/'

case uvalue of
	'camera':begin
		case value of 
			0:wparam.camera='orca4'
			1:wparam.camera='xeva640'
			2:wparam.camera='ge1650'
		endcase
	print,wparam.camera
	end
	'dir':begin
		wparam.dir=dialog_pickfile(path=wparam.dir,directory=1)
		widget_CONTROL,windex.dir,set_value=wparam.dir
		wparam.hazddir=wparam.dir
		widget_CONTROL,windex.hazddir,set_value=wparam.hazddir
		pqfiles=file_search(wparam.dir,wparam.pq_file+'.fits',count=nf)
		wparam.npq=nf
		widget_CONTROL,windex.npq,set_value=string(wparam.npq,format='(i)')

		date=strmid(wparam.dir,strpos(wparam.dir,'/DST/sp/')+8,8)
		dir=dir0+date+'/'
		svdir=dir1+date+'/'
		if is_dir(dir) eq 0 then spawn,'mkdir '+dir
		if is_dir(svdir) eq 0 then spawn,'mkdir '+svdir
		
	end
	'drk_file':begin
		wparam.drk_file=value
		widget_CONTROL,windex.drk_file,set_value=wparam.drk_file
		drkfiles=file_search(wparam.dir,wparam.drk_file+'.fits',count=nf)
		wparam.ndrk=nf
		widget_CONTROL,windex.ndrk,set_value=string(wparam.ndrk,format='(i)')
	end
	'ndrk':begin
		wparam.ndrk=value
		widget_CONTROL,windex.ndrk,set_value=string(wparam.ndrk,format='(i)')
	end
	'flat_file':begin
		wparam.flat_file=value
		widget_CONTROL,windex.flat_file,set_value=wparam.flat_file
		flatfiles=file_search(wparam.dir,wparam.flat_file+'.fits',count=nf)
		wparam.nflat=nf
		widget_CONTROL,windex.nflat,set_value=string(wparam.nflat,format='(i)')
		;wparam.flat_file=dialog_pickfile(path=wparam.flat_file,directory=0)
		;widget_CONTROL,windex.flat_file,set_value=wparam.flat_file
	end
	'nflat':begin
		wparam.nflat=value
		widget_CONTROL,windex.nflat,set_value=string(wparam.nflat,format='(i)')
	end
	'pq_file':begin
		wparam.pq_file=value
		widget_CONTROL,windex.pq_file,set_value=wparam.pq_file
		pqfiles=file_search(wparam.dir,wparam.pq_file+'.fits',count=nf)
		wparam.npq=nf
		widget_CONTROL,windex.npq,set_value=string(wparam.npq,format='(i)')
	end
	'npq':begin
		wparam.npq=value
		widget_CONTROL,windex.npq,set_value=string(wparam.npq,format='(i)')
	end
	'hazddir':begin
		wparam.hazddir=dialog_pickfile(path=wparam.hazddir,directory=1)
		widget_CONTROL,windex.hazddir,set_value=wparam.hazddir
	end
	'skipkxky':begin
		wparam.skipkxky=value
		widget_CONTROL,windex.skipkxky,set_value=wparam.skipkxky
	end
;
	'prep_dark':begin
		print,''
		print,'####################################################'
		print,'Start making DARK'
		print,'vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv'
		print,''
;goto,jump1
		files=(file_search(wparam.dir,wparam.drk_file+'.fits',count=nf))[0:wparam.ndrk-1]
		nf=wparam.ndrk
		case wparam.camera of
			'orca4':mkdark_orca4,files[0:wparam.ndrk-1],darks,dh
			'xeva640':mkdark_xeva640,files[0:wparam.ndrk-1],darks,dh
			'ge1650':mkdark_ge1650,files[0:wparam.ndrk-1],darks,dh
			else:begin
				print,'else'
			end
		endcase
;save,darks,dh,files,file=dir+'dark0.sav'

		iii=0l
		nx=(size(darks))[1]
		ny=(size(darks))[2]

		nopos=1
		pos=selectdark(dh,dh[0],nopos=nopos)
		darks1=fltarr(nx,ny)
		darks1[0:dh[0].naxis1-1,0:dh[0].naxis2-1]=	$	;20151104
			reform(rebin(darks[0:dh[0].naxis1-1,0:dh[0].naxis2-1,pos],dh[0].naxis1,dh[0].naxis2,1))	
		ndh=dh[0]

		ndarks=(size(darks))[3]
		if (size(nopos))[0] eq 0 then ndarks1=0 else ndarks1=(size(nopos))[1]
		while ndarks1 ne 0 do begin
			iii=iii+1l
			dh=dh[nopos]
			darks=darks[*,*,nopos]
			ndarks=ndarks1

			nopos=1
			pos=selectdark(dh,dh[0],nopos=nopos)
			darks0=darks1
			darks1=fltarr(nx,ny,iii+1)
			darks1[*,*,0:iii-1]=darks0
			darks1[0:dh[0].naxis1-1,0:dh[0].naxis2-1,iii]=	$	;20151104
				reform(rebin(darks[0:dh[0].naxis1-1,0:dh[0].naxis2-1,pos],dh[0].naxis1,dh[0].naxis2,1))	
			ndh=[ndh,dh[0]]
		
			if (size(nopos))[0] eq 0 then ndarks1=0 else ndarks1=(size(nopos))[1]
		endwhile

		darks=darks1
		dh=ndh
		save,darks,dh,file=svdir+'dark.sav'
		print,'saved '+svdir+'dark.sav'
 
		print,''
		print,'^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^'
		print,'End making DARK'
		print,'####################################################'
		print,''
	end
	'prep_ref':begin
		if wparm.skipkxky eq 1 then begin
			print,'skip alignment'
			goto,skiptmp
		endif
		print,''
		print,'####################################################'
		print,'Start KX & KY'
		print,'vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv'
		print,''
		file=(file_search(wparam.dir,wparam.pq_file+'.fits',count=nf))[0:wparam.npq-1]
		nf=wparam.npq
		kx=fltarr(2,2,nf)
		ky=fltarr(2,2,nf)
		shiftx1=fltarr(nf)
		ii=0
		for i=0,nf-1 do begin
			if (ii eq 0) then begin
				mkkxky_orca4,file[i],mergin=5,offx=0,offy=0,camera=wparam.camera,	$;keywords
					kx0,ky0,shiftx10,kxkyh0,outfile=0
				ans=''
				read,'save this result? y or n: ',ans
				if ans eq 'y' then begin
					kxkyh=kxkyh0
					kx[*,*,ii]=kx0
					ky[*,*,ii]=ky0
					shiftx1[ii]=shiftx10
					ii=ii+1
				endif
				
			endif else begin
				case wparam.camera of
					'ge1650':mreadfits,file[i],index,data,/nodata
					'xeva640':mreadfits,file[i],index,data,/nodata
					'orca4':read_orca,file[i],index,data,/nodata
				endcase
				pos=selectkxky(kxkyh,index[0])
			   	if pos[0] eq -1 then begin
					mkkxky_orca4,file[i],mergin=5,offx=0,offy=0,camera=wparam.camera, $;keywords
						kx0,ky0,shiftx10,kxkyh0,outfile=0
					ans=''
					read,'save this result? y or n: ',ans
					if ans eq 'y' then begin
						kx[*,*,ii]=kx0
						ky[*,*,ii]=ky0
						shiftx1[ii]=shiftx10
						kxkyh=[kxkyh,kxkyh0]	
						ii=ii+1
					endif
				endif
			endelse
		endfor
		kx=kx[*,*,0:ii-1]
		ky=ky[*,*,0:ii-1]
		shiftx1=shiftx1[0:ii-1]
		kxkyh=kxkyh[0:ii-1]
		save,kx,ky,shiftx1,kxkyh,file=svdir+'kxky.sav'
		print,'saved '+svdir+'kxky.sav'
		skiptmp:
                print,''
                print,'####################################################'
                print,'Start calculation of retardation and offset angle'
                print,'vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv'
		restore,svdir+'dark.sav'

		files=(file_search(wparam.dir,wparam.pq_file+'.fits',count=nf))[0:wparam.npq-1]
		nf=wparam.npq
		mq=0
		if wparam.camera eq 'xeva640' then mq=1
		kxkyfile=svdir+'kxky.sav'
		restore,kxkyfile
		ss=size(kx)
		if ss[0] eq 2 then nf=1 else nf=ss[3]
		kxs=fltarr(4,nf)
		kys=fltarr(4,nf)
		for i=0,nf-1 do begin
        		kxs[*,i]=kx[*,*,i]
        		kys[*,i]=ky[*,*,i]
		endfor
    		shiftx1s=shiftx1

		s=size(files)
		if s[0] eq 0 then nf=1 else nf=s[1]
		rets=fltarr(nf)
		offsets=fltarr(nf)
		ratios=fltarr(nf)
		frates=fltarr(nf)
		iii=0
		for i=0,nf-1 do begin
        		print,i+1,nf
		        case wparam.camera of
				'xeva640':mreadfits,files[i],index,data
				'ge1650':mreadfits,files[i],index,data
				'orca4':read_orca,files[i],index,data
			endcase
		        hd=index[0]
		        nx=hd.naxis1
		        ny=hd.naxis2
		        nt=hd.naxis3

        		dpos=selectdark(dh,index[0])
        		if dpos[0] ne -1 then begin
				dark=reform(rebin(darks[0:nx-1,0:ny-1,selectdark(dh,index[0])],nx,ny,1))
			endif else begin
				print,'no dark'
				dark=fltarr(nx,ny)
			endelse

       			pos=selectkxky(kxkyh,index[0])
		        if pos[0] ne -1 then begin
        		        kx=kxs[*,pos]
                		ky=kys[*,pos]
                		shiftx1=shiftx1s[pos]
        		endif else begin
        		        print,'no kx, ky'
                		stop
        		endelse

			;retore,savfile
        		data=reform((rebin(data[0:nx/2-1-shiftx1,*,*],1,ny,nt))[0,ny/2,1:*])
        		xx=findgen(nt-1)+1.
        		yfit=ta_sinfit_mpfit(xx,data,av=av,amp=amp,k=k,ph=ph,tprint=0)
        		wdef,1,400,400
        		plot,xx,data,psym=1,title=string(i)
        		oplot,xx,yfit

        		;frate=2.*!pi/median(par10[1,2,*])*4.
        		frates[i]=2.*!pi/k*4.
        		;ref=ref2irarp(files,dark,kx,ky,mq=mq,header=0,array=1,ver=1,orca=1,shiftx=shiftx1,frate=frate);single=1)
        		if wparam.camera eq 'orca4' then begin
				ref=ref2irarp(files[i],dark,kx,ky,header=0,array=1,ver=1,shiftx=shiftx1,frate=frates[i],mq=mq,/orca);single=1)
			endif else begin
				ref=ref2irarp(files[i],dark,kx,ky,header=0,array=1,ver=1,orca=0,shiftx=shiftx1,frate=frates[i],mq=mq);single=1)

			endelse
		        if (iii ne 0) then if selectkxky(refh[(iii-1)>0],index[0]) ne -1 then goto,skip
        		wx=500.
        		wy=600.
        		factorx=wx/nx*2.
        		factory=wy/ny
        		wdef,0,wx,wy
        		tvscl,congrid(ref[*,*,0],wx,wy)
        		print,'click left-lower corner 1'
        		cursor,/dev,h1x,h1y
        		h1x=h1x/factorx
        		h1y=h1y/factory
        		wait,1.
        		print,'click right-upper corner 2'
        		cursor,/dev,h2x,h2y
        		h2x=h2x/factorx
        		h2y=h2y/factory
        		wait,1.
        		offsetys=fltarr(ny,nf)
        		skip:

			ans=''
			;read,'save this result? y or n: ',ans
			pos=where(abs(data-yfit) ge av/2.,npos)
			if npos eq 0 then ans='y' else ans='n'			
			print,'ans',ans

			if ans eq 'y' then begin	
        			rets[iii]=median(ref[h1x:h2x,h1y:h2y,3])
        			offsets[iii]=median(ref[h1x:h2x,ny/2,2])
        			ratios[iii]=median(ref[h1x:h2x,h1y:h2y,1])
        			if wparam.camera eq 'orca4' then begin
					offsety=reform(rebin(ref[h1x:h2x,*,2],1,ny))-offsets[i]
		       			offsety0=offsety
        				yy1=fix(findgen(float(h2y-h1y+1)/4)+h1y)
        				coe1=poly_fit(yy1,offsety[yy1],1,yfit=yfit1)
        				yy2=reverse(fix(h2y-findgen(float(h2y-h1y+1)/4)))
        				coe2=poly_fit(yy2,offsety[yy2],1,yfit=yfit2)
        				xx=-(coe1[0]-coe2[0])/(coe1[1]-coe2[1])
       		 			offsetys[*,iii]=[coe1[1]*findgen(fix(xx)+1)+coe1[0],coe2[1]*(findgen(ny-fix(xx)-1)+fix(xx)+1)+coe2[0]]
				endif

			        print,'retardation (deg)',rets[iii]*!radeg
	        		print,'ratio',ratios[iii]
	        		print,'frame rate (Hz)',frates[iii]
	        		print,'offset angle (deg)',offsets[iii]*!radeg
				if wparam.camera eq 'orca4' then begin
        				wdef,2,400,400
        				plot,(offsets[iii]+offsetys[*,iii])*!radeg,xstyle=1,xtitle='Y (pix)',ytitle='offset angle (deg)'
        				oplot,(offsets[iii]+offsety0)*!radeg
				endif
	        		if iii eq 0 then refh=hd else refh=[refh,hd]
				iii=iii+1
			endif else begin
			endelse
		endfor
		rets=rets[0:iii-1]
		offsets=offsets[0:iii-1]
		offsetys=offsetys[*,0:iii-1]
		ratios=ratios[0:iii-1]
		frates=frates[0:iii-1]
		refh=refh[0:iii-1]
		save,rets,offsets,offsetys,ratios,frates,refh,file=svdir+'ref.sav'
		print,'saved '+svdir+'ref.sav'

            	print,''
                print,'vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv'
                print,'Complete calculation of retardation and offset angle'
                print,'####################################################'
                print,''
	end
	'prep_hazd':begin
		print,''
		print,'####################################################'
		print,'Read Hour angle & Zenith distance'
		print,'vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv'

		print,''
		txt2hazd,wparam.hazddir,time,ha0,zd0,check=1,radius=radius,pangle=pangle,incli=incli,gangle=gangle
		save,time,ha0,zd0,radius,pangle,incli,gangle,file=svdir+'hazd.sav'
		print,'saved '+svdir+'hazd.sav'

		print,''
		print,'vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv'
		print,'Complete reading Hour angle & Zenith distance'
		print,'####################################################'
		print,''
	end
	'prep_flat':begin
		print,''
		print,'####################################################'
		print,'make flat'
		print,'vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv'

		if wparam.nflat ne 0 then begin
			files=(file_search(wparam.dir,wparam.flat_file+'.fits',count=nf))[0:wparam.nflat-1]
			nf=wparam.nflat
		        case wparam.camera of
				'orca4':read_orca,files,index,data,nodata=1
				else:mreadfits,files,index,data,/nodata
			endcase
			index=index[findgen(nf)*100]
		        nx=max(index.naxis1)
		        ny=max(index.naxis2)
		        nt=max(index.naxis3)

			key=1
			ind=index
			refind=ind[0]
			while key ge 1 do begin
				pos=where($
				        (ind.detnam ne ind[0].detnam) or $
				        (ind.wave ne ind[0].wave) or $
				        (ind.fgbinx ne ind[0].fgbinx) or $
				        (ind.fgbiny ne ind[0].fgbiny) or $
				        (ind.x0 ne ind[0].x0) or $
				        (ind.x1 ne ind[0].x1) or $
				        (ind.y0 ne ind[0].y0) or $
				        (ind.y1 ne ind[0].y1),$
			        	npos)

				if key ne 1 then begin
					refind=[refind,ind[0]]
					key=2
				endif
				if npos eq 0 then begin
					key=0
				endif else begin
					ind=ind[pos]
				endelse
			endwhile
			nflat=n_elements(refind)

			flats=fltarr(nx,ny,nflat)
			restore,svdir+'dark.sav'
			for iflat=0,nflat-1 do begin
				pos=selectkxky(index,refind[iflat])
				npos=n_elements(pos)
				nx=refind[iflat].naxis1
				ny=refind[iflat].naxis2

        			dpos=selectdark(dh,refind[iflat])
        			if dpos[0] ne -1 then begin
					dark=reform(rebin(darks[0:nx-1,0:ny-1,dpos],nx,ny,1))
				endif else begin
					print,'no dark'
					dark=fltarr(nx,ny)
				endelse
				for ipos=0,npos-1 do begin
		        		case wparam.camera of
						'orca4':file2data,files[pos[ipos]],data,index,ver=1,orca=1,dark=dark
						else:file2data,files[pos[ipos]],data,index,ver=1,dark=dark
					endcase
        				if ipos eq 0 then flat0=reform(rebin(data,nx,ny,1)) else $
 	               				flat0=flat0+reform(rebin(data,dh[0].naxis1,dh[0].naxis2,1))

				endfor
				flat0=flat0/float(npos)
			        set_plot,'x'
			        wdef,0,800,600
			        !p.multi=0
			        loadct,0
			        plot_image,flat0
			        print,'click center between left and right images'
			        xycursor,xcen,y

				for i=0,1 do begin
				        case i of
                				0:begin
                				        flat1=flat0[0:xcen,*]
                				end
               					1:begin
                				        flat1=flat0[xcen+1:*,*]
                				end
        				endcase
        				nnx=(size(flat1))[1]
        				nny=ny
        				prof=rebin(flat1,nnx,1)
        				xx=where(prof ge max(prof)*.5,nxx)

        				wdef,0,600,600
        				plot_image,flat1
        				print,'click line'
        				xycursor,x,y
        				print,x,y
        				res=fltarr(nxx)
        				for j=0,nxx-1 do begin
                				tmp=min(flat1[xx[j],y-10:y+10],pmin)
                				pmin=y-10+pmin
                				yyy=(findgen(3)-1)+pmin
                				coe=poly_fit(yyy,flat1[xx[j],yyy],2,yfit=yfit)
                				res[j]=-coe[1]/2./coe[2]
        				endfor
        				coe=poly_fit(xx,res,1,yfit=yfit)
        				angle=atan(coe[1])*!radeg
        				flat2=rot(flat1,angle,/interp)
	
        				;mflat=morph_close((flat2*256.)>1e-10,replicate(1,30,1),/gray,/uint)/256.    ;comout 20161208 TA
					factor=round(4.-alog10(max(flat2)))                                          ;20161208 TA
        				mflat=morph_close((flat2*10.^factor)>1e-10,replicate(1,30,1),/gray,/uint)*10.^(-factor) ;20161208 TA
        				flat3=rot(flat2/mflat,-angle,/interp)

        				if i eq 0 then flat=flat3 else flat=[flat,flat3]
				endfor
				flat[where(finite(flat) eq 0)]=1.



				flats[0:refind[iflat].naxis1-1,0:refind[iflat].naxis2-1,iflat]=flat
				plot_image,flat>0.8<1.2
			endfor
			fh=refind
		endif else begin
			print,'no flat field'
			flats=fltarr(2048,2048)+1.
			fh=index_form()
		endelse
		;save,flat,file=svdir+'flat.sav'
		save,flats,fh,file=svdir+'flat.sav'

            	print,''
                print,'vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv'
                print,'Complite flat'
                print,'####################################################'
                print,''
	end
	'file':begin
		wparam.file=value
		widget_CONTROL,windex.file,set_value=wparam.file
		files=file_search(wparam.dir,wparam.file+'.fits',count=nf)
		wparam.nfile=nf
		widget_CONTROL,windex.nfile,set_value=string(wparam.nfile,format='(i)')
	end
	'nfile':begin
		wparam.nfile=value
		widget_CONTROL,windex.nfile,set_value=string(wparam.nfile,format='(i)')
	end
	'xn':begin
		wparam.xn=value
		widget_CONTROL,windex.xn,set_value=string(wparam.xn,format='(f8.5)')
	end
	'tn':begin
		wparam.tn=value
		widget_CONTROL,windex.tn,set_value=string(wparam.tn,format='(f8.3)')
	end
	'xc':begin
		wparam.xc=value
		widget_CONTROL,windex.xc,set_value=string(wparam.xc,format='(f8.5)')
	end
	'tc':begin
		wparam.tc=value
		widget_CONTROL,windex.tc,set_value=string(wparam.tc,format='(f8.3)')
	end
	'sc':begin
		wparam.sc=value
		widget_CONTROL,windex.sc,set_value=string(wparam.sc,format='(f6.4)')
	end
	'sign': begin
		if value eq 0 then wparam.sign=1. else wparam.sign=-1.
	end
	'show_memo': begin
		memo=file_search(wparam.dir,'memo.txt',count=nf)
		if nf eq 1 then spawn,'emacs '+memo+' -X'
	end
	'reduction':begin
		print,''
		print,'####################################################'
		print,'Make IQUV'
		print,'####################################################'
		print,''
		date=strmid(wparam.dir,strpos(wparam.dir,'/DST/sp/')+8,8)
		dir=dir0+date+'/'
		svdir=dir1+date+'/'
		files=(file_search(wparam.dir,wparam.file+'.fits',count=nf))[0:wparam.nfile-1]
		dst=[wparam.xn,wparam.tn*!dtor,wparam.xc,wparam.tc*!dtor,wparam.sc]
		nn=40
		if nf ne 0 then begin
			ni=wparam.nfile / nn
			for i=0,ni do begin
			case wparam.camera of
				'orca4':begin
				main,files[nn*i:(nn*(i+1)-1)<(wparam.nfile-1)],svdir+'dark.sav',svdir+'flat.sav',svdir+'kxky.sav',svdir+'hazd.sav',svdir+'ref.sav',dst,         $;inputs
			        	orca4=1,    $
				        signv=wparam.sign,    $; 1 for +V, -1 for -V, sign of V of induceing light to slit
				        minus=0,    $; 0 for normal camera setting, 1 for reverse
				        ;abcd=abcd,      $; Kuhn's method
				        limb=1,      $; rotate +Q axis to parallel limb
				        ;remove_icrosstalk=remove_icrosstalk,    $; [x1,x2,y1,y2], range of continuum to subtract crosstalk from I
					;sky=sky,        $; [x1,x2,y1,y2], range of continuum to subtract sky
				        iquv,hds,outfile=0,slitiquv=slitiquv,dualiquv=dualiquv                                              ;outputs
                                    end
				'xeva640':begin
				main,files[nn*i:(nn*(i+1)-1)<(wparam.nfile-1)],svdir+'dark.sav',svdir+'flat.sav',svdir+'kxky.sav',svdir+'hazd.sav',svdir+'ref.sav',dst,         $;inputs
				;main,files[nn*i:(nn*(i+1)-1)<(wparam.nfile-1)],svdir+'dark.sav',svdir+'flat.sav',svdir+'20160701_1.sav',svdir+'hazd.sav',svdir+'ref.sav',dst,         $;inputs
			        	orca4=0,    $
				        signv=wparam.sign,    $; 1 for +V, -1 for -V, sign of V of induceing light to slit
				        minus=1,    $; 0 for normal camera setting, 1 for reverse
				        ;abcd=abcd,      $; Kuhn's method
				        limb=1,      $; rotate +Q axis to parallel limb
				        ;remove_icrosstalk=remove_icrosstalk,    $; [x1,x2,y1,y2], range of continuum to subtract crosstalk from I
					;sky=sky,        $; [x1,x2,y1,y2], range of continuum to subtract sky
				        iquv,hds,outfile=0,slitiquv=slitiquv,dualiquv=dualiquv                                              ;outputs
                                    end
				'ge1650':begin
				main,files[nn*i:(nn*(i+1)-1)<(wparam.nfile-1)],svdir+'dark.sav',svdir+'flat.sav',svdir+'kxky.sav',svdir+'hazd.sav',svdir+'ref.sav',dst,         $;inputs
			        	orca4=0,    $
				        signv=wparam.sign,    $; 1 for +V, -1 for -V, sign of V of induceing light to slit
				        minus=0,    $; 0 for normal camera setting, 1 for reverse
				        ;abcd=abcd,      $; Kuhn's method
				        limb=1,      $; rotate +Q axis to parallel limb
				        ;remove_icrosstalk=remove_icrosstalk,    $; [x1,x2,y1,y2], range of continuum to subtract crosstalk from I
					;sky=sky,        $; [x1,x2,y1,y2], range of continuum to subtract sky
				        iquv,hds,outfile=0,slitiquv=slitiquv,dualiquv=dualiquv                                              ;outputs
                                    end
                        endcase
;iquv=slitiquv & print,'slit IQUV'


			;save,iquv,hds,slitiquv,dualiquv,wparam,file=svdir+strmid(wparam.file,0,strlen(wparam.file)-1)+'_iquv'+string(i,format='(i4.4)')+'.sav'
			save,iquv,hds,wparam,file=svdir+strmid(wparam.file,0,strlen(wparam.file)-1)+'_iquv'+string(i,format='(i4.4)')+'.sav'
			print,'saved '+svdir+strmid(wparam.file,0,strlen(wparam.file)-1)+'_iquv'+string(i,format='(i4.4)')+'.sav'
			iquv=0b;!NULL
			hds=0b;!NULL
			slitiquv=0b;!NULL
			;if i eq 0 then begin
			;	spawn,'cp -f /home/observer/lib/sp/dstvssp_cal.pro '+dir
			;	spawn,'cp -f /home/anan/lib/DSTPOL/polarimeterlib_v2.pro '+dir
			;endif
			endfor ;i

			nnx=300
			nny=500
			for i=0,ni do begin
				restore,svdir+strmid(wparam.file,0,strlen(wparam.file)-1)+'_iquv'+string(i,format='(i4.4)')+'.sav'
				ss=size(iquv)
				nx=ss[1]
				ny=ss[2]
				if ss[0] eq 3 then nf1=1 else nf1=ss[4]
				if i eq 0 then begin
					iquvs=fltarr(nnx*4,nny,wparam.nfile)
					hds1=hds
				endif else begin
					hds1=[hds1,hds]
				endelse
				;if nf1 eq 1 then begin
	                        ;      	quv=[iquv[*,*,1]/iquv[*,*,0],iquv[*,*,2]/iquv[*,*,0],iquv[*,*,3]/iquv[*,*,0]]
        	                ;        iii=iquv[*,*,0]
                	        ;        mm=minmax(quv)
                        	;        iii=iii/(max(iii)-min(iii))*(mm[1]-mm[0])
                               ; 	iii=iii-min(iii)+mm[0]
                               ; 	iquvs[*,*,i*nn]=congrid([iii,quv],nnx*4,nny)
				;endif else begin
					for j=0,nf1-1 do begin 
	                               		quv=[iquv[*,*,1,j]/iquv[*,*,0,j],iquv[*,*,2,j]/iquv[*,*,0,j],iquv[*,*,3,j]/iquv[*,*,0,j]]
        	                        	iii=iquv[*,*,0,j]
                	                	mm=minmax(quv)
                        	        	iii=iii/(max(iii)-min(iii))*(mm[1]-mm[0])
                                		iii=iii-min(iii)+mm[0]
                                		iquvs[*,*,i*nn+j]=congrid([iii,quv],nnx*4,nny)
 					endfor
				;endelse
			endfor
			hds=hds1
			;for i=0,wparam.nfile-1 do begin
			;	;quv=[iquv[*,*,1,i],iquv[*,*,2,i],iquv[*,*,3,i]]
			;	quv=[iquv[*,*,1,i]/iquv[*,*,0,i],iquv[*,*,2,i]/iquv[*,*,0,i],iquv[*,*,3,i]/iquv[*,*,0,i]]
			;	iii=iquv[*,*,0,i]
			;	mm=minmax(quv)
			;	iii=iii/(max(iii)-min(iii))*(mm[1]-mm[0])
			;	iii=iii-min(iii)+mm[0]
			;	iquvs[*,*,i]=[iii,quv]
			;endfor
			set_plot,'x'
			wdef,0,nnx*4,nny
			stepper,reform(iquvs[*,*,*])
			wdelete,0
                    endif
                    ;save,iquv,hds,slitiquv,wparam,file=dir+'iquv.sav'
                    ;print,'saved '+dir+'iquv.sav'

		print,''
		print,'####################################################'
		print,'Complete IQUV'
		print,'####################################################'
		print,''
		jump:
	end
	"write_html":begin
		print,''
		print,'####################################################'
		print,'Make and add Stokes spectra to HTML file'
		print,'####################################################'
		print,''
;restore,'./tmp.sav'
		yyyy	=strmid(hds[0].date,0,4)
		mm	=strmid(hds[0].date,5,2)
		dd	=strmid(hds[0].date,8,2)
		hh	=strmid(hds[0].date,11,2)
		minu	=strmid(hds[0].date,14,2)
		ss	=strmid(hds[0].date,17,6)

		hh_jst =hh+9.
		if hh_jst ge 24 then begin
			hh_jst=hh_jst-24.
			if (mm eq 1) or (mm eq 3) or (mm eq 5) or (mm eq 7) or (mm eq 8) or (mm eq 10) or (mm eq 12) then maxdd=31
			if (mm eq 2) or (mm eq 4) or (mm eq 6) or (mm eq 9) or (mm eq 11) then maxdd=30
			if mm eq 2 then begin
				if ((yyyy-660) mod 100) eq 0 then begin
					if (((yyyy-660)/100) mod 4) eq 0 then maxdd=29 else maxdd=28
				endif else begin
 					if yyyy mod 4 eq 0 then maxdd=29 else maxdd=28
				endelse
			endif
			if dd lt maxdd then begin
				dd_jst=dd+1
				mm_jst=mm
				yyyy_jst=yyyy
			endif else begin
				dd_jst=1
				if mm eq 12 then begin
					mm_jst=1
					yyyy_jst=yyyy+1
				endif else begin
					mm_jst=mm+1
					yyyy_jst=yyyy
				endelse
			endelse
		endif else begin
			dd_jst=dd
			mm_jst=mm
			yyyy_jst=yyyy
		endelse
		hh_jst=string(hh_jst,format='(i2.2)')
		dd_jst=string(dd_jst,format='(i2.2)')
		mm_jst=string(mm_jst,format='(i2.2)')
		yyyy_jst=string(yyyy_jst,format='(i4.4)')

		dir='/sp_pub/'+yyyy_jst+mm_jst+dd_jst+'/'
		if is_dir(dir) eq 0 then spawn,'mkdir '+dir
		if lun eq -1 then begin
			openw,lun,dir+ yyyy_jst + mm_jst + dd_jst +'.html',/get_lun
			printf,lun,'<html><body bgcolor=white>'
			printf,lun,'<font size=5> DST spectropolarimetric observation on '+ yyyy_jst +'.'+ mm_jst +'.'+ dd_jst +' in JST </font><br>'
			observer='' & read,'Observer : ',observer
			printf,lun,'<font size=4> Observer : '+observer+'<font><br>'

			openw,lun1,dir+'/LOG'+ yyyy_jst + mm_jst + dd_jst +'.html',/get_lun
			printf,lun1,'<html><body bgcolor=white>'
			memo=file_search(wparam.dir,'memo.txt',count=nmemo)
			if nmemo gt 0 then begin 
				for imemo=0,nmemo-1 do begin
					str=''
					openr,lun2,memo[imemo],/get_lun
					while not EOF(lun2) do begin
						readf,lun2,str	
						printf,lun1,'<font size=2>'+str+'</font><br>'
					endwhile
					free_lun,lun2
				endfor
			endif
			printf,lun1,'</body></html>'
			free_lun,lun1
			;spawn,'scp /home/anan/DSTSP_sample/html/LOG'+yyyy+mm+dd+'.html anan@kipsua.kwasan.kyoto-u.ac.jp:/home/anan/public_html/'

			printf,lun,'<font size=4> <a href="http://www.hida.kyoto-u.ac.jp/DST/SP/'+ yyyy_jst + mm_jst + dd_jst +'/LOG'+yyyy_jst + mm_jst + dd_jst +'.html"> Log file </a> <font><br>'

			printf,lun,'<font size=4> <a href="http://www.hida.kyoto-u.ac.jp/DST/his/DST'+ yyyy_jst + mm_jst + dd_jst +'.html"> Slit-jaw movies </a> <font><br>'
			printf,lun,'<hr>'
		endif
		target='' & read,'Target : ',target
		printf,lun,'<font size=4> Target : '+target+'<font><br>'
		printf,lun,'<font size=4> Time in UT  : '+strmid(hds[0].date,11,8)+' - '+strmid(hds[(size(hds))[1]-1].date,11,8)+' </font><br>'
		printf,lun,'<font size=4> Wavelength : '+string(hds[0].wave/10.,format='(i4)')+' nm </font><br>'

		printf,lun,'<table border="1">'

		printf,lun,'<tr>'
		;printf,lun,'<td align=center width=150> Slit-jaw image </td>'
		printf,lun,'<td align=center width=600> Stokes spectra </td>'
		printf,lun,'</tr>'

		;timestr=hds[0].date_obs
		;hh=strmid(timestr,11,2)
		;if hh ge 15 then hh=hh+9-24
		;minu=strmid(timestr,14,2)
		;ss=strmid(timestr,17,2)
		;sttime=hh*60.*60.+minu*60.+ss*1.
		;timestr=hds[(size(hds))[1]-1].date_obs
		;hh=strmid(timestr,11,2)
		;if hh ge 15 then hh=hh+9-24
		;minu=strmid(timestr,14,2)
		;ss=strmid(timestr,17,2)
		;entime=hh*60.*60.+minu*60.+ss*1.
		;hisf=file_search('http://www.hida.kyoto-u.ac.jp/DST/his/'+yyyy+mm+dd+'/jpeg/','*.jpeg',count=nhis)
		;timestr=hisf
		
		if is_dir(dir+'jpeg/') eq 0 then spawn,'mkdir -p '+dir+'jpeg/'
		nx=(size(iquvs))[1]
		ys=0.15
		ye=0.9
		xs=0.1
		xd=0.2
		xdd=0.01
		jpegfiles='jpeg/'+strmid(hds.date,11,2)+strmid(hds.date,14,2)+strmid(hds.date,17,6)+'.jpeg'
		;jpegfiles=strmid(hds.date,11,2)+strmid(hds.date,14,2)+strmid(hds.date,17,6)+'.jpeg'
		set_plot,'z'
		!p.font=-1
		;device,set_resolution=(size(iquvs))[1:2]*2./3.
		device,set_resolution=[700,500]
		for iii=0,(size(hds))[1]-1 do begin
			print,iii,(size(hds))[1]-1
			;tvscl,iquvs[*,*,iii]
			for jjj=0,3 do begin
				xtitle=''
				ytitle=''
				case jjj of 
					0:begin
						title='I'
						xtitle='slit'
						ytitle='wavelength'
					end
					1:title='Q/I'
					2:title='U/I'
					3:title='V/I'
				endcase
				plot_image,iquvs[jjj*nx/4.:(jjj+1)*nx/4.-1,*,iii],title=title,charsize=1,	$
					norm=1,noerase=jjj,pos=[xs+jjj*(xd+xdd),ys,xs+jjj*(xd+xdd)+xd,ye],	$
					xtickname=replicate(' ',10),ytickname=replicate(' ',10),xtitle=xtitle,ytitle=ytitle
			endfor
			write_jpeg,dir+jpegfiles[iii],tvrd()
		endfor
		set_plot,'x'

		outfile=dir+'stokes'+string(iobs,format='(i2.2)')+'.html'
		jsmovie2,outfile,jpegfiles,title='DST SP'

		printf,lun,'<tr>'
		;printf,lun,'<td align=center width=150> <imgsrc="hoge" height=150 width=150> </td>'
		printf,lun,'<td align=center width=600> <a href="./stokes'+string(iobs,format='(i2.2)')+'.html">'+	$
			'<img src="./jpeg/'+strmid(hds[0].date,11,2)+strmid(hds[0].date,14,2)+strmid(hds[0].date,17.6)+'.jpeg" height=300 width=600> </a></td>'	
			;'<img src="./'+strmid(hds[0].date,11,2)+strmid(hds[0].date,14,2)+strmid(hds[0].date,17.6)+'.jpeg" height=300 width=600> </a></td>'	
		printf,lun,'</tr>'

		iobs=iobs+1	
		printf,lun,'<table clear center>'
		printf,lun,'<hr>'
		print,''
		print,'####################################################'
		print,'Finishing to add Stokes spectra to HTML file'
		print,'####################################################'
		print,''
	end
	"EXIT":begin
		if lun ne -1 then begin
			yyyy	=strmid(hds[0].date,0,4)
			mm	=strmid(hds[0].date,5,2)
			dd	=strmid(hds[0].date,8,2)
			hh	=strmid(hds[0].date,11,2)
			minu	=strmid(hds[0].date,14,2)
			ss	=strmid(hds[0].date,17,6)
			hh_jst=hh+9.
			if hh_jst ge 24 then begin
				hh_jst=hh_jst-24.
				if (mm eq 1) or (mm eq 3) or (mm eq 5) or (mm eq 7) or (mm eq 8) or (mm eq 10) or (mm eq 12) then maxdd=31
				if (mm eq 2) or (mm eq 4) or (mm eq 6) or (mm eq 9) or (mm eq 11) then maxdd=30
				if mm eq 2 then begin
					if ((yyyy-660) mod 100) eq 0 then begin
						if (((yyyy-660)/100) mod 4) eq 0 then maxdd=29 else maxdd=28
					endif else begin
 						if yyyy mod 4 eq 0 then maxdd=29 else maxdd=28
					endelse
				endif
				if dd lt maxdd then begin
					dd_jst=dd+1
					mm_jst=mm
					yyyy_jst=yyyy
				endif else begin
					dd_jst=1
					if mm eq 12 then begin
						mm_jst=1
						yyyy_jst=yyyy+1
					endif else begin
						mm_jst=mm+1
						yyyy_jst=yyyy
					endelse
				endelse
			endif else begin
				dd_jst=dd
				mm_jst=mm
				yyyy_jst=yyyy
			endelse
			hh_jst=string(hh_jst,format='(i2.2)')
			dd_jst=string(dd_jst,format='(i2.2)')
			mm_jst=string(mm_jst,format='(i2.2)')
			yyyy_jst=string(yyyy_jst,format='(i4.4)')

			;printf,lun,'<font size=4> The Stokes spectra are not subtracted dark and not corrected flat fielding.</font><br>'
			;printf,lun,'<font size=4> They are made from one of dual orthogonal spectra, and are calibrated for instrumental polarization by a model whose parameters are measured in June or April, 2012. </font><br>'
			;printf,lun,'<font size=4> If you want to use the data, please contact us.</font><br>'
			;printf,lun,'<font size=4> E-mail: data_info [at] kwasan.kyoto-u.ac.jp</font><br>'
			printf,lun,'</body></html>'
			free_lun,lun

			spawn,'cp -f /home/observer/lib/sp/dstvssp_cal.pro '+dir
			spawn,'cp -f /home/anan/lib/DSTPOL/polarimeterlib_v2.pro '+dir
			update_cal_dstsp,yyyy_jst+mm_jst+dd_jst
			;spawn,'scp /sp_pub/DSTSP*.html observer@kipsua.kwasan.kyoto-u.ac.jp:/smart3/DST/SP/'
			spawn,'scp /sp_pub/DSTSP*.html observer@kipsua.kwasan.kyoto-u.ac.jp:/data/DST/SP/'
			;spawn,'scp /sp_pub/'+yyyy_jst+mm_jst+dd_jst+'/*.html observer@kipsua.kwasan.kyoto-u.ac.jp:/smart3/DST/SP/'+yyyy_jst+mm_jst+dd_jst+'/'
			;spawn,'scp -r /sp_pub/'+yyyy_jst+mm_jst+dd_jst+'/*.jpeg observer@kipsua.kwasan.kyoto-u.ac.jp:/smart3/DST/SP/'+yyyy_jst+mm_jst+dd_jst+'/'
			;spawn,'scp -r /sp_pub/'+yyyy_jst+mm_jst+dd_jst+'/ observer@kipsua.kwasan.kyoto-u.ac.jp:/smart3/DST/SP/'
			spawn,'scp -r /sp_pub/'+yyyy_jst+mm_jst+dd_jst+'/ observer@kipsua.kwasan.kyoto-u.ac.jp:/data/DST/SP/'
			print,'scp /work2/sp_pub/DSTSP*.html observer@kipsua.kwasan.kyoto-u.ac.jp:/data/DST/SP/'
			print,'scp -r /work2/sp_pub/'+yyyy_jst+mm_jst+dd_jst+'/ observer@kipsua.kwasan.kyoto-u.ac.jp:/data/DST/SP/'
		endif
		WIDGET_CONTROL, /destroy, ev.top
	end
	else:
endcase

end
;************************************************************************
pro dstvssp_cal
;function dstvssp_ql
;--------------------------------------------------------------
common widgetlib,wparam,windex,date,dir,iquvs,hds,lun,iobs,svdir
;common widgetlib,wparam,windex,date,dir,	$
;	refs,refh,time,ha0,zd0,iquvs,hds,radius0,pangle0,incli0,gangle0,lun,iobs

loadct,0
iobs=0
;hazddir=dir+'hazd'
;pqfiles=[(file_search(dir,'XEVA640/ref089*'))[1]]
;files=[(file_search(dir,'XEVA640/scan02*.fits'))[1]]
;sign=-1.

lun=-1
wparam={widget_param, 							$
	camera  :	'orca4',					$
	dir	:	'/mnt/',	$	
	drk_file:	'd*',						$
	flat_file:	'flat*',						$
	pq_file:	'ref*',					$	
	hazddir	:	'/mnt/',		$
	skipkxky:	0,		$ ;1) skip, 0) no skip, 20170117 TA	
	file	:	'scan02*',					$
	ndrk	:	0,						$	
	nflat	:	0,						$	
	npq	:	0,						$	
	nfile	:	0,						$
	sign	:	1.,						$
	xn	:	-0.0219,						$
	tn	:	-10.0,						$
	xc	:	0.0204,						$ ; deg
	tc	:	-4.01,						$ ; deg
	sc	:	0.079						$ 
	}

windex={widget_index,		$
	camera:		0l,	$
	dir:		0l,	$
	drk_file:	0l,	$
	flat_file:	0l,	$
	pq_file:	0l,	$
	hazddir:	0l,	$
	file:		0l,	$
	ndrk:		0l,	$
	nflat:		0l,	$
	npq:		0l,	$
	nfile:		0l,	$
	skipkxky:	0l,	$
	sign:		0l,	$
	xn	:	0l,						$
	xc	:	0l,						$
	tn	:	0l,						$ ; deg
	tc	:	0l,						$ ; deg
	sc	:	0l,						$ 
	Exit:		0l	$
	}

;drkfiles=file_search(wparam.dir,wparam.drk_file+'.fits',count=nf)
wparam.ndrk=0;nf
;pqfiles=file_search(wparam.dir,wparam.pq_file+'.fits',count=nf)
wparam.npq=0;nf
;files=file_search(wparam.dir,wparam.file+'.fits',count=nf)
wparam.nfile=0;nf

main = WIDGET_BASE(title='DST/VS/SP CAL',/column)

  ;lab= widget_label(main,value=' ')
  base1=widget_base(main, /column, /frame)
    base1_0=widget_base(base1, /row, frame=0)
    	lab= widget_label(base1_0,value='Camera',xsize=60)
	strings=['ORCA-Flash4.0','XEVA-640','GE1650']
	windex.camera=cw_bgroup(base1_0,strings,/row,uvalue='camera',set_value=0,/exclusive)

    base1_1=widget_base(base1, /row, frame=0)
    lab= widget_label(base1_1,value='Dir',xsize=60)
    windex.dir=widget_button(base1_1, value=wparam.dir, uvalue = "dir",	$
				/align_left,xsize=280,ysize=30)

    base1_2=widget_base(base1, /row, frame=0)
      lab= widget_label(base1_2,value='DARK',xsize=60)
      windex.drk_file=widget_text(base1_2,value=wparam.drk_file, $
				xsize=10, ysize=1, uvalue='drk_file',/edit)
      lab= widget_label(base1_2,value='.fits    # of files')
      windex.ndrk=widget_text(base1_2,value=string(wparam.ndrk,format='(i)'), $
				xsize=10, ysize=1, uvalue='ndrk',/edit)
      bt=widget_button(base1_2, value='PREP', uvalue = "prep_dark",ysize=30)

    base1_2_1=widget_base(base1, /row, frame=0)
      lab= widget_label(base1_2_1,value='Flat file',xsize=60)
      windex.flat_file=widget_text(base1_2_1,value=wparam.flat_file, $
				xsize=10, ysize=1, uvalue='flat_file',/edit)
      lab= widget_label(base1_2_1,value='.fits    # of files')
      windex.nflat=widget_text(base1_2_1,value=string(wparam.nflat,format='(i)'), $
				xsize=10, ysize=1, uvalue='nflat',/edit)
      ;windex.flat_file=widget_button(base1_2_1, value=wparam.flat_file, uvalue = "flat",	$
	;		/align_left,xsize=280,ysize=30)
      bt=widget_button(base1_2_1, value='PREP', uvalue = "prep_flat",ysize=30)

    base1_3=widget_base(base1, /row, frame=0)
      lab= widget_label(base1_3,value='+Q',xsize=60)
      windex.pq_file=widget_text(base1_3,value=wparam.pq_file, $
				xsize=10, ysize=1, uvalue='pq_file',/edit)
      lab= widget_label(base1_3,value='.fits    # of files')
      windex.npq=widget_text(base1_3,value=string(wparam.npq,format='(i)'), $
				xsize=10, ysize=1, uvalue='npq',/edit)
      bt=widget_button(base1_3, value='PREP', uvalue = "prep_ref",ysize=30)
    base1_3_1=widget_base(base1, /row, frame=0)
      lab= widget_label(base1_3_1,value='  skip alignment ',xsize=100)
      signskipkxky=['no','yes']
      windex.skipkxky=cw_bselector(base1_3_1,signskipkxky,label_left='', $
				uvalue="skipkxky",set_value=0, ysize=1)

    base1_4=widget_base(base1, /row, frame=0)
      lab= widget_label(base1_4,value='HA & ZD',xsize=60)
      windex.hazddir=widget_button(base1_4, value=wparam.hazddir, uvalue = "hazddir",	$
				/align_left,xsize=280,ysize=30)
      bt=widget_button(base1_4, value='PREP', uvalue = "prep_hazd",ysize=30)


    base1_5=widget_base(base1, /column, frame=1)
      lab= widget_label(base1_5,value='DST MM parameters')

    	base1_5_1=widget_base(base1_5, /row)
    	base1_5_1_1=widget_base(base1_5_1, /column)
        lab= widget_label(base1_5_1_1,value='XN')
        windex.xn=widget_text(base1_5_1_1,value=string(wparam.xn,format='(f8.5)'), $
				xsize=8, ysize=1, uvalue='xn',/edit)
    	base1_5_1_2=widget_base(base1_5_1, /column)
        lab= widget_label(base1_5_1_2,value='tauN')
        windex.tn=widget_text(base1_5_1_2,value=string(wparam.tn,format='(f8.3)'), $
				xsize=8, ysize=1, uvalue='tn',/edit)
    	base1_5_1_3=widget_base(base1_5_1, /column)
        lab= widget_label(base1_5_1_3,value='XC')
        windex.xc=widget_text(base1_5_1_3,value=string(wparam.xc,format='(f8.5)'), $
				xsize=8, ysize=1, uvalue='xc',/edit)
    	base1_5_1_4=widget_base(base1_5_1, /column)
        lab= widget_label(base1_5_1_4,value='tauC')
        windex.tc=widget_text(base1_5_1_4,value=string(wparam.tc,format='(f8.3)'), $
				xsize=8, ysize=1, uvalue='tc',/edit)
    	base1_5_1_5=widget_base(base1_5_1, /column)
        lab= widget_label(base1_5_1_5,value='sc')
        windex.sc=widget_text(base1_5_1_5,value=string(wparam.sc,format='(f6.4)'), $
				xsize=8, ysize=1, uvalue='sc',/edit)


    base1_6=widget_base(base1, /row, frame=0)
      signv=['+V','-V']
      lab= widget_label(base1_6,value='Sign V',xsize=50)
      windex.sign=cw_bselector(base1_6,signv,label_left='', $
				uvalue="sign",set_value=0, ysize=1)



  lab= widget_label(main,value=' ')
  base2=widget_base(main, /column, /frame)
    base2_1=widget_base(base2, /row, frame=0)
      lab= widget_label(base2_1,value='FILE',xsize=60)
      windex.file=widget_text(base2_1,value=wparam.file, $
				xsize=10, ysize=1, uvalue='file',/edit)
      lab= widget_label(base2_1,value='.fits    # of files')
      windex.nfile=widget_text(base2_1,value=string(wparam.nfile,format='(i)'), $
				xsize=10, ysize=1, uvalue='nfile',/edit)
    bt=widget_button(base2, value='REDUCTION', uvalue = "reduction",	$
				ysize=30)


  lab= widget_label(main,value=' ')
  base3=widget_base(main, /column, frame=1)

    base3_3=widget_base(base3, /row, frame=0)
    bt=widget_button(base3_3, value='memo.txt', uvalue = "show_memo",	$
				/align_center,xsize=80,ysize=30)

    bt=widget_button(base3_3, value='Write Stokes spectra on HTML file', uvalue = "write_html",	$
				/align_center,xsize=250,ysize=30)


windex.Exit = widget_button(main, value="Exit", uvalue = "EXIT")
widget_control, main, /realize
XMANAGER,'dstvssp_cal',main,modal=modal

;return,iquvs


END


;  2017.02.06 T.A.


@~anan/lib/ta_sinfit_mpfit.pro
@~anan/lib/ta_ct_kuhn_0.pro
@~anan/lib/DSTPOL//polarimeterlib_v2.pro
@~anan/lib/DSTPOL//polarimeterlib_v3.pro
@~anan/lib/ta_calib_hsp_mpfit.pro
;--------------------------------
pro plotres,stks,stds,hd,res,eps=eps,xx=xx,yy=yy,yfit=yfit,key=key,dx=dx,mms=mms,dd=dd
            ;save,stks,stds,hd,res,fixed,bad,stks0,hd0,stds0,thr,wparam_calib,  $
            ;     file='/home/observer/lib/sp/data/'+strmid(hd[0].date,0,10)+telpos+hd[0].wave+'.sav'
nd = n_elements(hd)
dstsp_hd2angle,hd,ha,zd,r,p,incli,azimuth,imgrot
hds= [hd,hd,hd]
zd = [zd,zd,zd]
ha = [ha,ha,ha]
incli=[incli,incli,incli]
ir = [imgrot,imgrot,imgrot]
az = [azimuth,azimuth,azimuth]

key= fltarr(3*nd)
for kk=0,4 do begin
    case kk of
        0:pp=where(hd.polstate eq '')
        1:pp=where((hd.polstate eq '0') or (hd.polstate eq '180'))
        2:pp=where((hd.polstate eq '90') or (hd.polstate eq '270'))
        3:pp=where((hd.polstate eq '45') or (hd.polstate eq '225'))
        4:pp=where((hd.polstate eq '135') or (hd.polstate eq '315'))
    endcase
    key[pp]=3*kk+0 & key[pp+nd]=3*kk+1 & key[pp+2*nd]=3*kk+2
endfor

yy = [stks[*,0],stks[*,1],stks[*,2]]
yerr= [stds[*,0],stds[*,1],stds[*,2]]

pos=where(zd ne 0 and ha ne 0 and abs(yy) le 1,npos)
if npos ge 1 then begin
        zd=zd[pos]
        ha=ha[pos]
        incli=incli[pos]
        yy=yy[pos]
        yerr=yerr[pos]
        key=key[pos]
        ir=ir[pos]
        az=az[pos]
endif

xx      = [[ha],[zd],[ir],[az],[incli],[key]]
weight=10.
;yfit0w=fit_fun(xx,parinfo.value)
;yfit0[where(key eq 0 or key eq 1 or key eq 2)]=yfit0w[where(key eq 0 or key eq 1 or key eq 2)]/weight
yfit=fit_fun(xx,res,vertmp=3,hdstmp=hds,weighttmp=weight,mms=mms)
yfit[where(key eq 0 or key eq 1 or key eq 2)]=yfit[where(key eq 0 or key eq 1 or key eq 2)]/weight
yfit0=yfit
no_origin_plot=1
ys=0.1
ye=0.9
ydd=0.02
nny=3
yd=(ye-ys-(nny-1)*ydd)/float(nny)
xd=0.15
xdd=0.02
xs=0.32

if keyword_set(eps) then begin 
   chs=0.8
endif else begin
   chs=1.5
   window,0,xs=1000,ys=600
endelse
set_line_color
!p.multi=0
   for i=0,2 do begin
      if i eq 0 then begin
         noerase=0
         title='(1,0,0,0)'
      endif else begin
         title=''
      endelse
      if i eq 2 then begin
         xtickname=''
         xtitle='HA (h)'
      endif else begin
         xtickname=replicate(' ',10)
         xtitle=''
      endelse

      ytickname=''
      case i of
           0:ytitle='Q/I'
           1:ytitle='U/I'
           2:ytitle='V/I'
      endcase
      pos=where(key eq 0+i)
      plot_err,xx[pos,0]*!radeg/15.,yy[pos],yerr=yerr[pos],psym=3,   $
         yr=[(min([yy[pos],yfit0[pos],yfit[pos]])-0.01)>(-1.),  $
             (max([yy[pos],yfit0[pos],yfit[pos]])+0.01)<(1.)], $
         ystyle=1, $
         xtickname=xtickname,xtitle=xtitle,ytitle=ytitle,ytickname=ytickname, $
         norm=1,noerase=noerase,pos=[0.1,ys+(yd+ydd)*(2-i),0.1+xd,ys+(yd+ydd)*(2-i)+yd],   $
         charsize=chs,color=0,background=1,title=title
      if no_origin_plot eq 0 then oplot,xx[pos,0]*!radeg,yfit0[pos],line=2,color=0
      oplot,xx[pos,0]*!radeg/15.,yfit[pos],color=3
      oplot_err,xx[pos,0]*!radeg/15.,yy[pos],yerr=yerr[pos],psym=3,color=0

      noerase=1
      ytitle=''

      ix=0
      if i eq 0 then title='(1,1,0,0)' else title=''
      pos=where(key eq 1*3+i)
      plot_err,xx[pos,0]*!radeg/15.,yy[pos],psym=3,yr=[-1,1],ystyle=1,yerr=yerr[pos],   $
         norm=1,noerase=noerase,   $
         xtickname=xtickname,xtitle=xtitle,ytitle=ytitle,ytickname=ytickname, $
         pos=[xs+ix*(xd+xdd),ys+(yd+ydd)*(2-i),xs+ix*(xd+xdd)+xd,ys+(yd+ydd)*(2-i)+yd],   $
         charsize=chs,color=0,background=1,title=title
      if no_origin_plot eq 0 then oplot,xx[pos,0]*!radeg,yfit0[pos],line=2,color=0
      oplot,xx[pos,0]*!radeg/15.,yfit[pos],color=3
      oplot_err,xx[pos,0]*!radeg/15.,yy[pos],yerr=yerr[pos],psym=3,color=0

      ytickname=replicate(' ',10)
      ix=1
      if i eq 0 then title='(1,-1,0,0)' else title=''
      pos=where(key eq 2*3+i)
      plot_err,xx[pos,0]*!radeg/15.,yy[pos],psym=3,yr=[-1,1],ystyle=1,yerr=yerr[pos],   $
         norm=1,noerase=noerase,   $
         xtickname=xtickname,xtitle=xtitle,ytitle=ytitle,ytickname=ytickname, $
         pos=[xs+ix*(xd+xdd),ys+(yd+ydd)*(2-i),xs+ix*(xd+xdd)+xd,ys+(yd+ydd)*(2-i)+yd],   $
         charsize=chs,color=0,background=1,title=title
      if no_origin_plot eq 0 then oplot,xx[pos,0]*!radeg,yfit0[pos],line=2,color=0
      oplot,xx[pos,0]*!radeg/15.,yfit[pos],color=3
      oplot_err,xx[pos,0]*!radeg/15.,yy[pos],yerr=yerr[pos],psym=3,color=0

      ix=2
      if i eq 0 then title='(1,0,1,0)' else title=''
      pos=where(key eq 3*3+i)
      plot_err,xx[pos,0]*!radeg/15.,yy[pos],psym=3,yr=[-1,1],ystyle=1,yerr=yerr[pos],   $
         norm=1,noerase=noerase,   $
         xtickname=xtickname,xtitle=xtitle,ytitle=ytitle,ytickname=ytickname, $
         pos=[xs+ix*(xd+xdd),ys+(yd+ydd)*(2-i),xs+ix*(xd+xdd)+xd,ys+(yd+ydd)*(2-i)+yd],   $
         charsize=chs,color=0,background=1,title=title
      if no_origin_plot eq 0 then oplot,xx[pos,0]*!radeg,yfit0[pos],line=2,color=0
      oplot,xx[pos,0]*!radeg/15.,yfit[pos],color=3
      oplot_err,xx[pos,0]*!radeg/15.,yy[pos],yerr=yerr[pos],psym=3,color=0

      ix=3
      if i eq 0 then title='(1,0,-1,0)' else title=''
      pos=where(key eq 4*3+i)
      plot_err,xx[pos,0]*!radeg/15.,yy[pos],psym=3,yr=[-1,1],ystyle=1,yerr=yerr[pos],   $
         xtickname=xtickname,xtitle=xtitle,ytitle=ytitle,ytickname=ytickname, $
         norm=1,noerase=noerase,   $
         pos=[xs+ix*(xd+xdd),ys+(yd+ydd)*(2-i),xs+ix*(xd+xdd)+xd,ys+(yd+ydd)*(2-i)+yd],   $
         charsize=chs,color=0,background=1,title=title
      if no_origin_plot eq 0 then oplot,xx[pos,0]*!radeg,yfit0[pos],line=2,color=0
      oplot,xx[pos,0]*!radeg/15.,yfit[pos],color=3
      oplot_err,xx[pos,0]*!radeg/15.,yy[pos],yerr=yerr[pos],psym=3,color=0
   endfor;

   if keyword_set(eps) then goto,jump
   dx=fltarr(4,4)
;=============================
   defmax=0.5
   window,1,xs=700,ys=700
   set_line_color
   !p.multi=[0,4,4]

   ; I=>I
   pos=where(key eq 0)
   plot,xx[pos,0]*!radeg,yfit[pos]-yy[pos],psym=1,   $
      ystyle=1,nodata=1,  $
      charsize=chs,color=0,background=1,xtitle='HA (deg)',ytitle='MDL - OBS'
   ; Q=>I
   pos=where(key eq 0)
   plot,xx[pos,0]*!radeg,yfit[pos]-yy[pos],psym=1,   $
      ystyle=1,nodata=1,  $
      charsize=chs,color=0,background=1,xtitle='HA (deg)',ytitle='MDL - OBS'
   ; U=>I
   pos=where(key eq 0)
   plot,xx[pos,0]*!radeg,yfit[pos]-yy[pos],psym=1,   $
      ystyle=1,nodata=1,  $
      charsize=chs,color=0,background=1,xtitle='HA (deg)',ytitle='MDL - OBS'
   ; V=>I
   pos=where(key eq 0)
   plot,xx[pos,0]*!radeg,yfit[pos]-yy[pos],psym=1,   $
      ystyle=1,nodata=1,  $
      charsize=chs,color=0,background=1,xtitle='HA (deg)',ytitle='MDL - OBS'

   ; I=>Q
   yr=0.1
   pos=where(key eq 0)
   def=yfit[pos]-yy[pos]
   pos2=where(abs(def) le defmax)
   dx[0,1]=sqrt(mean((def[pos2])^2))
   plot,xx[pos,0]*!radeg,yfit[pos]-yy[pos],psym=1,   $
      title='RMS '+string(dx[0,1],format='(f6.4)'),   $
      ystyle=1,yr=[-yr,yr], $
      charsize=chs,color=0,background=1,xtitle='HA (deg)',ytitle='MDL - OBS'
   ;oplot,!x.crange,[yr,yr],color=0,line=0
   ;oplot,!x.crange,-[yr,yr],color=0,line=0

   ; Q=>Q
   yr=0.1
   pos=where(key eq 3 or key eq 6)
   def=yfit[pos]-yy[pos]
   pos2=where(abs(def) le defmax)
   dx[1,1]=sqrt(mean((def[pos2])^2))
   pos=where(key eq 3)
   plot,xx[pos,0]*!radeg,yfit[pos]-yy[pos],psym=1,   $
      title='RMS '+string(dx[1,1],format='(f6.4)'),   $
      ystyle=1,yr=[-yr,yr], $
      charsize=chs,color=0,background=1,xtitle='HA (deg)',ytitle='MDL - OBS'
   pos=where(key eq 6)
   oplot,xx[pos,0]*!radeg,yfit[pos]-yy[pos],color=0,psym=4
   ;oplot,!x.crange,[yr,yr],color=0,line=0
   ;oplot,!x.crange,-[yr,yr],color=0,line=0

   ; U=>Q
   yr=0.1
   pos=where(key eq 9 or key eq 12)
   def=yfit[pos]-yy[pos]
   pos2=where(abs(def) le defmax)
   dx[2,1]=sqrt(mean((def[pos2])^2))
   pos=where(key eq 9)
   plot,xx[pos,0]*!radeg,yfit[pos]-yy[pos],psym=1,   $
      title='RMS '+string(dx[2,1],format='(f6.4)'),   $
      ystyle=1,yr=[-yr,yr], $
      charsize=chs,color=0,background=1,xtitle='HA (deg)',ytitle='MDL - OBS'
   pos=where(key eq 12)
   oplot,xx[pos,0]*!radeg,yfit[pos]-yy[pos],color=0,psym=4
   ;oplot,!x.crange,[yr,yr],color=0,line=0
   ;oplot,!x.crange,-[yr,yr],color=0,line=0

   ; V=>Q
   pos=where(key eq 0)
   plot,xx[pos,0]*!radeg,yfit[pos]-yy[pos],psym=1,   $
      ystyle=1,nodata=1,  $
      charsize=chs,color=0,background=1,xtitle='HA (deg)',ytitle='MDL - OBS'

   ; I=>U
   yr=0.1
   pos=where(key eq 1)
   def=yfit[pos]-yy[pos]
   pos2=where(abs(def) le defmax)
   dx[0,2]=sqrt(mean((def[pos2])^2))
   plot,xx[pos,0]*!radeg,yfit[pos]-yy[pos],psym=1,   $
      title='RMS '+string(dx[0,2],format='(f6.4)'),   $
      ystyle=1,yr=[-yr,yr], $
      charsize=chs,color=0,background=1,xtitle='HA (deg)',ytitle='MDL - OBS'
   ;oplot,!x.crange,[yr,yr],color=0,line=0
   ;oplot,!x.crange,-[yr,yr],color=0,line=0

   ; Q=>U
   yr=0.1
   pos=where(key eq 4 or key eq 7)
   def=yfit[pos]-yy[pos]
   pos2=where(abs(def) le defmax)
   dx[1,2]=sqrt(mean((def[pos2])^2))
   pos=where(key eq 4)
   plot,xx[pos,0]*!radeg,yfit[pos]-yy[pos],psym=1,   $
      title='RMS '+string(dx[1,2],format='(f6.4)'),   $
      ystyle=1,yr=[-yr,yr], $
      charsize=chs,color=0,background=1,xtitle='HA (deg)',ytitle='MDL - OBS'
   pos=where(key eq 7)
   oplot,xx[pos,0]*!radeg,yfit[pos]-yy[pos],color=0,psym=4
   ;oplot,!x.crange,[yr,yr],color=0,line=0
   ;oplot,!x.crange,-[yr,yr],color=0,line=0

   ; U=>U
   yr=0.1
   pos=where(key eq 10 or key eq 13)
   def=yfit[pos]-yy[pos]
   pos2=where(abs(def) le defmax)
   dx[2,2]=sqrt(mean((def[pos2])^2))
   pos=where(key eq 10)
   plot,xx[pos,0]*!radeg,yfit[pos]-yy[pos],psym=1,   $
      title='RMS '+string(dx[2,2],format='(f6.4)'),   $
      ystyle=1,yr=[-yr,yr], $
      charsize=chs,color=0,background=1,xtitle='HA (deg)',ytitle='MDL - OBS'
   pos=where(key eq 13)
   oplot,xx[pos,0]*!radeg,yfit[pos]-yy[pos],color=0,psym=4
   ;oplot,!x.crange,[yr,yr],color=0,line=0
   ;oplot,!x.crange,-[yr,yr],color=0,line=0

   ; V=>U
   pos=where(key eq 0)
   plot,xx[pos,0]*!radeg,yfit[pos]-yy[pos],psym=1,   $
      ystyle=1,nodata=1,  $
      charsize=chs,color=0,background=1,xtitle='HA (deg)',ytitle='MDL - OBS'

   ; I=>V
   yr=0.1
   pos=where(key eq 2)
   def=yfit[pos]-yy[pos]
   pos2=where(abs(def) le defmax)
   dx[0,3]=sqrt(mean((def[pos2])^2))
   plot,xx[pos,0]*!radeg,yfit[pos]-yy[pos],psym=1,   $
      title='RMS '+string(dx[0,3],format='(f6.4)'),   $
      ystyle=1,yr=[-yr,yr], $
      charsize=chs,color=0,background=1,xtitle='HA (deg)',ytitle='MDL - OBS'
   ;oplot,!x.crange,[yr,yr],color=0,line=0
   ;oplot,!x.crange,-[yr,yr],color=0,line=0

   ; Q=>V
   yr=0.1
   pos=where(key eq 5 or key eq 8)
   def=yfit[pos]-yy[pos]
   pos2=where(abs(def) le defmax)
   dx[1,3]=sqrt(mean((def[pos2])^2))
   pos=where(key eq 5)
   plot,xx[pos,0]*!radeg,yfit[pos]-yy[pos],psym=1,   $
      title='RMS '+string(dx[1,3],format='(f6.4)'),   $
      ystyle=1,yr=[-yr,yr], $
      charsize=chs,color=0,background=1,xtitle='HA (deg)',ytitle='MDL - OBS'
   pos=where(key eq 8)
   oplot,xx[pos,0]*!radeg,yfit[pos]-yy[pos],color=0,psym=4
   ;oplot,!x.crange,[yr,yr],color=0,line=0
   ;oplot,!x.crange,-[yr,yr],color=0,line=0

   ; U=>V
   yr=0.1
   pos=where(key eq 11 or key eq 14)
   def=yfit[pos]-yy[pos]
   pos2=where(abs(def) le defmax)
   dx[2,3]=sqrt(mean((def[pos2])^2))
   pos=where(key eq 11)
   plot,xx[pos,0]*!radeg,yfit[pos]-yy[pos],psym=1,   $
      title='RMS '+string(dx[2,3],format='(f6.4)'),   $
      ystyle=1,yr=[-yr,yr], $
      charsize=chs,color=0,background=1,xtitle='HA (deg)',ytitle='MDL - OBS'
   pos=where(key eq 14)
   oplot,xx[pos,0]*!radeg,yfit[pos]-yy[pos],color=0,psym=4
   ;oplot,!x.crange,[yr,yr],color=0,line=0
   ;oplot,!x.crange,-[yr,yr],color=0,line=0

   ; V=>V
   pos=where(key eq 0)
   plot,xx[pos,0]*!radeg,yfit[pos]-yy[pos],psym=1,   $
      yr=[(min([yy[pos],yfit0[pos],yfit[pos]])-0.01)>(-1.),  $
          (max([yy[pos],yfit0[pos],yfit[pos]])+0.01)<(1.)], $
      ystyle=1,nodata=1,  $
      charsize=chs,color=0,background=1,xtitle='HA (deg)',ytitle='MDL - OBS'
;=============================
   window,2,xs=700,ys=700
   set_line_color
   !p.multi=[0,4,4]
   for i=1,3 do dx[3,i]=max([dx[1,i],dx[2,i]])
   nd=(size(mms))[3]/3.
   dd=fltarr(4,4,nd)
   ee=fltarr(4,4)
   for i=0,3 do ee[i,i]=1.
   for id=0,nd-1 do begin
        mmdst=reform(mms[*,*,id]/mms[0,0,id])
	ss=invert(mmdst) ## (mmdst+dx)
	ss=ss/float(ss[0,0])
	dd[*,*,id]=abs(ss - ee)
   endfor
   for ix=0,3 do begin
       for iy=0,3 do begin
           plot,dd[iy,ix,*],psym=1,charsize=chs*4./3.,color=0,background=1
       endfor
   endfor

jump:
!p.multi=0
loadct,0

print,'exclude outlier (dp>50%)'

END



;**************************************************************
pro dsthssp_calib_event, ev
;--------------------------------------------------------------
common widgetlib,wparam_calib,windex

widget_control, ev.id, get_uvalue=uvalue,get_value=value
case uvalue of
   'file':begin
      wparam_calib.file=value
      widget_CONTROL,windex.file,set_value=wparam_calib.file
   end
   'reffile':begin
      wparam_calib.reffile=value
      widget_CONTROL,windex.reffile,set_value=wparam_calib.reffile
   end
   'bad':begin
      wparam_calib.bad=value
      widget_CONTROL,windex.bad,set_value=wparam_calib.bad
   end
   'calibration':begin
       reffiles=findfile(wparam_calib.reffile,count=nref)
       files=findfile(wparam_calib.file,count=nf)
       if (nref eq 0) or (total(reffiles eq files[0:nref-1]) eq nf) then begin
          print,'First triggered camera'
          ref_index=0
       endif else begin
          print,'reading index of first triggered camera'
          for i=0,nref-1 do begin
             restore,reffiles[i]
             if i eq 0 then ref_index=hds else ref_index=[ref_index,hds]
          endfor
          ref_time=((anytim2utc(ref_index.date_end)).time/1000d0 + 9.*3600.) mod (24.*3600.) 
          print,'complete reading index'
       endelse 

       if nf ge 1 then begin
          restore,files[0],/verb
          wdef,0,600,600
          !p.multi=0
          loadct,0
          hmin=0
          hmax=max(iquv[*,*,0,*])
          nbin=100
          xx=hmax/float(nbin-1)*findgen(nbin)
          plot,charsize=2.5,xx,histogram(iquv[*,*,0,*],nbin=nbin,min=hmin,max=hmax)
          print,'click threshold to select spectra'
          xycursor,thr,y
          oplot,[thr,thr],!y.crange,line=1

          stks=fltarr(40*nf,4)
          stds=fltarr(40*nf,4)
          for i=0,nf-1 do begin
              caldat,systime(/julian),mon1,day1,year1,hour1,minu1,seco1
              print,i,nf-1,',     time',hour1,minu1
              restore,files[i],/verb
              niquv=(size(iquv))[4]
              for j=0,niquv-1 do begin
                  for k=0,3 do begin
                      if k eq 0 then begin
                          arr=iquv[*,*,0,j]
                          arri=arr
                          pos=where(arri ge thr,npos)
                      endif else begin
                          arr=iquv[*,*,k,j]/iquv[*,*,0,j]
                      endelse
                      if npos ge 1 then begin
                          stks[i*40+j,k]=mean(arr[pos])
                          stds[i*40+j,k]=stddev(arr[pos])
                      endif
                  endfor
              endfor
              if i eq 0 then hd0=hds else hd0=[hd0,hds]
           endfor
           stks0=stks[0:40*(nf-1)+(size(iquv))[4]-1,*]
           stds0=stds[0:40*(nf-1)+(size(iquv))[4]-1,*]

           if (size(ref_index))[0] eq 1 then begin
              print,'comparing reference index'
              nhdcam1=n_elements(ref_index)
              stks=fltarr(nhdcam1,4)
              stds=fltarr(nhdcam1,4)
              hd=replicate(hd0[0],nhdcam1)
              time0=((anytim2utc(hd0.date_end)).time/1000d0 + 9.*3600.) mod (24.*3600.)
              for i=0,nhdcam1-1 do begin
                 if (i mod 10)  eq 0 then print,i,nhdcam1
                 tmp=min(abs(ref_time[i]-time0),pos)
                 hd[i]=hd0[pos]
                 ;hd[i].polstate=ref_index[i].polstate
                 stks[i,*]=stks0[pos,*]
                 stds[i,*]=stds0[pos,*]
              endfor
            endif else begin
              stks=stks0
              stds=stds0
              hd=hd0
            endelse

            str=wparam_calib.bad
            tmp=strpos(str,',')
            nbad=0l
            while tmp ne -1 do begin
               if nbad eq 0l then begin
                  bad=strmid(str,0,tmp)
               endif else begin 
                  bad=[bad,strmid(str,0,tmp)]
               endelse
               str=strmid(str,tmp+1,strlen(str)-(tmp+1))
               tmp=strpos(str,',')
               nbad=nbad+1l
            endwhile
            if nbad eq 0 then bad=str else bad=[bad,str]
            nbad=nbad+1l
            pos=where(bad ne -1,npos)
            if npos eq 0 then begin
               bad=-1
               nbad=0
            endif else begin
               bad=bad[pos]
               nbad=npos
            endelse
;TEMPOLARY 2016.12.8 8542A
;bad=[116,104,12,29,40,60,100,120,150,160,190,220,240,250,260,320,350,377,378,379,where(hd.polstate eq 45)]
;TEMPOLARY 2016.12.8 10830A
bad=[105,104,135,324,343,344,323,322,26,11,where(hd.polstate eq 45)]
nbad=n_elements(bad)
            if nbad ge 1 then print,'bad data :'+string(bad,format='(i5)')  
            pos=findgen(n_elements(hd))
            for i=0,n_elements(bad)-1 do begin
               if i eq 0 and (bad[i] lt 0) then begin
                  print,'No bad data'
               endif else begin 
                  pos=pos[where(pos ne bad[i])]
               endelse
            endfor
            stks0=stks
            stds0=stds
            hd0=hd
            stks=stks[pos,*]
            stds=stds[pos,*]
            hd=hd[pos,*]

            fixed=intarr(46)
            fixed[5:8]=1
            fixed[11:27]=1
            fixed[43:44]=1

            par=par_dst(hd[0],th_mmsp2_hsp=(88.2 - 131.2)*!dtor)
            par.xn=-0.0387;xn
            par.tn=-16.8*!dtor;tn
            par.xc=-0.0321;xc
            par.tc=12.5*!dtor;tc
            par.sc=0.0521;sc
            par.par_mmsp2[16:31]=par_mmsp2(float(hd[0].wave)*.1,version=3)
            th_calunit=0.
            pars=[par.xn,par.tn,par.xc,par.tc,par.sc,par.t_en,par.dlen,par.t_ex,par.dlex,   $
                  par.th_dst_mmsp2,par.th_mmsp2_hsp,par.par_mmsp2,th_calunit]
            caldat,systime(/julian),mon1,day1,year1,hour1,minu1,seco1
            res=ta_calib_hsp_mpfit(stks[*,1:3],hd,pars,imgrot=hd.imgrot/3600.*!dtor,azimuth=hd.az/3600.*!dtor, $
                                   fixed=fixed,/draw,ver=3)
            caldat,systime(/julian),mon2,day2,year2,hour2,minu2,seco2

            if hd[0].zd ge 0 then telpos='w' else telpos='e'
            save,stks,stds,hd,res,fixed,bad,stks0,hd0,stds0,thr,wparam_calib,  $
                 file='/home/observer/lib/sp/data/'+strmid(hd[0].date,0,10)+telpos+hd[0].wave+'.sav'
            print,'saved /home/observer/lib/sp/data/'+strmid(hd[0].date,0,10)+telpos+hd[0].wave+'.sav'
            print,'start time',year1,mon1,day1,hour1,minu1
            print,'end time',year2,mon2,day2,hour2,minu2
       endif else begin
          print,'no file'
       endelse 
   end
   'loadfile':begin
      wparam_calib.loadfile=dialog_pickfile(path='/home/observer/lib/sp/data/',directory=0)
      widget_CONTROL,windex.loadfile,set_value=wparam_calib.loadfile
   end
   "png":begin
      restore,wparam_calib.loadfile,/verb
      if hd[0].zd ge 0 then telpos='w' else telpos='e'
      set_plot,'x'
      !p.font=-1
      !p.thick=1
      !x.thick=1 
      !y.thick=1
      plotres,stks[*,1:3],stds[*,1:3],hd,res,dd=dd
      wset,0
      outfile='/home/observer/lib/sp/data/figures/'+strmid(hd[0].date,0,10)+telpos+hd[0].wave+'_1.png'
      write_png,outfile,tvrd(/true)
      print,'saved '+outfile
      wset,1
      outfile='/home/observer/lib/sp/data/figures/'+strmid(hd[0].date,0,10)+telpos+hd[0].wave+'_2.png'
      write_png,outfile,tvrd(/true)
      print,'saved '+outfile
      wset,2
      outfile='/home/observer/lib/sp/data/figures/'+strmid(hd[0].date,0,10)+telpos+hd[0].wave+'_3.png'
      write_png,outfile,tvrd(/true)
      print,'saved '+outfile
      outfile='/home/observer/lib/sp/data/'+strmid(hd[0].date,0,10)+telpos+hd[0].wave+'_err.sav'
      save,dd,file=outfile
      print,'saved '+outfile
   end
   'eps':begin
      restore,wparam_calib.loadfile,/verb
      if hd[0].zd ge 0 then telpos='w' else telpos='e'
      outfile='/home/observer/lib/sp/data/figures/'+strmid(hd[0].date,0,10)+telpos+hd[0].wave+'_1.eps'
      set_plot,'ps'
      device,xsize=16,ysize=12,filename=outfile,/encapsulated,bits=8,/color
      !p.font=0
      device,/tt_font,set_font='Times',font_size=12
      !p.thick=4
      !x.thick=4 
      !y.thick=4
      plotres,stks[*,1:3],stds[*,1:3],hd,res,/eps
      device,/close
      set_plot,'x'
      !p.font=-1
      !p.thick=1
      !x.thick=1 
      !y.thick=1
      print,'saved '+outfile
   end
   "EXIT":begin
      WIDGET_CONTROL, /destroy, ev.top
   end
   else:print,'no uvalue'
endcase

end
;************************************************************************
pro dsthssp_calib
;--------------------------------------------------------------
common widgetlib,wparam_calib,windex

wparam_calib={widget_param_calib, 			$
	file     :	'/sp_pub/save/20161202/camera03/cal*.sav',	$
	reffile  :	'/sp_pub/save/20161202/camera01/cal*.sav',	$
	bad      :	'-1,-1',	$
	loadfile  :	'/home/observer/lib/sp/data/2016-12-02w5890.sav'	$
	}

windex={widget_index,		$
	file:		0l,	$
	reffile:	0l,	$
	bad:    	0l,	$
	loadfile:	0l,	$
	png:	0l,	$
	eps:	0l,	$
	Exit:		0l	$
	}

main = WIDGET_BASE(title='DST/HS/SP CALIBRATION',/column)
 main1=widget_base(main, /column, frame=1)
   base1=widget_base(main1, /row, frame=0)
      lab= widget_label(base1,value='Calibration data: ',xsize=300,/align_left)
   base2=widget_base(main1, /row, frame=0)
      windex.file=widget_text(base2,value=wparam_calib.file, $
				xsize=70, ysize=1, uvalue='file',/edit)
   base3=widget_base(main1, /row, frame=0)
      lab= widget_label(base3,value='Data taken by first triggered camera: ',xsize=300,/align_left)
   base4=widget_base(main1, /row, frame=0)
      windex.reffile=widget_text(base4,value=wparam_calib.reffile, $
				xsize=70, ysize=1, uvalue='reffile',/edit)
   base4a=widget_base(main1, /row, frame=0)
      lab= widget_label(base4a,value='Bad data: ',xsize=50,/align_left)
      windex.bad=widget_text(base4a,value=wparam_calib.bad, $
				xsize=60, ysize=1, uvalue='bad',/edit)
   base5=widget_base(main1, /row, frame=0)
   bt = widget_button(base5, value="calibration", uvalue = "calibration")

 main2=widget_base(main, /column, frame=1)
   base6=widget_base(main2, /row, frame=0)
      lab= widget_label(base6,value='Load data: ',xsize=300,/align_left)
   base7=widget_base(main2, /row, frame=0)
      windex.loadfile=widget_button(base7,value=wparam_calib.loadfile, $
				xsize=400,uvalue='loadfile',/align_left)
   base8=widget_base(main2, /row, frame=0)
      bt = widget_button(base8, value="png", uvalue = "png")
      bt = widget_button(base8, value="eps", uvalue = "eps")

windex.Exit = widget_button(main, value="Exit", uvalue = "EXIT")
widget_control, main, /realize
XMANAGER,'dsthssp_calib',main,modal=modal



END

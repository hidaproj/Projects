pro ssw_gev_locator, time0, time1, refresh=refresh,  $
   _extra=_extra, www=www, fdtype=fdtype, subdir=subdir

;
;+
;   Name: ssw_gev_locator
;
;   Purpose: for each GEV entry, use specified full disk set to locate flares
;
;   Input Paramters:
;      time0 - start time
;      time1 - optional stop time
; 
;   Keyword Parameters:
;      www - if set, produce some WWW output
;      fdtype - full disk set to use ('eit','sxin','sxt'...) - def=EIT195
;      _extra - unspecified keywords passed to 'ssw_flare_locator.pro;
;      refresh - if set AND www, then regenerate www files even if they exist
;
;   History:
;      26-Feb-2002 - S.L.Freeland
;
;-
if not data_chk(fdtype,/string) then fdtype='eit'
www=keyword_set(www)
topwww='/net/diapason/www1/htdocs/solarsoft'
set_logenv,'path_http',topwww
set_logenv,'top_http','http://www.lmsal.com/solarsoft'

case 1 of 
   n_elements(subdir) eq 0: subdir=concat_dir(topwww,'last_events')
   strpos(subdir,'/') ne -1: 
   else: subdir=concat_dir(topwww,subdir)
endcase 
ecat=concat_dir(subdir,'ssw_gev_locate.geny')
if not file_exist(subdir) then begin 
   box_message,'Output path: ' + subdir + ' does not exist, creating it now'
   spawn,['mkdir','-p',subdir],/noshell
endif
case n_params() of
   0: begin
        time0=reltime(days=-2,/day_only)
        time1=reltime(/now)
   endcase
   1: time1=reltime(/now)
   else:
endcase

gev_loc_template= $
    {date_obs:'',ename:'',class:'',fstart:'', fstop:'', fpeak:'', xcen:0,ycen:0,        helio:'', lfiles:'',recok:0}

gev=get_gev(time0,time1)
if not data_chk(gev,/struct) then begin 
   box_message,'No GEV events from ' + anytim(time0,/vms,/trunc) + $
                              ' through ' + anytim(time1,/vms,/trunc)
   return
endif

nev=n_elements(gev)
if file_exist(ecat) then restgenx,file=ecat,catrecs else catrecs=gev_loc_template
retval=replicate(gev_loc_template,nev)

ename='gev_'+time2file(gev)
pname=concat_dir(subdir,ename+'.png')
retval.ename=ename
for i = nev-1,0,-1 do begin 
    chkec=where(catrecs.ename eq ename(i) and catrecs.recok,okcnt)
    if okcnt eq 0 then begin 
       decode_gev,gev(i),f0,f1,fp, class=class,out='ecs' 
       retval(i).date_obs=anytim(fp,/ccsds)
       retval(i).class=class(0)
       retval(i).fstart=f0
       retval(i).fstop=f1
       retval(i).fpeak=fp
       lfiles=''                           ; initialize to nothing
       case fdtype of 
          'eit': begin 
               time_window,[f0,f1],t0x,t1x,min=45,out='vms'
               lxfiles=sswdb_files(t0x,t1x,/eit,/l1q,pat='_195_')
               ftimes=file2time(lxfiles,out='int')
               fbef=last_nelem(where(ssw_deltat(ftimes,ref=f0,/min) lt 0,bcnt))
               faft=(where(ssw_deltat(ftimes,ref=fp,/min) gt 0,acnt))(0)
               if bcnt gt 0 and acnt gt 0 then lfiles=lxfiles([fbef,faft])  
          endcase
          else: box_message,'Only FDTYPE=EIT available for today'
       endcase
       if n_elements(lfiles) gt 1 then begin
          retval(i).lfiles=arr2str(ssw_strsplit(lfiles,/tail,'/',/last),$
                                /compress,/no_duplicate) 
          delvarx,index,data
          if n_elements(lfiles) gt 2 then stop,'too man...'
          mreadfits,lfiles,index,data
          xycen=ssw_flare_locator(index,data,oindex,ldata=ldata,$
              flare_helio=flare_helio,_extra=_extra)
          if n_elements(xycen) eq 2 then begin 
            retval(i).xcen=xycen(0)
            retval(i).ycen=xycen(1)
            if www then begin 
               dindex=retval(i)
               dindex=add_tag(dindex,oindex.cdelt1,'cdelt1')
               dindex=add_tag(dindex,oindex.cdelt2,'cdelt2')
               dindex=add_tag(dindex,50,'naxis1')
               dindex=add_tag(dindex,50,'naxis2')
               ddata=bytarr(50,50,3)
               index2map,oindex,ldata,fdmap
               dindex=replicate(struct2ssw(dindex),3)
               dtags=str2arr('time,day,mjd,date_obs,crpix1,xrpix2,xcen,ycen')
               for ind=0,1 do  begin
                  temp=dindex(ind)
                  copy_struct,index(ind),temp, select=dtags
                  dindex(ind+(ind ne 0))=temp
               endfor
               context=ssw_fov_context(dindex,ddata,fdmap=fdmap,grid=15,$
                  xsize=512,margin=.07,composite=3,fov=15,ss=1,/goes,tw_percent=100.,/oplotnar)
               wdef,im=context,/zbuffer
               tv,context
               delvarx,context
               ct2rgb,5,r,g,b
               fhns=flare_helio(0) & fhew=flare_helio(1)
               hstring=$
                (['S','N'])(fhns gt 0)+ string(abs(fhns),format='(i2.2)') + $
                (['E','W'])(fhew gt 0)+ string(abs(fhew),format='(i2.2)')
               itop=data_chk(context,/ny)
               retval(i).helio=hstring 
               xmar=60
               xyouts,xmar,itop-60,'Flare Peak: '+ fp + ' Class: ' + class,/device
               xyouts,xmar,itop-80,'Derived flare XCEN/YCEN: ' + $
                          arr2str(string(xycen,format='(2f6.1)'),$
                          /compress,delim=' / '),/device, color=250
               xyouts,xmar,itop-100, 'Heliographic: ' + hstring + $
               ; (['S','N'])(fhns gt 0) + string(abs(fhns),format='(i2.2)') + $
               ; (['E','W'])(fhew gt 0) + string(abs(fhew),format='(i2.2)') + $
                           ' @ ' + anytim(oindex,/ecs,/truncate) ,/device, color=250
               xyouts,xmar,itop-120,'Difference: ' + $
                  arr2str(reverse(anytim(index,/ecs,/trunc)),delim=' - '),/device
               zbuff2file2,pname(i),r,g,b,/png 
            endif 
          endif else box_message,'Problem with location derivation?'
       endif else box_message,'no FD data (yet?)' 
    endif else box_message,'Event ' + ename(0) + ' already completed'

endfor 
  
savegenx,file=ecat,retval,/overwrite

return
end

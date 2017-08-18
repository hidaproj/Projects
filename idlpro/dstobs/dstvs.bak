;+ 
; calculate grating angle, count, dispertion etc. from wave length      */ 
; for Norikura 25-cm G1 & G2 spectrograph                               */ 
;       K.Ichimoto  '93/03/13                                           */ 
;                   '93/07/01  translated from g1.c 
;                   '95/08/30  include G2, IDL widget 
;                   '96/05/09  reviced 
;                   '97/02/19  draw config for 2nd wl 
;                   '97/08/26  Pro-B base line
;                   '03/10/27  keep compatibility with IDL V5.2
;                   '05/08/15  gif save
;- 
 
;************************************************************************ 
function spconst        ; return spectrograph constant for NKR 25-cm SP 
;------------------------------------------------------------------------ 
p = { spconst,  $       ; constants of spectrograph 
        n_groove : 632.,        $ ; grooves/mm   L-grating              */ 
        blaze    : 57.0 ,       $ ; blaze angle (deg.)                  */ 
        ;n_groove : 1200., 	$ ; grooves/mm   S-grating              */ 
        ;blaze    : 28.8167,  	$ ; blaze angle (deg.)                  */ 
        f1       : 7000.,       $ ; focal length of G1 (mm)             */ 
        g1x      : [-150.,295.],$ ; G1 window (mm)              	*/ 
;        magni1   : 0.65,        $ ; magnification factor                */ 
        magni1   : 1.0,        $ ; magnification factor                */ 
        ccdx1    : 13.5,        $ ; ccd size (mm)                       */ 
        offset   : -0.04,      	$ ; offset of origin (count)            */ 
        cfact    : 0.4,         $ ; count/degree                        */ 
        f2       : 2400.,       $ ; focal length of G2 (mm)             */ 
        thg2     : -8.,         $ ; angle of G2 axis (deg.)        	*/ 
        magni2   : 1.0,         $ ; magnification factor                */ 
        ccdx2    : 6.6    	$ ; ccd size (mm)                       */ 
;        ccdx2    : 30.       	$ ; ccd size (mm)                	*/ 
        } 
return,p 
end 
 
;************************************************************************ 
pro wlgaplot,p 
;------------------------------------------------------------------------ 
;  plot wl vs. G-angle relation 
 
        n_groove = p.n_groove   ; grooves/mm     
        blaze    = p.blaze      ; blaze angle (deg.)                    */ 
        f1       = p.f1         ; focal length of G1 (mm)               */ 
        offset   = p.offset     ; offset of origin (count)              */ 
        cfact    = p.cfact      ; count/degree                  	*/ 
        f2       = p.f2         ; focal length of G2 (mm)               */ 
        thg2     = p.thg2       ; angle of G2 axis (deg.)         	*/ 
 
        d=1./n_groove           ; space mm 
        dd = d*cos(blaze*!pi/180.) 
        wl=findgen(16000-3000)+3000. ;  <- 11000
        nn=n_elements(wl) 
        plot,[0,0],[0,0],/nodata,       $ 
                title='25-cm G1 & G2 spectrograph ',         $ 
                xstyle=1,xrange=[wl(0),wl(nn-1)],xminor=10,     $ 
                xtitle='wave length (A)',xticks=11-3,   $ 
                xtickname=strcompress(string((indgen(12-3)+3)*1000),    $ 
                                                        /remove_all), $  
                ystyle=1,ytitle='G-angle',yrange=[0,80.],yticks=8 
        for i=0,8 do oplot,[wl(0),wl(nn-1)],[i*10,i*10],linestyle=1 
        for i=3,11 do oplot,[i*1000.,i*1000.],[0,80],linestyle=1 
        for m=1,7 do begin 
                sth = m*wl/1.e7/2./d 
                ii=where(sth lt 1., count) 
		if count eq 0 then return
                th = asin(sth(ii)) 
                count = cfact*(th*180./!pi)+offset 
                disp1 = d*cos(th)/f1/m * 1e7 
                omeg = 2.*!pi*dd*sin(th-blaze*!pi/180.)/wl*1.e7 
                oplot,wl,th*180/!pi 
                xyouts,wl(0)+100,th(0)*180/!pi+3,       $ 
                        'm='+strcompress(string(m),/remove_all),/data 
                sth2 = m*wl/1.e7/2./d/cos(thg2/180.*!pi/2.) 
                ii=where(sth2 lt 1.) 
                th = asin(sth2(ii))-thg2/180.*!pi/2. 
                count = cfact*(th*180./!pi)+offset 
                disp2 = d*cos(th)/f2/m * 1e7 
                omeg = 2.*!pi*dd*sin(th-blaze*!pi/180.)/wl*1.e7 
                oplot,wl,th*180/!pi,line=2 
        endfor 
end 
 
;************************************************************************ 
pro spdisp,p,wl0,spg,m0 
;------------------------------------------------------------------------ 
;  plot overlapping spectra 
 
	loadct,0
	stretch,255,0
        d=1./p.n_groove         ; space mm 
        dd = d*cos(p.blaze*!pi/180.) 
        print,spg 
        if spg eq 'G1' then begin 
                dth=0.  
                dfact=p.f1*p.magni1 
        endif else begin 
                dth=p.thg2/180.*!pi 
                dfact=p.f2*p.magni2 
        endelse 
        ttt=m0*wl0/1.e7/2./d/cos(dth/2.) 
        if ttt gt 1. then begin 
                print,'Setting is not adequet!' 
                return 
        endif 
        th0=asin(ttt)-dth/2. 
        Gcount = p.cfact*(th0*180./!pi)+p.offset 
        disp = d*cos(th0+dth)/m0/dfact * 1e7    ; A/mm 
        print,'th0=',th0/!pi*180.,'  GC=',Gcount 
 
        nx1=512 &       ny=30 
        nx1=600 &       ny=30 
        dx=(p.g1x(1)-p.g1x(0))/nx1      ; x-step (mm) 
        g1x=findgen(nx1)*dx+p.g1x(0) 
        dth1=atan(g1x/p.f1) 
        mwl1=d*(sin(th0)+sin(th0-dth1))*1.e7 
 
        x0=40 &         y0=30 &         dy=30 
	chsize=1.2

        ;window,xsize=850,ysize=(ny+dy)*7+150 
        erase 
        for m=1,7 do begin 
                wl1=mwl1/m 
                ii=where(wl1 gt 2960. and wl1 lt 12000., count) 
                if count ne 0 then begin 
                        wlmin=min(wl1(ii)) &    wlmax=max(wl1(ii)) 
                        sp1=atlas(wlmin,wlmax,0.05,wl=wl) 
                        nsp=n_elements(sp1) 
                        if wlmin gt min(wl1) then begin 
                                ndmy=nsp*(wlmin-min(wl1))/(wlmax-wlmin) 
                                sp1=[fltarr(ndmy),sp1] 
                        endif 
                        if wlmax lt max(wl1) then begin 
                                ndmy=nsp*(max(wl1)-wlmax)/(wlmax-wlmin) 
                                sp1=[sp1,fltarr(ndmy)] 
                        endif 
                        sp1=sp1(sort(-indgen(n_elements(sp1)))) 
                        sp=rebin(sp1,n_elements(sp1),2) 
                        sp=congrid(sp,nx1,ny) 
                        tvscl,sp,x0,y0 
                        plot,wl1,sp(*,0),/nodata,/noerase, $ 
                                xstyle=1+8,xrange=[max(wl1),min(wl1)], $ 
                                xticklen=-0.04,ystyle=1+4,      $ 
                                pos=[x0,y0,x0+nx1,y0+dy],/dev, $
				charsize=0.7
                endif 
                xyouts,20,y0+10,string(m,format='(i1)'),/dev,charsize=chsize
                y0=y0+dy+ny 
        endfor 
        xyouts,x0+nx1/2-40,y0,'-- G1 --',/dev,charsize=chsize
        xyouts,20,y0,'m',/dev,charsize=chsize 
        wx=p.g1x(1)-p.g1x(0) 
        x0ccd1=nx1/wx*(0.-p.g1x(0)) 
        wccd1=nx1/wx*p.ccdx1/p.magni1 
        draw,x0+[x0ccd1-wccd1/2,x0ccd1-wccd1/2],[30,y0-dy+10],line=1 
        draw,x0+[x0ccd1+wccd1/2,x0ccd1+wccd1/2],[30,y0-dy+10],line=1 
 
        nx2=128 &       ny=30
        dx=p.ccdx2/float(nx2)  ; x-step (mm) 
        g2x=findgen(nx2)*dx-p.ccdx2/2. 
        dth2=atan(g2x/p.f2) 
        mwl2=d*(sin(th0)+sin(th0+p.thg2/180*!pi+dth2))*1.e7 
        x02=x0+nx1+30 &         y0=30 
        for m=1,7 do begin 
                wl2=mwl2/m 
                ii=where(wl2 gt 2960. and wl2 lt 12000., count) 
                if count ne 0 then begin 
                        wlmin=min(wl2(ii)) &    wlmax=max(wl2(ii)) 
                        sp2=atlas(wlmin,wlmax,0.05,wl=wl) 
                        nsp=n_elements(sp2) 
                        if wlmin gt min(wl2) then begin 
                                ndmy=nsp*(wlmin-min(wl2))/(wlmax-wlmin) 
                                sp2=[fltarr(ndmy),sp2] 
                        endif 
                        if wlmax lt max(wl2) then begin 
                                ndmy=nsp*(max(wl2)-wlmax)/(wlmax-wlmin) 
                                sp2=[sp2,fltarr(ndmy)] 
                        endif 
                        sp2=sp2(sort(-indgen(n_elements(sp2)))) 
                        sp=rebin(sp2,n_elements(sp2),2) 
                        sp=congrid(sp,nx2,ny) 
                        tvscl,sp,x02,y0 
                        plot,wl2,sp(*,0),/nodata,/noerase, $ 
                                xstyle=1+8,xrange=[max(wl2),min(wl2)], $ 
                                xticklen=-0.04,ystyle=1+4,      $ 
                                pos=[x02,y0,x02+nx2,y0+dy],/dev, $
				charsize=0.7
                endif 
                y0=y0+dy+ny 
        endfor 
        xyouts,x02+nx2/2-40,y0,'-- G2 --',/dev,charsize=chsize 
 
        title1='25-cm Coronagraph: '+string(wl0,format='(f7.1)')+' A on '+spg $ 
                +', m='+string(m0,format='(i1)') 
        title2='G.A.='+string(th0/!pi*180.,format='(f6.2)')+' deg.  G.C.=' $ 
                +string(Gcount,format='(f7.3)') $ 
                +' (offset='+string(p.offset,format='(f6.3)')+'),' $ 
                +'  Disp.='+string(disp,format='(f6.3)')+' A/mm ' 
        xyouts,x0,y0+60,title1,charsize=chsize,/dev 
        xyouts,x0,y0+35,title2,charsize=chsize,/dev 
 
end 
 

;************************************************************************ 
function imenu,menustr
;------------------------------------------------------------------------ 
nn=n_elements(menustr)
for i=0,nn-1 do begin
	print,i,'  ',menustr(i)
endfor
i=0
read,'   enter No. :',i
return,i

end

;************************************************************************ 
pro cnfdraw,p,wl0,spg,m0,wlb 
;------------------------------------------------------------------------ 
; draw directions of the 2nd line
	wx=!d.x_size &	wy=!d.y_size
	scale=wx*0.8/p.f1
	wgrat=400.*scale	; grating length (mm)
	wcoll=750.*scale	; collimator size
	dxgf=500.*scale		; distance btwn grating and sp.image
	dgl2=2560.*scale	; distance btwn grating and G2 lens
	wl2=280.*scale		; size of G2 lens
	lbase=385.5		; length of base line (cm), 97/08/26
	f1=p.f1*scale
	f2=p.f2*scale
	rad=!pi/180.

        d=1./p.n_groove         ; space mm 
        dd = d*cos(p.blaze*!pi/180.) 
        if spg eq 'G1' then begin 
                dth=0.  
        endif else begin 
                dth=p.thg2*rad
        endelse 
        ttt=m0*wl0/1.e7/2./d/cos(dth/2.) 
        if ttt gt 1. then begin 
                print,'Setting is not adequet!' 
                return 
        endif 
        th0=asin(ttt)-dth/2. 
        Gcount = p.cfact*(th0/rad)+p.offset 
        print,'th0=',th0/rad,'  GC=',Gcount 

	erase
	x0=wx-(dxgf+700.*scale)	&	y0=wy/2	; grating pos.
	;-- grating
	x=x0+wgrat/2.*sin(th0)*[1.,-1.]
	y=y0+wgrat/2.*cos(th0)*[1.,-1.]
	draw,x,y,thick=2
	;-- collimator
	amax=wcoll/2./f1/2.
	a=-amax+findgen(51)*amax/25.
	xc=x0-(f1-dxgf)
	yc=y0
	x=xc+2.*f1*(1.-cos(a))
	y=yc+2.*f1*sin(a)
	draw,x,y,thick=2
	;-- G2 lens
	xl2=x0-dgl2*cos(p.thg2*rad)
	yl2=y0-dgl2*sin(p.thg2*rad)
	x=xl2+wl2/2.*sin(p.thg2*rad)*[-1.,1.]
	y=yl2+wl2/2.*cos(p.thg2*rad)*[1.,-1.]
	draw,x,y,thick=2
	;-- rays
	xg2=x0+(xl2-x0)*(dgl2+f2)/dgl2
	yg2=y0+(yl2-y0)*(dgl2+f2)/dgl2
	draw,[x0+dxgf,xc],[y0,yc]
	draw,[x0,xg2],[y0,yg2]
	rem1=string(wl0,format='(f7.1)') $
			+'  m='+string(m0,format='(i1)')
	case spg of
	  'G1': begin
		arrow,xc,yc,xc+f1,yc
		xyouts,xc+f1-50,yc-20,rem1,/dev,charsize=1.2
		end
	  'G2': begin
		arrow,x0,y0,xg2,yg2
		xyouts,xg2-30,yg2+5,rem1,/dev,charsize=1.2
		end
	endcase
	tvlct,0,255,0,200 
	for m=0,7 do begin
	    as=(m*wlb/1.e7-d*sin(th0))/d
	    ;if as lt sin(!pi/2-th0) and as gt -1. then begin
	    if as lt 1. and as gt -1. then begin
		thb=-asin(as)+th0
		print,m,thb/rad,' deg.  ',lbase*tan(thb),' cm'
		xb=x0-cos(thb)*(f2+dgl2)
		yb=y0+sin(thb)*(f2+dgl2)
		arrow,x0,y0,xb,yb,color=200
		rem2='m='+string(m,format='(i1)')
		xyouts,xb-10,yb-20,rem2,/dev,charsize=1.2,color=200
	    endif
	endfor

        title2='G.A.='+string(th0/rad,format='(f6.2)')+' deg.  G.C.=' $ 
                +string(Gcount,format='(f7.3)') $
		+'  wl2='+string(wlb,format='(f7.1)')
	xyouts,50,wy-50,title2,/dev,charsize=1.5
	xyouts,0,0,'       ',/dev
end

;************************************************************************ 
function line_pdm,base,lines=mstr
;------------------------------------------------------------------------ 
 
l_cor=['-- Corona --',  $ 
        ' FeXIV  5302.9',       $ 
        ' CaXV   5694.5',               $ 
        ' FeX    6374.5',               $ 
        ' FeXI   7891.9',               $ 
        ' FeXIII 10746.8',      $ 
        ' FeXIII 10797.9'] 
l_mag=['-- Magnetic --',        $ 
        ' FeI    5250.2',       $ 
        ' FeI    6302.5',       $ 
        ' FeI    6337.'         $ 
        ] 
l_chr=['-- Chromosphere --',    $ 
        ' CaII   3933.7  (K)',  $ 
        ' CaII   3968.5  (H)',  $ 
        ' HI     4861.3  (Hb)', $ 
        ' HeI    5876.   (D3)', $ 
        ' HI     6562.8  (Ha)', $ 
        ' HeI    10830.'] 
         
mstr=['Line Table',l_cor,l_chr,l_mag,'-- cancel'] 
ns=n_elements(mstr)
flags=intarr(ns)
flags(0)=1
flags(1)=1 
flags(n_elements(l_cor))=2
flags(1+n_elements(l_cor))=1
flags(n_elements([l_cor,l_chr]))=2
flags(1+n_elements([l_cor,l_chr]))=1
flags(n_elements([l_cor,l_chr,l_mag]))=2
flags(ns-1)=2
desc=replicate({pdst, flags:0, name:''},ns)
desc.name=mstr
desc.flags=flags
wdid = cw_pdmenu(base,desc,return_name=0,uvalue='Cw_pdmenu')

return,wdid
end 
 
;************************************************************************ 
; g1g2_event 
pro g1g2_event, ev 
 
common spparm, p, wl0, spg, m0, wlb 
common widget, spwd, lines
 
  widget_control, ev.id, get_uvalue=value 
  if (n_elements(value) eq 0) then value = '' 
  name = strmid(tag_names(ev, /structure_name), 7, 1000) 
  skip_plot=0 

form1="('wl=',f7.1,'  ',a2,'  m=',i1,'  offset=',f6.3)"
case ev.id of 
spwd.g1g2: begin
    case ev.value of
	0: spg='G1'
	1: spg='G2'
    endcase
    print,wl0,spg,m0,p.offset,format= form1
    end
spwd.order: begin
    case ev.value of
	0: m0=1
	1: m0=2
	2: m0=3
	3: m0=4
	4: m0=5
	5: m0=6
    endcase
    print,wl0,spg,m0,p.offset,format= form1
    end
spwd.line_pdm: begin
	line=lines(ev.value)
	print,line
	if strmid(line,0,1) ne '-' then begin
	        wl0=float(strmid(lines(ev.value),8,6)) 
	        widget_control, spwd.slider, set_value=wl0 
	endif
    end
else: begin
  case (strmid(name,0,4)) of 
  "BUTT": begin 
        case value of 
        "DONE":  begin 
          WIDGET_CONTROL, /destroy, ev.top 
          return 
          end 
        "GifSave":  begin 
          win2gif,'c:\tmp\sp.gif'
	  print,'window saved to  c:\tmp\sp.gif'
          return 
          end 
        "WLGAPLOT": wlgaplot,p 
        "PPrint": begin 
          set_plot,'ps' 
          device,/landscape,xoffset=3,yoffset=26,ysize=15 
          wlgaplot,p 
          device,/close 
	  print,'fig saved in idl.ps..'
          ;spawn,'lpr -h idl.ps' 
          set_plot,'win' 
          end 
        "SHOW": spdisp,p,wl0,spg,m0 
        "OFFSET": begin 
          setoff 
          print,'Offset is set to ',p.offset 
          end 
	"CONF": cnfdraw,p,wl0,spg,m0,wlb
        "GOGO":  
        else : begin 
          if (ev.select eq 0) then begin 
            value = value + ' (released)' 
          endif 
          end 
        endcase 
	end
  "DRAW": begin 
        value = string(format="(A,' X(', I0, ')', ' Y(', I0, ')', ' Press('," $ 
                + "I0, ')', ' Release(', I0, ')')", value, ev.x, ev.y, $ 
                ev.press, ev.release) 
        print,ev.x,ev.y 
        end 
  "SLID": begin 
        wl0 = float(ev.value) 
        end 
  "TEXT": begin 
	case value of
	"WL0": begin
	   widget_control, ev.id, get_value=value, set_value='' 
	   wl0 = float(value(0)) 
	   widget_control, spwd.slider, set_value=fix(wl0) 
	   end
	"WLb": begin
	   widget_control, ev.id, get_value=value, set_value='' 
	   wlb = float(value(0)) 
	   widget_control, ev.id, set_value=string(wlb) 
	   end
	endcase
        end 
  "LIST": value=value(ev.index) 
  endcase 
  end
endcase

end 
 
 
;************************************************************************ 
pro xinput_event, ev 
common xinput, str 
common spparm, p, wl0, spg, m0, wlb 
 
  widget_control, ev.id, get_uvalue=value 
  if (n_elements(value) eq 0) then value = '' 
  name = strmid(tag_names(ev, /structure_name), 7, 1000) 
  skip_plot=0 
  case (name) of 
  "TEXT": begin 
        widget_control, ev.id, get_value=value, set_value='' 
        str = value(0) 
        WIDGET_CONTROL, /destroy, ev.top 
        end 
  endcase 
 
end 
;************************************************************************ 
pro xinput,prompt,outstr,title=title 
common xinput, str 
        base = WIDGET_BASE(title=title, /column) ; base window 
        b0 = WIDGET_BASE(base, /frame, /row) 
        t0 = widget_label(b0,value=prompt) 
        inputwc = widget_text(b0, /editable, xsize=10, ysize=1) 
        widget_control, base, /realize 
        XMANAGER, 'xinput', base, /MODAL 
        outstr=str 
end 
 
;************************************************************************ 
pro setoff      ; set G counter offset 
common spparm, p, wl0, spg, m0, wlb 
 
;i=wmenu(['Set Offset','Enter G-Count','Enter Offset',   $ 
;        'Reset Offset','Show Offset','Cancel'],$ 
;        title=0) 
i=smenu(['Set Offset','Enter G-Count','Enter Offset',   $ 
        'Reset Offset','Show Offset','Cancel']) 
 
title='Set Counter Offset' 
 
if i eq 1 then begin 
        print,wl0,spg,fix(m0),format="('wl=',f7.1,'  ',a2,'  m=',i1)" 
        d=1./p.n_groove         ; space mm 
        dd = d*cos(p.blaze*!pi/180.) 
        if spg eq 'G1' then dth=0. else dth=p.thg2/180.*!pi 
        ttt=m0*wl0/1.e7/2./d/cos(dth/2.) 
        if ttt gt 1. then begin 
                print,'Setting is not adequet!' 
                return 
        endif 
        th0=asin(ttt)-dth/2. 
        Gcount0 = p.cfact*(th0*180./!pi) 
        print,'Ideal count=',Gcount0 
        xinput,'Enter G-Count:',str,title=title 
        Gcount=float(str) 
        p.offset=Gcount-Gcount0 
endif 
if i eq 2 then begin 
        xinput,'Enter Offset:',str,title=title 
        p.offset=float(str) 
endif 
if i eq 3 then begin 
        p.offset=0. 
endif 
if i eq 4 then begin 
        print,'Current Offset=',p.offset 
endif 
 
end 
 
;************************************************************************ 
pro g1g2	; to avoid inconsistency with obsa.pro

common spparm, p, wl0, spg, m0, wlb 
common widget, spwd, lines
;logfile='~/g1g2.log' 			
logfile='c:\nkrprj\log\g1g2.log' 
xsize=850 &	ysize=550
;xsize=750 &	ysize=450

p=spconst() 
wl0=5303. &  m0=1 &  offset=0. &   spg='G1' &	wlb=7891.9
ff=findfile(logfile)
if ff(0) ne '' then restore,file=logfile 

spwd={sp_widget,	$
	slider:		0l,	$
	g1g2:		0l,	$
	line_pdm:	0l,	$
	order:		0l	$
	}
if wl0 lt 3500. then wl0=3500. 
p.offset=offset 
print,wl0,spg,m0,offset,        $ 
        format="('Log=> wl=',f7.1,'  ',a2,'  m=',i1,'  offset=',f6.3)" 

;-------------  widget  -------------------------------------------------- 
base = WIDGET_BASE(title='25-cm Spectrograph', /column)         ; base window 
b0 = WIDGET_BASE(base, /frame, /row) 
draw = widget_draw(b0, xsize=xsize, ysize=ysize, /button_events) 
b1 = WIDGET_BASE(base, /frame, /row) 
t0 = widget_button(b1, value="Draw wl vs. G-angle", uvalue = "WLGAPLOT") 
t01 = widget_button(b1, value="Printout", uvalue = "PPrint") 
 
b2 = WIDGET_BASE(base, /frame, /row, space=5) 
b21= WIDGET_BASE(b2, /column) 
b211= WIDGET_BASE(b21, /row) 
sel=['G1','G2'] 
;xmenu,sel,b211,/exclusive,/row,/frame,uvalue=sel,buttons=buttons 
if spg eq 'G1' then val=0 else val=1
spwd.g1g2 = cw_bgroup(b211,sel,/row, uvalue=sel, $
	/no_release,set_value=val,/exclusive, /frame)
order=['1','2','3','4','5','6'] 
;xmenu,order,b21,/exclusive,/row,/frame,uvalue=order,    $ 
;        title='m',buttons=buttons
i=where(m0 eq fix(order))
spwd.order = cw_bgroup(b21,order,/row, uvalue=sel, $
	/no_release,set_value=i(0),/exclusive, /frame)
b22= WIDGET_BASE(b2, /frame, /column) 
b221= WIDGET_BASE(b22, /frame, /row) 
t1 = widget_label(b221, value='Wavelength (A) :') 
inputwc = widget_text(b221, /editable, xsize=10, ysize=1, uvalue='WL0') 
;t12 = widget_button(b221, value="Table", uvalue = "LINETBL") 
;xpdmenu,['/Table/ {', '/-- Corona --/','/-- Magnetic --/'],b221 
spwd.line_pdm = line_pdm(b221,lines=lines)
spwd.slider = widget_slider(b22, minimum=3500.,maximum=16000., $ 
                xsize=150,value=wl0,scroll=20/20.) 
b23= WIDGET_BASE(b2, /column) 
t2 = widget_button(b23, value="Show Spectra", uvalue = "SHOW") 
t3 = widget_button(b23, value="Offset Setting", uvalue = "OFFSET") 
b24= WIDGET_BASE(b2, /column, /frame) 
t21 = widget_label(b24, value='2nd line') 
inputw2 = widget_text(b24, /editable, xsize=10, ysize=1, uvalue='WLb', $
	value=string(wlb)) 
t22 = widget_button(b24, value="Conf.", uvalue = "CONF") 
t5 = widget_button(b2, value="Go", uvalue = "GOGO") 
t6 = widget_button(b2, value="Gif", uvalue = "GifSave") 
t4 = widget_button(b2, value="Done", uvalue = "DONE") 
 
 
widget_control, base, /realize 
 
  WIDGET_CONTROL, get_value=window, draw 
;  WIDGET_CONTROL, buttons(4), set_button=1      ; unselect menu button '0' 
;  WIDGET_CONTROL, buttons(0), set_button=0      ; select menu button '20' 
  old_window = !d.window        ;Previous window 
  wset, window 
 
XMANAGER, 'g1g2', base ;, /MODAL 
 
offset=p.offset 
save,wl0,m0,spg,offset,wlb,filename=logfile 
 
end 

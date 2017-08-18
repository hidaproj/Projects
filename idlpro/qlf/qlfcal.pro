; LUCEO quartz Lyot filter
;*****************************************************
pro comcom,com
;-----------------------------------------------------
; send command string to COMx

common qlflib,dllfile,PNs,Vss,Vms,Acs,Unknown,Dev_exist
retn = call_external(dllfile,'CommWrite',com)

end


;*****************************************************
common qlflib,dllfile,PNs,Vss,Vms,Acs,Unknown,Dev_exist
common hr2000_com, wd, p, sp1

@hr2000lib

cd,'C:\Projects\IDLPRO\qlf'

;init hr2000
hr_init,spname

wl=hr_getwl()
nwl=n_elements(wl)
dwl=0.5
mwl=dwl*round(minmax(wl)/dwl)
nwl2=(mwl[1]-mwl[0])/dwl+1
wl2=mwl[0]+dwl*findgen(nwl2)
ip=interpol(findgen(nwl),wl,wl2)

;exposure setting
expo=1	;[ms]
hr_setexpo,expo

wdef,0,800,600
!p.color=0
!p.background=255

goto,jump

;dark obs.
integ=100
ans=dialog_message('observing dark. turn off the right')

dark=fltarr(nwl)
for i=0,integ-1 do dark+=hr_getsp1()
dark/=integ

plot,wl,dark,title='dark',xtit='nm',yra=[0,2l^16-1],xs=3,ys=3,psy=3
write_gif,'dark.gif',tvrd()
save,file='dark.sav',wl,dark

;clear obs.
integ=100
ans=dialog_message('observing clear. turn on the right and remove the filter from the light path')

clear=fltarr(nwl)
for i=0,integ-1 do clear+=hr_getsp1()/integ

plot,wl,clear,title='clear',xtit='nm',yra=[0,2l^16-1],xs=3,ys=3,psy=3
write_gif,'clear.gif',tvrd()
save,file='clear.sav',wl,clear

jump:
restore,'dark.sav'
restore,'clear.sav'
clear-=dark

;filter obs.
;init the filter

dpb=360/9000.                   ;[degree/bit]
ddeg=1.                         ;[degree]
dbit=round(ddeg/dpb)            ;[bit]
ndeg=round(360/ddeg)

wt=0.5                          ;wait [sec]

dllfile='C:\Projects\cprog\VS2010\RS232C_64\x64\Debug\RS232C_64.dll'

COMx='COM4'
ret=call_external(dllfile,'CommOpen', COMx, $
                  ' baud=19200 parity=N data=8 stop=1', /PORTABLE )

nb=5
integ=10
thri=1e3
clear[where(clear lt thri)]=!values.F_NAN

;to home position
for k=0,nb-1 do begin
   comcom,"M"+string(k+1,form='(i1)')+"O\r\n"
   wait,wt
endfor

ans=dialog_message('observation start. OK?')

for k=0,nb-1 do begin
   b=string(k+1,form='(i1)')
   
   spec=fltarr(nwl,ndeg)
   ;home position
   comcom,"M"+b+"O\r\n"
   
   for j=0,ndeg-1 do begin
      print,k,j
      for i=0,integ-1 do spec[*,j]+=hr_getsp1()/integ
      plot,wl,spec[*,j]-dark,title='block'+b+' '+string(j+ddeg,form='(i3)')+ $
           ' deg',xtit='nm',yra=[1,2l^16-1], $
           xs=3,ys=3,psy=3,/ylog
      write_gif,'block'+b+'_1.gif',tvrd(),/multi
      
      comcom,"M"+b+"+"+string(dbit,form='(i6.6)')+"\r\n"
      wait,wt
   endfor
   write_gif,'block'+b+'_1.gif',/close
   
   cspec=(spec-rebin(dark,nwl,ndeg))/rebin(clear,nwl,ndeg)
   maxspec=max(spec-rebin(dark,nwl,ndeg),dim=2)
   thri2=median(maxspec[0:399])
   maxspec[where(maxspec lt 2*thri2)]=!values.F_NAN
   maxspec2=maxspec/clear
   cspec2=cspec/rebin(maxspec2,nwl,ndeg)
   
   cspec3=interpolate(cspec2,ip,indgen(ndeg),/grid)
  
   save,file='block'+b+'_1.sav',wl,spec,wl2,cspec3,ddeg
   
endfor

retn=call_external(dllfile,'CommClose')

hr_close

end

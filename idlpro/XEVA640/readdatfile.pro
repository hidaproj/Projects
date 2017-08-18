pro readdatfile,filename,nx,nl,nfr,ngrab,data,time,nodata=nodata,tv=tv, $
  byte=byte,long=long,tap=tap,dalsa1m30=dalsa1m30,pixofftap=pixofftap, $
  modpattern=modpattern

if keyword_set(nodata) then data=uintarr(nx,nl) $
  else data=uintarr(nx,nl,nfr*ngrab)
buf=uintarr(nx,nl)
if keyword_set(byte) then begin
  if keyword_set(nodata) then data=bytarr(nx,nl) $
    else data=bytarr(nx,nl,nfr*ngrab)
  buf=bytarr(nx,nl)
endif
if keyword_set(long) then begin
  if keyword_set(nodata) then data=lonarr(nx,nl) $
    else data=lonarr(nx,nl,nfr*ngrab)
  buf=lonarr(nx,nl)
endif


frmno=1L
frmtime=intarr(8)
time=intarr(8,ngrab)


openr,1,filename

for igrab=0,ngrab-1 do begin
  readu,1,frmno
  if !version.os eq 'sunos' then byteorder,frmno,/lswap
  readu,1,frmtime
  if !version.os eq 'sunos' then byteorder,frmtime
  print,frmno,frmtime
  for ifr=0,nfr-1 do begin
    readu,1,buf
    if not keyword_set(byte) then $
      if !version.os eq 'sunos' then byteorder,buf
    if keyword_set(dalsa1m30) then buf=buf/16
    if keyword_set(modpattern) then buf=buf mod modpattern
    if keyword_set(tv) then tvscl,reverse(buf,2)
    if keyword_set(nodata) then data=buf $
      else data(*,*,ifr+igrab*nfr)=buf(*,*)
  endfor
  time(*,igrab)=frmtime
endfor

close,1

if keyword_set(tap) then begin
  tapbuf=data
  data(0:(nx/4-1),*,*)=tapbuf(indgen(nx/4)*4,*,*)
  data((nx/4):(nx/4*2-1),*,*)=tapbuf(indgen(nx/4)*4+1,*,*)
  data((nx/4*2):(nx/4*3-1),*,*)=tapbuf(indgen(nx/4)*4+2,*,*)
  data((nx/4*3):(nx-1),*,*)=tapbuf(indgen(nx/4)*4+3,*,*)
endif

if keyword_set(pixofftap) then begin
  tapbuf2=bytarr(long(nx)*long(nl)+4)
  tapbuf=bytarr(nx,nl)
  for igrab=0,ngrab-1 do begin
    for ifr=0,nfr-1 do begin
      tapbuf2(0:long(nx)*long(nl)-1)=data(*,*,igrab*nfr+ifr)
      tapbuf(*,*)=tapbuf2(4:long(nx)*long(nl)+3)
      data(0:(nx/4-1),*,igrab*nfr+ifr)=tapbuf(indgen(nx/4)*4,*)
      data((nx/4):(nx/4*2-1),*,igrab*nfr+ifr)=tapbuf(indgen(nx/4)*4+1,*)
      data((nx/4*2):(nx/4*3-1),*,igrab*nfr+ifr)=tapbuf(indgen(nx/4)*4+2,*)
      data((nx/4*3):(nx-1),*,igrab*nfr+ifr)=tapbuf(indgen(nx/4)*4+3,*)
    endfor
  endfor
endif

end

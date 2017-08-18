; pro_test64.pro

prodll='C:\Projects\cprog\VS2010\prosilica_64\x64\Debug\prosilica_64.dll'

err=call_external(prodll,'CamInit',/all_value,/cdecl)

err=call_external(prodll,'SetParam',    $
                            1000l, $	; wp.expo,      $
                            10l, $	; wp.framerate, $
                            0l, $	; wp.gain,      $
                            1l, $	; wp.binx,      $
                            1l, $	; wp.biny,      $
                            1600l, $	; wp.Width,     $
                            1200l, $	; wp.Height,    $
                            0l, $	; wp.RegionX,   $
                            0l, $	; wp.RegionY,   $
                            /all_value,/cdecl)


r=call_external(prodll,'GrabImg',1,/all_value,/cdecl)

r=call_external(prodll,'DivBuf',0,/all_value,/cdecl)
img=intarr(1600,1200)
r=call_external(prodll,'GoIdl',img,/cdecl)
img1=rebin(img,800,600)
window,xs=800,ys=600
tvscl,img1



stop

err=call_external(prodll,'CamFin',/all_value,/cdecl)

end


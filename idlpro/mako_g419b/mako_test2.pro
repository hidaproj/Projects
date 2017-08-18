.r makolib

p=mako_init()

set_parameters,p

img=uintarr(p.NAXIS1,p.NAXIS2)  ;buffer

imgs=mako_obs(p,img,header=header)


mako_startstream,p

tic
tvscl,mako_getimg(p,img,header=header)
toc

mako_stopstream




r=call_external(dllfile,'GrabImg',nimg,/all_value,/cdecl)



mako_fin


; 
@emplib

pm=emp_ctl()
pm.m1.name='M1: P-Analyzer'
pm.m2.name='M2: P-Generator'
pm.m1.vm = 54000l
pm.m2.vm = pm.m1.vm/5
pm.dev_exist=1

empinit,'COM8';,p=pm

stop
empset,1,vm=pm.m1.vm
wait,0.05
empstart,1,/CW
wait,0.1
empset,2,vm=pm.m2.vm
wait,0.05
empstart,2,/CW

stop

empclose


end

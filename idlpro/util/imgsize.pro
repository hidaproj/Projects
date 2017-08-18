; imgsize
pro imgsize,img,nx,ny,nf,n4,n5,dim=dim

;	99/01/06	k.i. dim keyword
;	05/06/15	k.i. dim=5

s=size(img)
dim=s(0)	; dimension of array
case dim of
    0: begin
	nx=1 &		ny=1 &		nf=1 	&	n4=1
	end
    1: begin
	nx=s(1) &	ny=1 &		nf=1	&	n4=1
	end
    2: begin
	nx=s(1) &	ny=s(2) &	nf=1 	&	n4=1
	end
    3: begin
	nx=s(1) &	ny=s(2) &	nf=s(3) &	n4=1
	end
    4: begin
	nx=s(1) &	ny=s(2) &	nf=s(3) &	n4=s(4)
	end
    5: begin
	nx=s(1) &	ny=s(2) &	nf=s(3) &	n4=s(4) &	n5=s(5)
	end
endcase

end

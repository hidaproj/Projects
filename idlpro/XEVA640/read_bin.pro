FUNCTION read_bin,file=file

buf=UINTARR(640,512)

OPENR,1,file
READU,1,buf

CLOSE,1

data=FLOAT(buf)

RETURN,data

END

;+
; Project     : HESSI
;
; Name        : HTTP__DEFINE
;
; Purpose     : Define a HTTP class
;
; Explanation : defines a HTTP class to open URL's and download (GET)
;               files. Example:
;
;               a=obj_new('http')                  ; create a HTTP object
;               a->open,'orpheus.nascom.nasa.gov'  ; open a URL socket 
;               a->head,'~zarro/dmz.html'          ; view file info
;               a->list,'~zarro/dmz.html'          ; list text file from server
;               a->copy,'~zarro/dmz.html'          ; copy file
;               a->close                           ; close socket
;
;               If using a proxy server, then set environmental 
;               http_proxy, e.g:
;
;               setenv,'http_proxy=orpheus.nascom.nasa.gov:8080'
;                
;               or
;
;               a->hset,'http_proxy='orpheus.nascom.nasa.gov:8080'
;
; Category    : objects sockets 
;               
; Syntax      : IDL> a=obj_new('http')
;
; History     : Written, 6 June 2001, D. Zarro (EITI/GSFC)
;               Modified, 31 December 2002, Zarro (EER/GSFC) - made PORT
;               a property
;               Modified, 5 January 2003, Zarro (EER/GSFC) - improved
;               proxy support
;               13-Jan-2002, Zarro (EER/GSFC) - added cache support
;               and /reset
;               26-Jan-2003, Zarro (EER/GSFC) - added URL path check
;               4-May-2003, Zarro (EER/GSFC) - added POST support & switched
;               default to HTTP/1.1
;               20-Sep-2003, Zarro (GSI/GSFC) - added RANGE and CACHE-CONTROL directives
;               30-Sep-2003, Zarro (GSI/GSFC) - added READ_TIMEOUT property to control
;               proxy timeouts
;               28-Jun-2004, Kim Tolbert - set self.unit=0 after closing to avoid conflicts
;               15-Apr-2005, Zarro (L-3Com/GSFC) - added call to HTML_DECHUNK
;               20-Apr-2005, Zarro (L-3Com/GSFC) - allowed PORT to be set from URL
;               11-Sep-2005, Zarro (L-3Com/GSFC) - added COPY_FILE keyword to ::COPY
;               10-Nov-2005, Hourcle (L-3Com/GSFC) - added support for adding headers
;               11-Nov-2005, Zarro (L-3Com/GSFC) - changed from using pointer 
;                to keyword for extra header
;               1-Dec-2005, Zarro (L-3Com/GSFC) - minor bug fixes with
;                closing and validating servers
;               16-Dec-2005, Zarro (L-3Com/GSFC) - fixed case where
;                socket was not closing after a request
;               26-Dec-2005, Zarro (L-3Com/GSFC) - added more
;                diagnostic output to /VERBOSE
;
; Contact     : dzarro@solar.stanford.edu
;-

;-- init HTTP socket

function http::init,err=err,_ref_extra=extra

err=''

if not allow_sockets(err=err,_extra=extra) then return,0b

add_method,'gen',self

self->hset,retry=2,buffsize=1024l,connect_timeout=1,protocol='1.1',$
      port=80,_extra=extra,$
      user_agent='IDL '+!version.release+' on '+!version.os+'/'+!version.arch

return,1

end
;--------------------------------------------------------------------------

pro http::cleanup

self->close

return & end

;--------------------------------------------------------------------------

function http::hget,_ref_extra=extra

return,self->getprop(_extra=extra)

end

;---------------------------------------------------------------------------
;-- send request for specified type [def='text']

pro http::request,url,type=type,err=err,cgi_bin=cgi_bin,request=request,$
                      no_head=no_head,_ref_extra=extra,verbose=verbose

err=''
request=''
this_type='text/plain'

;-- don't send a HEAD request if a CGI_BIN or a filename is not specified in the
;   URL 

get_type=(1b-keyword_set(cgi_bin)) and (1b-keyword_set(no_head)) 

verbose=keyword_set(verbose)
if verbose then message,'Requesting '+url,/cont

if get_type then begin
 this_type=self->get_url_type(url,err=err,size=bsize,verbose=verbose)
 if is_string(err) then return
endif 

if is_blank(type) then type='text'

if type ne '*' then begin
 chk=stregex(this_type,type)
 if chk[0] eq -1 then begin
  err='Remote file is not of type = '+type
  message,err,/cont
  return
 endif
endif

self->open,url,file=file,err=err

if is_string(err) then begin
 self->close & return
endif

self->make,file,request,_extra=extra
self->send,request,err=err

return & end

;--------------------------------------------------------------------------
;-- create request string

pro http::make,file,request,head=head,$
           no_close=no_close,post=post,form=form,$
           encode=encode,xml=xml,range=range,info=info

arg=strtrim(file,2)
server=self->hget(/server)

have_delim=stregex(arg,'^/',/bool)
if not have_delim then arg='/'+arg
if self->use_proxy() or self->hget(/gateway) then arg='http://'+server+arg

;-- assume GET request

cmd='GET '
method='get'
if keyword_set(head) then begin cmd='HEAD ' & method='head' & endif
if is_string(post) then begin cmd='POST ' & method='post' & endif
cmd=cmd+arg

;-- if sending a request to read a file, then don't include protocol 
;   information or the server will send back extra header information

protocol=self->hget(/protocol)
cmd=cmd+' '+protocol

;-- send user-agent in header telling server that this is IDL calling

user_agent='User-Agent: '+self->hget(/user_agent)
cache_control='Cache-control: no-cache'

header=[cache_control,user_agent]

;-- append extra header info if included

if is_string(info) then header=[header,strarrcompress(info)]

;-- if using HTTP/1.1, need to send host/port information

port=self->hget(/port)
port=trim(port)

tserver=server
tport=port
if self->use_proxy() then begin
 tserver=self->hget(/proxy_host)
 tport=self->hget(/proxy_port)
endif 

host='Host: '+tserver+':'+trim(tport)
if stregex(protocol,'1.1',/bool) then header=[header,host]

;-- if this is a POST request, then we compute content length
;   if this is a FORM then inform server of content type

if method eq 'post' then begin
 if keyword_set(xml) then header=[header,'Content-type: text/xml'] else $
  if keyword_set(form) then header=[header,'Content-type: application/x-www-form-urlencoded']
 length=strlen(post)
 header=[header,'Content-length: '+strtrim(length,2)] 
endif 

;-- check if partial ranges requested

if (method eq 'get') and is_string(range) then header=[header,'Range: bytes= '+strtrim(range,2)]

;-- if using HTTP/1.0, and want to keep connection open, send keep-alive header

if stregex(protocol,'1.0',/bool) and keyword_set(no_close) then header=[header,'Connection: keep-alive']
if stregex(protocol,'1.1',/bool) and (1-keyword_set(no_close)) then header=[header,'Connection: Close']

request=[cmd,header,'']

if method eq 'post' then begin
 if keyword_set(encode) then request=[request,url_encode(post)] else $
  request=[request,post]
endif

;-- if this is a HEAD request, then we append extra space

if method eq 'head' then request=[request,'']

return
end

;---------------------------------------------------------------------------
;-- set method

pro http::hset,file=file,server=server,retry=retry,connect_timeout=connect_timeout,port=port,$
               buffsize=buffsize,protocol=protocol,user_agent=user_agent,$
               proxy=proxy,gateway=gateway,read_timeout=read_timeout,$
               rawio=rawio,secure=secure,cache=cache,swap_endian=swap_endian,$
               headers=headers

if is_number(connect_timeout) then self.connect_timeout=connect_timeout
if is_number(read_timeout) then self.read_timeout=read_timeout
if is_number(retry) then self.retry=abs(fix(retry))
if is_string(file) then self.file=file
if is_string(server) then begin
 self->parse_url,server,temp
 if is_string(temp) then self.server=temp
endif

if is_number(buffsize) then self.buffsize=abs(long(buffsize)) > 1l
if is_string(protocol) then self.protocol=' HTTP/'+trim(protocol)+' '
if is_string(user_agent) then self.user_agent=' '+user_agent+' '
if is_number(port) then self.port=fix(port)

if is_string(proxy,/blank) then begin
 if strtrim(proxy,2) ne ''  then begin
  self->parse_url,proxy,proxy_host,port=proxy_port
  if is_string(proxy_host) then self.proxy_host=proxy_host 
  if is_number(proxy_port) then self.proxy_port=proxy_port else self.proxy_port=80
 endif else begin
  self.proxy_host='' & self.proxy_port=0
 endelse
endif

if is_number(gateway) then self.gateway= 0b > gateway < 1b
if is_number(cache) then self.cache= 0b > cache < 1b
if is_number(rawio) then self.rawio= 0b > rawio < 1b
if is_number(swap_endian) then self.swap_endian= 0b > swap_endian < 1b
if is_number(secure) then self.secure= 0b > secure < 1b

if is_string(headers) then begin
    if ptr_valid(self.headers) then ptr_free,self.headers
    self.headers = ptr_new(headers)
endif

return & end

;---------------------------------------------------------------------------

pro http::help

message,'current server - '+self->hget(/server),/cont

if self->use_proxy() then begin
 message,'proxy server - '+self->hget(/proxy_host),/cont
 message,'proxy port - '+trim(self->hget(/proxy_port)),/cont
endif

if not self->is_socket_open() then begin
 message,'No socket open',/cont
 return
endif

;help,/st,fstat(self.unit)

return & end

;----------------------------------------------------------------------------
;-- check if proxy server being used

function http::use_proxy
 
proxy_host=self->hget(/proxy_host)
if is_string(proxy_host) then return,1b

http_proxy=chklog('http_proxy')
if is_blank(http_proxy) then begin
 self.proxy_host=''
 self.proxy_port=0
 return,0b
endif

self->parse_url,http_proxy,proxy_host,port=proxy_port
self.proxy_host=proxy_host
self.proxy_port=proxy_port
return,1b & end

;-----------------------------------------------------------------------
;-- list links

pro http::links,url,hrefs,sizes=sizes,err=err,_extra=extra,count=count

self->list,url,page,err=err,_extra=extra

if is_string(err) then return

;-- check if links for this url were cached

self->parse_href,page,hrefs,sizes=sizes,count=count

return & end

;-------------------------------------------------------------------------
;-- parse HREF links 

pro http::parse_href,input,hrefs,sizes=sizes,count=count

hrefs='' & sizes=0. & count=0l

if is_blank(input) then return

input=html_dechunk(input)

;regex='< *a *href *= *"?([^ "]+) *"? *>.+( +[0-9]+\.?[0-9]* *)[a-z]'

regex='< *a +href *= *"?([^ "]+) *"?.*>.+( +[0-9]+\.?[0-9]*[a-z]+ +)'

dprint,'% PARSE_HREF: parsing links...'
match=stregex(input,regex,/subex,/extra,/fold)

chk=where(match[1,*] ne '',count)
if count gt 0 then begin
 hrefs=reform(match[1,chk])
 sizes=reform(match[2,chk])
endif

return & end

;--------------------------------------------------------------------------
;-- open URL via HTTP 

pro http::open,url,file=file,server=server,err=err,gateway=gateway,_extra=extra

err=''
error=0

self->hset,gateway=gateway
self->parse_url,url,server,file,port=port
self->hset,port=port

;-- reopen last closed server if not entered

last_server=self->hget(/server)
if is_blank(server) and is_string(last_server) then server=last_server

if not self->valid_server(server) then begin
 if is_blank(server) then err='Missing remote server name' else $
  err='Invalid server name - '+server
 message,err,/cont
 return
endif

if self->is_open(server) then goto,skip

;-- default to port 80 (or URL-entered port) if not via proxy

tserver=server
tport=self->hget(/port)
if self->use_proxy() then begin
 tserver=self->hget(/proxy_host)
 tport=self->hget(/proxy_port)
endif

count=0
again: count=count+1
self->close
error=1
dprint,'% HTTP::OPEN: server, port: ',tserver,tport,count

connect_timeout=self->hget(/connect_timeout)
read_timeout=self->hget(/read_timeout)
rawio=self->hget(/rawio)
swap_endian=self->hget(/swap_endian)
dprint,'% rawio: ',rawio

on_ioerror,done
self->hset,server=server
socket,lun,tserver,tport,/get_lun,error=error,connect_timeout=connect_timeout,$
            read_timeout=read_timeout,rawio=rawio,swap_endian=swap_endian
done:on_ioerror,null
if error eq 0 then begin
 self.unit=lun 
endif else begin
 retry=self->hget(/retry)
 if stregex(!error_state.msg,'Unable to connect|Unable to lookup',/bool,/fold) then retry=0
 if count ge retry then begin
  err='Could not open socket to server - '+tserver+' on port '+trim(tport)
  message,err,/cont
  self->close
 endif else begin
  message,'Retrying...',/cont
  goto,again
 endelse
endelse

skip:
if is_string(file) then self->hset,file=file

return & end

;-------------------------------------------------------------------------

function http::is_url,url

if is_blank(url) then return,0b

self->parse_url,url,server,file

return,is_string(server)

end

;--------------------------------------------------------------------------
;-- parse URL input

pro http::parse_url,url,server,file,port=port,secure=secure

file=''
if is_blank(url) then return
port=80 & secure=0b

temp=str_replace(url,'\','/')

s='(http[s]?://)?([^/:,]+)?(/[^:]+)?(:[0-9]+)?'

chk=strtrim(stregex(temp,s,/ext,/sub),2)
server=chk[2]
file=chk[3]
sport=chk[4]
if is_string(sport) then port=fix(strmid(sport,1,strlen(sport)))

if not self->valid_server(server) then begin
 server='' & file=url
endif

;if is_string(sfile) then file=strmid(sfile,1,strlen(sfile))

secure=stregex(url,'https:',/bool)

return & end

;-------------------------------------------------------------------------
;-- check if server attached to this socket

function http::is_open,server

if is_blank(server) then return,0b

if not self->is_socket_open() then return,0b

stat=fstat(self.unit)
tserver=server
tport=self->hget(/port)
if self->use_proxy() then begin
 tserver=self->hget(/proxy_host)
 tport=self->hget(/proxy_port)
endif 

op=trim(stat.name) eq tserver+'.'+trim(tport)

dprint,'% IS_OPEN: ',tserver,op

return,op
end

;-------------------------------------------------------------------------
;-- check if socket is open

function http::is_socket_open

error=0
catch,error
if error ne 0 then begin
 catch,/cancel
 return,0b
endif

stat=fstat(self.unit)
return,stat.open

end

;-------------------------------------------------------------------------
;-- close socket

pro http::close

if self.unit gt 0 then close_lun,self.unit
self.unit = 0
return & end

;---------------------------------------------------------------------------
;-- clear socket URL

pro http::clear

self.server=''

return
end

;----------------------------------------------------------------------------
;-- validate server address 

function http::valid_server,server

if is_blank(server) then return,0b
patt='/,\,:,~'
if str_match(server,patt) then return,0b

pos=strpos(server,'.')
if pos eq -1 then return,0b

chk='([^\.\\/]+\.)+[^\.\\/]+'
ok=stregex(trim(server),chk,/extra,/sub,/fold)
server_part=trim(ok[0])

return,server_part ne '' 

end

;---------------------------------------------------------------------------
;--- send a request to server

pro http::send,request,err=err

err=''
if is_blank(request) then return

if not self->is_socket_open() then self->open,err=err
if is_string(err) then return


dprint,'% HTTP::SEND: ',request

for i=0,n_elements(request)-1 do printf,self.unit,request[i]

return & end

;--------------------------------------------------------------------------
;-- restore last URL

function http::last_url

file=self->hget(/file)
server=self->hget(/server)
if is_blank(file) or is_blank(server) then return,''

return,server+'/'+file

end

;---------------------------------------------------------------------------
;--- send HEAD request to determine server content

pro http::head,url,content,type=type,size=bsize,date=date,err=err,no_close=no_close

err='' & bsize=0l & date='' & type=''

;-- use last saved URL

if is_blank(url) then url=self->last_url()

if is_blank(url) then begin
 err='Invalid URL entered'
 message,err,/cont
 return
endif

self->open,url,file=file,err=err
if is_string(err) then return

self->make,file,request,/head,no_close=no_close
self->send,request,err=err

if err eq '' then self->readf,content,err=err,/keep

if (1-keyword_set(no_close)) then self->close

if is_string(err) then return

if is_string(content) and n_params() eq 1  then print,content

if is_blank(content) then return

content=strcompress(strlowcase(strtrim(content,2)))

if arg_present(type) or arg_present(bsize) or arg_present(date) then $
 self->file_content,content,type=type,size=bsize,date=date

return & end

;----------------------------------------------------------------------------
;-- compare local and remote file sizes

function http::same_size,url,file,err=err,rsize=rsize,lsize=lsize

err=''

rsize=-1 & lsize=-1
if not self->file_found(url,content,err=err) then return,0b

self->file_content,content,size=rsize

if is_blank(file) then self->parse_url,url,server,lfile else lfile=file

lsize=file_size(lfile)
if lsize lt 0 then begin
 err='Local file not found'
 return,0b
endif

dprint,'% lsize, rsize: ',trim(lsize),' ',trim(rsize)
return, lsize eq rsize

end

;----------------------------------------------------------------------------
;-- get URL file type

function http::get_url_type,url,size=bsize,err=err,_extra=extra

err=''
bsize=0l
if not self->file_found(url,content,err=err,_extra=extra) then return,''

self->file_content,content,type=type,size=bsize

if err eq '' then begin
 if bsize eq 0 then err='Requested file has zero bytes'
endif

if is_string(err) then message,err,/cont

return,type
end

;--------------------------------------------------------------------------
;-- POST content to a server

pro http::post,url,content,output,_ref_extra=extra

if is_blank(content) then begin
 output=''
 return
endif

if arg_present(output) then self->list,url,output,post=content,_extra=extra else $
 self->list,url,post=content,_extra=extra

return & end

;---------------------------------------------------------------------------
;--- list text file from server

pro http::list,url,output,err=err,count=count,_extra=extra,reset=reset,$
                   verbose=verbose,cache=cache,links=links

common http_list,fifo

err=''
count=0l & output=''
if is_blank(url) or (n_elements(url) ne 1) then url='/'

;-- create a FIFO object to cache list results, or empty it if
;   /RESET

cache=keyword_set(cache) or (self->hget(/cache))

if (not exist(fifo)) and cache then fifo=obj_new('fifo',100) 
if keyword_set(reset) and exist(fifo) then fifo->empty

;-- check if this URL cached

self->parse_url,url,server,file
last_server=self->hget(/server)
if is_blank(server) and is_string(last_server) then server=last_server
if is_blank(server) then begin
 err='Missing remote server name'
 message,err,/cont
 return
endif
url_cache=server+'/'+file

status=0b
links=keyword_set(links)
if cache then fifo->get,url_cache,output,status=status
if links and (not is_struct(output)) then status=0b
if (not links) and (not is_string(output,/blank)) then status=0b

;-- if /LINKS then we parse the output for link names

if (not status) or (not cache) then begin
 self->request,url,err=err,_extra=extra,/no_head
 if err eq '' then self->readf,output,err=err,_extra=extra
 self->close
 if links then begin
  self->parse_href,output,files,sizes=sizes
  output={files:temporary(files),sizes:temporary(sizes)}
 endif
 if cache then fifo->set,url_cache,output
endif else begin
 if keyword_set(verbose) then message,'Restoring from cache - '+url,/cont
endelse

count=n_elements(output)
if (not links) and is_string(output) and n_params() ne 2 then print,output

if links and is_struct(output) then check=where(output.files ne '',count)

return & end

;---------------------------------------------------------------------------
;-- extract file names and sizes from remote directory listing

pro http::extract_href,url,files,sizes=sizes,times=times,_extra=extra,$
                       full=full,count=count
 
full=keyword_set(full)
do_times=arg_present(times)
files='' & sizes='' & count=0 & times=-1
if is_blank(url) then return
for i=0,n_elements(url)-1 do begin
 self->list,url[i],out,_extra=extra,count=ocount,/links,/keep
 if ocount gt 0 then begin
  if full then out.files='http://'+url[i]+temporary(out.files)
  tfiles=append_arr(tfiles,out.files,/no_copy)
  tsizes=append_arr(tsizes,out.sizes,/no_copy)
  if do_times then begin
   if have_tag(out,'times') then otimes=out.times else otimes=parse_time(out.files,/tai)
   ttimes=append_arr(ttimes,otimes,/no_copy)
  endif
 endif
endfor

if exist(tfiles) then begin
 files=temporary(tfiles)
 count=n_elements(files)
endif
if exist(tsizes) then sizes=temporary(tsizes)
if do_times then if exist(ttimes) then times=temporary(ttimes)

return & end

;-----------------------------------------------------------------------------
;-- read the server output

pro http::readf,output,bsize,err=err,keep=keep,_ref_extra=extra


err=''
delvarx,output

if is_number(bsize) then begin
 if bsize gt 0 then begin
  output=strarr(bsize)
  readf,self.unit,output
 endif
endif

if not exist(output) then output=rd_ascii_buff(self.unit,self.buffsize,_extra=extra)

if is_blank(output) then begin
 err='No response from server'
 message,err,/cont
 return
endif

;-- strip response header

if not keyword_set(keep) then begin
 dprint,'% READF: stripping HTTP headers'
 np=n_elements(output)
 chk=where(trim2(output) eq '',count)
 if count gt 0 then output=output[ (chk[0]+1):(np-1)]
endif

return & end

;--------------------------------------------------------------------------
;-- check if file exists on server (look for 404 response)

function http::file_found,url,content,err=err,_ref_extra=extra

err='' 
self->head,url,content,err=err,_extra=extra
if is_string(err) then return,0b
if is_blank(content) then return,0b

self->check_header,content,err=err,_extra=extra,url=url
if is_string(err) then return,0b

return,1b

end

;---------------------------------------------------------------------
;-- check HTTP header for tell-tale errors

pro http::check_header,content,err=err,verbose=verbose,url=url
err=''
verbose=keyword_set(verbose)

if is_string(url) then tfile=url else tfile=''

chk=where(stregex(content,'404 .* found',/bool,/fold),count)
if count gt 0 then begin
 err=strcompress('File '+tfile+' not found')
 if verbose then message,err,/cont
 return
endif

chk=where(stregex(content,'403 .* forbidden',/bool,/fold),count)
if count gt 0 then begin
 err=strcompress('File '+tfile+' access denied')
 if verbose then message,err,/cont
 return
endif

chk=where(stregex(content,'200 .* ok',/bool,/fold),count)
if count ne 0 then begin
 err=strcompress('File '+tfile+' unknown error')
 if verbose then message,err,/cont
 return
endif

return
end

;--------------------------------------------------------------------------
;-- parse file content

pro http::file_content,content,type=type,size=bsize,date=date

type='' & date='' & bsize=0l

if is_blank(content) then return

;-- get type

cpos=strpos(content,'content-type:')
chk=where(cpos gt -1,count)
if count gt 0 then begin
 temp=content[chk[0]]
 pos=strpos(temp,':')
 if pos gt -1 then type=strmid(temp,pos+1,strlen(temp))
endif

;-- get last modified data

cpos=strpos(content,'last-modified')
chk=where(cpos gt -1,count)
if count gt 0 then begin
 temp=content[chk[0]]
 pos=strpos(temp,':')
 if pos gt -1 then begin
  time=strtrim(strmid(temp,pos+1,strlen(temp)),2)
  pie=str2arr(time,delim=' ')
  date=anytim2utc(pie[1]+'-'+pie[2]+'-'+pie[3]+' '+pie[4],/vms)
 endif
endif

;-- get size

cpos=strpos(content,'content-length')
chk=where(cpos gt -1,count)
if count gt 0 then begin
 temp=content[chk[0]]
 pos=strpos(temp,':')
 if pos gt -1 then bsize=long(strmid(temp,pos+1,strlen(temp)))
endif

return & end

;---------------------------------------------------------------------------
;-- strip HTTP response header

pro http::strip,header

on_ioerror, done
linesread=0
text='xxx'
header = strarr(256)
while text ne '' do begin
 readf,self.unit,text
 header[linesread]=text
 linesread=linesread+1
 if (linesread mod 256) eq 0 then header=[header, strarr(256)]
endwhile
done:on_ioerror,null

return & end

;---------------------------------------------------------------------------
;-- GET binary data from server

pro http::copy,url,new_name,err=err,out_dir=out_dir,verbose=verbose,$
                    clobber=clobber,status=status,prompt=prompt,$
                    cancelled=cancelled,_ref_extra=extra,copy_file=copy_file,$
                    no_change=no_change,range=range,head_first=head_first

status=0b
err=''
verbose=keyword_set(verbose)
cancelled=0b
out_file=''

self->parse_url,url,server,file
last_server=self->hget(/server)
if is_blank(server) and is_string(last_server) then server=last_server

if is_blank(server) or is_blank(file) then begin
 err='Blank URL filename entered'
 message,err,/cont
 return
endif

if is_string(out_dir) then tdir=local_name(out_dir) else begin
 tdir=curdir()
 out_dir=tdir
endelse

break_file,local_name(file),dsk,dir,name,ext
out_name=trim(name+ext)

if is_string(new_name) then begin
 break_file,local_name(new_name),dsk,dir,name,ext
 out_name=trim(name+ext)
 if is_string(dsk+dir) then tdir=trim(dsk+dir)
endif

;-- check if clobbering existing file

copy_file=concat_dir(tdir,out_name)
no_clobber=1b-keyword_set(clobber)
if no_clobber then begin
 chk=loc_file(copy_file,count=count)
 if count ne 0 then begin
  if verbose then begin
   message,'Local file '+copy_file+' exists (not copied)',/cont
  endif
  if keyword_set(no_change) then begin status=1b & return & endif
  osize=file_content(copy_file,/size)
 endif
endif

;-- test for write access to destination directory

if not write_dir(tdir,err=err) then return

;-- determine remote file size before downloading

bsize=0l
do_head=keyword_set(head_first)
if do_head then begin
 if not self->file_found(url,content,err=err,verbose=verbose) then return
 self->file_content,content,size=bsize
 if is_string(err) then return
endif

;-- send a GET request

self->open,url,file=file,err=err,server=server
if is_string(err) then return
self->make,file,request,range=range
self->send,request,err=err
counts=0
if err eq '' then begin

;-- strip and check HTTP header if skipped earlier

 self->strip,header
 if not do_head then begin
  self->check_header,header,err=err,_extra=extra,verbose=verbose,url=url
  if is_string(err) then begin
   self->close
   return
  endif
 endif

;-- get byte size from header if HEAD request is by-passed

 if bsize eq 0l then begin
  cpos=strpos(strlowcase(header),'content-length')
  chk=where(cpos gt -1,lcount)
  if lcount gt 0 then begin
   temp=header[chk[0]]
   pos=strpos(temp,':')
   if pos gt -1 then bsize=long(strmid(temp,pos+1,strlen(temp)))
  endif
 endif

 if bsize eq 0l then begin
  err='Remote file '+file+' not found (or could not determine remote file size)'
  if verbose then message,err,/cont
  self->close
  return
 endif

;-- override no_clobber if local and remote file sizes have changed
 
 if no_clobber then begin
  if exist(osize) then begin
   if osize eq bsize then begin self->close & return & endif
   if verbose then $ 
    message,'Remote & local files differ in size: '+trim(bsize)+','+trim(osize),/cont
  endif
 endif

;-- prompt before downloading

 if keyword_set(prompt) then begin
  ans=xanswer(["Remote file: "+file+" is "+trim(str_format(bsize,'(i10)'))+" bytes.",$
               "Proceed with download?"])
  if not ans then begin self->close & return & endif
 endif

 cmess=['Please wait. Downloading...','File: '+file,$
        'Size: '+trim(str_format(bsize,"(i10)"))+' bytes',$
        'From: '+server,'To: '+tdir]

 if verbose then begin
;  for i=0,n_elements(cmess)-1 do message,cmess[i],/cont,noname=(i gt 0)
  t1=systime(/seconds)
 endif

;-- next read bytes

 openw,lun,copy_file,/get_lun
 rdwrt_buff,self.unit,lun,bsize,err=err,counts=counts,omessage=cmess, $
           _extra=extra,verbose=verbose,cancelled=cancelled
 close_lun,lun
endif
self->close

dprint,'% HTTP::COPY: cancelled = ',cancelled

if (counts ne bsize) or is_string(err) or cancelled then begin
 file_delete,copy_file,/quiet
 xack,err,/info
 return
endif

if verbose then begin
 t2=systime(/seconds)
 tdiff=t2-t1
 output1=trim(string(counts,'(i10)'))+' bytes of '
 output2=trim(string(bsize,'(i10)'))+' total bytes copied'
 output=output1+output2+' in '+str_format(tdiff,'(f8.2)')+' seconds'
 message,output,/cont
 message,'Wrote '+trim(string(counts,'(i10)'))+' bytes to file '+copy_file,/cont
endif 
status=1b
chmod,copy_file,/g_write,/g_read

return & end

;---------------------------------------------------------------------

pro http__define                 

struct={http,server:'',unit:0l,file:'',retry:0,connect_timeout:0,buffsize:512l,$
         user_agent:'',protocol:'',proxy_host:'',proxy_port:0l,swap_endian:0b,$
         port:0l,gateway:0b,read_timeout:0,rawio:0b,secure:0b,cache:0b}

return & end

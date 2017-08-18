;+
; Project     : VSO
;
; Name        : VSO__DEFINE
;
; Purpose     : Define a VSO class
;
; Explanation : Defines a VSO class to provide a client to connect to the
;               Virtual Solar Observatory <http://virtualsolar.org>
;
; Category    : Utility, Class4, VSO
;
; Syntax      : IDL> a=obj_new('vso')
;
; Examples    :
;               IDL> a = obj_new('vso')               ; create a VSO object
;               IDL> q = a->buildQuery( date='2005-JAN-1 - 2005-JAN-1 01:00', $
;               IDL>              instrument='eit' )  ; build a VSO query
;               IDL> stack   = a->query(q)            ; send a VSO query
;               IDL> meta    = stack->contents()      ; extract the metadata
;               IDL> records = a->getdata(meta)       ; order the products
;               IDL> urls    = records.url            ; extract the product URLs
;
;               See also vso_search() and vso_get()
;
; History     : Ver 0.1, 07-Oct-2005, J A Hourcle.  written
;               Ver 0.2, 14-Oct-2005, J A Hourcle.  changed how 'nulls' get passed
;               Ver 0.3, 21-Oct-2005, J A Hourcle.  return metadata as array of struct
;               Ver 0.4, 24-Oct-2005, J A Hourcle.  more liberal 'getdata' input
;               Ver 1,   08-Nov-2005, J A Hourcle.  documentation; released
;                        12-Nov-2005, Zarro (L-3Com/GSFC)
;                          -added TSTART/TEND for compatability with SSW usage
;                          -added CLEANUP and call to VSO_FORMAT
;               Ver 1.2, 18-Nov-2005, Hourcle.  fixed memory leaks
;               Ver 1.3, 1-Dec-2005, Zarro (L-3Com/GSFC) - added VSO_SERVER call;                
;               Ver 1.4, 1-Dec-2005, Hourcle.  more consistent date handling
;               Modified 1-Jan-2006, Zarro - removed add_method,'gen' since
;                 already inherited from SOAP class
;
; Contact     : oneiros@grace.nascom.nasa.gov
;
; Limitations : Not fully tested against problem providers (ie, errors thrown)
;-



; See vso_search.pro and vso_get.pro for higher level functions


;=========

function vso::init, _ref_extra=extra

;-- get VSO server proxy and URI
                 
    server=vso_server(uri,/no_check)                                                                                                          
    if not self->soap::init( _extra=extra ) then return,0
    if not self->soap::open(server,uri) then return,0

    self.version = 0.6 ; 1.2, but it's a long story
    self.methods = ['URL-FILE', 'URL']
    return,1
end


;=============

pro vso::cleanup

self->soap::cleanup

return & end

;==============

; Map element names to the case sensitive XML equivalent
; see soap__define.pro

function vso::element_name, name

    ; if there's some way to pass this to the parent, without
    ; knowing that the parent is soap, I'd love to know how.

    name = self->soap::element_name( name )

    name = strlowcase( name )

    ; take care of those special items that need to be mixed case

    case name of
        $ ; query items
        'vsoiresult' : return, 'VSOiResult'
        'vsoquery'   : return, 'VSOQuery'
        'query'      : return, 'Query'
        'getdata'    : return, 'GetData'
        $ ; getdata items
        else         : break
    endcase

    return, name
end


;=========

; which elements should be encoded as arrays, even if they have
; only one value.  See soap__define.pro

function vso::is_array_name, name
    name = strlow(name)
    switch name of
    'item'   : return, 0
    'method' :
;   'data'   :
    'fileid' : return, 1
    endswitch

    return, -1
end

;=========

; Given a string that a user might enter, attempt to generate a
; VSO compatable string

; Input :
;   INPUT : string ; the user entered string
; Optional Flag:
;   ENDDATE : the value was an end-date (affects windowing)
; Output
;   string ; the VSO representation of the date

function vso::parse_date, input, enddate=enddate,err=err

;-- DMZ - better to use SSW parsers

    return,vso_format(input,err=err)

    parts = uint( strsplit(input,'[^0-9]+',/regex, /extract))
    if (n_elements(parts) gt 6) then begin
        ; too many parts (year, month, day, hour, minutes, seconds)
        message, 'Unknown date format.  Please use YYYY-MM-DD HH:mm:ss'
        return,0
    endif

    returnValue = [ 0,1,1,0,0,0 ]

    if keyword_set(enddate) then $
        returnValue = [ 3000,12,31,23,59,59 ]

    for i = 0, n_elements(parts)-1 do returnValue[i] = parts[i]

    return, string( returnValue, format='(%"%04d%02d%02d%02d%02d%02d")' )

end

;=========

; Given a string, attempt to extract a start and end date
; 2005-12-01 : slight modification -- will handle all of the logic
;              to determine if we were given a range or individual
;              start/end values.

; Input :
;   RANGE : string ; the user entered timerange string
; Opt. Input :
;   TSTART: string ; a start time
;   TEND  : string ; an end time
; Output
;   string[2] ; the VSO representation of the start and end dates

function vso::parse_daterange, range, tstart, tend, err=err

    ; date range will override specifying start/end individually
    if ( n_elements(range) ) then if ( not is_blank(range) ) then begin
        parts = strtrim( strsplit(range,'-',/extract), 2 )
        if (n_elements(parts) gt 2) then begin
            ; they've used YYYY-MM-DD formatted dates or something
            ; we can try for '(space)-(space)'
            parts = strtrim( strsplit(range, ' - ', /extract, /regex), 2 )
            if (n_elements(parts) gt 2) then begin
                message, 'Bad date range.  Please seperate dates with " - " if using dashes in dates'
                return,0
            endif
        endif

        ; no shortcutting in IDL, so (if n_elements) keeps us from checking
        ; (if is_blank) on something that doesn't exist.
        if n_elements(parts) gt 0 then if not is_blank(parts[0]) then tstart = parts[0]
        if n_elements(parts) gt 1 then if not is_blank(parts[1]) then tend   = parts[1]
    endif

    if is_blank(tstart) then $
        message, 'No time range supplied (TSTART or named "date" or "start_date")'

    dstart=vso_format(tstart, err=err)
    if not is_blank(err) then message, 'Error parsing start date'

    ; JAH -- an invalid end date is handled differently from an empty one
    dend = ''
    if is_blank(tend) then begin
        dend=anytim2utc(tstart)
        dend.mjd=dend.mjd+1
        dend.time=0
        dend=vso_format(dend, err=err)
    endif else dend=vso_format(tend, err=err)

    if is_string(err) then message, 'Error parsing end date'

    return, [dstart, dend]

end


;=========

; Given a string, attempt to extract min/max and units of a spectral range
; in the correct format for VSO to process

; Input :
;   INPUT : string ; the user entered string
; Output
;   struct ; { wave, wavemin:float, wavemax:float, waveunit:string }

function vso::parse_wave, input

    ; idl regexes don't seem to support \d,\s, or (?: )
    matches = stregex( strtrim(input,2), '^([0-9]+)( *- *([0-9]+))? *(.*)', /subexpr, /extract )

    wave = { wave, wavemin:matches[1], wavemax:matches[3], waveunit:matches[4] }

    if ( wave.waveunit eq '' ) then wave.waveunit = 'Angstrom'
    if ( wave.wavemax  eq '' ) then wave.wavemax  = matches[1]

    return, wave
end

;=========

; Given a series a inputs, generate a structure representing the VSO
; query.

; More information is available at http://virtualsolar.org/
; See also http://vso.nascom.nasa.gov/cgi-bin/show_details.pl
; for values used in enumerations

; Input:
;   (note -- you must either specify DATE, START_DATE or TSTART)
; Optional Input:
; (positional)
;   TSTART     : string ; the start date
;   TEND       : string ; the end date
; (named)
;   DATE       : string ; (start date) - (end date)
;   START_DATE : string ; the start date
;   END_DATE   : string ; the end date
;   WAVE       : string ; (min) - (max) (unit)
;   MIN_WAVE   : string ; minimum spectral range
;   MAX_WAVE   ; string ; maximum spectral range
;   UNIT_WAVE  ; string ; spectral range units (Angstrom, GHz, keV)
;   EXTENT     ; string ; VSO 'extent type' ... (FULLDISK, CORONA, LIMB, etc)
;   PHYSOBS    ; string ; VSO 'physical observable;
;   PROVIDER   ; string ; VSO ID for the data provider (SDAC, NSO, SHA, MSU, etc)
;   SOURCE     ; string ; spacecraft or observatory (SOHO, YOHKOH, BBSO, etc)
;   INSTRUMENT ; string ; instrument ID (EIT, SXI-0, SXT, etc)
; (placeholders for the future)
;   DETECTOR   ; string ; detector ID (not supported by all providers; use inst also)
;   FILTER     ; string ; filter name (same problems as detector)
; Output:
;   struct ; something appropriate to pass to 'query' method.

; note -- resolution of date parameters: order of precidence
;   DATE (named parameter)
;   TSTART/TEND (positional parameters)
;   START_DATE/END_DATE (named parameters)
; it is possible to mix TSTART/END_DATE, or DATE(only a start)/END_DATE
; but it is not recommended, and may not be supported in the future.

; if no end date is specified, the system will use the start of the
; next day

; note -- for the time being, this just uses the first value of each
; item passed in -- it _does_ not take arrays of physobs, or intruments,
; or any other parameters.

; internally, structure items are (struct)_(field), but IDL doesn't seem
; to like arguments if there are other arguments with longer names,
; so to pass things in, they're (field)_(struct)

function vso::buildQuery, tstart,tend,$
        date=date_range, start_date=date_start, end_date=date_end, $
        wave=wave_string, min_wave=wave_min, max_wave=wave_max, unit_wave=wave_unit, $
        extent_type=extent_type, $
        provider=provider, source=source, instrument=instrument, $
        detector=detector, filter=filter, physobs=physobs

    vsoDate = { time, start:'', _end:'' }
    vsoWave = { wave, wavemin:'', wavemax:'', waveunit:'Angstrom' }
    vsoExtent = { extent, x:'', y:'', width:'', length:'',  type:'' }

;-- check if time entered as arguments. If valid TSTART is entered but
;   invalid or no TEND entered, then round to end of day

; 2005-12-01 : JAH -- moved bad end handling to 'parse_daterange'

    if ( n_elements(date_end)   eq 0 and not is_blank(tend)   ) then $
	date_end = tend[0]
    if ( n_elements(date_start) eq 0 and not is_blank(tstart) ) then $
	date_start = tstart[0]

    dates = self->parse_daterange(date_range, date_start, date_end)
    vsoDate.start=dates[0]
    vsoDate._end=dates[1]


    if ( n_elements(wave_string) ) then begin
        vsoWave = self->parse_wave(wave_string[0])
    endif else begin
        if ( n_elements(wave_min) or n_elements(wave_max) ) then begin
            if ( n_elements(wave_min)  ) then vsoWave.wavemin  = wave_min[0]
            if ( n_elements(wave_max)  ) then vsoWave.wavemax  = wave_max[0]
            if ( n_elements(wave_unit) ) then vsoWave.waveunit = wave_unit[0]
        endif else begin
            ; should I prompt for wave?
        endelse
    endelse

    if ( n_elements(extent_type) ) then $
        vsoExtent.type = extent_type[0]

    vsoQuery = { vsoQuery, time:vsoDate, extent:vsoExtent, wave:vsoWave, $
        instrument:'', provider:'', physobs:'', source:'', detector:'', filter:'' }

    if ( n_elements(instrument) ) then vsoQuery.instrument = instrument[0]
    if ( n_elements(provider)   ) then vsoQuery.provider   = provider[0]
    if ( n_elements(source)     ) then vsoQuery.source     = source[0]
    if ( n_elements(detector)   ) then vsoQuery.detector   = detector[0]
    if ( n_elements(filter)     ) then vsoQuery.filter     = filter[0]
    if ( n_elements(physobs)    ) then vsoQuery.physobs    = physobs[0]

    return, vsoQuery
end


;=========

; Send a Query to VSO

; Input :
;   ARGS : a query (generated from self->buildQuery())
; Optional Flags :
;   QUIET : don't print informational messages
; Output :
;   'stack' object.  call 'stack->contents()' to get an array (if any)
;   see stack_define.pro  (can test for stack->n_elements())

function vso::query, args, quiet=quiet, _extra=extra
    version = self->getprop(/version)

    dom = self->send('Query', { QueryRequest, block:args, version:version }, _extra=extra )

    ; walk the tree, and keep track of error messages, returned items, etc.

    parser = self->parser()

    errors = parser->findElement( 'error', dom );
    if ( errors->n_elements() ne 0 ) then begin
        print, rotate( ['===========', 'VSO ERROR :',parser->getText(errors->contents()), '==========='], 1)
    endif
    obj_destroy, errors

    providerResponses = parser->findElement( '.*', dom, type='(^|:)ProviderQueryResponse$' )
    records = obj_new('stack')

    if ( providerResponses->n_elements() ne 0) then begin
        for i = 0, providerResponses->n_elements()-1 do begin
            node = providerResponses->item(i)
            ; look at each response
            provider = parser->gettext( parser->walktree( node, 'provider' ) )

            if not keyword_set(quiet) then  $
                print, 'Records Returned : '+parser->getElementValue( node, 'provider')+ ' : '+ $
                parser->getElementValue( node, 'no_of_records_returned' )+ '/'+ $
                parser->getElementValue( node, 'no_of_records_found' )
            subrecords = parser->findElement( '.*', node, type='(^|:)QueryResponseBlock$' );

            if ( subrecords->n_elements() ne 0 ) then begin
                records->push, self->parseRecord( subrecords->contents(), _extra=extra )
                obj_destroy, subrecords
            endif
        endfor
        obj_destroy, providerResponses
    endif
; 2004/11/17 : better garbage collection -- JAH
    if obj_valid(dom->getOwnerDocument()) then obj_destroy, dom->getOwnerDocument()
    if obj_valid(dom) then obj_destroy, dom

    return, records
end

;=========

; convert the returned records (as a DOM object) to an IDL structure

; Input :
;   RECORD : a DOM object (or array of objects) containing VSO records
; Optional Flags :
;   FLATTEN : return a flattened structure (no sub-structures)
; Output :
;   struct[n] : an array of vsoRecord structs.  (unless '/FLATTEN')
;   see also self->parseFlatRecord()


function vso::parseRecord, record, flatten=flatten, _extra=extra
    if n_elements(record) gt 1 then begin
        stack = obj_new('stack')
        for i = 0, n_elements(record)-1 do $
            stack->push, self->parseRecord( record[i], flatten=flatten, _extra=extra )
        temp = stack->contents();
        if obj_valid(stack) then obj_destroy, stack
        return, temp
    endif

    if keyword_set(flatten) then return, self->parseFlatRecord( record )
    parser = self->parser()

    timeNode = parser->walkTree( record, 'time' )
    time     = { vsoTime, $
        start:self->formatTime( parser->getElementValue( timeNode, 'start' ) ), $
         _end:self->formatTime( parser->getElementValue( timeNode, 'end'   ) ) }

    waveNode = parser->walkTree( record, 'wave' )
    wave     = { vsoWave, $
         min:parser->getElementValue( waveNode, 'wavemin', /float ), $
         max:parser->getElementValue( waveNode, 'wavemax', /float ), $
        unit:parser->getElementValue( waveNode, 'waveunit' ) $
    }

    extentNode = parser->walkTree( record, 'extent' )
    extent   = { vsoExtent, $
      type:parser->getElementValue( extentNode, 'type' ), $
     width:parser->getElementValue( extentNode, 'width',  /float ), $
    length:parser->getElementValue( extentNode, 'length', /float ), $
         x:parser->getElementValue( extentNode, 'x',      /float ), $
         y:parser->getElementValue( extentNode, 'y',      /float ) $
    }

    structure =  { vsoRecord, time:time, extent:extent, wave:wave, $
    instrument:parser->getElementValue( record, 'instrument' ), $
        source:parser->getElementValue( record, 'source'     ), $
      provider:parser->getElementValue( record, 'provider'   ), $
          info:parser->getElementValue( record, 'info'       ), $
       physobs:parser->getElementValue( record, 'physobs'    ), $
        fileid:parser->getElementValue( record, 'fileid'     ), $
          size:parser->getElementValue( record, 'size', /float ), $
        url:'', getinfo:'' $ ; placeholders for 'getdata' merging
    }
    return, structure


end

; convert the returned records (as a DOM object) to an IDL structure
; without sub-structures

; Input :
;   RECORD[n] : a DOM object (or array of objects) containing VSO records
; Output :
;   struct[n] : an array of vsoFlatRecord structs.

function vso::parseFlatRecord, record
    parser = self->parser()

      timeNode = parser->walkTree( record, 'time' )
      waveNode = parser->walkTree( record, 'wave' )
    extentNode = parser->walkTree( record, 'extent' )

    structure =  { vsoFlatRecord, $
        time_start:self->formatTime( parser->getElementValue( timeNode, 'start' ) ), $
          time_end:self->formatTime( parser->getElementValue( timeNode, 'end'   ) ), $
          wave_min:parser->getElementValue( waveNode, 'wavemin', /float ), $
          wave_max:parser->getElementValue( waveNode, 'wavemax', /float ), $
         wave_unit:parser->getElementValue( waveNode, 'waveunit' ), $
       extent_type:parser->getElementValue( extentNode, 'type' ), $
      extent_width:parser->getElementValue( extentNode, 'width',  /float ), $
     extent_length:parser->getElementValue( extentNode, 'length', /float ), $
          extent_x:parser->getElementValue( extentNode, 'x',      /float ), $
          extent_y:parser->getElementValue( extentNode, 'y',      /float ), $
        instrument:parser->getElementValue( record, 'instrument' ), $
            source:parser->getElementValue( record, 'source'     ), $
          provider:parser->getElementValue( record, 'provider'   ), $
              info:parser->getElementValue( record, 'info'       ), $
           physobs:parser->getElementValue( record, 'physobs'    ), $
            fileid:parser->getElementValue( record, 'fileid'     ), $
              size:parser->getElementValue( record, 'size', /float ),$
        url:'', getinfo:'' $ ; placeholders for 'getdata' merging
    }

    return, structure
end

; convert the returned DOM objects from a 'getdata' request to IDL structs

; Input :
;   RECORD[n] : a DOM object (or array of objects) containing VSO records
; Output :
;   struct[n] : an array of vsoGetDataRecord structs


function vso::parseGetDataRecord, record
    parser = self->parser()

    return, { vsoGetDataRecord, $
        provider:self->getElementValue( record, 'provider' ), $
          fileid:self->getElementValue( record, 'fileid'   ), $
             url:self->getElementValue( record, 'url'      )  $
    }
end

;=========

; convert a VSO time string into a more scientifically acceptable format

; Input:
;   TIMESTRING : a VSO time string
; Output:
;   string ; an ISO time string

function vso::formatTime, timeString
    parts = stregex( timeString, '^(.{4})(..)(..)(..)(..)(..)', /extract, /subexpr )
    return, string( format='(%"%4i-%02i-%02iT%02i:%02i:%02i")', fix(parts[1]), fix(parts[2]), fix(parts[3]), fix(parts[4]), fix(parts[5]), fix(parts[6]) )

end


;=========

; call the VSO 'GetData' method via SOAP

; Input:
;   ARGS : Can be one of:
;     struct[n] : vsoRecord (returned from vso::query())
;     struct[n] : vsoFlatRecord (returned from vso::query(/FLAT))
;     struct[n] : datarequest (you probably don't want this)
; Optional Input:
;   METHODS : string[n] : acceptable transfer methods
; Optional Flags:
;   MERGE : if input is vsoRecord or vsoFlatRecord, will insert URLs into
;           the input structures
;   URLS  : override METHODS to only use URL-type transfer methods.
; Output:
;   struct[n] : getDataRecord

function vso::getdata, args, methods=methods, merge=merge, urls=urls, _extra=extra
    version = self->getprop(/version)
    if ( n_elements(methods) eq 0 ) then $
        methods = self->getprop(/methods)

    ; only URL type methods, if they set the /urls flag
    if ( keyword_set(urls) or keyword_set(merge)) then $
        methods = [ 'URL-FILE', 'URL-TAR_GZ', 'URL-ZIP', 'URL' ]

    ; time to figure out how we were called ...

    can_merge = 0
    argtype = size(args, /tname)
    if ( argtype eq 'STRUCT' ) then begin
        structname = tag_names( args, /structure_name )
        switch structname of
            'DATAREQUEST' : begin
                request = args
                break ; exactly what gets sent to VSO
            end
            'VSORECORD' :
            'VSOFLATRECORD' : begin ; need to convert from the metadata records from query
                request = self->generateGetData( args )
                can_merge = 1
                break;
            end
            else : $
                message, 'VSO GetData input structure unknown '+structname
        endswitch
    endif else if ( argtype eq 'OBJECT' ) then begin
        message, 'VSO GetData -- no support for input objs yet'
    endif else $
        message, 'VSO GetData -- unknown input format'

    methodPtr = ptr_new(methods)
    dom = self->send('GetData', { request:{ VSOiGetDataRequest, version:version, request:{ GetDataRequest, data:request, method:methodPtr } } }, _extra=extra )
    ptr_free,methodPtr

    parser = self->parser()

    ; were there any status messages to worry about?
    status = parser->getElementValue( dom, 'status' )
    if ( n_elements(status) ne 0 and status ne '' ) then begin
        if obj_valid(dom->getOwnerDocument()) then obj_destroy, dom->getOwnerDocument()
            if obj_valid(dom) then obj_destroy, dom
        message, status,/cont
    endif

    dataList = parser->findElement( 'data', dom )
    dataNodes = obj_new('stack')

    for i = 0, dataList->n_elements()-1 do begin
        ArrayNode = dataList->item(i)
        ItemList  = ArrayNode->getChildNodes()
        for j = 0, ItemList->getLength()-1 do $
            dataNodes->push, ItemList->item(j)
    endfor
    obj_destroy,dataList

    ; replace stack w/ the underlying array & garbage collect stack obj
    temp = dataNodes->contents()
    if obj_valid(dataNodes) then obj_destroy, dataNodes
    dataNodes = temp

    records = obj_new('stack')
    for i=0, n_elements(dataNodes)-1 do begin
        provider = parser->getElementValue( datanodes[i], 'provider' )
        fileid   = parser->getElementValue( datanodes[i], 'fileid' )
        url      = parser->getElementValue( datanodes[i], 'url' )
        info     = parser->getElementValue( datanodes[i], 'info' )
        size     = parser->getElementValue( datanodes[i], 'size', /float )

        if ( n_elements(fileid) gt 1 ) then for j=0, n_elements(fileid)-1 do begin
            records->push, { GetDataRecord, provider:provider, fileid:fileid[j], url:url, info:info, size:size }
        endfor else $
            records->push, { GetDataRecord, provider:provider, fileid:fileid, url:url, info:info, size:size }
    endfor

; 2004/11/17 : better garbage collection -- JAH
    if obj_valid(dom->getOwnerDocument()) then obj_destroy, dom->getOwnerDocument()
    if obj_valid(dom) then obj_destroy, dom
; if we generated the 'request' on the fly, clean it up, because it has
; a pointer in it.
    if ( can_merge ) then heap_free, request

    if ( keyword_set(merge) ) then begin
        if ( can_merge ) then begin
            temp = self->merge_getdata( args, records )
            if obj_valid(records) then obj_destroy, records
            return, temp
        endif $
        else print, "Warning : Can't merge records (must be 'vsorecord' or 'vsoflatrecord' as input)"
    endif

    temp = records->contents()
    if obj_valid(records) then obj_destroy, records

    return, temp
end

;=========

; given the results from a VSO Query, and VSO GetData, will add the URLs from
; GetData into the struct from the query.

; Input:
;   RECORDS : Can be one of:
;     struct[n] : vsoRecord (returned from vso::query())
;     struct[n] : vsoFlatRecord (returned from vso::query(/FLAT))
;   GETDATA : struct[n] : GetDataRecord
; Output:
;   struct[n], the modified 'RECORDS' (well, the same records, technically)

function vso::merge_getdata, records, getdata
    for i = 0, getdata->n_elements()-1 do begin
        record=getdata->item(i)
        if ( record.fileid eq '' ) then $ ; may be a problem -- applies to all results from this provider
            update = where ( records.provider eq record.provider ) $
        else update = where ( records.provider eq record.provider and records.fileid eq record.fileid )
        records[update].url = record.url
        records[update].getinfo = record.info
        if ( record.size ne 0 ) then records[update].size = record.size
    endfor
    obj_destroy,getdata

    return, records
end


;=========

; Given the results from a VSO Query, will generate the appropriate
; GetData request

; Input:
;   INPUT : Can be one of:
;     struct[n] : vsoRecord (returned from vso::query())
;     struct[n] : vsoFlatRecord (returned from vso::query(/FLAT))
; Output:
;   struct[n] : DataRequest



; convert an array of metadata records into the preferred VSO GetData structure
; (fileids grouped by providers)
function vso::generateGetData, input
    providers = input.provider

    ; just get the uniq values
    providers = providers[ uniq( providers, sort(providers) ) ]

    request = obj_new('stack')

    for i=0, n_elements(providers)-1 do begin
        fileids = ptr_new(input[ where ( input.provider eq providers[i] ) ].fileid)
        request->push, { DataRequest, provider:providers[i], fileid:fileids }
    endfor

    temp = request->contents()
    if obj_valid(request) then obj_destroy, request
    return, temp
end

;=========


pro vso__define

    struct = { vso, INHERITS soap, version:1.2, methods:['URL-FILE', 'URL'] }

return & end

;=========


structure Server :> SERVER =
struct

fun debug f = ()

infix |>
fun x |> f = f x

fun qq s = "'" ^ s ^ "'"

exception InternalServerError
exception BadRequest
exception MissingConnection

open Http

structure SS = Substring

type opts = {logfile:string}

type conn = Socket.active Socket.stream INetSock.sock * INetSock.sock_addr * opts
type header = string * string
type ctx = {conn:conn, req:Request.t, resp_headers:header list ref}
type filepath = string

structure Serialize : SERVER_SERIALIZE = ServerSerialize

structure Info : sig include SERVER_INFO
                     val setAndLoadConfigFile : string -> bool
                 end = struct
  fun hostname () = NetHostDB.getHostName()

  fun pid () =
      Posix.ProcEnv.getpid() |> Posix.Process.pidToWord
                             |> SysWord.toIntX

  fun username () = Posix.ProcEnv.getlogin()

  val time0 = Time.now()
  fun uptimeProcess () : Time.time =
      Time.-(Time.now(), time0)

  val cfileRef = ref ""
  val cfileValues : (string * string) list ref = ref nil

  fun readFile f =
      let val is = TextIO.openIn f
      in let val s = TextIO.inputAll is
         in TextIO.closeIn is
          ; s
         end handle ? => (TextIO.closeIn is; raise ?)
      end handle _ => raise Fail ("failed to read configuration file " ^ qq f)

  fun trim s = SS.full s
            |> SS.dropl Char.isSpace
            |> SS.dropr Char.isSpace
            |> SS.string
            |> (fn "" => NONE | s => SOME s)

  fun setAndLoadConfigFile cfile =
      if cfile = "" then true
      else let val content = readFile cfile
               val lines = String.tokens (fn c => c = #"\n") content
               val pairs =
                   List.foldr
                       (fn (line,acc) =>
                           if line = "" orelse String.sub(line,0) = #"%"
                              orelse CharVector.all Char.isSpace line
                           then acc
                           else
                             let fun err () =
                                     raise Fail ("wrong line format for line:\n " ^ qq line ^ "\n" ^
                                                 " - use ':' to separate key and value.\n " ^
                                                 " - use an initial '%' to specify a line comment.")
                             in case String.tokens (fn c => c = #":") line of
                                    [k,v] => (case (trim k, trim v) of
                                                  (SOME k, SOME v) => (k,v)::acc
                                                | _ => err())
                                  | _ => err()
                             end) nil lines
               fun check nil = ()
                 | check ((k,_)::rest) =
                   if List.exists (fn (k',_) => k=k') rest then
                     raise Fail ("multiple definitions for key " ^ qq k ^
                                 " in the configuration file " ^ qq cfile ^ ".")
                   else check rest
           in check pairs
            ; cfileValues := pairs
            ; cfileRef := cfile
            ; true
           end handle Fail msg => ( print ("Error: " ^ msg ^ "\nExiting...\n")
                                  ; false )

  fun configValues () = !cfileValues

  fun configGetValue k =
      List.find (fn (k',_) => k=k') (!cfileValues)
      |> Option.map #2

  fun configFile () = case !cfileRef of
                          "" => NONE
                        | s => SOME s
end

structure Util : SERVER_UTIL = ServerUtil

structure Req : SERVER_REQ = struct

  type ctx = ctx
  fun full (ctx:ctx) = #req ctx

  fun method (ctx:ctx) : Request.method =
      #method(#line(full ctx))

  fun path (ctx:ctx) : string =
      case #uri (#line (full ctx)) of
          Uri.PATH {path,...} => path
        | Uri.URL {path,...} => path
        | Uri.AST => "*"

  fun query_of_uri (Uri.URL {query,...}) = query
    | query_of_uri (Uri.PATH {query,...}) = query
    | query_of_uri Uri.AST = ""

  fun queryAll (ctx:ctx) : (string * string) list =
      let val query = query_of_uri (#uri(#line(full ctx)))
          val tokens = String.tokens (fn c => c = #"&") query
      in List.foldr (fn (t,acc)=>
                        case String.tokens (fn c => c = #"=") t of
                            [k,v] => (k,v)::acc
                          | nil => acc
                          | [k] => (k,"")::acc
                          | k :: _ => (k,String.extract(t,size k,NONE)
                                         handle _ => "")::acc)
                    nil tokens
      end

  fun query (ctx:ctx) (k:string) : string option =
      let val pairs = queryAll ctx
      in case List.find (fn (x,_) => x=k) pairs of
             SOME (_,y) => SOME y
           | NONE => NONE
      end

  fun headers (ctx:ctx) : (string * string) list =
      #headers(full ctx)

  fun header (ctx:ctx) (k:string) : string option =
      Header.look (headers ctx) k

  fun host (ctx:ctx) : string =
      case header ctx "Host" of
          SOME v => v
        | NONE => ""

  fun postData (ctx:ctx) : string =
      let val s = case #body(#req ctx) of
                      SOME s => s
                    | NONE => ""
      in s
      end
end

fun sendVecAll (sock, slc) =
    let val i = Socket.sendVec (sock, slc)
        val len = Word8VectorSlice.length slc
    in if i < len then
         sendVecAll (sock, Word8VectorSlice.subslice(slc, i, NONE))
       else ()
    end

fun sendStrClose (sock, s) =
    let val () = debug (fn () => "Sending string:\n" ^ s ^ "\n")
        val slc = Word8VectorSlice.full (Byte.stringToBytes s)
    in sendVecAll (sock, slc)
     ; Socket.close sock
    end handle _ => raise MissingConnection

fun sendStatusCodeClose sock sc =
    let val line = {version=Version.HTTP_1_1, status=sc}
        val s = Response.toString {line=line, headers=nil, body=NONE}
    in sendStrClose (sock, s)
    end

fun sendResponseClose sock resp =
    let val res = Response.toString resp
        val () = debug (fn () => "Sending response:\n" ^ res ^ "\n")
    in sendStrClose (sock, res)
    end

fun accesslog ((sock,sa,opts):conn) (req:Request.t option) (status,bytes) =
    let val logOnce =
            if #logfile opts = "stdout" then print
            else
              let val os = TextIO.openAppend (#logfile opts)
              in fn s => ( TextIO.output(os,s)
                         ; TextIO.flushOut os
                         ; TextIO.closeOut os
                         )
              end
        val (time, localoffset) = (Time.now(), Date.localOffset())
        val localoffset = IntInf.toInt(Time.toSeconds localoffset) div 60 div 60
        val localoffset = if localoffset > 12 then 24-localoffset
                          else localoffset
        val localoffset = if localoffset < 0 then
                            "-" ^ StringCvt.padLeft #"0" 4 (Int.toString (~localoffset) ^ "00")
                          else "+" ^ StringCvt.padLeft #"0" 4 (Int.toString localoffset ^ "00")
        val date = Date.fmt "%d/%b/%Y:%H:%M:%S" (Date.fromTimeLocal time)
                   ^ " " ^ localoffset
        val reqline = case req of
                          SOME r => Request.lineToString (#line r)
                        | NONE => "-"
        val st = StatusCode.toString status
        val client_in_addr = #1 (INetSock.fromAddr sa)
        val client = case NetHostDB.getByAddr client_in_addr of
                         SOME e => NetHostDB.name e
                       | NONE => NetHostDB.toString client_in_addr
    in logOnce (client ^ " - - [" ^ date ^ "] \"" ^ reqline ^ "\" " ^
                st ^ " " ^ Int.toString bytes ^ "\n")
    end

(* Receive request line and headers first - that is, read until we
   have received the characters "\r\n\r\n"; at this point, we have
   received all headers and we can now assume that there is a
   Content-Length header, if more data should be read... *)

fun recvRequestLineAndHeaders (conn:conn) :
    {line: Request.line,
     headers: (string*string) list,
     rest: CharVectorSlice.slice} =
    let val sock = #1 conn
        fun recv acc =
            let val ss = Socket.recvVec (sock, Word8Vector.maxLen)
                         |> Byte.bytesToString
                         |> SS.full
            in if SS.size ss = 0 then raise MissingConnection
               else let val (prefix,suffix) = SS.position "\r\n\r\n" ss
                    in if SS.size suffix = 0   (* we have not read the double-nl yet *)
                       then recv (ss::acc)
                       else let val prefix =
                                    let val (s,i,j) = SS.base prefix
                                    in SS.substring (s,i,j+2) (* include \r\n *)
                                    end
                                val suffix = SS.triml 4 suffix (* drop \r\n\r\n *)
                                val line_and_headers =
                                    List.rev (prefix::acc) |> SS.concat
                            in (line_and_headers,
                                suffix)
                            end
                    end
            end
        val (line_and_headers,rest) = recv nil
    in case Request.parse_line_and_headers SS.getc (SS.full line_and_headers) of
           NONE => raise BadRequest
         | SOME ((line,headers),_) => {headers=headers,
                                       line=line,
                                       rest=rest}
    end

fun headers_look_int headers k =
    Header.look headers k |> Option.mapPartial Int.fromString

fun recvN sock n acc =
    if n <= 0 then String.concat (List.rev acc)
    else let val s = Socket.recvVec (sock, n) |> Byte.bytesToString
         in recvN sock (n - size s) (s::acc)
         end

fun recvRequest (conn:conn) : ctx =
    let val {line,headers,rest} = recvRequestLineAndHeaders conn
        val body =
            case #method line of
                Request.HEAD => NONE
              | Request.GET => NONE
              | _ =>
                case headers_look_int headers "Content-Length" of
                    NONE => NONE
                  | SOME n =>
                    let val n' = n - SS.size rest
                        val s = recvN (#1 conn) n' [SS.string rest]
                    in SOME s
                    end
        val req = {line=line,headers=headers,body=body}
    in {conn=conn,req=req,resp_headers=ref nil}
    end

structure Fetch : SERVER_FETCH = struct

  fun fetchRaw {host:string, port:int, msg:string} : string option =
      case NetHostDB.getByName host of
          NONE => NONE
        | SOME e =>
          let val addr = INetSock.toAddr (NetHostDB.addr e, port)
              val bufsz = 2048
              val sock = INetSock.TCP.socket()
              fun loop acc =
                  let val v = Socket.recvVec(sock, bufsz)
                      val l = Word8Vector.length v
                  in if l < bufsz
                     then rev (v::acc)
                              |> Word8Vector.concat
                              |> Byte.bytesToString
                     else loop (v::acc)
                  end
          in ( Socket.connect (sock, addr)
             ; sendVecAll (sock, Byte.stringToBytes msg
                                 |> Word8VectorSlice.full )
             ; SOME (loop nil) before Socket.close sock
             ) handle _ => ( Socket.close sock; NONE )
          end

  fun fetch {scheme,host,port,req} =
      if scheme <> "http" then NONE
      else let val msg = Request.toString req
           in case fetchRaw {host=host,port=port,msg=msg} of
                  NONE => NONE
                | SOME s =>
                  case Response.parse SS.getc (SS.full s) of
                      SOME(r,sl) => SOME r
                    | NONE => NONE
           end

  fun fetchUrl url =
      case Uri.parse SS.getc (SS.full url) of
          SOME (Uri.URL{scheme,host,port,path,query}, sl) =>
          let val line = {method=Request.GET,
                          uri=Uri.PATH {path=path,query=query},
                          version=Version.HTTP_1_1}
              val req = {line=line, headers=[("Host",host)],
                         body=NONE}
              val port = case port of SOME p => p
                                    | NONE => 80
          in fetch {scheme=scheme,host=host,port=port,req=req}
          end
        | _ => NONE
end

structure Resp : SERVER_RESP = struct

  type ctx = ctx
  type filepath = string
  type sc = Http.StatusCode.t

  fun addHeader (ctx:ctx) p =
      #resp_headers ctx := (p :: (!(#resp_headers ctx)))

  fun sendLine (ctx:ctx) sc : unit =
      let val line = {version=Version.HTTP_1_1, status=sc}
          val resp = {line=line, headers=rev(!(#resp_headers ctx)), body=NONE}
      in accesslog (#conn ctx) (SOME(#req ctx)) (sc, 0)
       ; sendResponseClose (#1(#conn ctx)) resp
      end

  fun send (ctx:ctx) (sc:Http.StatusCode.t, body:string) : unit =
      let val bytes = size body
          val line = {version=Version.HTTP_1_1, status=sc}
          val () = addHeader ctx ("Content-Length",Int.toString bytes)
          val resp = {line=line, headers=rev(!(#resp_headers ctx)), body=SOME body}
      in accesslog (#conn ctx) (SOME(#req ctx)) (sc, bytes)
       ; sendResponseClose (#1(#conn ctx)) resp
      end

  fun sendOK ctx body =
      send ctx (StatusCode.OK, body)

  fun sendRedirectSC (ctx:ctx) (sc:Http.StatusCode.t, loc:string) : unit =
      let val line = {version=Version.HTTP_1_1, status=sc}
          val () = addHeader ctx ("Location",loc)
          val resp = {line=line, headers=rev(!(#resp_headers ctx)), body=NONE}
      in accesslog (#conn ctx) (SOME(#req ctx)) (sc, 0)
       ; sendResponseClose (#1(#conn ctx)) resp
      end

  fun sendRedirect ctx loc =
      sendRedirectSC ctx (Http.StatusCode.Redirect,loc)

  fun write (ctx:ctx) (s:string) : unit =
      let val slc = Word8VectorSlice.full (Byte.stringToBytes s)
          val sock = #1(#conn ctx)
      in sendVecAll (sock, slc)
      end handle _ => raise MissingConnection

  fun logAndClose (ctx:ctx) (sc:Http.StatusCode.t, bytes:int) : unit =
      let val sock = #1(#conn ctx)
      in accesslog (#conn ctx) (SOME(#req ctx)) (sc, bytes)
       ; Socket.close sock
      end

  fun maybeAddEncoding mts =
      let fun add () =
              let val enc =
                      case Info.configGetValue "standardFileEncoding" of
                          SOME enc => enc
                        | NONE => "ISO-8859-1"
              in mts ^ "; charset=" ^ enc
              end
      in case mts of
             "text/html" => add()
           | "text/plain" => add()
           | "application/xhtml+xml" => add()
           | _ => mts
      end

  fun setContentTypeStr ctx mts =
      let val mts = maybeAddEncoding mts
      in addHeader ctx ("Content-Type", mts)
      end

  fun setContentType ctx mt =
      setContentTypeStr ctx (Http.Mime.toString mt)

  fun sendHtml ctx (sc,body) : unit =
      ( setContentTypeStr ctx "text/html"
      ; addHeader ctx ("Cache-Control", "no-store")
      ; send ctx (sc,body)
      )

  fun sendHtmlOK ctx body : unit =
      sendHtml ctx (Http.StatusCode.OK,body)

  fun sendXhtml ctx (sc,body) : unit =
      ( setContentTypeStr ctx "application/xhtml+xml"
      ; addHeader ctx ("Cache-Control", "no-store")
      ; send ctx (sc,body)
      )

  fun sendXhtmlOK ctx body : unit =
      sendXhtml ctx (Http.StatusCode.OK,body)

  fun sendBinary ctx body : unit =
      ( setContentTypeStr ctx "application/octet-stream"
      ; addHeader ctx ("Cache-Control", "no-cache")
      ; sendOK ctx body
      )

  fun readBinFile f =
      let val is = BinIO.openIn f
      in let val s = BinIO.inputAll is
         in BinIO.closeIn is
          ; SOME (Byte.bytesToString s)
         end handle ? => (BinIO.closeIn is; raise ?)
      end handle _ => NONE

  fun sendFileMimeStr (ctx:ctx) mts fp : unit =
      if String.isSubstring ".." fp
         orelse String.isSubstring "//" fp
         orelse size fp = 0
         orelse String.sub(fp,0) <> #"/"
      then sendLine ctx Http.StatusCode.Forbidden
      else
        let val fp = String.extract(fp,1,NONE)
        in case readBinFile fp of
               SOME s => ( setContentTypeStr ctx mts
                         ; sendOK ctx s )
             | NONE => sendLine ctx Http.StatusCode.NotFound
        end

  fun sendFileMime ctx mt fp =
      sendFileMimeStr ctx (Http.Mime.toString mt) fp

  fun sendFile ctx fp =
      let val mts =
              case OS.Path.ext fp of
                  SOME x =>
                  (case Http.Mime.fromExt x of
                       SOME mt =>
                       let val mt = Http.Mime.toString mt
                       in maybeAddEncoding mt
                       end
                     | NONE => "application/octet-stream")
                | NONE => "application/octet-stream"
      in sendFileMimeStr ctx mts fp
      end
end

structure Cookie : SERVER_COOKIE = struct
  type ctx = ctx
  fun getCookies (ctx:ctx) = Cookie.getCookies (#headers(#req ctx))
  fun getCookie ctx = Cookie.getCookie (getCookies ctx)
  fun getCookieValue ctx = Cookie.getCookieValue (getCookies ctx)
  type cookiedata = Cookie.cookiedata
  fun setCookie ctx = Cookie.setCookie (Resp.addHeader ctx)
  fun deleteCookie ctx = Cookie.deleteCookie (Resp.addHeader ctx)
end

structure Conn : SERVER_CONN = struct
  type ctx = ctx
  fun sock (c:ctx) = #1(#conn c)

  fun peer (c:ctx) : string =
      ( sock c |> Socket.Ctl.getPeerName
               |> (NetHostDB.toString o #1 o INetSock.fromAddr)
      ) handle _ => raise MissingConnection

  fun peerPort (c:ctx) : int =
      ( sock c |> Socket.Ctl.getPeerName
               |> (#2 o INetSock.fromAddr)
      ) handle _ => raise MissingConnection

  fun port (c:ctx) : int =
      sock c |> Socket.Ctl.getSockName
             |> (#2 o INetSock.fromAddr)

  fun host (c:ctx) : string =
      sock c |> Socket.Ctl.getSockName
             |> (NetHostDB.toString o #1 o INetSock.fromAddr)

  fun server () = NetHostDB.getHostName()

  fun connected (c:ctx) =
      ( sock c |> Socket.Ctl.getPeerName |> (fn _ => true)
      ) handle _ => false
end

(** The server code **)

type 'db handler = conn * 'db -> unit

fun runHandler (conn:conn) (db:'db) (handler:'db handler) =
    let fun sendSC sc = ( accesslog conn NONE (sc,0)
                        ; sendStatusCodeClose (#1 conn) sc)
    in handler (conn,db)
       handle BadRequest => sendSC StatusCode.BadRequest
            | MissingConnection => ()
            | _ => sendSC StatusCode.InternalServerError
    end

fun acceptLoop (opts:opts) serv (db:'db) (handler:'db handler) : unit =
    let val (sock, sa) = Socket.accept serv
        val () = debug (fn () => "Accepted a connection...\n")
        val conn : conn = (sock,sa,opts)
        val () = runHandler conn db handler
    in acceptLoop opts serv db handler
    end

fun serve (opts:opts) (port:int)
          (connectdb: unit -> 'db)
          (handler:'db handler) : unit =
    let val sock = INetSock.TCP.socket()
    in Socket.Ctl.setREUSEADDR (sock, true);
       Socket.bind(sock, INetSock.any port);
       Socket.listen(sock, 5);
       print ("HTTP/1.1 server started on port " ^ Int.toString port ^ "\n");
       print ("Use C-c to exit the server loop...\n");
       acceptLoop opts sock (connectdb()) handler
    end

val version = "v0.0.1"
val portDoc = ["Start the web server on port N."]
val logDoc = ["Log messages to file S. The default is 'stdout'."]
val confDoc = ["Read configuration parameters from the file S."]

fun startConnect (connectdb: unit -> 'db)
                 (handler: 'db handler) : unit =
    let
      val getPort : unit -> int =
          CmdArgs.addInt ("port", 8000, SOME portDoc)

      val getLog : unit -> string =
          CmdArgs.addString ("log", "stdout", SOME logDoc)

      val getConf : unit -> string =
          CmdArgs.addString ("conf", "", SOME confDoc)

      val () = CmdArgs.addUsage ("help", "option...")
      val () = CmdArgs.addVersion ("version", "sml-server " ^ version)

    in case CmdArgs.processOptions() of
           nil => let val opts : opts = {logfile=getLog()}
                  in if Info.setAndLoadConfigFile (getConf()) then
                       serve opts (getPort()) connectdb handler
                     else OS.Process.exit OS.Process.failure
                  end
        | _ => CmdArgs.printUsageExit OS.Process.failure
    end

fun start (h: conn -> unit) : unit =
    startConnect (fn () => ()) (fn (c,()) => h c)

end

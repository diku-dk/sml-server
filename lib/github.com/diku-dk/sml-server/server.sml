
structure Server :> SERVER =
struct

fun debug f = ()

infix |>
fun x |> f = f x

exception InternalServerError
exception BadRequest
exception MissingConnection

open Http

type opts = {logfile:string}

type conn = Socket.active Socket.stream INetSock.sock * INetSock.sock_addr * opts
type header = string * string
type ctx = {conn:conn, req:Request.t, resp_headers:header list ref}

structure Serialize : SERVER_SERIALIZE = ServerSerialize

structure Util : SERVER_UTIL = ServerUtil

type filepath = string

structure Req : SERVER_REQ = struct

  type ctx = ctx
  fun full (ctx:ctx) = #req ctx

  fun method (ctx:ctx) : Request.method =
      #method(#line(full ctx))

  fun path (ctx:ctx) : string =
      case #uri (#line (full ctx)) of
          Http.Uri.PATH {path,...} => path
        | Http.Uri.URL {path,...} => path
        | Http.Uri.AST => "*"

  fun query_of_uri (Uri.URL {query,...}) = query
    | query_of_uri (Uri.PATH {query,...}) = query
    | query_of_uri Uri.AST = ""

  fun query (ctx:ctx) (k:string) : string option =
      let val query = query_of_uri (#uri(#line(full ctx)))
          val tokens = String.tokens (fn c => c = #"&") query
          val pairs = List.foldr (fn (t,acc)=>
                                     case String.tokens (fn c => c = #"=") t of
                                         [k,v] => (k,v)::acc
                                       | nil => acc
                                       | [k] => (k,"")::acc
                                       | k :: _ => (k,String.extract(t,size k,NONE)
                                                      handle _ => "")::acc)
                                 nil tokens
      in case List.find (fn (x,_) => x=k) pairs of
             SOME (_,y) => SOME y
           | NONE => NONE
      end

  fun headers (ctx:ctx) : (string * string) list =
      #headers(full ctx)

  fun header (ctx:ctx) (k:string) : string option =
      case List.find (fn (x,_) => x=k) (headers ctx) of
          SOME (_,y) => SOME y
        | NONE => NONE

  fun host (ctx:ctx) : string =
      case header ctx "Host" of
          SOME v => v
        | NONE => ""

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
    end

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

fun recvVecAll sock =
    let val vec = Socket.recvVec (sock, CharVector.maxLen)
        val () = debug (fn () => "received vector of size " ^
                                 Int.toString (Word8Vector.length vec) ^ "\n")
    in vec
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
        val username = "frank"
    in logOnce (client ^ " - - [" ^ date ^ "] \"" ^ reqline ^ "\" " ^
                st ^ " " ^ Int.toString bytes ^ "\n")
    end

fun recvRequest (conn as (sock,sa,opts):conn) : ctx =
    let val s = Byte.bytesToString (recvVecAll sock)
    in case Request.parse CharVectorSlice.getItem (CharVectorSlice.full s) of
           SOME (req, s) => if CharVectorSlice.isEmpty s
                            then {conn=conn, req=req, resp_headers=ref nil}
                            else raise BadRequest
         | NONE => raise BadRequest
    end

structure Resp : SERVER_RESP = struct
  type ctx = ctx
  type filepath = string
  type sc = Http.StatusCode.t
  fun addHeader (ctx:ctx) p =
      #resp_headers ctx := (p :: (!(#resp_headers ctx)))
  fun sendOK (ctx:ctx) (body:string) : unit =
      let val bytes = size body
          val line = {version=Version.HTTP_1_1, status=StatusCode.OK}
          val () = addHeader ctx ("Context-Length",Int.toString bytes)
          val resp = {line=line, headers=rev(!(#resp_headers ctx)), body=SOME body}
      in accesslog (#conn ctx) (SOME(#req ctx)) (StatusCode.OK, bytes)
       ; sendResponseClose (#1(#conn ctx)) resp
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

  fun qq s = "'" ^ s ^ "'"

  fun readFile f =
      let val is = TextIO.openIn f
      in let val s = TextIO.inputAll is
         in TextIO.closeIn is
          ; s
         end handle ? => (TextIO.closeIn is; raise ?)
      end handle _ => raise Fail ("failed to read configuration file " ^ qq f)

  fun trim s = Substring.full s
            |> Substring.dropl Char.isSpace
            |> Substring.dropr Char.isSpace
            |> Substring.string
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

(** The server code **)

type 'db handler = conn * 'db -> unit

fun runHandler (conn:conn) (db:'db) (handler:'db handler) =
    let fun sendSC sc = ( accesslog conn NONE (sc,0)
                        ; sendStatusCodeClose (#1 conn) sc)
    in handler (conn,db)
       handle BadRequest => sendSC StatusCode.BadRequest
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

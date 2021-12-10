
structure Server :> SERVER =
struct

fun debug f = ()

open Http

type opts = {logfile:string}

type conn = Socket.active Socket.stream INetSock.sock * INetSock.sock_addr * opts
type header = string * string
type ctx = {conn:conn, req:Request.t, resp_headers:header list ref}
type handler = conn -> unit

type filepath = string

fun request (ctx:ctx) = #req ctx

fun req_path (ctx:ctx) : string =
    case #uri (#line (request ctx)) of
        Http.Uri.PATH {path,...} => path
      | Http.Uri.URL {path,...} => path
      | Http.Uri.AST => "*"

fun req_method (ctx:ctx) : Request.method =
    #method(#line(request ctx))

fun req_header (ctx:ctx) (k:string) : string option =
    case List.find (fn (x,_) => x=k) (#headers(request ctx)) of
        SOME (_,y) => SOME y
      | NONE => NONE

fun query_of_uri (Uri.URL {query,...}) = query
  | query_of_uri (Uri.PATH {query,...}) = query
  | query_of_uri Uri.AST = ""

fun req_query (ctx:ctx) (k:string) : string option =
    let val query = query_of_uri (#uri(#line(request ctx)))
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

fun add_header (ctx:ctx) p =
    #resp_headers ctx := (p :: (!(#resp_headers ctx)))

fun fromString p s =
    case p CharVectorSlice.getItem (CharVectorSlice.full s) of
        SOME (v, s) =>
        if CharVectorSlice.isEmpty s then v
        else raise Fail "expecting empty slice"
      | NONE => raise Fail "parsing failed"

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

exception BadRequest

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

fun sendOK (ctx:ctx) (body:string) : unit =
    let val bytes = size body
        val line = {version=Version.HTTP_1_1, status=StatusCode.OK}
        val () = add_header ctx ("Context-Length",Int.toString bytes)
        val resp = {line=line, headers=rev(!(#resp_headers ctx)), body=SOME body}
    in accesslog (#conn ctx) (SOME(#req ctx)) (StatusCode.OK, bytes)
     ; sendResponseClose (#1(#conn ctx)) resp
    end

fun runHandler (conn:conn) handler =
    let fun sendSC sc = ( accesslog conn NONE (sc,0)
                        ; sendStatusCodeClose (#1 conn) sc)
    in handler conn
       handle BadRequest => sendSC StatusCode.BadRequest
            | _ => sendSC StatusCode.InternalServerError
    end

fun acceptLoop (opts:opts) serv handler : unit =
    let val (sock, sa) = Socket.accept serv
        val () = debug (fn () => "Accepted a connection...\n")
        val conn : conn = (sock,sa,opts)
        val () = runHandler conn handler
    in acceptLoop opts serv handler
    end

fun serve (opts:opts) (port:int) (handler:handler) : unit =
    let val sock = INetSock.TCP.socket()
    in Socket.Ctl.setREUSEADDR (sock, true);
       Socket.bind(sock, INetSock.any port);
       Socket.listen(sock, 5);
       print ("HTTP/1.1 server started on port " ^ Int.toString port ^ "\n");
       print ("Use C-c to exit the server loop...\n");
       acceptLoop opts sock handler
    end

val version = "v0.0.1"
val portDoc = ["Start the web server on port N."]
val logDoc = ["Log messages to file S. The default is stdout."]

fun start (handler:handler) : unit =
    let
      val getPort : unit -> int =
          CmdArgs.addInt ("port", 8000, SOME portDoc)

      val getLog : unit -> string =
          CmdArgs.addString ("log", "stdout", SOME logDoc)

      val () = CmdArgs.addUsage ("help", "option...")
      val () = CmdArgs.addVersion ("version", "sml-server " ^ version)

    in case CmdArgs.processOptions() of
           nil => let val opts : opts = {logfile=getLog()}
                  in serve opts (getPort()) handler
                  end
        | _ => CmdArgs.printUsageExit OS.Process.failure
    end


end

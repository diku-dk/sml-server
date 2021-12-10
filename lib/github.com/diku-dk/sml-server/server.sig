
signature SERVER = sig

(*
  structure Conn     : SERVER_CONN
  structure Cookie   : SERVER_COOKIE
  structure Info     : SERVER_INFO

  val encodeUrl      : string -> string
  val decodeUrl      : string -> string
  val buildUrl       : string -> (string * string) list -> string
*)

  type conn   (* connection socket *)
  type ctx    (* context including parsed request and state *)

  type filepath = string

  exception BadRequest
  val recvRequest  : conn -> ctx                       (* May raise BadRequest *)
  val request      : ctx -> Http.Request.t
  val sendOK       : ctx -> string -> unit
(*
  val sendRedirect : ctx -> string -> unit
  val sendFileMime : ctx -> Http.Mime.t -> filepath -> unit  (* May raise Fail *)
  val sendFile     : ctx -> filepath -> unit
*)
  val req_path     : ctx -> string
  val req_method   : ctx -> Http.Request.method
  val req_header   : ctx -> string -> string option
  val req_query    : ctx -> string -> string option

  val add_header    : ctx -> string * string -> unit

  val start        : (conn -> unit) -> unit

end


(**

Description:

[returnRedirect loc] sends redirection HTTP response to client (status
code 302), with information that the client should request location
loc. May raise MissingConnection.

[encodeUrl s] returns an encoded version of the argument s as URL
query data. All characters except the alphanumerics are encoded as
specified in RFC1738, Uniform Resource Locators.  This function can be
used to append arguments to a URL as query data following a `?'.

[decodeUrl s] decodes data s that was encoded as URL query data. The
decoded data is returned.

[returnFileMime mimetype file] returns the entire contents of the
given file to the client. In addition to setting the HTTP status
response line to 200 and the Content-Type header from the given
parameter, the function also uses the stat system call to generate the
appropriate Last-Modified and Content-Length headers. May raise
Fail(msg) if file cannot be accessed.

[returnFile file] as returnFileMime, but gets the Content-Type
(mimetype) argument from calling the function Web.Mime.getMime with
the given file as parameter.

[buildUrl u l] constructs a link to the URL u with the form variable
pairs l appended to u?, delimited by &, and with the form values URL
encoded.

[run handler] starts a webserver. It reads configuration arguments
from the command line according to the following specification:

--port N : Start the web server on port N.

--log f  : Log messages to file f. The default is error.log.

--log-level N

         : Log messages according to log severity level N.

--P N    : Prefork N processes.

If handler returns a response value, the response is send to the
client. Otherwise, run will attempt to serve the request as follows:

1. If the request is a GET request for a file on the server (relative
   to the folder in which the server is started, the file is
   served. The server replies with a Bad Request response if '..'
   occurs in the file path part of the request or if the file path
   denotes a folder.

2. ... multi-part file uploads...

*)

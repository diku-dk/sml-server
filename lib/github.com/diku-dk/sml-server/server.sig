
signature SERVER = sig

(*
  structure Conn     : SERVER_CONN
  structure Cookie   : SERVER_COOKIE
  structure Info     : SERVER_INFO
*)
  val encodeUrl      : string -> string
  val decodeUrl      : string -> string
  val buildUrl       : string -> (string * string) list -> string

  type conn   (* connection socket *)
  type ctx    (* context including parsed request and state *)

  type filepath = string

  exception InternalServerError
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

[sendRedirect ctx loc] sends redirection HTTP response to client
(status code 302), with information that the client should request
location loc. May raise MissingConnection.

[encodeUrl s] returns an encoded version of the argument s as URL
query data. All characters except the alphanumerics are encoded as
specified in RFC1738, Uniform Resource Locators.  This function can be
used to append arguments to a URL as query data following a `?'.

[decodeUrl s] decodes data s that was encoded as URL query data. The
decoded data is returned.

[sendFileMime ctx mimetype file] returns the entire contents of the
given file to the client. In addition to setting the HTTP status
response line to 200 and the Content-Type header from the given
parameter, the function also uses the stat system call to generate the
appropriate Last-Modified and Content-Length headers. May raise
Fail(msg) if file cannot be accessed.

[sendFile ctx file] as sendFileMime, but gets the Content-Type
(mimetype) argument from calling the function Http.Mime.fromExt with
the given file's extension as parameter.

[buildUrl u l] adds query arguments (as they appear in l) to the base
url u by appending the the character '?' and URL-encoded versions of
the query arguments, deliminated by the character '&'.

[start handler] starts a server that will serve requests using the
supplied handler. The handler takes a connection socket as argument,
which can be used for receiving the request and for supplying a
response.

The function reads configuration arguments from the command line
according to the following specification:

--port N  : Start the web server on port N.

--log S   : Log messages to file S. The default is stdout.

--version : Print version information.

--help    : Print help information.

If handler raises the exception BadRequest or the exception
InternalServerError, the start function will take care of sending an
appropriate response to the client (status code
Http.StatusCode.BadRequest or Http.StatusCode.InternalServerError).

The connection attached to conn is closed when the handler returns.

*)

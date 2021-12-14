(** Server response functionality

This signature specifies functionality for sending responses to the
client.

*)

signature SERVER_RESP = sig

  type ctx

  type filepath = string

  type sc = Http.StatusCode.t

  val sendOK         : ctx -> string -> unit
  val sendRedirect   : ctx -> string -> unit
  val sendRedirectSC : ctx -> sc * string -> unit

  val sendHtml       : ctx -> sc * string -> unit
  val sendXhtml      : ctx -> sc * string -> unit
  val sendHtmlOK     : ctx -> string -> unit
  val sendXhtmlOK    : ctx -> string -> unit
  val sendBinary     : ctx -> string -> unit

  val write          : ctx -> string -> unit
  val logAndClose    : ctx -> sc * int -> unit

  val sendFileMime   : ctx -> Http.Mime.t -> filepath -> unit
  val sendFile       : ctx -> filepath -> unit

  val setContentType : ctx -> Http.Mime.t -> unit   (* maybe add encoding *)

  val addHeader      : ctx -> string * string -> unit

end

(**

Description:

[ctx] The context type.

[sendOK ctx s] sends the string s to the client with status code 200
(OK). Headers registered in the context (using e.g., addHeader or
setContentType) are sent along with the response. May raise
MissingConnection.

[sendRedirect ctx loc] sends redirection HTTP response to client
(status code 302), with information that the client should request
location loc. Headers registered in the context (using e.g.,
addHeader) are sent along with the response. May raise
MissingConnection.

[sendHtml ctx (sc,s)] sends the (HTML) string s with status code sc
and content-type 'text/html' to the client (set using setContentType),
including a Cache-Control header set to 'no-store'. Headers registered
in the context (using e.g., addHeader) are sent along with the
response. May raise MissingConnection.

[sendXHtml ctx (sc,s)] sends the (XHTML) string s with status code sc
and content-type 'application/xhtml+xml' to client (set using
setContentType), including a Cache-Control header set to
'no-store'. Headers registered in the context (using e.g., addHeader)
are sent along with the response. May raise MissingConnection.

[sendHtmlOK ctx s] sends the (HTML) string s with status code OK (200)
and content-type 'text/html' to the client (set using setContentType),
including a Cache-Control header set to 'no-store'. Headers registered
in the context (using e.g., addHeader) are sent along with the
response. May raise MissingConnection.

[sendBinary ctx s] sends the string s with status code OK (200) and
content-type 'application/octet-stream' to the client, including a
Cache-Control header set to 'no-cache'. Headers registered in the
context (using e.g., addHeader) are sent along with the response. May
raise MissingConnection.

[write ctx s] sends the string s to the client, excluding HTTP
headers. May raise MissingConnection.

[logAndClose ctx (sc,n)] writes an entry in the access log (status
code sc and n bytes written) before closing the socket connection.

[sendFileMime ctx mimetype file] returns the entire contents of the
given file to the client. In addition to setting the HTTP status
response line to 200 and the Content-Type header from the given
parameter, the function also uses the stat system call to generate the
appropriate Last-Modified and Content-Length headers. Headers
registered in the context (using e.g., addHeader) are sent along with
the response. May raise MissingConnection. Raises Fail(msg) if file
cannot be opened for reading.

[sendFile ctx file] as sendFileMime, but gets the Content-Type
(mimetype) argument from calling the function Http.Mime.fromExt with
the given file's extension as parameter. Headers registered in the
context (using e.g., addHeader) are sent along with the response. May
raise MissingConnection. Raises Fail(msg) if file cannot be opened for
reading.

[setContentType ctx mt] adds a content-type header to the contexts
header list. For content-types 'text/plain', 'text/html', and
'application/xhtml+xml', a charset-encoding is added to the
content-type (defaults to 'ISO-8859-1'), which can be controlled by
adding an entry with name 'standardFileEnconding' in the configuration
file.)

[addHeader ctx (n,v)] adds a header with field name n and field value
v to the context's header list. When a reponse is sent to the client
using one of the send functions above, registered headers are sent
along with the response.

*)

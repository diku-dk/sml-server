(** Server response functionality

This signature specifies functionality for sending responses to the
client.

*)

signature SERVER_RESP = sig

  type ctx

  val sendOK       : ctx -> string -> unit
(*  val sendRedirect : ctx -> string -> unit *)
(*
  val sendFileMime   : ctx -> Http.Mime.t -> filepath -> unit  (* May raise Fail *)
  val sendFile       : ctx -> filepath -> unit
  val sendHtml           : ctx -> sc * string -> unit
  val sendXhtml          : ctx -> sc * string -> unit
  val sendHtmlOK         : ctx -> string -> unit
  val sendBinary         : ctx -> string -> unit
  val sendFile           : ctx -> sc * string * string -> unit
  val write              : ctx -> string -> unit
  val sendRedirectSC     : ctx -> sc * string -> unit
  val setMimeType        : ctx -> string -> unit

*)
  val add_header    : ctx -> string * string -> unit

end

(**

Description:

[ctx] The context type.

[sendOK ctx s] sends the ]

[sendRedirect ctx loc] sends redirection HTTP response to client
(status code 302), with information that the client should request
location loc. May raise MissingConnection.

[sendFileMime ctx mimetype file] returns the entire contents of the
given file to the client. In addition to setting the HTTP status
response line to 200 and the Content-Type header from the given
parameter, the function also uses the stat system call to generate the
appropriate Last-Modified and Content-Length headers. May raise
Fail(msg) if file cannot be accessed.

[sendFile ctx file] as sendFileMime, but gets the Content-Type
(mimetype) argument from calling the function Http.Mime.fromExt with
the given file's extension as parameter.

[sendHtml (sc,s)] sends HTML string s with status code sc and
mime-type text/html to client, including HTTP headers and
Cache-Control header set to no-cache. May raise MissingConnection.

[sendXHtml (sc,s)] sends XHTML string s with status code sc and
mime-type application/xhtml+xml to client, including HTTP headers and
Cache-Control header set to must-revalidate. May raise
MissingConnection.

[sendHtmlOK s] sends HTML string s with status code OK (200) to
client, including HTTP headers. May raise MissingConnection.

[sendFile (sc,mt,f)] sends file f with status code sc to client,
including HTTP headers. The mime type is mt. Raises MissingConnection
if the execution is not associated with a connection. Raises Fail(msg)
if the file cannot be opened for reading.

[write s] sends string s to client, excluding HTTP headers. May raise
MissingConnection.

[sendRedirect loc] sends redirection HTTP response to client (status
code 302), with information that the client should request location
loc. May raise MissingConnection.

*)

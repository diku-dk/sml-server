(** Server request functionality

This signature specifies utilities for extracting data from requests.

*)

signature SERVER_REQ = sig

  type ctx

  val full        : ctx -> Http.Request.t

  val method      : ctx -> Http.Request.method
  val path        : ctx -> string

  val queryVar    : ctx -> string -> string option
  val queryVarAll : ctx -> string -> string list
  val queryVars   : ctx -> Http.Header.t list

  val postVar     : ctx -> string -> string option
  val postVarAll  : ctx -> string -> string list
  val postVars    : ctx -> Http.Header.t list

  val mpfdVar     : ctx -> string -> Http.Request.mpfd option
  val mpfdVarAll  : ctx -> string -> Http.Request.mpfd list
  val mpfdVars    : ctx -> Http.Request.mpfd list

  val getVar      : ctx -> string -> string option
  val getVarAll   : ctx -> string -> string list
  val getVars     : ctx -> Http.Header.t list

  val host        : ctx -> string
  val header      : ctx -> string -> string option
  val headers     : ctx -> Http.Header.t list

  val postData    : ctx -> string

(*
  val storeMultiformData : ctx -> string * string -> unit
*)

end

(**

Description:

[full ctx] returns the full HTTP request.

[method ctx] returns the method associated with the request (e.g., GET
or HEAD).

[path ctx] returns the path value associated with the request.

[host ctx] returns the value associated with the Host header of the
request or the empty string if no Host header is available.

[header ctx n] returns `SOME v` if v is a header value associated with
the header name `n` in the request.

[headers ctx] returns the list of all headers associated with the
request.

[queryVar ctx n] returns SOME v if v is the first value binding
present in url-encoded query data (associated with the requested path)
for which the key equal n (upto case sensitivity). Returns NONE if no
such binding exists.

[queryVarAll ctx n] returns the list of value bindings present in
url-encoded query data (associated with the requested path) for which
the key equal n (upto case sensitivity). Returns nil if no such
binding exists.

[queryVars ctx] returns the key-value bindings present in url-encoded
query data (associated with the requested path).

[postData ctx] returns the data string that follows the headers.

[postVar ctx n] returns SOME v if v is the first value binding present
in url-encoded post data for which the key equal n (upto case
sensitivity). Returns NONE if no such binding exists.

[postVarAll ctx n] returns the list of value bindings present in
url-encoded post data for which the key equal n (upto case
sensitivity). Returns nil if no such binding exists.

[postVars ctx] returns the key-value bindings present in url-encoded
post data.

[mpfdVar ctx n] returns SOME p if p is the first part in potential
multi-part form-data for which the name equal n (upto case
sensitivity). Returns NONE if no such part exists.

[mpfdVarAll ctx n] returns the list of potential multi-part form-data
for which the key equal n (upto case sensitivity). Returns nil if no
such part exists.

[mpfdVars ctx] returns the list of potential multi-part form-data.

[getVar ctx n] returns 'SOME v' if 'mfpdVar ctx n = SOME v' orelse
'postVar ctx n = SOME v' orelse 'queryVar ctx n = SOME v'. Returns
NONE, otherwise.

[getVarAll ctx n] returns 'vs' if vs<>nil andalso ('mfpdVarAll ctx n =
vs' orelse 'postVarAll ctx n = vs' orelse 'queryVarAll ctx n =
vs'). Returns 'nil', otherwise.

[getVars ctx] returns 'kvs' if kvs<>nil andalso ('mfpdVars ctx = kvs'
orelse 'postVars ctx = kvs' orelse 'queryVars ctx = kvs'). Returns
'nil', otherwise.

[storeMultiformData (fv,filename)] stores the uploaded file
represented by formvariable fv in file filename. Raises Fail if some
error occurs (e.g., filename can't be opened, fv does not exists or fv
is not an uploaded file.

*)

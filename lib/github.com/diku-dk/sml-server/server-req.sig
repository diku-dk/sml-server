(** Server request functionality

This signature specifies utilities for extracting data from requests.

*)

signature SERVER_REQ = sig

  type ctx

  val full     : ctx -> Http.Request.t

  val method   : ctx -> Http.Request.method
  val path     : ctx -> string
  val query    : ctx -> string -> string option
  val queryAll : ctx -> (string * string) list

  val host     : ctx -> string
  val header   : ctx -> string -> string option
  val headers  : ctx -> (string * string) list

  val postData : ctx -> string
(*
  val getQuery           : ctx -> (string * string) list
  val formvar            : ctx -> string -> string option
  val formvarAll         : ctx -> string -> string list
  val storeMultiformData : ctx -> string * string -> unit
*)
end

(**

Description:

[full ctx] returns the full HTTP request.

[method ctx] returns the method associated with the request (e.g., GET
or HEAD).

[path ctx] returns the path value associated with the request.

[query ctx k] returns SOME v if k is associated with v in the query
data. Returns NONE otherwise.

[host ctx] returns the value associated with the Host header of the
request or the empty string if no Host header is available.

[header ctx n] returns `SOME v` if v is a header value associated with
the header name `n` in the request.

[headers ctx] returns the list of all headers associated with the
request.

[getQuery()] constructs and returns a set representing the query data
associated with the connection. It reads the POST content or the query
string. The POST content takes precedence over the query string.

[formvar k] returns the first query data associated with the key k;
the function returns NONE if no query data is present for the argument
key k.

[formvarAll k] returns all values associated with key k in the query
data; the function returns the empty list if no query data is present
for the argument key k.

[storeMultiformData (fv,filename)] stores the uploaded file
represented by formvariable fv in file filename. Raises Fail if some
error happens (e.g., filename can't be opened, fv does not exists or
fv is not an uploaded file.

*)

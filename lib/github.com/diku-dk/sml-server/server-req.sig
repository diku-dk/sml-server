(** Server request functionality

This signature specifies utilities for extracting data from requests.

*)

signature SERVER_REQ = sig

  type ctx

  val full          : ctx -> Http.Request.t

  val method        : ctx -> Http.Request.method
  val path          : ctx -> string
  val query         : ctx -> string -> string option
  val queryAll      : ctx -> Http.Header.t list

  val host          : ctx -> string
  val header        : ctx -> string -> string option
  val headers       : ctx -> Http.Header.t list

  val postData      : ctx -> string

  val postQueryAll  : ctx -> Http.Header.t list

  val getPostVar    : ctx -> string -> string option
  val getPostVarInt : ctx -> string -> int option

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

[queryAll ctx] returns the key-value bindings provided as the query
element of a request.

[host ctx] returns the value associated with the Host header of the
request or the empty string if no Host header is available.

[header ctx n] returns `SOME v` if v is a header value associated with
the header name `n` in the request.

[headers ctx] returns the list of all headers associated with the
request.

[postData ctx] returns the data string that follows the headers.

[postQueryAll ctx] returns the key-value bindings present in the post
data string.

[getQuery()] returns the key-value bindings present in the post data
string if available. Otherwise, the function returns the key-value
bindings in the path-query string, if available.

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

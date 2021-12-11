(** Cookie support

This signature specifies functionality for receiving cookie
information from clients (by reading header information in client
requests) and for adding and deleting cookies on clients by adding
header specifications to future client responses.

*)

signature SERVER_COOKIE = sig

  type ctx

  val getCookies     : ctx -> (string * string) list
  val getCookie      : ctx -> string -> (string * string) option
  val getCookieValue : ctx -> string -> string option

  type cookiedata = {name   : string,
		     value  : string,
		     expiry : Date.date option,
		     domain : string option,
		     path   : string option,
		     secure : bool}

  val setCookie      : ctx -> cookiedata -> unit
  val deleteCookie   : ctx -> {name: string, path: string option} -> unit

end

(**

Description:

[getCookies ctx] returns a list [(n1,v1), (n2,v2), ..., (nm,vm)] of
all the name=value pairs of cookies received from the client.

[getCookie ctx cn] returns SOME(cn,v) if 'cn=v' is a cookie received
from the client; returns NONE otherwise.

[getCookieValue ctx cn] returns SOME(v) where v is the value associated
with the cookie cn, if any; otherwise returns NONE.

[cookiedata] type of cookie data used for setting a cookie on a
client.

[setCookie ctx {name,value,expiry,domain,path,secure}] adds a header
(to the context ctx) that has the effect of defines a client cookie
with the given name, value, expiry date, domain, path, and security
level.  The added header is sent to the client as part of a HTTP
response (send using, e.g., sendOK).

[deleteCookie ctx {name,path}] adds a header (to the context ctx) that
has the effect of deleting a client cookie defined by the given name
and the given path. The added header is sent to the client as part of
a HTTP response (send using, e.g., sendOK).

*)

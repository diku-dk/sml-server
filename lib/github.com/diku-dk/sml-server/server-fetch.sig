(** Support for making HTTP requests

This signature specifies functionality for fetching data from other
sites, also termed "scraping", using HTTP requests.

*)

signature SERVER_FETCH = sig

  val fetch        : {scheme: string, host: string, port: int,
                      req: Http.Request.t} -> Http.Response.t option

  val fetchUrl     : string -> Http.Response.t option

  val fetchRaw     : {host: string, port: int, msg: string}
                     -> string option

end

(**

Description:

[fetch {scheme, host, port, req}] attempts at opening a TCP/IP socket
connection to host on the specified port. On success, it attempts to
send the request specified by req. If this succeeds, the function
waits for a response. The function returns SOME r if r is a
successfully returned and HTTP response. On failure, the function
returns NONE. The function may block until a response is successfully
obtained or return NONE if the socket is timing out. Only the scheme
'http' is supported.

[fetchUrl url] is identical to calling fetch with a GET request object
and the other record data (including other data in the request object)
extracted from the url string. The function returns NONE if url is not
a well-formed url address.

[fetchRaw {host,port,msg}] returns a successful response as a raw
string. The socket is opened on the address defined by host and
port. Only the string msg is send on the socket.

*)

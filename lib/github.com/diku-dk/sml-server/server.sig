
signature SERVER = sig

  type conn   (* connection socket *)
  type ctx    (* context including parsed request and state *)

  exception MissingConnection
  exception InternalServerError
  exception BadRequest

  structure Util      : SERVER_UTIL
  structure Serialize : SERVER_SERIALIZE
  structure Cookie    : SERVER_COOKIE     where type ctx = ctx
  structure Req       : SERVER_REQ        where type ctx = ctx
  structure Resp      : SERVER_RESP       where type ctx = ctx
  structure Conn      : SERVER_CONN       where type ctx = ctx
  structure Info      : SERVER_INFO
  structure Fetch     : SERVER_FETCH

  val recvRequest  : conn -> ctx          (* May raise BadRequest *)

  val start        : (conn -> unit) -> unit

  val startConnect : (unit -> 'db) -> (conn * 'db -> unit) -> unit
end

(**

Description:

[start handler] starts a server that will serve requests using the
supplied handler. The handler takes a connection socket as argument,
which can be used for receiving the request and for supplying a
response.

The function reads configuration arguments from the command line
according to the following specification:

--port N  : Start the HTTP server on port N.

--log S   : Log access messages to file S. The default is stdout.

--conf S  : Read configuration parameters from file S. Configuration
            parameters can be accessesd using functionality in the
            Info structure.

--version : Print version information.

--help    : Print help information.

If handler raises the exception BadRequest or the exception
InternalServerError, the start function will take care of sending an
appropriate response to the client (status code
Http.StatusCode.BadRequest or Http.StatusCode.InternalServerError).

The connection attached to conn is closed when the handler returns.

[startConnect connectf handler] starts a server that will serve
requests using the supplied handler. The startConnect function first
makes a call to the connectf function (e.g., to create a database
connection) before it starts processing HTTP requests using the
handler function, which besides from its connection socket argument
also takes as argument the result of the call to the connectf function
(e.g., the database connection).

*)

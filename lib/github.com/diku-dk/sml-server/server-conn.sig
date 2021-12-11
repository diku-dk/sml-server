(** Connection functionality

This signature specifies functionality for obtaining
connection-specific information and information about the host.

*)

signature SERVER_CONN = sig

  type ctx

  val host               : ctx -> string
  val port               : ctx -> int

  val peer               : ctx -> string
  val peerPort           : ctx -> int
  val connected          : ctx -> bool

  val server             : unit -> string

end

(**

Description:

[ctx] The context type.

[host ctx] returns the server hostname associated with the connection.

[port ctx] returns the server port number associated with the
connection.

[peer ctx] returns the name of the peer associated with the
connection. Typically, it is a dotted IP address, for example,
199.221.53.205, but this is not guaranteed. May raise
MissingConnection.

[peerPort ctx] returns the port from which the peer is connected. May
raise MissingConnection.

[connected ctx] returns true if a connection is available. Returns
false otherwise. This function may be used to protect execution of
code that requires a connection (e.g., execution of library code).

[server ()] returns the name of the server running the application.

*)

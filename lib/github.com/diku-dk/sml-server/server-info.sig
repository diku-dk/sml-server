(** General server information and configuration file functionality

This signature specifies functionality for querying information about
the underlying server process and for accessing configuration file
settings.

*)

signature SERVER_INFO = sig

  val hostname       : unit -> string
  val pid            : unit -> int
  val username       : unit -> string
  val uptimeProcess  : unit -> Time.time

  val configGetValue : string -> string option
  val configFile     : unit -> string option
  val configValues   : unit -> (string * string) list

end

(**

Description:

[hostname()] returns the host name of the machine.

[pid()] returns the process id of the server process.

[username()] returns the username of the server process.

[uptimeProcess()] returns the time the server process has been
running.

[configGetValue key] returns SOME v if v is associated with the key
`k` in the configuration file (specified using the --conf command-line
argument to the server program). Returns NONE otherwise.

[configFile()] returns the location of the configuration file if such
a file has been passed as a command-line argument to the server
program, using the --conf option.

[configValues()] returns the set of key-value pairs defined in the
configuration file, if such a file has been specified as a
command-line argument to the server program, using the --conf
option.

*)

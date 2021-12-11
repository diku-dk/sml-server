(** Server-related utility functions

This signature specifies various utility functionality.

*)

signature SERVER_UTIL = sig

  val encodeUrl  : string -> string
  val decodeUrl  : string -> string
  val buildUrl   : string -> (string * string) list -> string

end

(**

Description:

[encodeUrl s] returns an encoded version of the argument s as URL
query data. All characters except the alphanumerics are encoded as
specified in RFC1738, Uniform Resource Locators.  This function can be
used to append arguments to a URL as query data following a `?'.

[decodeUrl s] decodes data s that was encoded as URL query data. The
decoded data is returned.

[buildUrl u l] adds query arguments (as they appear in l) to the base
url u by appending the the character '?' and URL-encoded versions of
the query arguments, deliminated by the character '&'.

*)

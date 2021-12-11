(** Type-indexed serialization

This signature specifies type-indexed serialization support, which is
useful, for instance, for data caching.

*)

signature SERVER_SERIALIZE = sig

  type 'a t = {name: string,
	       to_string: 'a -> string,
	       from_string: string -> 'a}

  val pair    : 'a t -> 'b t -> ('a * 'b) t
  val option  : 'a t -> 'a option t
  val list    : 'a t -> 'a list t
  val triple  : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

  val unit    : unit t
  val int     : int t
  val real    : real t
  val bool    : bool t
  val char    : char t
  val string  : string t
  val time    : Time.time t

end

(**

Description:

['a t] generic type, which is indexed over the type of values to be
serialized and deserialized.

[pair ta tb] returns a type-indexed witness for serializing pairs of
values.

[option t] returns a type-indexed witness for serializing optional
values.

[list t] returns a type-indexed witness for serializing lists of
values.

[triple ta tb tc] returns a type-indexed witness for serializing
triples of values.

[unit] predefined type-indexed witness for serializing unit values.

[int] predefined type-indexed witness for serializing integers.

[real] predefined type-indexed witness for serializing reals.

[bool] predefined type-indexed witness for serializing booleans.

[char] predefined type-indexed witness for serializing characters.

[string] predefined type-indexed witness for serializing strings.

[time] predefined type-indexed witness for serializing values of type
Time.time.

*)

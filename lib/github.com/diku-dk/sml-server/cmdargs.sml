(** Library for managing commandline arguments and for printing
    usage information.

    Commandline arguments are comprised of commandline options, which
    can be both nullary (taking zero parameters) and unary (taking
    single parameters). Nullary commandline options are also referred
    to as flags.

    Users of the library specify possible commandline options using
    the add* functions, each of which is given a name for the
    commandline option, a default value for the option, and optional
    documentation for the option.

    The functions addUsage and addVersion adds usage information and
    version information to the system.

    The function processOptions is used to process the options and
    returns the non-option arguments. If the version flag (e.g.,
    "version") or usage flag (e.g., "help") is provided as flags,
    usage or version information is printed and the process exits.
*)

signature CMD_ARGS = sig
  type 'a getter = unit -> 'a
  type doc = string list

  val addInt     : string * int * doc option -> int getter
  val addReal    : string * real * doc option -> real getter
  val addBool    : string * bool * doc option -> bool getter
  val addString  : string * string * doc option -> string getter
  val addFlag    : string * doc option -> bool getter

  val addUsage   : (string * string) -> unit   (* e.g., ("help", "options... files") *)
  val addVersion : string * string -> unit     (* e.g., ("version", "v2.0.1") *)

  val processOptions : unit -> string list

  val printUsageExit : OS.Process.status -> unit   (* print usage and exit with error code *)
end

structure CmdArgs :> CMD_ARGS = struct

(* Some utility functions *)
fun string_to_real s : real option =
    let fun getC i =
            SOME(String.sub(s,i),i+1)
            handle _ => NONE
    in if CharVector.all (fn c => not(Char.isSpace c) andalso c <> #"~") s then
         case Real.scan getC 0 of
             SOME (r,i) => if i = size s then SOME r else NONE
           | NONE => NONE
       else NONE
    end

fun string_to_int s : int option =
    let fun getC i =
            SOME(String.sub(s,i),i+1)
            handle _ => NONE
    in if CharVector.all (fn c => not(Char.isSpace c) andalso c <> #"~") s then
         case Int.scan StringCvt.DEC getC 0 of
             SOME (n,i) => if i = size s then SOME n else NONE
           | NONE => NONE
       else NONE
    end

fun string_to_bool s =
    case s of
        "true" => SOME true
      | "false" => SOME false
      | _ => NONE

fun int_to_string i =
    CharVector.map (fn #"~" => #"-" | c => c) (Int.toString i)

fun real_to_string r =
    CharVector.map (fn #"~" => #"-" | c => c) (Real.toString r)

fun bool_to_string b =
    Bool.toString b

(* End of utilities *)

type 'a getter = unit -> 'a
type doc = string list

val cmdargs = CommandLine.arguments()                              (* given on the commandline *)
val options : (string * doc * string option) list ref = ref nil    (* specified by the program *)

fun lookoptions s =
    List.find (fn (a,_,_) => "--" ^ a = s) (!options)

fun lookarg s xs =
    case xs of
        x :: (ys as y :: _) => if "--" ^ s = x then SOME y
                               else lookarg s ys
      | _ => NONE

fun addX (string_to_v: string -> 'v option, v_to_string: 'v -> string)
         (s:string, v:'v, docopt:doc option) : 'v getter =
    let val doc = Option.getOpt(docopt,nil)
        val () = options := (s,doc,SOME(v_to_string v)) :: (!options)
        val v = case lookarg s cmdargs of
                    SOME value =>
                    (case string_to_v value of
                         SOME v => v
                       | NONE => raise Fail ("invalid value (" ^ value ^ ") found for option --" ^ s))
                  | NONE => v
    in fn () => v
    end

val addInt : string * int * doc option -> int getter =
    addX (string_to_int, int_to_string)

val addReal : string * real * doc option -> real getter =
    addX (string_to_real, real_to_string)

val addBool : string * bool * doc option -> bool getter =
    addX (string_to_bool, bool_to_string)

val addString : string * string * doc option -> string getter =
    addX (fn s => SOME s, fn s => s)

fun addFlag (s:string, docopt:doc option) : bool getter =
    let val doc = Option.getOpt(docopt,nil)
        val () = options := (s,doc,NONE) :: (!options)
        val v = List.exists (fn k => "--" ^ s = k) cmdargs
    in fn () => v
    end

val usage : (string*string) ref = ref ("","")
val version = ref ("","")
fun println s = print (s ^ "\n")

fun indent n s =
    if n <= 0 then s
    else " " ^ indent (n-1) s

fun rightPad n s = StringCvt.padRight #" " n s

fun leftPad n s = StringCvt.padLeft #" " n s

fun printUsageExit ecode =
    let val n = CommandLine.name()
        val d = List.foldl
                    (fn ((s,doc,NONE),d) =>
                        ("--" ^ s :: map (indent 2) doc @ d)
                      | ((s,doc,SOME def),d) =>
                        let val parName = case Int.fromString def of
                                              SOME _ => "N"
                                            | _ =>
                                              case Bool.fromString def of
                                                  SOME _ => "B"
                                                | NONE => "S"
                        in
                          (rightPad 39 ("--" ^ s ^ " " ^ parName) ^ " " ^ leftPad 38 ("(" ^ def ^ ")") ::
                           map (indent 2) doc @ d)
                        end)
                    []
                    (!options)
        val d =
            ["Usage: " ^ n ^ " " ^ #2(!usage),
             "",
             "Options:",
             ""] @ d
    in List.app println d
     ; OS.Process.exit ecode
    end

fun addUsage (s,h) : unit =
    ( usage := (s,h)
    ; addFlag (s, SOME["Print usage information and exit."])
    ; ()
    )

fun addVersion (s,v) =
    ( version := (s,v)
    ; addFlag (s, SOME["Print version information and exit."])
    ; ()
    )

fun processOptions () : string list =
    let fun process args =
            case args of
                arg::rest =>
                if String.isPrefix "--" arg then
                  (case lookoptions arg of
                       SOME(x, _, NONE) =>
                       if x = #1(!version) then
                         ( println (#2(!version))
                         ; OS.Process.exit OS.Process.success
                         )
                       else if x = #1(!usage) then
                         printUsageExit OS.Process.success
                       else process rest
                     | SOME(_, _, SOME _) =>
                       (case rest of
                            par :: rest =>
                            if String.isPrefix "-" par then
                              raise Fail ("expects parameter to option " ^ arg ^ " - got " ^ par)
                            else process rest
                          | _ => raise Fail ("expects parameter to option " ^ arg ^ " - got nothing"))
                     | NONE => raise Fail ("unknown option " ^ arg))
                else arg::rest
              | nil => nil
        val args = process cmdargs
    in args
    end handle Fail s => (println ("Error: " ^ s); OS.Process.exit OS.Process.failure)

end

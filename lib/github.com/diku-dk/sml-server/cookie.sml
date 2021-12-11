signature COOKIE = sig

  type cookiedata =
       {name   : string,
        value  : string,
        expiry : Date.date option,
        domain : string option,
        path   : string option,
        secure : bool}

  type headers = (string * string) list
  type cookies = (string * string) list

  val getCookies : headers -> cookies

  val getCookie  : cookies -> string -> (string * string) option
  val getCookieValue : cookies -> string -> string option

  val setCookie : (string * string -> unit) -> cookiedata -> unit

  val deleteCookie : (string * string -> unit) ->
                     {name:string, path:string option} -> unit
end


structure Cookie : COOKIE =
struct

(* This is a modified implementation of Cookies found in MoscowML.
   This is from the MoscowML source:

   (c) Hans Molin, Computing Science Dept., Uppsala University, 1999.
   http://www.csd.uu.se/~d97ham/, d97ham@csd.uu.se

   Documentation, cleanup and efficiency improvements by
   sestoft@dina.kvl.dk

   Anyone is granted the right to copy and/or use this code, provided
   that this note is retained, also in modified versions. The code is
   provided as is with no guarantee about any functionality.  I take
   no responsibility for its proper function. *)

structure SS = Substring

val decodeUrl = ServerUtil.decodeUrl
val encodeUrl = ServerUtil.encodeUrl

fun concatOpt s NONE     = ""
  | concatOpt s (SOME t) = s ^ t

exception CookieError of string

type cookiedata =
     {name   : string,
      value  : string,
      expiry : Date.date option,
      domain : string option,
      path   : string option,
      secure : bool}

type headers = (string * string) list
type cookies = (string * string) list

infix |>
fun x |> f = f x

fun getCookies (headers: headers) : cookies =
    let fun splitNameAndValue sus =
            let val (pref,suff) = SS.position "=" sus
            in (decodeUrl (SS.concat (SS.fields (fn c => c = #" ") pref)),
                decodeUrl (SS.concat (SS.fields (fn c => c = #" ")
                                                (SS.triml 1 suff))))
            end
        val cvs =
            List.filter (fn (k,_) => k = "Cookie") headers
         |> map (SS.tokens (fn c => c = #";") o SS.full o #2)
         |> List.concat
         |> List.map splitNameAndValue
    in cvs
    end

fun getCookie (cookies: cookies) (cn:string) : (string * string) option =
    List.find (fn (name,value) => cn = name) cookies

fun getCookieValue (cookies: cookies) (cn:string) : string option =
    case getCookie cookies cn of
        NONE => NONE
      | SOME (n,v) => SOME v

(* Date must be GMT time, that is, use Date.fromTimeUniv *)
fun setCookie (add_header: (string*string) -> unit)
              {name : string, value : string,
               expiry : Date.date option,
               domain : string option, path : string option,
               secure : bool} : unit =
    let fun datefmt date = Date.fmt "%a, %d-%b-%Y %H:%M:%S GMT" date
    in if name = "" orelse value= ""
       then ()
       else let val header_out_key = "Set-cookie"
                val header_out_value =
                    String.concat
                        [encodeUrl name, "=",
                         encodeUrl value,
                         concatOpt "; expires=" (Option.map datefmt expiry),
                         concatOpt "; domain=" domain,
                         concatOpt "; path=" path,
                         "; ", if secure then "secure" else ""]
            in add_header (header_out_key,header_out_value)
            end
    end

fun deleteCookie add_header {name:string, path:string option} : unit =
    let val key_out = "Set-cookie"
        val val_out = String.concat[encodeUrl name, "=deleted;",
                                    "expires=Fri, 11-Feb-77 12:00:00 GMT",
                                    concatOpt "; path=" path]
    in add_header (key_out,val_out)
    end
end

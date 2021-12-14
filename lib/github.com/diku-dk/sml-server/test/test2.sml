
fun test s p e f =
    let val () = print (s ^ ": ")
        val res = f ()
    in if res = e then print "OK\n"
       else print ("ERR - expecting '" ^ p e ^ "' - got '" ^ p res ^ "'\n")
    end
    handle Fail msg => print ("EXN Fail(" ^ msg ^ ") - expected '" ^ p e ^ "'\n")
         | ? => print ("EXN - expected '" ^ p e ^ "' - " ^ General.exnMessage ? ^ "\n")

fun id x = x

open Server.Util

val () = test "url-encode-1" id "abc" (fn () => encodeUrl "abc")
val () = test "url-encode-2" id "a%20bc" (fn () => encodeUrl "a bc")

val () = test "url-decode-1" id "abc" (fn () => decodeUrl "abc")
val () = test "url-decode-2" id "a bc" (fn () => decodeUrl "a%20bc")
val () = test "url-decode-3" id "a bc%" (fn () => decodeUrl "a%20bc%")
val () = test "url-decode-4" id "a bc%x" (fn () => decodeUrl "a%20bc%x")
val () = test "url-decode-5" id "a bc%ax a" (fn () => decodeUrl "a%20bc%ax%20a")

fun test_urlencode s = test "url-deencode" id s (fn () => decodeUrl(encodeUrl s))

val () = app test_urlencode ["", "abc", "sdfj3wewe", " efd eed & /(982s"]

val () = print "Ending\n"

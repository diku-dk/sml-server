
local
structure Page = struct
fun return ctx t s =
    let val page =
            String.concat ["<html><head><title>", t, "</title></head>",
                           "<body>",s,"<p><i>Served by SMLserver</i></p></body></html>"]
    in Server.Resp.sendOK ctx page
    end
end

fun sendTime ctx =
    let val time_of_day =
            Date.fmt "%H.%M.%S" (Date.fromTimeLocal(Time.now()))
    in Page.return ctx "Time of Day"
                   ("The time-of-day is " ^ time_of_day ^ ".")
    end

fun handler conn =
    let val ctx = Server.recvRequest conn
    in case Server.Req.path ctx of
           "/time" => sendTime ctx
         | _ => Server.Resp.sendOK ctx "Hello World"
    end

in
val () = Server.start handler
end

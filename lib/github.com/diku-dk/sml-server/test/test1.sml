
local
structure Page = struct
fun return ctx t s =
    let val page =
            String.concat ["<html><head><title>", t, "</title></head>",
                           "<body><h2>",t,"</h2>",
                           s,
                           "<hr />",
                           "<p><i>Served by SMLserver</i></p>",
                           "</body></html>"]
    in Server.Resp.sendOK ctx page
    end
end

fun sendTime ctx =
    let val time_of_day =
            Date.fmt "%H.%M.%S" (Date.fromTimeLocal(Time.now()))
    in Page.return ctx "Time of Day"
                   (String.concat ["<p>The time-of-day is ", time_of_day, ".</p>",
                                   "<h3>Some SMLserver Logos</h3>",
                                   "<p><img src='smlserver_logo_color.svg' /></p>",
                                   "<p>",
                                   "<img src='poweredby_smlserver1_24.png' />&nbsp;",
                                   "<img src='poweredby_smlserver2_24.png' />&nbsp;",
                                   "<img src='poweredby_smlserver3_24.png' />&nbsp;",
                                   "<img src='poweredby_smlserver2_grey_24.png' />&nbsp;",
                                   "<img src='poweredby_smlserver3_grey_24.png' />",
                                   "</p>"])
    end

fun handler conn =
    let val ctx = Server.recvRequest conn
        val path = Server.Req.path ctx
    in case OS.Path.ext path of
           SOME "png" => Server.Resp.sendFile ctx path
         | SOME "svg" => Server.Resp.sendFile ctx path
         | SOME "ico" => Server.Resp.sendFile ctx path
         | _ => sendTime ctx
    end

in
val () = Server.start handler
end


local

infix |>
fun x |> f = f x

structure Page = struct

  fun return ctx t s =
      let val page =
              String.concat ["<html><head><title>", t, "</title></head>",
                             "<body><h2>",t,"</h2>",
                             s,
                             "<hr />",
                             "<p><i>Served by <a href='/'>SMLserver</a></i></p>",
                             "</body></html>"]
      in Server.Resp.sendHtmlOK ctx page
      end
end

structure Guess = struct

  fun mkForm n =
      "<form action=/guess method=post>\
      \  <input type=hidden name=n value=" ^ Int.toString n ^ ">\
      \  <input type=text name=guess>\
      \  <input type=submit value=Guess>\
      \</form>"

  fun page ctx title pic body =
      Page.return ctx title ("<center><img src='" ^ pic ^ "'><p>" ^
                             body ^ "</p></center>")

  (* we'll simplify this thing later *)
  fun getPostVar ctx n =
      let val lines =
              String.tokens (fn c => c = #"&") (Server.Req.postData ctx)
      in List.foldr (fn (l,SOME v) => SOME v
                      | (l,NONE) =>
                        case String.tokens (fn c => c = #"=") l of
                            [k,v] => if n=k then SOME v
                                     else NONE
                          | _ => NONE)
                    NONE lines
      end

  fun getInt ctx name =
      Option.mapPartial Int.fromString (getPostVar ctx name)

  fun send ctx =
      case getInt ctx "n" of
          NONE => page ctx "Guess a number between 0 and 100"
                       "bill_guess.jpg"
                       (mkForm (Server.Info.uptimeProcess ()
                                |> Time.toSeconds
                                |> IntInf.toInt
                                |> (fn i => (31 * i) mod 100)
                               ))
        | SOME n =>
          case getInt ctx "guess" of
              NONE => page ctx "You must type a number - try again"
                           "bill_guess.jpg"
                           (mkForm n)
            | SOME g =>
              if g > n then
                page ctx "Your guess is too big - try again"
                     "bill_large.jpg"
                     (mkForm n)
              else if g < n then
                page ctx "Your guess is too small - try again"
                     "bill_small.jpg"
                     (mkForm n)
              else
                page ctx "Congratulations!"
                     "bill_yes.jpg"
                     ("You guessed the number " ^ Int.toString n ^
                      "<p> <a href='/guess'>Play again?</a>")
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

fun sendIndex ctx =
    Page.return ctx "SMLserver demos"
                (String.concat["<ul>",
                               "<li>", "<a href='/time'>Time of day</a></li>",
                               "<li>", "<a href='/guess'>Guess a number</a></li>",
                               "</ul>"])

fun handler conn =
    let val ctx = Server.recvRequest conn
        val path = Server.Req.path ctx
    in case OS.Path.ext path of
           SOME "png" => Server.Resp.sendFile ctx path
         | SOME "svg" => Server.Resp.sendFile ctx path
         | SOME "ico" => Server.Resp.sendFile ctx path
         | SOME _ => Server.Resp.sendFile ctx path      (* is it safe to send all these? *)
         | NONE => case path of
                       "/time" => sendTime ctx
                     | "/guess" => Guess.send ctx
                     | _ => sendIndex ctx
    end

in
val () = Server.start handler
end


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

fun getPostVarInt ctx name =
    Option.mapPartial Int.fromString (getPostVar ctx name)

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

  fun send ctx =
      case getPostVarInt ctx "n" of
          NONE => page ctx "Guess a number between 0 and 100"
                       "bill_guess.jpg"
                       (mkForm (Server.Info.uptimeProcess ()
                                |> Time.toSeconds
                                |> IntInf.toInt
                                |> (fn i => (31 * i) mod 100)
                               ))
        | SOME n =>
          case getPostVarInt ctx "guess" of
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

fun sendCount ctx =
    let val counter =
            (case Option.mapPartial Int.fromString (Server.Req.query ctx "counter") of
                NONE => 0
              | SOME c =>
                 case Server.Req.query ctx "button" of
                     SOME "Up" => c + 1
                   | SOME "Down" => c - 1
                   | _ => c
            ) |> Int.toString
    in Page.return ctx ("Count: " ^ counter)
            ("<form action=/count>\
             \  <input type=hidden name=counter value=" ^ counter ^ ">\
             \  <input type=submit name=button value=Up>\
             \  <input type=submit name=button value=Down>\
              \</form>")
    end

structure Recipe = struct

  fun error ctx s =
      ( Page.return ctx ("Error: " ^ s)
                    "An error occurred while generating a recipe for \
                    \you; use your browser's back-button to backup \
                    \and enter a number in the form."
      ; raise Server.MissingConnection
      )

  fun pr_num s r =
      if Real.== (r,1.0) then "one " ^ s
      else if Real.==(real(round r),r) then
	Int.toString (round r) ^ " " ^ s ^ "s"
      else Real.toString r ^ " " ^ s ^ "s"

  fun send ctx =
      case Option.map real (getPostVarInt ctx "persons") of
          NONE => error ctx "You must type a number!"
       |  SOME ps =>
          Page.return ctx "Apple Pie Recipe"
            ( ["To make an Apple pie for ", pr_num "person" ps, ", you ",
               "need the following ingredients:",
               "<ul>",
               "<img align=right src=applepie.jpg>",
               "<li> ", pr_num "cup" (ps / 16.0), " butter",
               "<li> ", pr_num "cup" (ps / 4.0), " sugar",
               "<li> ", pr_num "egg" (ps / 4.0),
               "<li> ", pr_num "teaspoon" (ps / 16.0), " salt",
               "<li> ", pr_num "teaspoon" (ps / 4.0), " cinnamon",
               "<li> ", pr_num "teaspoon" (ps / 4.0), " baking soda",
               "<li> ", pr_num "cup" (ps / 4.0), " flour",
               "<li> ", pr_num "cup" (2.5 * ps / 4.0), " diced apples",
               "<li> ", pr_num "teaspoon" (ps / 4.0), " vanilla",
               "<li> ", pr_num "tablespoon" (ps / 2.0), " hot water",
               "</ul>",
               "Combine ingredients in the order given. Bake in greased 9-inch ",
               "pie pans for 45 minutes at 350F. Serve warm with whipped ",
               "cream or ice cream. <p>",
               "Create <a href=recipe.html>another recipe</a>?"]
            |> String.concat
            )
end

structure ServerInfo = struct

  fun send ctx =
    Page.return ctx "Server Information"
    (["<table border=1>",
      "<tr><th>Hostname</th>           <td>", Server.Info.hostname(), "</td></tr>",
      "<tr><th>Pid</th>                <td>", Int.toString (Server.Info.pid()), "</td></tr>",
      "<tr><th>Uptime (seconds)</th>   <td>", Time.toString (Server.Info.uptimeProcess()), "</td></tr>",
(*      "<tr><th>Pageroot</th>           <td>", Web.Info.pageRoot(), " </td></tr>", *)
      "<tr><th>Host</th>               <td>", Server.Conn.host ctx, "</td></tr>",
      "<tr><th>Port</th>               <td>", Int.toString (Server.Conn.port ctx), "</td></tr>",
      "<tr><th>Peer</th>               <td>", Server.Conn.peer ctx, "</td></tr>",
      "<tr><th>Peer port</th>          <td>", Int.toString (Server.Conn.peerPort ctx), "</td></tr>",
      "<tr><th>Server Name</th>        <td>", Server.Conn.server(), "</td></tr>",
      "</table>",

      "<h2>Request Information</h2>",
      "<table border=1>",
      "<tr><th>Host</th>               <td>", Server.Req.host ctx, "</td></tr>",
      "<tr><th>Path</th>               <td>", Server.Req.path ctx, "</td></tr>",
      "<tr><th>Method</th>             <td>", Http.Request.methodToString(Server.Req.method ctx), "</td></tr>",
      "</table>",

      "<h2>Headers Information</h2>",
      "<table border=1>",
      "<tr><th>Key</th><th>Value</th></tr>",
      String.concat(foldr (fn ((k,v),acc) =>
		              "<tr><td>" :: k :: "</td><td>" :: v :: "</td></tr>" :: acc)
	                  nil (Server.Req.headers ctx)),
      "</table>",

      "<h2>Form Data</h2>",
      "<table border=1>",
      "<tr><th>Key</th><th>Value</th></tr>",
      String.concat(foldr(fn ((k,v),acc) =>
			     "<tr><td>" :: k :: "</td><td>" :: v :: "</td></tr>" :: acc)
	                 nil (Server.Req.queryAll ctx)),
      "</table>"]
    |> String.concat
    )

end

fun sendIndex ctx =
    Page.return ctx "SMLserver demos"
                (String.concat["<ul>",
                               "<li>", "<a href='/time'>Time of day</a></li>",
                               "<li>", "<a href='/count'>Counter</a></li>",
                               "<li>", "<a href='/recipe.html'>Dynamic Recipe</a></li>",
                               "<li>", "<a href='/guess'>Guess a number</a></li>",
                               "<li>", "<a href='/server'>Server information</a></li>",
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
                     | "/count" => sendCount ctx
                     | "/recipe" => Recipe.send ctx
                     | "/server" => ServerInfo.send ctx
                     | _ => sendIndex ctx
    end

in
val () = Server.start handler
end

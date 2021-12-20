
local

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



structure Examples = ExamplesFn (Page)
open Examples

fun sendIndex links =
    let val s =
        String.concat ("<ul>"::foldr (fn (l,a) =>
                                         "<li>"::l::"</li>"::a)
                                     ["</ul>"]
                                     links)
    in fn ctx => Page.return ctx "SMLserver demos" s
    end

val services =
    [("/time",        TimeService.service),
     ("/guess",       GuessService.service),
     ("/count",       CountService.service),
     ("/recipe",      RecipeService.service),
     ("/server",      ServerInfoService.service),
     ("/cookie",      CookieService.service),
     ("/file_upload", FileUploadService.service)
    ]

fun find k nil = NONE
  | find k ((x,y)::rest) = if k=x then SOME y else find k rest

fun setup (services: (path*service) list) : path -> Server.ctx -> unit =
    let val instances =
            map (fn (p,s) => (p, s p)) services
        val links =
            List.mapPartial (fn (p,i) =>
                                case #name i of
                                    SOME t => SOME ("<a href='" ^ p ^ "'>" ^ t ^ "</a>")
                                  | NONE => NONE)
                       instances
        val index = sendIndex ("<a href=recipe.html>Recipe</a>" :: links)
    in fn (path:path) =>
          case String.tokens (fn c => c = #"/") path of
              p :: ps => (case find ("/" ^ p) instances of
                              SOME {name,handler} => handler ps
                            | NONE => index)
            | _ => index
    end

val handler = setup services

fun runHandler conn =
    let val ctx = Server.recvRequest conn
        val path = Server.Req.path ctx
    in case OS.Path.ext path of
           SOME "png" => Server.Resp.sendFile ctx path
         | SOME "svg" => Server.Resp.sendFile ctx path
         | SOME "ico" => Server.Resp.sendFile ctx path
         | SOME _ => Server.Resp.sendFile ctx path      (* is it safe to send all these? *)
         | NONE => handler path ctx
    end

in
val () = Server.start runHandler
end

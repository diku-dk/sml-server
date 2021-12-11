
structure ServerUtil = struct

fun encodeUrl (s:string) : string =
    let val len = CharVector.foldl (fn (c,n) =>
                                        if Char.isAlphaNum c
                                        then n+1 else n+3) 0 s
        val a = CharArray.array(len,#"0")
        val hex = "0123456789abcdef"
    in CharVector.foldl (fn (c,i) =>
                            if Char.isAlphaNum c
                            then ( CharArray.update(a,i,c)
                                 ; i+1 )
                            else let val e = Char.ord c
                                     val h1 = CharVector.sub(hex, e div 16)
                                     val h2 = CharVector.sub(hex, e mod 16)
                                 in CharArray.update(a, i, #"%")
                                  ; CharArray.update(a, i+1, h1)
                                  ; CharArray.update(a, i+2, h2)
                                  ; i+3
                                 end) 0 s
     ; CharArray.vector a
    end

fun hexDigitNum c =
    let val v = Char.ord (Char.toLower c)
    in if 48 <= v andalso v <= 57 then SOME(v - 48)
       else if 97 <= v andalso v <= 102 then SOME(v - 97 + 10)
       else NONE
    end

fun decodeUrl (s:string) : string =
    let val (size,opt) =
            CharVector.foldl
                (fn (c,(i,ac)) =>
                    case ac of
                        NONE =>
                        if c = #"%" then (i,SOME NONE)
                        else (i+1,ac)
                      | SOME NONE =>
                        if Char.isHexDigit c then (i,SOME (SOME c))
                        else (i + 2, NONE)
                      | SOME (SOME c0) =>
                        case (hexDigitNum c0, hexDigitNum c) of
                            (SOME _, SOME _) => (i + 1, NONE)
                          | _ => (i + 3, NONE)
                ) (0,NONE) s
        val size = case opt of
                       SOME NONE => size+1
                     | SOME (SOME _) => size+2
                     | NONE => size
        val a = CharArray.array(size,#"0")
        val res =
            CharVector.foldl
                (fn (c,(i,ac)) =>
                    case ac of
                        NONE => if c = #"%" then (i,SOME NONE)
                                else ( CharArray.update(a,i,c)
                                     ; (i+1,ac) )
                      | SOME NONE =>
                        if Char.isHexDigit c then
                          (i,SOME (SOME c))
                        else
                          ( CharArray.update(a,i,#"%")
                          ; CharArray.update(a,i+1,c)
                          ; (i + 2, NONE) )
                      | SOME (SOME c0) =>
                        case (hexDigitNum c0, hexDigitNum c) of
                            (SOME v0, SOME v) =>
                            ( CharArray.update(a,i,Char.chr (16 * v0 + v))
                            ; (i + 1, NONE) )
                          | _ =>
                            ( CharArray.update(a,i,#"%")
                            ; CharArray.update(a,i+1,c0)
                            ; CharArray.update(a,i+2,c)
                            ; (i + 3, NONE) )
                ) (0,NONE) s
        val () =  case res of
                      (i,SOME (SOME c)) => ( CharArray.update(a,i,#"%")
                                           ; CharArray.update(a,i+1,c) )
                    | (i,SOME NONE) => CharArray.update(a,i,#"%")
                    | (i,NONE) => ()
    in CharArray.vector a
    end

fun buildUrl action hvs =
    action ^ "?" ^ (String.concatWith "&" (List.map (fn (n,v) => n ^ "=" ^ encodeUrl v) hvs))


end

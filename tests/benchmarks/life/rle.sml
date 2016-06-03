(* Run-Length-Encoded file translator *)

(* Translates to Futhark array syntax *)

fun println s = print (s ^ "\n")
fun eprintln s = TextIO.output(TextIO.stdErr, s ^ "\n")

fun printusage () =
    (println "rle [OPTIONS] < file.in > file.out";
     println "OPTIONS:";
     println " --help: print usage information and exit";
     println " --flat: print a flat vector preceeded by a\n\
             \         2-element dimension vector (x,y)";
     println " --verbose: print debugging information")
             
fun die s = (eprintln s; OS.Process.exit(1))

local val verbose = ref false
      val flat = ref false
in fun msg s = if !verbose then eprintln s else ()
   fun flatFlag() = !flat
   fun process nil = nil
     | process ("--verbose"::rest) = (verbose:=true; process rest)
     | process ("--flat"::rest) = (flat:=true; process rest)
     | process ("--help"::_) = (printusage(); OS.Process.exit(0))
     | process xs = xs
end

val args = process(CommandLine.arguments())

val file = TextIO.inputAll TextIO.stdIn
val lines = String.tokens (fn #"\n" => true | #"\r" => true 
                            | _ => false) file

val lines = List.filter (fn l => size l > 0 andalso String.sub(l,0) <> #"#") lines     (* drop comments *)

fun readAssgn var0 s =
    case String.tokens (fn c => c = #"=") s of
        [var,value] =>
        (case Int.fromString value of
             SOME x => x
           | NONE => die ("expecting integer in assignment for " ^ var0 ^ "(" ^ var ^ ")"))
      | _ => die ("readAssgn.problem with assignment for " ^ var0 ^ " - " ^ s)
                        
val (x,y,lines) =
    case lines of
        header :: lines =>
        (case String.tokens (fn c => c = #",") header of
             xassgn::yassgn::_ =>
             let val x = readAssgn "x" xassgn
                 val y = readAssgn "y" xassgn
             in (x,y,lines)
             end
           | _ => die "expecting header with assignments for x and y")
      | _ => die "expecting file with at least one line (except comments)"

fun repeat n v acc =
    if n <= 0 then acc
    else repeat (n-1) v (v::acc)
                 
fun transLines (nil,n,acc,rows,argopt) = die "Premature end of file - expects the ! character"
  | transLines (l::ls,n,acc,rows,argopt) = 
    let val cs = explode l
        fun readint0 (nil,NONE) = NONE
          | readint0 (nil,SOME v) = SOME (v,nil)
          | readint0 (c::cs,NONE) =
            if Char.isDigit c then readint0(cs,SOME [c])
            else NONE
          | readint0 (c::cs,SOME l) =
            if Char.isDigit c then readint0(cs,SOME (c::l))
            else SOME (l,c::cs)
        fun readint cs = case readint0 (cs,NONE) of
                             NONE => NONE
                           | SOME (l,cs') => case Int.fromString (implode (rev l)) of
                                                 NONE => NONE
                                               | SOME i => SOME (i,cs')
        fun unArgOpt NONE = 1
          | unArgOpt (SOME i) = i
        fun loop (cs,n,acc,rows,argopt) =
            case cs of
                nil => transLines(ls,n,acc,rows,argopt)
              | #"$" :: cs => loop(cs,0,nil,List.rev(repeat (x-n) 0 acc)::rows,NONE)
              | #"!" :: _ => List.rev(repeat (x-n) 0 acc) :: rows
              | #"b" :: cs =>
                let val i = unArgOpt argopt
                in loop (cs,n+i,repeat i 0 acc,rows,NONE)
                end
              | #"o" :: cs => 
                let val i = unArgOpt argopt
                in loop (cs,n+i,repeat i 1 acc,rows,NONE)
                end
              | c::_ => case readint cs of
                            SOME (i,cs) => loop(cs,n,acc,rows,SOME i)
                          | NONE => die ("Unrecognized character '" ^ String.str c ^ "' (" ^ Char.toString c ^ ")")
    in loop (explode l,n,acc,rows,argopt)
    end
        
val res = transLines (lines,0,nil,nil,NONE)

fun spar s = "[" ^ s ^ "]"

fun layoutRow row = spar(String.concatWith "," (List.map Int.toString row))

fun flatten nil = nil
  | flatten (x::xs) = x @ flatten xs
               
val () =
    if flatFlag() then
      (println (spar(Int.toString x ^ "," ^ Int.toString y));
       println(layoutRow(flatten res)))
    else
      println(spar(String.concatWith "," (List.map layoutRow res)))

val () = eprintln "Done!"

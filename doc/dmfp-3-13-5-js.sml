val _ = print ("<html><body><h1>Check for common integer in 2 lists</h1>" ^ 
               "<table border='1'>" ^
               "<tr><th align='left'>List of numbers:(e.g. 5,2,3,1)</th><td><input type='text' id='jL1'></td></tr>" ^
               "<tr><th align='left'>List of numbers:(e.g. 6,7,2,8)</th><td><input type='text' id='jL2'></td></tr>" ^
               "<tr><th align='left'>There is a common integer in those lists:</th><td><div id='jR'>?</div></td></tr>" ^ 
               "</table></body></html>")
fun get id = 
    case Js.getElementById Js.document id of
      SOME e => e
    | NONE => raise Fail ("Missing id in document: " ^ id)

val input1 = get "jL1"
val input2 = get "jL2"
val () = Js.setStyle input1 ("backgroundColor", "yellow")
val () = Js.setStyle input2 ("backgroundColor", "yellow")

fun hasCommonElement([], bb) = false
  | hasCommonElement(a::aRest, bb) = let
       fun containsA([]) = false
         | containsA(b::bRest) = a = b orelse containsA(bRest);
    in containsA(bb) orelse hasCommonElement(aRest, bb) end;

fun numbers s = let
   fun isSep #"," = true | isSep c = Char.isSpace c
   in map (valOf o Int.fromString) (String.tokens isSep s) end;
fun comp () = 
    let val v1 = Js.value input1
        val v2 = Js.value input2         
        val res = Bool.toString (hasCommonElement ((numbers v1),(numbers v1)))
        val r = get "jR"
        val () = Js.setStyle r ("backgroundColor", "yellow")
    in Js.innerHTML r res; false end
val () = Js.installEventHandler input1 Js.onchange comp
val () = Js.installEventHandler input2 Js.onchange comp

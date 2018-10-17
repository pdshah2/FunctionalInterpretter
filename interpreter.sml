fun interpreter(inFile : string, outFile : string) =

let

datatype opertation = add | sub | mul | division | rem | neg | swap | pop | And | or | not | equal | lessThan | bind | iff

datatype element = INT of int | NAME of string | STRING of string | BOOL of string | COMMAND of opertation

fun find(key : element, [] : (element * element) list) = STRING "notFound"
  | find(key, l) = if((#1(hd l)) = key) then (#2(hd l)) else find(key, tl l);


fun foo(add, ((INT x)::(INT y)::(xs)), myMap : (element * element) list) = (((INT (y + x))::(xs)), myMap)
  | foo(add, ((NAME x)::(NAME y)::(xs)), myMap : (element * element) list) =
        if(find(NAME x, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(NAME y)::(xs)), myMap)
        else if(find (NAME y, myMap) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(NAME y)::(xs)), myMap)
        else (foo(add, (find(NAME x, myMap))::(find(NAME y, myMap))::(xs), myMap))
  | foo(add, ((NAME x)::(INT y)::(xs)), myMap : (element * element) list) =
        if(find(NAME x, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(INT y)::(xs)), myMap)
        else (foo(add, (find(NAME x, myMap))::(INT y)::(xs), myMap))
  | foo(add, ((INT x)::(NAME y)::(xs)), myMap : (element * element) list) =
        if(find(NAME y, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(INT x)::(NAME y)::(xs)), myMap)
        else (foo(add, (INT x)::(find(NAME y, myMap))::(xs), myMap))
  | foo(add, z, myMap : (element * element) list) = (((STRING ":error:")::z), myMap)

  | foo(sub, (INT x)::(INT y)::(xs), myMap : (element * element) list) = (((INT (y - x))::(xs)), myMap)
  | foo(sub, ((NAME x)::(NAME y)::(xs)), myMap : (element * element) list) =
        if(find(NAME x, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(NAME y)::(xs)), myMap)
        else if(find (NAME y, myMap) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(NAME y)::(xs)), myMap)
        else (foo(sub, (find(NAME x, myMap))::(find(NAME y, myMap))::(xs), myMap))
  | foo(sub, ((NAME x)::(INT y)::(xs)), myMap : (element * element) list) =
        if(find(NAME x, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(INT y)::(xs)), myMap)
        else (foo(sub, (find(NAME x, myMap))::(INT y)::(xs), myMap))
  | foo(sub, ((INT x)::(NAME y)::(xs)), myMap : (element * element) list) =
        if(find(NAME y, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(INT x)::(NAME y)::(xs)), myMap)
        else (foo(sub, (INT x)::(find(NAME y, myMap))::(xs), myMap))
  | foo(sub, z, myMap : (element * element) list) = (((STRING ":error:")::z), myMap)

  | foo(mul, (INT x)::(INT y)::(xs), myMap : (element * element) list) = (((INT (y * x))::(xs)), myMap)
  | foo(mul, ((NAME x)::(NAME y)::(xs)), myMap : (element * element) list) =
        if(find(NAME x, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(NAME y)::(xs)), myMap)
        else if(find (NAME y, myMap) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(NAME y)::(xs)), myMap)
        else (foo(mul, (find(NAME x, myMap))::(find(NAME y, myMap))::(xs), myMap))
  | foo(mul, ((NAME x)::(INT y)::(xs)), myMap : (element * element) list) =
        if(find(NAME x, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(INT y)::(xs)), myMap)
        else
          (case find(NAME x, myMap) of
            (INT _) => (foo(mul, (find(NAME x, myMap))::(INT y)::(xs), myMap))
            |_ => ((STRING(":error:")::(NAME x)::(INT y)::(xs)), myMap))
  | foo(mul, ((INT x)::(NAME y)::(xs)), myMap : (element * element) list) =
        if(find(NAME y, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(INT x)::(NAME y)::(xs)), myMap)
        else
          (case find(NAME y, myMap) of
            (INT _) => (foo(mul, (INT x)::(find(NAME y, myMap))::(xs), myMap))
            |_ => ((STRING(":error:")::(INT x)::(NAME y)::(xs)), myMap))
  | foo(mul, z, myMap : (element * element) list) = (((STRING ":error:")::z), myMap)

  | foo(division, (INT 0)::(INT y)::(xs), myMap : (element * element) list) = (((STRING ":error:")::(INT 0)::(INT y)::xs), myMap)
  | foo(division, (INT x)::(INT y)::(xs), myMap : (element * element) list) = (((INT (y div x))::(xs)), myMap)
  | foo(division, ((NAME x)::(NAME y)::(xs)), myMap : (element * element) list) =
        if(find(NAME x, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(NAME y)::(xs)), myMap)
        else if(find (NAME y, myMap) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(NAME y)::(xs)), myMap)
        else (foo(division, (find(NAME x, myMap))::(find(NAME y, myMap))::(xs), myMap))
  | foo(division, ((NAME x)::(INT y)::(xs)), myMap : (element * element) list) =
        if(find(NAME x, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(INT y)::(xs)), myMap)
        else (foo(division, (find(NAME x, myMap))::(INT y)::(xs), myMap))
  | foo(division, ((INT x)::(NAME y)::(xs)), myMap : (element * element) list) =
        if(find(NAME y, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(INT x)::(NAME y)::(xs)), myMap)
        else (foo(division, (INT x)::(find(NAME y, myMap))::(xs), myMap))
  | foo(division, z, myMap : (element * element) list) = (((STRING ":error:")::z), myMap)

  | foo(neg, (INT x)::xs, myMap : (element * element) list) = (((INT (~x))::xs), myMap)
  | foo(neg, (NAME x)::xs, myMap : (element * element) list) =
        if(find(NAME x, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(xs)), myMap)
        else (foo(neg, (find(NAME x, myMap))::(xs), myMap))
  | foo(neg, z, myMap : (element * element) list) = (((STRING ":error:")::z), myMap)

  | foo(swap, x::y::xs, myMap : (element * element) list) = ((y::x::xs), myMap)
  | foo(swap, z, myMap : (element * element) list) = (((STRING ":error:")::z), myMap)

  | foo(rem, (INT x)::(INT y)::(xs), myMap) = (((INT (y mod x))::(xs)), myMap)
  | foo(rem, ((NAME x)::(NAME y)::(xs)), myMap : (element * element) list) =
        if(find(NAME x, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(NAME y)::(xs)), myMap)
        else if(find (NAME y, myMap) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(NAME y)::(xs)), myMap)
        else (foo(rem, (find(NAME x, myMap))::(find(NAME y, myMap))::(xs), myMap))
  | foo(rem, ((NAME x)::(INT y)::(xs)), myMap : (element * element) list) =
        if(find(NAME x, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(INT y)::(xs)), myMap)
        else (foo(rem, (find(NAME x, myMap))::(INT y)::(xs), myMap))
  | foo(rem, ((INT x)::(NAME y)::(xs)), myMap : (element * element) list) =
        if(find(NAME y, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(INT x)::(NAME y)::(xs)), myMap)
        else (foo(rem, (INT x)::(find(NAME y, myMap))::(xs), myMap))
  | foo(rem, z, myMap : (element * element) list) = (((STRING ":error:")::z), myMap)

  | foo(pop, x::xs, myMap) = (xs, myMap)
  | foo(pop, z, myMap : (element * element) list) = (((STRING ":error:")::z), myMap)

  | foo(And, (BOOL ":true:")::(BOOL ":true:")::(xs), myMap) = (((BOOL ":true:")::xs), myMap)
  | foo(And, (BOOL _)::(BOOL _)::(xs), myMap) = (((BOOL ":false:")::xs), myMap)
  | foo(And, ((NAME x)::(NAME y)::(xs)), myMap : (element * element) list) =
        if(find(NAME x, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(NAME y)::(xs)), myMap)
        else if(find (NAME y, myMap) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(NAME y)::(xs)), myMap)
        else (foo(And, (find(NAME x, myMap))::(find(NAME y, myMap))::(xs), myMap))
  | foo(And, ((NAME x)::(BOOL y)::(xs)), myMap : (element * element) list) =
        if(find(NAME x, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(BOOL y)::(xs)), myMap)
        else (foo(And, (find(NAME x, myMap))::(BOOL y)::(xs), myMap))
  | foo(And, ((BOOL x)::(NAME y)::(xs)), myMap : (element * element) list) =
        if(find(NAME y, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(BOOL x)::(NAME y)::(xs)), myMap)
        else (foo(And, (BOOL x)::(find(NAME y, myMap))::(xs), myMap))
  | foo(And, z, myMap : (element * element) list) = (((STRING ":error:")::z), myMap)

  | foo(or, (BOOL ":false:")::(BOOL ":false:")::(xs), myMap) = (((BOOL ":false:")::xs), myMap)
  | foo(or, (BOOL _)::(BOOL _)::(xs), myMap) = (((BOOL ":true:")::xs), myMap)
  | foo(or, ((NAME x)::(NAME y)::(xs)), myMap : (element * element) list) =
        if(find(NAME x, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(NAME y)::(xs)), myMap)
        else if(find (NAME y, myMap) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(NAME y)::(xs)), myMap)
        else (foo(or, (find(NAME x, myMap))::(find(NAME y, myMap))::(xs), myMap))
  | foo(or, ((NAME x)::(BOOL y)::(xs)), myMap : (element * element) list) =
        if(find(NAME x, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(BOOL y)::(xs)), myMap)
        else (foo(or, (find(NAME x, myMap))::(BOOL y)::(xs), myMap))
  | foo(or, ((BOOL x)::(NAME y)::(xs)), myMap : (element * element) list) =
        if(find(NAME y, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(BOOL x)::(NAME y)::(xs)), myMap)
        else (foo(or, (BOOL x)::(find(NAME y, myMap))::(xs), myMap))
  | foo(or, z, myMap : (element * element) list) = (((STRING ":error:")::z), myMap)

  | foo(not, (BOOL ":true:")::(xs), myMap) = (((BOOL ":false:")::xs), myMap)
  | foo(not, (BOOL ":false:")::(xs), myMap) = (((BOOL ":true:")::xs), myMap)
  | foo(not, (NAME x)::xs, myMap : (element * element) list) =
        if(find(NAME x, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(xs)), myMap)
        else (foo(not, (find(NAME x, myMap))::(xs), myMap))
  | foo(not, z, myMap : (element * element) list) = (((STRING ":error:")::z), myMap)

  | foo(equal, (INT x)::(INT y)::(xs), myMap) = if (x = y) then (((BOOL ":true:")::xs), myMap) else (((BOOL ":false:")::xs), myMap)
  | foo(equal, ((NAME x)::(NAME y)::(xs)), myMap : (element * element) list) =
        if(find(NAME x, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(NAME y)::(xs)), myMap)
        else if(find (NAME y, myMap) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(NAME y)::(xs)), myMap)
        else (foo(equal, (find(NAME x, myMap))::(find(NAME y, myMap))::(xs), myMap))
  | foo(equal, ((NAME x)::(INT y)::(xs)), myMap : (element * element) list) =
        if(find(NAME x, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(INT y)::(xs)), myMap)
        else (foo(equal, (find(NAME x, myMap))::(INT y)::(xs), myMap))
  | foo(equal, ((INT x)::(NAME y)::(xs)), myMap : (element * element) list) =
        if(find(NAME y, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(INT x)::(NAME y)::(xs)), myMap)
        else (foo(equal, (INT x)::(find(NAME y, myMap))::(xs), myMap))
  | foo(equal, z, myMap : (element * element) list) = (((STRING ":error:")::z), myMap)

  | foo(lessThan, (INT x)::(INT y)::(xs), myMap) = if (y < x) then (((BOOL ":true:")::xs), myMap) else (((BOOL ":false:")::xs), myMap)
  | foo(lessThan, ((NAME x)::(NAME y)::(xs)), myMap : (element * element) list) =
        if(find(NAME x, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(NAME y)::(xs)), myMap)
        else if(find (NAME y, myMap) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(NAME y)::(xs)), myMap)
        else (foo(lessThan, (find(NAME x, myMap))::(find(NAME y, myMap))::(xs), myMap))
  | foo(lessThan, ((NAME x)::(INT y)::(xs)), myMap : (element * element) list) =
        if(find(NAME x, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(NAME x)::(INT y)::(xs)), myMap)
        else (foo(lessThan, (find(NAME x, myMap))::(INT y)::(xs), myMap))
  | foo(lessThan, ((INT x)::(NAME y)::(xs)), myMap : (element * element) list) =
        if(find(NAME y, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::(INT x)::(NAME y)::(xs)), myMap)
        else (foo(lessThan, (INT x)::(find(NAME y, myMap))::(xs), myMap))
  | foo(lessThan, z, myMap : (element * element) list) = (((STRING ":error:")::z), myMap)

  |foo(bind, ((NAME x)::(NAME y)::(xs)), myMap : (element * element) list) =
        if(find(NAME x, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":unit:")::(xs)), ((NAME y, NAME x)::myMap))
        else (((STRING ":unit:")::(xs)), ((NAME y, find(NAME x, myMap))::myMap))
  |foo(bind, ((BOOL x)::(NAME y)::(xs)), myMap : (element * element) list) = (((STRING ":unit:")::(xs)), ((NAME y, BOOL x)::myMap))
  |foo(bind, ((INT x)::(NAME y)::(xs)), myMap : (element * element) list) = (((STRING ":unit:")::(xs)), ((NAME y, INT x)::myMap))
  |foo(bind, ((STRING ":unit:")::(NAME y)::(xs)), myMap : (element * element) list) = (((STRING ":unit:")::(xs)), ((NAME y, STRING ":unit:")::myMap))
  |foo(bind, z, myMap : (element * element) list) = (((STRING ":error:")::z), myMap)

  | foo(iff, x::y::(BOOL z)::(xs), myMap) = if(z = ":true:") then ((x::(xs)), myMap) else ((y::(xs)), myMap)
  | foo(iff, (x::y::(NAME z)::xs), myMap : (element * element) list) =
        if(find(NAME z, myMap : (element * element) list) = (STRING "notFound")) then (((STRING ":error:")::x::y::(NAME z)::(xs)), myMap)
        else (foo(iff, x::y::(find(NAME z, myMap))::(xs), myMap))
  | foo(iff, z, myMap : (element * element) list) = (((STRING ":error:")::z), myMap)

  fun lazy x = (STRING "placeholer")::x

  (*fun ending x = *)

  fun contains ([] , e) = false
  | contains (x::y, e) = if x = e then true else contains (y , e)

  val inStream = TextIO.openIn inFile
  val outStream = TextIO.openOut outFile

  fun reader(readInput : string) =

    let
      fun helper(readLine : string option) =
      case readLine of
      NONE => ([])
      | SOME(c) => c::(helper(TextIO.inputLine inStream));
    in
        List.rev(helper (TextIO.inputLine inStream))
    end

  val readStrings = reader(inFile)

  fun quit(myStack : element list) =
    if(List.length(myStack) = 0) then (TextIO.flushOut(outStream); [])
    else
      case (hd myStack) of
        INT(x) => if (x < 0)
                    then (TextIO.output(outStream, ("-"^(String.extract((Int.toString(x)), 1, NONE))^"\n")); quit (tl myStack))
                  else
                  (TextIO.output(outStream, (Int.toString(x)^"\n")); quit (tl myStack))
        |NAME(x) => (TextIO.output(outStream, (x)); quit (tl myStack))
        |STRING(x) => (TextIO.output(outStream, (x^"\n")); quit (tl myStack))
        |BOOL(x) => (TextIO.output(outStream, (x^"\n")); quit (tl myStack))


  fun parse(words : string list) =

  let
    fun something(ww) = if List.length(ww) = 0 then ([], [])
    else
    let
    val hello = something(tl ww)
    in
      if(List.length(ww) = 0)
        then ([], [])
      else
        case (String.substring(hd ww, 0, 2)) of
          "pu" => (if contains (String.explode((String.extract(hd ww, 5, NONE))), #".")
                      then (((STRING ":error:")::(#1(hello))), #2(hello))
                  else if (contains (String.explode((String.extract(hd ww, 5, NONE))), #":"))
                      then (((STRING ":error:")::(#1(hello))), #2(hello))
                  else if (contains (String.explode((String.extract(hd ww, 5, NONE))), #"\""))
                      then (((NAME (String.extract(hd ww, 6, SOME (String.size(hd ww)-8))^"\n"))::(#1(hello))), #2(hello))
                  else if (contains (String.explode((String.extract(hd ww, 5, NONE))), #"?"))
                      then (((STRING ":error:")::(#1(hello))), #2(hello))
                  else
                    (case Int.fromString (String.extract(hd ww, 5, NONE)) of
                      NONE => (((NAME (String.extract(hd ww, 5, NONE)))::(#1(hello))), #2(hello))
                      |SOME(i) => (((INT i)::(#1(hello))), #2(hello))))

          |"po" => foo(pop, #1(hello), #2(hello))
          |":t" => (((BOOL ":true:")::(#1(hello))), #2(hello))
          |":f" => (((BOOL ":false:")::(#1(hello))), #2(hello))
          |"ad" => foo(add, #1(hello), #2(hello))
          |"su" => foo(sub, #1(hello), #2(hello))
          |"mu" => foo(mul, #1(hello), #2(hello))
          |"di" => foo(division, #1(hello), #2(hello))
          |"ne" => foo(neg, #1(hello), #2(hello))
          |"sw" => foo(swap, #1(hello), #2(hello))
          |"re" => foo(rem, #1(hello), #2(hello))
          |"an" => foo(And, #1(hello), #2(hello))
          |"or" => foo(or, #1(hello), #2(hello))
          |"no" => foo(not, #1(hello), #2(hello))
          |"eq" => foo(equal, #1(hello), #2(hello))
          |"le" => (if (String.substring(hd ww, 0, 4) = "less")
                      then foo(lessThan, #1(hello), #2(hello))
                    else (((#1(hello))), #2(hello)))
          |"en" => (((#1(hello))), #2(hello))
          |"bi" => foo(bind, #1(hello), #2(hello))
          |"if" => foo(iff, #1(hello), #2(hello))
          |"qu" => (quit(#1(hello)), #2(hello))
          |_ => (((STRING ":error:")::(#1(hello))), #2(hello))

    end
  in
      something(words)
  end
in
    (parse(readStrings); ())
end

val result = interpreter("input.txt", "output.txt")

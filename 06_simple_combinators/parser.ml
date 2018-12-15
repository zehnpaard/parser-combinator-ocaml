type 'a result =
  | Success of 'a
  | Failure of string
;;

type 'a parser = Parser of (char list -> ('a * char list) result);; 

let p c = 
  let f = function
    | [] -> Failure "No more input"
    | c'::cs' when c = c' -> Success (c', cs')
    | c'::_ -> Failure (Printf.sprintf "Expecting '%c'. Got '%c'" c c')
  in
  Parser f
;;

let run (Parser p) cs = p cs;;

let andThen p1 p2 =
  let f cs = match run p1 cs with
    | Failure err -> Failure err
    | Success (v1, cs1) ->
      (match run p2 cs1 with
         | Failure err -> Failure err
         | Success (v2, cs2) -> Success ((v1, v2), cs2))
  in
  Parser f
;;

let ( @>> ) = andThen;;

let explode s = List.init (String.length s) (String.get s);;

let parse c s = run (p c) (explode s);;

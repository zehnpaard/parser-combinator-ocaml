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

let explode s = List.init (String.length s) (String.get s);;

let parse c s = run (p c) (explode s);;

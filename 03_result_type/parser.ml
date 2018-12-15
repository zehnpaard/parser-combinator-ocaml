type 'a result =
  | Success of 'a
  | Failure of string
;;

let p c = function
  | [] -> Failure "No more input"
  | c'::cs' when c = c' -> Success (c', cs')
  | c'::_ -> Failure (Printf.sprintf "Expecting '%c'. Got '%c'" c c')
;;

let explode s = List.init (String.length s) (String.get s);;

let parse c s = p c (explode s);;

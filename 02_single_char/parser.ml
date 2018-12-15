let p c = function
 | [] -> ("No more input", [])
 | c'::cs' when c' = c -> (Printf.sprintf "Found '%c'" c, cs')
 | c'::cs' as cs -> (Printf.sprintf "Expecting '%c'. Got '%c'" c c', cs)
;;

let explode s = List.init (String.length s) (String.get s);;

let parse c s = p c (explode s);;

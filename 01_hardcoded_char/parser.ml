let p = function
  | [] -> (false, [])
  | 'A'::cs -> (true, cs)
  | cs -> (false, cs)
;;

let parse s = p (List.init (String.length s) (String.get s));;

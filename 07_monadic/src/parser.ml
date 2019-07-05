open Containers

type 'a t = char list -> ('a * char list) option

let map f p = fun cs -> (match p cs with
  | Some (v, cs') -> Some (f v, cs')
  | None -> None)
let (<$>) = map

let pure v = fun s -> Some (v, s)

let (<*>) fp xp = fun s -> (match fp s with
  | Some (f, s') -> (map f xp) s'
  | None -> None)
let ( <* ) xp yp = (fun x _ -> x) <$> xp <*> yp
let ( *> ) xp yp = (fun _ x -> x) <$> xp <*> yp


let product xp yp = (fun x y -> (x, y)) <$> xp <*> yp

let (let+) x f = map f x
let (and+) xp yp = product xp yp

let empty = fun _ -> None

let (<|>) xp yp = fun s -> (match xp s with
  | Some _ as r -> r
  | None -> yp s)


let bind xp f = fun s -> (match xp s with
  | Some (x, s') -> f x s'
  | None -> None)

let (>>=) = bind

let (let*) x f = bind x f
let (and*) = (and+)

let parse p s = p @@ String.to_list s

let get_char = fun cs -> (match cs with
  | [] -> None
  | c :: cs' -> Some (c, cs'))

let satisfy p =
  let* x = get_char in
  if p x then pure x else empty

let match_char c = satisfy (Char.equal c)

let match_digit =
  let p = function
    | '0' .. '9' -> true
    | _ -> false
  in
  satisfy p

let match_upper =
  let p = function
    | 'A' .. 'Z' -> true
    | _ -> false
  in
  satisfy p

let match_lower =
  let p = function
    | 'a' .. 'z' -> true
    | _ -> false
  in
  satisfy p

let match_space =
  let p = function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false
  in
  satisfy p

let match_letter = match_upper <|> match_lower

let match_alphanum = match_letter <|> match_digit

let match_string s =
  let rec f = function
      [] -> pure []
    | c::cs -> let* _ = match_char c
               and* _ = f cs
               in pure (c::cs)
  in
  String.to_list s |> f |> map String.of_list

let rec many p cs =
  (some p <|> pure []) cs
and some p cs =
  (List.cons <$> p <*> many p) cs


let match_ident =
  List.cons <$> match_lower <*> many match_alphanum

let match_nat =
  int_of_string <$> (String.of_list <$> some match_digit)

let match_int =
  match_nat <|> ((~-) <$> (match_char '-' *> match_nat))

let get_token p = many match_space *> p <* many match_space

let get_ident = get_token match_ident
let get_nat = get_token match_nat
let get_int = get_token match_int
let get_symbol s = get_token (match_string s)

let rec expr cs =
  (let* t = term in
    (pure ((+) t) <*> (get_symbol "+" *> expr)) <|> pure t) cs
and term cs =
  (let* f = factor in
    (pure (( * ) f) <*> (get_symbol "*" *> term)) <|> pure f) cs
and factor cs =
  (get_symbol "(" *> expr <* get_symbol ")" <|> get_nat) cs

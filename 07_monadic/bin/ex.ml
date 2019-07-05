open Containers
open Mparser

let () =
  let s = read_line () in
  match Parser.parse Parser.expr s with
    | None -> print_endline "Parse failed"
    | Some (n, cs) -> print_endline @@ string_of_int n ^ " " ^ String.of_list cs

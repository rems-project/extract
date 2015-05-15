type tree = E of Xmlm.tag * tree list | D of string ;;

(* From XML to tree, and back *)

let in_tree i =
  let el tag c= E (tag, c)  in
  let data d = D d in
  let dtd, tree = Xmlm.input_doc_tree ~el ~data i in
  tree

let out_tree o t =
  let frag = function
    | E (tag, c) -> `El (tag, c)
    | D d -> `Data d
  in
  Xmlm.output_doc_tree frag o (None, t)

(* Utility functions *)

let rec filter_map f = function
  | [] -> []
  | x::xs ->
    match f x with
    | Some s -> s :: filter_map f xs
    | None -> filter_map f xs

let (|>) x f = f x;;

let (++) = List.rev_append

let filter_tree f l =
  let rec aux acc = function
    | [] -> List.rev acc
    | D s :: xs -> aux (f (D s) ++  acc) xs
    | E (t, c) :: xs -> aux (f (E (t, aux [] c)) ++ acc) xs
  in aux [] l


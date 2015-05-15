type position = int * int

type field =
    Opcode of int * position
  | Ifield of string * position
  | SplitIfield of string * position * position
  | Reserved of position

type t = field list
let pos_to_string (x, y) =
  if y = 1
  then Printf.sprintf "[%d]" x
  else Printf.sprintf "[%d:%d]" x (x+y-1)

(* Opcode are represented by their integer value, reserved bits by a
 * series of '/' and ifields by their name. *)
let parse_field s p =
  try Opcode (int_of_string s, p)
  with _ ->
    if Str.string_match (Str.regexp "^[ /]*$") s 0
    then Reserved p
    else Ifield (s, p)

(** Returns the position of the first bit of a field *)
let get_pos = function
  | Opcode (_, (p, _)) | Ifield (_, (p, _))
  | SplitIfield (_, (p, _), _) | Reserved (p, _) -> p

let compare_pos i i' = compare (get_pos i) (get_pos i')

let build fields positions =
  let rec compute_size = function
    | [] -> []
    | [x] -> [(x, 32-x)]
    | x :: y :: xs ->
      assert(y>x);
      (x, y-x) :: compute_size (y :: xs) in
  let rec make_split_ifields = function
    | Ifield (f, p) :: Ifield (f', p') :: l when f = f' ->
      make_split_ifields (SplitIfield (f, p, p') :: l)
    | SplitIfield (f, _, _) :: Ifield (f', _) :: _ when f = f' ->
      failwith ("split ifield of more than two parts: "^f)
    | x :: l -> x :: make_split_ifields l
    | [] -> [] in
  List.map2 parse_field fields (compute_size positions)
  |> List.sort compare |> make_split_ifields
  |> List.sort compare_pos

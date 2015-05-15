open Genlex

type sep = NoSep | Comma | Paren
type param = sep * string
type t = { name: string; dot: bool; params: param list; flags: (string * bool) list }

(** {2 Parsing} *)

let list_of_stream stream =
  let result = ref [] in
  Stream.iter (fun value -> result := value :: !result) stream;
  List.rev !result

let lexer = make_lexer ["."; " "; ","; "("; ")"; "="; ]

let lexwith s = list_of_stream (lexer (Stream.of_string s))

let rec parse_mnemo = function 
  | Ident name :: tl ->
    let dot, tl = parse_dot tl in
    let params, tl = parse_params tl in
    let flags, tl = parse_flags tl in
    let tl = skip_trailing_hexa_vector_scalar tl in
    let mnemo = { name; dot; params; flags } in
    assert(tl=[]);
    mnemo
  | tl -> failwith "parse error"
and parse_dot : token list -> bool * token list = function
  | Kwd "." :: tl -> true, tl
  | tl -> false, tl
and parse_params : token list -> param list * token list = function
  | Ident a :: tl ->
    let (l, tl) = parse_params tl in
    (NoSep, a) :: l, tl
  | Kwd "," :: Ident a :: tl ->
    let (l, tl) = parse_params tl in
    (Comma, a) :: l, tl
  | Kwd "(" :: Ident a :: Kwd ")" :: tl ->
    let (l, tl) = parse_params tl in
    (Paren, a) :: l, tl
  | tl -> [], tl
and parse_flags : token list -> (string * bool) list * token list = function
  | (Kwd "(" :: tl) as tokens ->
    let flags, tl = parse_cond tl in
    if flags = []
    then (* mispredicted, rollback *)
      [], tokens
    else
      flags, tl
  | tl -> [], tl
and parse_cond : token list -> (string * bool) list * token list = function
  | Ident a :: Kwd "=" :: Int n :: tl ->
    let flags, tl = parse_cond tl in
    let b = match n with
      | 0 -> false
      | 1 -> true
      | _ -> failwith "unexpected condition" in
    (a,b) :: flags, tl
  | Kwd ")" :: tl | tl -> [], tl
and skip_trailing_hexa_vector_scalar = function
  (* the vector-scalar manual embeds the hexadecimal encoding of the
   * opcode in the mnemonics, for some completely mysterious reason.
   * Just ignore it. Note that Genlex does not parse hexadecimal
   * numbers, so 0x... is parsed as 0 followed by an ident starting with
   * x. *)
  | Kwd "(" :: Int 0 :: Ident _ :: Kwd ")" :: tl | tl -> tl


let parse s = parse_mnemo (lexwith s)


(** {2 Pretty-printing} *)

let name_to_string m = m.name ^ (if m.dot then "." else "")
let param_to_string = function
  | NoSep, s -> s
  | Comma, s -> Printf.sprintf ",%s" s
  | Paren, s -> Printf.sprintf "(%s)" s

let to_string m =
  let name = name_to_string m in
  let params = String.concat "" (List.map param_to_string m.params) in
  let make_cond (k,v) =
    Printf.sprintf "%s=%s" k (if v then "1" else "0") in
  let flags' = String.concat " " (List.map make_cond m.flags) in
  if m.flags = [] then
    Printf.sprintf "%s %s" name params
  else
    Printf.sprintf "%s %s (%s)" name params flags'

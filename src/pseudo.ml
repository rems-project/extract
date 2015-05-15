(** {2 AST} *)

type ifield = string
type var = string

type unop =
  | Uminus
  | Not

type binop =
  | Plus
  | Minus
  | Times
  | Div
  | Or
  | Xor
  | Mod
  | And
  | Eq
  | Neq
  | Gt
  | Ge
  | Gtu
  | Lt
  | Le
  | Ltu
  | Replicate
  | Shiftl
  | Shiftr

type reg =
  | RLit of string
  | DCR of expr
  | GPR of expr
  | SPR of expr

and lval =
  | Reg of reg
  | Var of var
  | Ifield of ifield
  | Mem of expr * expr

and expr =
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | Func of string * expr list
  | Concat of expr list
  | Int of int
  | Bitfield of string
  | Lval of lval
  | Sub of expr * sub
  | StarIfield of ifield
  | Undef

and sub =
  | Bit of expr
  | Range of expr * expr  (* n:m *)
(* | Set of expr list *)

type for_init =
  | SimpleInit of expr
  | CondInit of expr * expr * expr

type for_direction = Inc | Dec

type assign_kind = Regular | Iea

type instr =
  | Assign of (lval * sub option) * expr * assign_kind
  | Block of block
  | If of expr * block * block
  | DoWhile of expr * block
  | For of string * for_direction * for_init * expr * expr * block
  | Trap
  | Leave
  | Expr of expr

and block = instr list


(** {2 Utility functions *)

(** Recurses into a block of pseudo-code and returns the list of
 * encountered {!Pseudo.lval.Var}. *)
let collect_vars =
  let rec aux_reg acc = function
    | RLit s -> acc
    | DCR e | GPR e | SPR e -> aux_expr acc e

  and aux_lval acc = function
    | Reg r -> aux_reg acc r
    | Var v -> if List.mem v acc then acc else (v :: acc)
    | Ifield i -> acc
    | Mem (lv, e) -> let acc' = aux_expr acc lv in aux_expr acc' e

  and aux_expr acc = function
    | Unop (_, e) -> aux_expr acc e
    | Binop (_, e, e') -> let acc' = aux_expr acc e in aux_expr acc' e'
    | Func (_, l)
    | Concat l -> List.fold_left aux_expr acc l
    | Int _
    | Bitfield _ -> acc
    | Lval lv -> aux_lval acc lv
    | Sub (e, sub) -> let acc' = aux_expr acc e in aux_sub acc' (Some sub)
    | StarIfield i -> acc
    | Undef -> acc

  and aux_sub acc = function
    | Some (Bit e) -> aux_expr acc  e
    | Some (Range (e, e')) -> let acc' = aux_expr acc e in aux_expr acc' e'
    | None -> acc
  in

  let rec aux_instr acc = function
    | Assign ((lv, sub), e, _) ->
      let acc' = aux_lval acc lv in
      let acc'' = aux_sub acc' sub in
      aux_expr acc'' e
    | Block b -> block_aux acc b
    | If (e, b, b') ->
      let acc' = aux_expr acc e in
      block_aux acc' (b@b')
    | DoWhile (e, b) ->
      let acc' = aux_expr acc e in
      block_aux acc' b
    | For (_, _, init, e, step, b) ->
      let acc = aux_init acc init in
      let acc = aux_expr acc e in
      let acc = aux_expr acc step in
      block_aux acc b
    | Trap
    | Leave -> acc
    | Expr e -> aux_expr acc e

  and aux_init acc = function
    | SimpleInit e -> aux_expr acc e
    | CondInit (e1, e2, e3) ->  aux_expr (aux_expr (aux_expr acc e1) e2) e3

  and block_aux acc l = List.fold_left aux_instr acc l
  in

  block_aux []

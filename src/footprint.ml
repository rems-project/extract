open Pseudo

(** {2 Register footprint} *)

type ifield_type = RegIn | RegOut | Imm | RegInOut
                 | RegInImm | RegOutImm | RegInOutImm
                 | RegInFrom | RegOutFrom

let ifield_type_to_string = function
  | RegIn -> "IN"
  | RegOut -> "OUT"
  | RegInOut -> "IN/OUT"
  | RegInImm -> "IN_"
  | RegOutImm -> "OUT_"
  | RegInOutImm -> "IN/OUT_"
  | RegInFrom -> "IN->32" 
  | RegOutFrom -> "OUT->32"
  | Imm -> "IMM"

(** Normalize ifield name [k] and update its value [v] in the
 *  association list [l]. The update is cumulative: a former RegIn
 *  becomes a RegInImm if we associate it with RegImm, etc. *)
let add_ifield k v l =
  let k = String.lowercase k in
  if not (List.mem_assoc k l) then
    (k, v) :: l
  else match  List.assoc k l, v with
    | v', v when v = v' -> l
    (* Imm should be upgraded to RegIn or RegOut because of code
       like: if RA=0 then ... else ...; b <- (RA) *)
    | Imm, RegIn -> (k, RegInImm) :: (List.remove_assoc k l)
    | Imm, RegOut -> (k, RegOutImm) :: (List.remove_assoc k l)
    | (RegIn|RegInOut), RegOut
    | (RegOut|RegInOut), RegIn ->
      (k, RegInOut) :: (List.remove_assoc k l)
    | (RegInImm|RegInOutImm), RegOut
    | (RegOutImm|RegInOutImm), RegIn ->
      (k, RegInOutImm) :: (List.remove_assoc k l)
    | _ -> assert false


(** Recurse into an instruction block and accumulate inferred types
 * about ifields. *)
let infer_ifield_type =
  let rec aux_reg acc = function
    (* To get a complete register footprint, those direct accesses should
     * be dealt with as well, but we focus on ifield here.  In chapter 3,
     * GPR is used for load/store multiple and string load/store, DCR/SPR
     * are used for load/store device control/special purpose register.
     * RLit is used for CR, among others, but it is conservatively marked
     * as clobbered in litmus anyway. (And more systematic way of dealing
     * with altered special registers would be to parse the relevant
     * section in the manual, since not all of them are mentionned in
     * pseudo-code.) *)
    | RLit s -> acc
    | DCR e | GPR e | SPR e -> aux_expr acc e

  and aux_lval acc = function
    | Reg r -> aux_reg acc r
    | Var v -> acc
    | Ifield i -> add_ifield i Imm acc
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
    | StarIfield i -> add_ifield i RegIn acc
    | Undef -> acc

  and aux_sub acc = function
    | Some (Bit e) -> aux_expr acc  e
    | Some (Range (e, e')) -> let acc' = aux_expr acc e in aux_expr acc' e'
    | None -> acc
  in

  let rec aux_instr acc = function
    | Assign ((lv, sub), e, _) ->
      let acc' = match lv with
        | Reg _ | Var _ | Mem _ -> aux_lval acc lv
        | Ifield i -> add_ifield i RegOut acc in
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

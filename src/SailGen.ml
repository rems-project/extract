(* Pseudo-code to Sail *)

open Pseudo

let (|>) x f = f x

let rec replace prev next = function
  | hd :: tl when hd = prev -> next :: tl
  | hd :: tl -> hd :: replace prev next tl
  | [] -> raise Not_found


let unop_to_string = function
  | Uminus -> "-"
  | Not -> "~"



let binop_to_string = function
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*" (* XXX or *_u depending on context *)
  | Div -> "quot"    (* XXX or "div" depending on context *)
  | And -> "&"
  | Or -> "|"
  | Xor -> "^"
  | Mod -> "mod"
  | Eq -> "=="
  | Neq -> "!="
  | Gt -> ">"
  | Ge -> ">="
  | Gtu -> ">_u"
  | Lt -> "<"
  | Le -> "<="
  | Ltu -> "<_u"
  | Replicate -> "^^"
  | Shiftl -> "<<"
  | Shiftr -> ">>"


(* Eliminate while loops, transforming them to for loops.
 * This seems do be always doable in practice for the ISA code,
 * but requires a bit of creativity to identify loop variables, along
 * with their initial value and increment. *)

let deconstruct_while = function
  | DoWhile(Binop (Le, Lval(Var n),Int bound), b) ->
    (n, bound, Inc, b)
  | DoWhile(Binop (Lt, Lval(Var n),Int bound), b) ->
    (n, bound-1, Inc, b)
  | DoWhile(Binop (Ge, Lval(Var n),Int bound), b) ->
    (n, bound, Dec, b)
  | DoWhile(Binop (Gt, Lval(Var n),Int bound), b) ->
    (n, bound+1, Dec, b)
  | _ -> failwith "unknown while form"


let find_inits enclosing loop =
  let rec split_around acc = function
    | hd :: tl when hd = loop -> tl, acc
    | hd :: tl -> split_around (hd :: acc) tl
    | [] -> assert false in
  let rec collect acc = function
    | rest -> (List.rev rest, acc) in
  let (before_rev, after) = split_around [] (List.rev enclosing) in
  let (before, inits) = collect [] before_rev in
  (before, inits, after)


let unwhile enclosing while_loop =
  let (index, bound, dir, loop_body) = deconstruct_while while_loop in
  let rec find_init acc = function
    | [] -> None, List.rev acc
    | Assign ((Var v, None), e, Regular) :: tl when v = index ->
      Some(SimpleInit e), (List.rev_append acc tl)
    | If(condexp, [Assign ((Var v, None), e1, Regular)], [Assign ((Var v', None), e2, Regular)]) :: tl
      when v = index && v' = index ->
      Some(CondInit (condexp, e1, e2)), (List.rev_append acc tl)
    | hd :: tl -> find_init (hd :: acc) tl in

  let extract pair = match pair with Some x, y -> x, y | _ -> assert false in
  let init, enclosing = extract (find_init [] enclosing) in
  let increment, loop_body = extract (find_init [] loop_body) in

  (* various sanity checks *)
  if(fst(find_init [] enclosing) <> None) then
    failwith "multiple index assignements outside of loop";
  if(fst(find_init [] loop_body) <> None) then
    failwith "multiple index assignements inside loop";
  if(dir = Inc && increment <> SimpleInit(Binop(Plus, Lval(Var index), Int 1))) then
    failwith "unexpected increment";
  if(dir = Dec && increment <> SimpleInit(Binop(Minus, Lval(Var index), Int 1))) then
    failwith "unexpected decrement";

  let for_loop = For(index, dir, init, Int bound, Int 1, loop_body) in
  replace while_loop for_loop enclosing


let unwhile b =
  try List.find (function DoWhile _ -> true | _ -> false) b |> unwhile b
  with Not_found -> b

(* Translation to Sail --- producing an AST *)

module A = Ast
module PA = Parse_ast

(* Helper functions to build Sail AST nodes *)

(* location and type annotations *)
let lunk = PA.Unknown
(* WAS let tunk = lunk, Type_internal.NoTyp -- SS guessing at appropriate replacement *)
let tunk = lunk, A.Typ_internal_unknown
(* identifiers *)
let id i = A.Id_aux (A.Id i, lunk)
let deiid i = A.Id_aux (A.DeIid i, lunk)
(* n-expressions *)
let nexp ne = A.Nexp_aux (ne, lunk)
let nexp_const n = nexp (A.Nexp_constant n)
(* order *)
let ord o = A.Ord_aux (o, lunk)
let ord_inc = ord A.Ord_inc
let ord_dec = ord A.Ord_dec
(* type arguments *)
let typ_arg ta = A.Typ_arg_aux (ta, lunk)
let typ_arg_nexp ne = typ_arg (A.Typ_arg_nexp ne)
let typ_arg_ord o = typ_arg (A.Typ_arg_order o)
let typ_arg_typ t = typ_arg (A.Typ_arg_typ t)
(* types *)
let typ t = A.Typ_aux (t, lunk)
let typ_id i = typ (A.Typ_id (id i))
let typ_app i tal = typ (A.Typ_app (id i, tal))
let typ_bit = typ_id "bit"
let typ_unit = typ_id "unit"
let typ_tup l = typ (A.Typ_tup l)
let typ_bitvec ?(inc=true) ?(n=0) m = typ_app "vector" [
    typ_arg_nexp (nexp_const n);
    typ_arg_nexp (nexp_const m);
    typ_arg_ord (if inc then ord_inc else ord_dec);
    typ_arg_typ typ_bit;
  ]
(* l-expressions *)
let lexp le = A.LEXP_aux (le, tunk)
let lexp_id i = lexp (A.LEXP_id (id i))
let lexp_mem i el = lexp (A.LEXP_memory (id i, el))
let lexp_cast t i = lexp (A.LEXP_cast (t, id i))
let lexp_vec le e = lexp (A.LEXP_vector (le, e))
let lexp_vec_range le e1 e2 = lexp (A.LEXP_vector_range (le, e1, e2))
let lexp_field e1 i = lexp (A.LEXP_field (e1, id i))
(* expressions *)
let exp e = A.E_aux (e, tunk)
let exp_id i = exp (A.E_id (id i))
let exp_vec l = exp (A.E_vector l)
let exp_vec_access e1 e2 = exp (A.E_vector_access (e1, e2))
let exp_vec_sub e1 e2 e3 = exp (A.E_vector_subrange (e1, e2, e3))
let exp_field e1 i = exp (A.E_field (e1, id i))
let exp_app i el = exp (A.E_app (id i, el))
let exp_appi e1 i e2 = exp (A.E_app_infix (e1, id i, e2))
let exp_lit lit = exp (A.E_lit (A.L_aux (lit, lunk)))
let exp_assign lexp e = exp (A.E_assign (lexp, e))
let exp_block el = exp (A.E_block el)
let exp_if e1 e2 e3 = exp (A.E_if (e1, e2, e3))
let exp_cast t e = exp (A.E_cast (t, e))
let exp_for i e1 e2 e3 inc e4 = exp (A.E_for (id i, e1, e2, e3, (if inc then ord_inc else ord_dec), e4))
let exp_vec_append e1 e2 = exp (A.E_vector_append (e1, e2))
let rec exp_concat = function
  | [] -> assert false
  | [e] -> e
  | e::l -> exp_vec_append e (exp_concat l)
let exp_int i = exp_lit (A.L_num i)
(* patterns *)
let pat p = A.P_aux (p, tunk)
let pat_wild = pat (A.P_wild)
let pat_typ t p = pat (A.P_typ (t, p))
let pat_id i = pat (A.P_id (id i))
let pat_lit l = pat (A.P_lit (A.L_aux (l, lunk)))
let pat_as p i = pat (A.P_as (p, id i))
let pat_app i pl = pat (A.P_app (id i, pl))
let pat_vector l = pat (A.P_vector l)
let pat_vector_concat l = pat (A.P_vector_concat l)
(* scattered definitions and related helpers *)
let funcl i pat exp = A.FCL_aux (A.FCL_Funcl (id i, pat, exp), tunk)
let scattered_funcl i pat exp = A.SD_scattered_funcl (funcl i pat exp)

let scattered_unioncl i type_union =
  A.SD_scattered_unioncl (id i, type_union)
let type_union t = A.Tu_aux (t, lunk)
let type_union_id i = type_union (A.Tu_id (id i))
let type_union_ty_id typ i = type_union (A.Tu_ty_id (typ, id i))

let scattered_def sd = A.SD_aux (sd, tunk)
let def_scattered sd = A.DEF_scattered (scattered_def sd)
let scattered_defs d = A.Defs (List.map def_scattered d)

(* those are useless currently - the header and footer for scattered
 * definitions are written manually in header.sail and footer.sail.

   let rec_opt r =
   A.Rec_aux ((if r then A.Rec_rec else A.Rec_nonrec), lunk)
   let tannot_opt (typq, typ) =
   A.Typ_annot_opt_aux (A.Typ_annot_opt_some (typq, typ), lunk)
   let scattered_function r tannot_opt effect_opt i =
   A.SD_scattered_function (rec_opt r, tannot_opt, effect_opt, id i)

   let scattered_variant i ns typq =
   A.SD_scattered_variant (id i, ns, typq)
   let name_scm_opt ns = A.Name_sect_aux (ns, lunk)
   let name_sect_none = name_scm_opt (A.Name_sect_none)
   let name_sect_some s = name_scm_opt (A.Name_sect_some s)

   let scattered_end i = A.SD_scattered_end (id i)

   (* type quantifiers - or rather lack thereof *)
   let typquant t = A.TypQ_aux (t, lunk)
   let typquant_no_forall = typquant A.TypQ_no_forall

   (* effects *)
   let effect e = A.Effect_opt_aux (e, lunk)
   let effect_pure = effect A.Effect_opt_pure
*)

let rec disc_log n b = match n with
  | 1 -> 0
  | n when n > 0 && n mod b = 0 -> 1 + disc_log (n/b) b
  | _ -> raise (Invalid_argument "disc_log")

(* The actual translation *)

(* Ifields representing registers are implicitly dereferenced to some
 * registers depending on the current chapter: GPR for fixed-point, FPR
 * for floating-point, etc. Instead of taking chapter into account, we
 * are more fine-grained and careful and use the name of the ifield. *)
let register_dereference = function
  | "RA" | "RB" | "RS" | "RT" -> "GPR"
  | "FRA" | "FRB" | "FRC" | "FRS" | "FRT" -> "FPR"
  | "FRTp" -> "FPRp"
  | "VRA" | "VRB" | "VRC" | "VRS" | "VRT" -> "VR"
  | i -> failwith ("bug: assignment to unknown ifield "^i^", update register_dereference")

let rec reg_to_sail = function
  | RLit s -> exp_id s
  | DCR e -> exp_vec_access (exp_id "DCR") (expr_to_sail e)
  | GPR e -> exp_vec_access (exp_id "GPR") (expr_to_sail e)
  | SPR e -> exp_vec_access (exp_id "SPR") (expr_to_sail e)

(* registers when they are assigned to - it is a good thing that Sail's
 * AST forces us to distinguish lexp and exp, since their translation
 * has to be done differently; compare with reg_to_sail above. *)
and lreg_to_sail = function
  | RLit s -> lexp_id s
  | DCR e -> lexp_vec (lexp_id "DCR") (expr_to_sail e)
  | GPR e -> lexp_vec (lexp_id "GPR") (expr_to_sail e)
  | SPR e -> lexp_vec (lexp_id "SPR") (expr_to_sail e)

(* lval when they are assigned to - as above, compare this code to the
 * Lval case in expr_to_sail. *)
and lval_to_sail = function
  | Reg r -> lreg_to_sail r
  | Var v -> lexp_id v
  | Ifield i -> lexp_vec (lexp_id (register_dereference i)) (exp_id i)
  | Mem (l, e) ->
    (* Memory write *)
    (* XXX endianness *)
    lexp_mem "MEMw" [expr_to_sail l; expr_to_sail e]

and expr_to_sail = function
  | Unop (op, e) ->
    exp_app (unop_to_string op) [expr_to_sail e]
  (* add cast for (e1 + n) mod m to resolve overloading ambiguity *)
  | Binop (Mod, Binop(op, e1, Int n), Int m) ->
    let inner_op =
      exp_cast (typ_bitvec (disc_log m 2)) (exp_appi ((expr_to_sail e1)) (binop_to_string op) (exp_int n)) in
    exp_appi inner_op (binop_to_string Mod) (exp_int m)
  | Binop (op, e1, e2) ->
    exp_appi (expr_to_sail e1) (binop_to_string op) (expr_to_sail e2)
  (* exts is not overloaded - convert bit to vector *)
  | Func("EXTS", [Sub (e1, Bit e2)]) -> expr_to_sail (Func("EXTS", [Sub(e1, Range (e2, e2))]))
  (* for vspltisb etc., size of exts is given explicitly - use a
   * specific function *)
  | Func("EXTS", [e1; e2]) -> expr_to_sail(Func("EXTS_EXPLICIT", [e1; e2]))
  | Func (f, el) ->
    exp_app f (List.map expr_to_sail el)
  | Concat l ->
    (* deal with special-cases, such as XER.SO, to make sure we only
     * have bitvectors in the concatenation *)
    let expr_to_bitvec e =
      let e' = expr_to_sail e in
      match e' with
      | A.E_aux(A.E_lit(A.L_aux ((A.L_zero|A.L_one),_)), _)
      | A.E_aux(A.E_vector_access (A.E_aux (A.E_id (A.Id_aux(A.Id ("sh"|"mb"|"me"), _)), _), _), _)
      (* in Parity Word/Doubleword, and vector-scalar chapter *)
      | A.E_aux(A.E_id(A.Id_aux(A.Id ("s"|"t"|"TX"|"SX"|"AX"|"BX"), _)), _)
      | A.E_aux(A.E_field (A.E_aux (A.E_id (A.Id_aux(A.Id "XER", _)), _), _), _) ->
        exp_vec [e']
      | _ -> e' in
    exp_concat (List.map expr_to_bitvec l)
  | Int (i) -> exp_int i
  | Bitfield "0" -> exp_lit A.L_zero
  | Bitfield "1" -> exp_lit A.L_one
  | Bitfield s -> exp_lit (A.L_bin s)
  | Lval l -> begin match l with
      | Reg r -> reg_to_sail r
      | Var v -> exp_id v
      | Ifield i -> exp_id i
      | Mem (l, e) ->
        (* Memory read *)
        (* XXX endianness *)
        exp_app "MEMr" [expr_to_sail l; expr_to_sail e]
    end
  (* Special case: translate XER[CA] to XER.CA, etc. *)
  | Sub (Lval(Reg(RLit "XER")), Bit (Lval (Var ("CA"|"OV"|"SO" as f)))) ->
    exp_field (exp_id "XER") f
  | Sub (e1, Bit e2) ->
    exp_vec_access (expr_to_sail e1) (expr_to_sail e2)
  | Sub (e1, Range (e2, e3)) ->
    exp_vec_sub (expr_to_sail e1) (expr_to_sail e2) (expr_to_sail e3)
  | StarIfield i ->
    (* XXX what if it's not a GPR? --- depends of context *)
    exp_vec_access (exp_id (register_dereference i)) (exp_id i)
  | Undef -> exp_lit A.L_undef (* XXX *)


let rec instr_to_sail = function
  (* special case: variable assignement with 0..2^n-1 range is always a
   * variable declaration in practice *)
  | Assign ((Var v, Some (Range(Int 0, Int ((31|63|127|255) as n)))), e, Regular) ->
    exp_assign (lexp_cast (typ_bitvec (n+1)) v) (expr_to_sail e)
  (* Help constraint inference *)
  | Assign ((l, Some (Range((Binop(Plus, e, Int n) as e1), (Binop(Plus, e', Int m) as e2)))), Int k, Regular) when e = e' ->
      exp_assign (lexp_vec_range (lval_to_sail l) (expr_to_sail e1) (expr_to_sail e2)) (exp_cast (typ_bitvec (m-n+1)) (exp_int k))
  (* XXX do address conversions for IEA *)
  | Assign ((l, None), e, (Regular|Iea)) ->
    exp_assign (lval_to_sail l) (expr_to_sail e)
  (* Special case: translate XER[CA] to XER.CA, etc. *)
  | Assign ((Reg(RLit "XER"), Some (Bit (Lval (Var ("CA"|"OV"|"SO" as f))))), e, Regular) ->
    exp_assign (lexp_field (lexp_id "XER") f) (expr_to_sail e)
  (* regular assignment with slicing *)
  | Assign ((l, Some sub), e, (Regular|Iea)) ->
    let lval = lval_to_sail l in
    let vec_access = match sub with
      | Bit e -> lexp_vec lval (expr_to_sail e)
      | Range (e1, e2) -> lexp_vec_range lval (expr_to_sail e1) (expr_to_sail e2)
    in
    exp_assign vec_access (expr_to_sail e)
  | Block b -> block_to_sail b
  | If (e, b1, b2) ->
    let cond = expr_to_sail e in
    exp_if cond (block_to_sail b1) (block_to_sail b2)
  | DoWhile _ -> assert false
  | For (var,  dir, lower, upper, step, b) ->
    let lower_exp = match lower with
      | SimpleInit lower -> expr_to_sail lower
      | CondInit (cond, lowt, lowf) ->
        exp_if (expr_to_sail cond) (expr_to_sail lowt) (expr_to_sail lowf)
    in
    exp_for var
      lower_exp (expr_to_sail upper)
      (expr_to_sail step) (dir = Inc)
      (block_to_sail b)
  | Trap -> exp_app "trap" [] (* XXX *)
  | Leave -> exp_app "break" [] (* XXX *)
  | Expr e -> expr_to_sail e

and block_to_sail b =
  (* eliminate while loops in the current block *)
  match unwhile b with
  (* If there is a single expression, do not insert a block. This is
   * important because type-inference relies on the absence of blocks
   * for scoping in cases like:
   * if cond then M := 1 else M := 2
  *)
  | [e] -> instr_to_sail e
  (* no need to special-case [], the pretty-printer takes care of it. *)
  (* need to special-case [], despite pretty-printer, to be able to insert content *)
  | [] -> exp_lit (A.L_unit)
  | b -> exp_block (List.map instr_to_sail b)


(* Build scattered clauses for a given instruction: ast, decode
 * and execute *)

(* AST *)
let make_ast name ifields =
  let field_type = function
  | BinRep.Ifield (_, (_, 1)) -> typ_bit
  | BinRep.Ifield (_, (_, size)) -> typ_bitvec size
  | BinRep.SplitIfield(_, (_, size), (_, size')) ->
      typ_bitvec (size+size')
  | BinRep.Opcode _ | BinRep.Reserved _ -> assert false in
  let ast_type = match ifields with
    | [] -> type_union_id name
    | _ -> let ftypes = List.map field_type ifields in
        type_union_ty_id (typ_tup ftypes) name in
  scattered_unioncl "ast" ast_type

(* Decode
 * We need a number of auxiliary functions here to pattern-match the
 * binary representation of Opcodes and handle split ifields *)

let to_big_endian (n:int) (size:int) : bool list =
  let rec aux acc n size =
    if size <= 0 then acc
    else
      let last, rest = n mod 2 = 1, n lsr 1 in
      aux (last :: acc) rest (size-1) in
  aux [] n size

(* Convert SplitIfield to Reserved for pattern matching - we cannot
 * pattern-match on split ifields directly, we need to reconstruct them
 * by slicing the instruction *)
let make_split_reserved (binrep : BinRep.t) : BinRep.t =
  binrep
  |> List.map (function
    | BinRep.SplitIfield (_, p, p') -> [BinRep.Reserved p; BinRep.Reserved p']
    | f -> [f])
  |> List.concat
  |> List.sort BinRep.compare_pos

let make_decode name binrep ifields =
  let binrep_nosplit = make_split_reserved binrep in
  let decode_pat = List.map (function
    | BinRep.Opcode(value, (_, size)) ->
          pat_lit (A.L_bin (List.fold_right 
			      (fun bit bin -> (if bit then "1" else "0") ^ bin) 
			      (to_big_endian value size) ""))
(*        pat_vector (List.map
          (fun bit -> pat_lit (if bit then A.L_one else A.L_zero))
          (to_big_endian value size)) *)
    | BinRep.Reserved (_, size) -> pat_typ (typ_bitvec size) pat_wild
    | BinRep.Ifield (name, (_, 1)) -> pat_vector [pat_id name]
    | BinRep.Ifield (name, (_, size)) ->
        pat_typ (typ_bitvec size) (pat_id name)
    | BinRep.SplitIfield _ -> assert false
    ) binrep_nosplit in
  let decode_extract = List.map (function
    | BinRep.Ifield (name, _) -> exp_id name
    | BinRep.SplitIfield(_, pos, pos') ->
      exp_concat (List.map
        (fun (p,l) -> exp_vec_sub (exp_id "instr") (exp_int p) (exp_int (p+l-1)))
        [pos; pos'])
    | _ -> assert false
    ) ifields in
  let decode_exp = exp_app "Some" [(exp_app name decode_extract)] in
  scattered_funcl "decode" (pat_as (pat_vector_concat decode_pat) "instr") decode_exp


(* make memory stores explicitly announce the address before the actual memory
  write *)
let rewrite_store iname body = 
  let open A in
  (* insert y into xs after the nth element of xs for which p holds *)
  let rec insert_after n p y xs = match xs with
    | [] -> failwith "no element found satifying predicate"
    | x :: xs ->
       if p x then
         if n = 1 then x :: y :: xs
         else x :: insert_after (n - 1) p y xs
       else x :: insert_after n p y xs in
  let is_assignment_of vname = function
    | E_aux (E_assign (LEXP_aux (LEXP_id (Id_aux (Id vname',_)),_),_),_)
    | E_aux (E_assign (LEXP_aux (LEXP_cast (_,Id_aux (Id vname',_)),_),_),_) ->
       vname = vname'
    | _ -> false in
  let is_recalculate_dependency = function
    | E_aux (E_app (Id_aux (Id "recalculate_dependency",_),_),_) -> true
    | _ -> false in
  (* should be a fold for exp types, but this suffices *)
  let rec find_memw_args exps = match exps with
    | [] -> failwith "MEMw call not found"
    | exp :: exps ->
       match exp with
       | E_aux (E_assign (LEXP_aux (LEXP_memory (Id_aux (Id _,_),args),_),_),_)
       | E_aux (E_app (Id_aux (Id "MEMw",_),args),_)
       | E_aux (E_app (Id_aux (Id "MEMw_conditional",_),args),_)
       | E_aux (E_assign (_,(E_aux (E_app (Id_aux (Id "MEMw",_),args),_))),_)
       | E_aux (E_assign (_,(E_aux (E_app (Id_aux (Id "MEMw_conditional",_),args),_))),_) ->
          let arg1 :: arg2 :: _ = args in
          [arg1;arg2]
       | E_aux (E_if (_,exp1,exp2),_) ->
          find_memw_args (exp1 :: exp2 :: exps)
       | _ -> find_memw_args exps in

  let announce_addr cond args =
    exp_app (if cond then "MEMw_EA_cond" else "MEMw_EA") args in

  match iname with
  | "Stmw"
  | "Stswi" -> 
     (* insert memw_addr only after "size := ..." *)
     let (E_aux (E_block exps,eannot)) = body in
     let fp = find_memw_args exps in
     let exps = insert_after 1 (is_assignment_of "size") (announce_addr false fp) exps in
     E_aux (E_block exps,eannot)
 | "Stswx" ->
     (* insert after recalculate_dependency *)
     let (E_aux (E_block exps,eannot)) = body in
     let fp = find_memw_args exps in
     let exps = insert_after 1 is_recalculate_dependency (announce_addr false fp) exps in
     E_aux (E_block exps,eannot)
  (* normal stores *)
  | "Stb" | "Stbx" | "Stbu" | "Stbux"
  | "Sth" | "Sthx" | "Sthu" | "Sthux"
  | "Stw" | "Stwx" | "Stwu" | "Stwux"
  | "Std" | "Stdx" | "Stdu" | "Stdux"
  | "Stq"
  | "Sthbrx" | "Stwbrx" | "Stdbrx" ->
     (* insert mem_addr after "EA := ...", and return arguments of
        MEMw/MEMw_conditional *)
     let (E_aux (E_block exps,eannot)) = body in
     let fp = find_memw_args exps in
     let exps = insert_after 2 (is_assignment_of "EA") (announce_addr false fp) exps in
     E_aux (E_block exps,eannot)
  (* store conditionals *) 
  | "Stbcx"  | "Sthcx" | "Stwcx" | "Stdcx" ->
     (* insert mem_addr after "EA := ...", and return arguments of
        MEMw/MEMw_conditional *)
     let (E_aux (E_block exps,eannot)) = body in
     let fp = find_memw_args exps in
     let exps = insert_after 2 (is_assignment_of "EA") (announce_addr true fp) exps in
     E_aux (E_block exps,eannot)
  | _ -> body
                  

let make_execute name ifields block patch = 
  let ifields_pat = List.map (function
      BinRep.Ifield (name, _) | BinRep.SplitIfield (name, _, _) -> pat_id name
    | _ -> assert false) ifields in
  let body = block_to_sail block in
  let body' = patch body in
  let body'' = rewrite_store name body' in
  scattered_funcl "execute" (pat_app name ifields_pat) body''

let make_instr name binrep block patch =
  let ifields = List.filter BinRep.(function Ifield _ | SplitIfield _ -> true | _ -> false) binrep in
  let ast_member = make_ast name ifields in
  let decode = make_decode name binrep ifields in
  let execute = make_execute name ifields block patch in
  scattered_defs [ast_member; decode; execute]


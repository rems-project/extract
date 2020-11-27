(** Sail model generation *)

(** [make_instr name binrep pseudo patch] produces Sail scattered definitions
 * for the AST, decode and execute functions, given an instruction name,
 * its binary representation, and a block of pseudo-code for its
 * semantics. The [patch] function is applied to the body of the execute
 * function. *)
val make_instr :
  Ast.x -> BinRep.t -> Pseudo.block -> 
    (Type_internal.tannot Ast.exp -> Type_internal.tannot Ast.exp)
    -> Type_internal.tannot Ast.defs
 
val exp_block : Type_internal.tannot Ast.exp list -> Type_internal.tannot Ast.exp

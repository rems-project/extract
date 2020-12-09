(** Sail model generation *)

(** [make_instr name binrep pseudo patch] produces Sail scattered definitions
 * for the AST, decode and execute functions, given an instruction name,
 * its binary representation, and a block of pseudo-code for its
 * semantics. The [patch] function is applied to the body of the execute
 * function. *)
val make_instr :
  Ast.x -> BinRep.t -> Pseudo.block -> 
    (Ast.typ_aux Ast.exp -> Ast.typ_aux Ast.exp)
    -> Ast.typ_aux Ast_defs.ast
    
val exp_block : Ast.typ_aux Ast.exp list -> Ast.typ_aux Ast.exp

(** Litmus boilerplate code generation *)

(** In this module, we make use of {!Footprint}. We also make a number
 * of assumptions about instructions, which are (almost certainly) valid
 * for the fixed-point chapter, but probably less-so for others. See
 * [XXX] comments in source code for details. *)

(** Lexer *)
val gen_lex : 'a -> Mnemo.t -> string

(** Parser *)
val gen_parse : (string * Footprint.ifield_type) list -> Mnemo.t -> string

(** List of tokens for the parser *)
val gen_token : 'a -> Mnemo.t -> string

(** AST *)
val gen_ast : (string * Footprint.ifield_type) list -> Mnemo.t -> string

(** Pretty-printer *)
val gen_pp : (string * Footprint.ifield_type) list -> Mnemo.t -> string

(** Fold over AST *)
val gen_fold : (string * Footprint.ifield_type) list -> Mnemo.t -> string

(** Map over AST *)
val gen_map : (string * Footprint.ifield_type) list -> Mnemo.t -> string

(** Translation to GCC inline assembly *)
val gen_compile : (string * Footprint.ifield_type) list -> Mnemo.t -> string

(** Translation to Lem-generated-OCaml type of Sail-generated-Lem instructions *)
val gen_trans_sail : (string * Footprint.ifield_type) list -> Instruction.t -> string

val gen_herdtools_ast_to_shallow_ast : (string * Footprint.ifield_type) list -> Instruction.t -> string

(** Translation from Lem-generated-Ocaml type of Sail-generated-Lem instructions to Ocaml *)
val gen_sail_trans_out : (string * Footprint.ifield_type) list -> Instruction.t -> string

val gen_shallow_ast_to_herdtools_ast : (string * Footprint.ifield_type) list -> Instruction.t -> string

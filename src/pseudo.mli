(** AST and utility functions for pseudo-code instructions *)

(** This AST aims at being as close as possible to the pseudo-code,
 * while giving just enough structure to allow a relatively
 * straight-forward translation to Sail. In particular, it is completely
 * untyped. We add a few type annotations during the translation
 * through ad-hoc heuristics, and Sail's type-inference with overloading
 * figures out the rest. *)

(** {2 Expressions} *)

(** Local or global variable name *)
type var = string

(** Instruction field. In practice, this is a variable the value of
 * which comes from the instruction being executed. Since it sometimes
 * represents a register, it can be dereferenced (implicitly as an
 * l-value, or explicitly as an r-value). Special care must be taken
 * with implicit dereferencing because the register it denotes depends
 * on the name of the ifield (or, equivalently, the kind of instruction:
   * fixed-point, floating-point, vector, etc.). *)
type ifield = string

(** Unary operators and functions. *)
type unop =
    Uminus      (** Unary minus *)
  | Not         (** Logical or bitwise not *)

(** Binary operators *)
type binop =
    Plus    (** Arithmetic and bitwise operators *)
  | Minus
  | Times
  | Div
  | Or
  | Xor
  | Mod
  | And
  | Eq      (** Comparison operators. Postfix [u] stands for {e unsigned}. *)
  | Neq
  | Gt
  | Ge
  | Gtu
  | Lt
  | Le
  | Ltu
  | Replicate  (** Replicate a bit a certain number of times *)
  | Shiftl
  | Shiftr

(** Registers *)
type reg =
    RLit of string (** Register literal. Mostly used for special registers. *)
  | DCR of expr    (** Device-control register. *)
  | GPR of expr    (** General-purpose register (eg. [GPR(RA)]). *)
  | SPR of expr    (** Special-purpose register. *)

(** L-values *)
and lval =
    Reg of reg  (** Register *)
  | Var of var  (** Variable *)
  | Ifield of ifield (** Ifield, intrepreted as an immediate if used in
                         {!Pseudo.expr.Lval}, and dereferenced if used in {!Pseudo.instr.Assign}.
                     *)
  | Mem of expr * expr (** [Mem(a,s)] is memory starting at address [a] of size [s] bytes. *)

(** Expressions *)
and expr =
    Unop of unop * expr (** Unary operation. *)
  | Binop of binop * expr * expr (** Binary operation. *)
  | Func of string * expr list (** Functions *)
  | Concat of expr list (** Bit vector concatenation. *)
  | Int of int          (** Literal integer. *)
  | Bitfield of string  (** Bit vector, stored as a string of "0" and "1". *)
  | Lval of lval        (** L-value. *)
  | Sub of expr * sub   (** Vector slice. *)
  | StarIfield of ifield  (** Ifield dereference. *)
  | Undef               (** Undefined *)

(** Vector slices *)
and sub =
    Bit of expr (** Single element *)
  | Range of expr * expr (** Slice of consecutive elements *)


(** {2 Instructions} *)

(** Initializer of a for-loop index variable. Initialization could in
 * principle be done by arbitrary pseudo-code, but restricting its form
 * helps with the translation to Sail. *)
type for_init =
    SimpleInit of expr (** {[  for x from e to ... ]} *)
  | CondInit of expr * expr * expr
  (** {[  if e1 then start = e2 else start = e3;
       for x from start to ... ]} *)

(** For-loop  direction *)
type for_direction =
    Inc (** Increasing *)
  | Dec (** Decreasing ([downto]) *)

(** Assignment type *)
type assign_kind =
    Regular (** Regular assignement *)
  | Iea (** Assignment to a variable holding a memory address. Each time
            a memory address is computed (and hence assigned), it must first be
            normalized to get the corresponding {e effective address} - mainly to
            deal with overflow. *)

(** Instructions *)
type instr =
    Assign of (lval * sub option) * expr * assign_kind (** Assignment. *)
  | Block of block  (** Block of code. *)
  | If of expr * block * block (** [If(condition, then-block, else-block] *)
  | DoWhile of expr * block (** [DoWhile(condition, body)] loop. *)
  | For of string * for_direction * for_init * expr * expr * block
  (** For-loop:
   * [For("x", Inc, start, end, increment, body)]. *)
  | Trap
  (** Special [trap] instruction. Should probably be a function instead.
   * Not translated to Sail currently. *)
  | Leave (** Break instruction inside a loop. Rarely used,
  untranslatable to Sail. *)
  | Expr of expr (** Top-level expression. Very uncommon, currently just a
  function call for side-effect. *)

(** A block is just a list of instructions *)
and block = instr list

(** {2 Utility functions} *)

(** @return the list of variables names used in a given block of
 * pseudo-code. *)
val collect_vars : block -> var list

(** Compute footprints of instructions. *)

(** {2 Register footprint} *)

(** Registers affected by an instruction can be either given explicitly
 * in the instruction fields (aka. {e ifields}), or set implicitly in the
 * case of {e special registers} (such as overflow flags, etc.).
 *
 * In this module, we focus on ifields, and we infer from the
 * pseudo-code of the instruction if each one designates an input or
 * output register, an immediate value, or if it used as combination of
 * those.  Note that affected special registers are given in a
 * dedicated section of each instruction, that we extract but do not
 * parse (it is unstructured text).
 *
 * Because there is some consistency in the names given to ifields, we
 * could perform extra-checks to detect errors but this is not done at
 * the moment. *)

(** Inferred type for each ifield *)
type ifield_type =
    RegIn           (** input register *)
  | RegOut          (** output register *)
  | Imm             (** immediate value *)
  | RegInOut        (** input/output register. RA is used like this in
                        load with update instructions. *)
  | RegInImm        (** input register or immediate. This happens in
                        particular for RA, which is interpreted as a
                        general purpose register, except if it is equal
                        to zero in which case it is an immediate 0. *)
  | RegOutImm       (** output register  or immediate. RT in load
                        string instructions is used as an immediate and
                        then set to an undefined value. *)
  | RegInOutImm     (** input/output and immediate. Never occurs as far
                        as I can tell. *)
  | RegInFrom       (** input from here to GPR32 *)
  | RegOutFrom      (** output from this to GPR32 *)

(** [ifield_type_to_string] converts an [ifield_type] to a human-readable string.  *)
val ifield_type_to_string : ifield_type -> string

(** [infer_ifield_type block] infers the type (footprint) of each ifield
 * used in the pseudo-code [block] of an instruction. This analysis is very
 * simple, and should be easy to reimplement for Sail's AST directly,
 * instead of {!Pseudo.block}.
 * @return association list of each ifield name to its type. *)
val infer_ifield_type : Pseudo.block -> (string * ifield_type) list

(** {2 Memory footprint} *)

(** Not implemented. It might be possible to do something by analyzing
 * {!Pseudo.lval.Mem} elements in pseudo-code, but it is unclear exactly
 * what we are interested in computing.
 *
 * A few ideas:
 * {ul
 * {- Herd needs a [get_naccess] function which gives the number of
 * memory accesses for each instruction (but Litmus seems not to use
 * it).}
 * {- This could also be useful to compute more precise clobbering
 * information.}} *)

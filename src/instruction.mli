(** Internal representation for instructions *)

(** {!Instruction.t} gathers every information we manage to extract from
 * the ISA. *)

type t = {
  name : string; (** Full name *)
  mnemonics : Mnemo.t list; (* Mnemonics attached to instruction *)
  repr : BinRep.t; (** Binary representation *)
  form : string;  (** Form of binary encoding *)
  ifields : string list; (** List of ifields (redundant with [repr] *)
  code : (int * string) list; (** Raw pseudo-code, as a list of lines with indentation *)
  pseudo : Pseudo.block; (** Parsed pseudo-code - the block is empty in
                             case parsing fails *)
  special_regs : string list; (** List of special registers affected by
                                  instruction. {b Note:} This is a list of free-form, unparsed lines. *)
}

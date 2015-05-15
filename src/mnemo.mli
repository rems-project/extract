(** Instruction mnemonics (assembler) *)

(** In general, a mnemonic looks like this:
  * {[divdeo.    RT,RA,RB       (OE=1 Rc=1)]}
  * The first part is the name (with optional suffixes - here [o] and
  * dot ([.]). The second part is the list of parameters. The last part
  * is the list of flags set in the instruction (related to the
  * suffixes). Note that some parameters may also be enclosed in
  * parentheses:
  * {[lwa        RT,DS(RA)]}
  *
  * In this module, we treat the suffix dot ([.]) as a special case, but
  * we do keep [o] as part of the name. This is probably a mistake, and
  * a more consistent approach to suffixes would be preferable. (I did
  * not understand what I was doing at the time.) *)

(** Separator between mnemonic parameters *)
type sep =
    NoSep (* Nothing (useful for the first parameter *)
  | Comma (** Separated from the previous parameter by a comma *)
  | Paren (** Enclosed in parentheses *)

(** A parameter is made of a name and a separator *)
type param = sep * string

(** Representation of a mnemonic *)
type t = {
  name : string;  (** Base name. *)
  dot : bool;     (** Whether the name is suffixed by a dot. *)
  params : param list; (** Parameters list. *)
  flags : (string * bool) list; (** List of ifield flags for the
  instruction, and whether
  they are set or not for this form of the mnemonic. *)
}

(** {2 Conversions from and to string} *)

(** [parse string] parses a string describing the mnemonic, extracted
 * from the ISA. *)
val parse : string -> t

(** [to_string] is the right-inverse of [parse]. *)
val to_string : t -> string

(** @return mnemonic name, suffixed with dot if necessary *)
val name_to_string : t -> string

(** @return parameter, prefixed with comma or enclosed in parentheses if
  * necessary. *)
val param_to_string : param -> string

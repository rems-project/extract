(** Binary representation of instructions *)

(** Instructions are four byte long and word aligned. They are
 * made of the following parts:
 * - opcode (constant, bits [0:5] but sometimes elsewhere too for extended opcodes),
 * - ifields (usually contiguous, but cf. {e split field notation} in ISA).
 * - reserved bits.
 *
 * Instructions are classified in a number of {e forms}, which we are
 * able to parse from the XML but do not seem to be useful to consider
 * (except maybe to factor the decoder?).
*)

(** A pair of first bit and length *)
type position = int * int

type field =
    Opcode of int * position (** {b big-endian} value and position of the opcode *)
  | Ifield of string * position (** name and position of ifield *)
  | SplitIfield of string * position * position (** in practice, there only one split ifield: [sh], and it is split into two parts only. *)
  | Reserved of position (* position of a chunk of reserved bits *)

(** Binary representation of an instruction as a list of fields (sorted
 * by position) *)
type t = field list

(** [pos_to_string] translates a position into a human-readable string *)
val pos_to_string : position -> string

(** [compare_pos x y] is a comparison function to sort ifields based on
 * their position. *)
val compare_pos : field -> field -> int

(** [build names positions] builds a binary representation from elements
 * extraced from ISA: a list of field names and list of the first
 * bit of each field. *)
val build : string list -> int list -> t

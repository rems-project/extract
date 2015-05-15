(** A thin layer and utility functions on top of Xmlm *)

(** Xmlm is designed with streams in mind. It is more convenient to use
 * an AST, and performance is not an issue for us: even if the  ISA
 * is large, it fits easily in memory. *)

type tree =
  E of Xmlm.tag * tree list (** XML node *)
| D of string (** Raw text *)


(** {2 Input/Output} *)

(** Convert between a {!Xmlm.input} or {!Xmlm.output} and a tree. *)

val in_tree : Xmlm.input -> tree
val out_tree : Xmlm.output -> tree -> unit


(** {2 Filtering} *)


(** [filter_tree f l] applies the function [f] recursively to each tree
 * of the list [l] in a bottom-up, depth-first way. At each level, the
 * list of results are concatenated, on which [filter_tree] is to be
 * applied at the upper level. *)
val filter_tree : (tree -> tree list) -> tree list -> tree list

(** [filter_map f l] maps [f] onto the list [l], dropping [None] results.  *)
val filter_map : ('a -> 'b option) -> 'a list -> 'b list

(** Note: there is no good reason for [filter_map] to be in this module,
 * except that it is really the equivalent of [filter_tree] in another monad.  *)

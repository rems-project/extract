(** Patching Sail code *)

(** We need to patch Sail code produced from pseudo-code, to fix
 * typing issues or clarify undefined behaviour for instance.
 *
 * Patches are stored in a {e patch database}, a text file which associates
 * instruction mnemonics to patches. The format of the patch database is
 * the following:
 * - blank lines and lines starting with # are ignored
 * - each patch is made of two lines consecutive lines
 * - the first line is a comma-separated list of mnemonics the patch should be applied to
 * - the second line is either of the form [d N], to delete
      instruction number {e N}, or one of [i N : b], [a N :
        b] or [d N : b] to insert the block of Sail code {e b}, respectively
      after, before or instead of instruction number {e N}. Whitespaces
      are optional.
 *
 * Instructions are numbered starting from 1. We recurse to count
 * instructions in blocks, nondeterministic blocks, branches of if, and
 * body of for loops. Beware that blocks are numbered too, as well as
 * implicit [else] branches. For instance:
{[
  1:   if RA == 0 then
  2:      b := 0
       else
  3:      b := GPR[RA];
  4:   EA := b + EXTS (DS : 0b00);
  5:   foreach (r from RT to 31 by 1 in inc)
  6:      {
  7:          GPR[r] := MEM (EA,8);
  8:          EA := EA + 8
          }
]}

  Numbering of instructions always refers to the initial version:
    insertions and deletion do not affect it.
 *)

(** Patch database. *)
type db

(** [load_db file] loads a patch database from a file. *)
val load_db : string -> db

(** [get mnemo db] returns a function to patch the Sail code for the
 * instruction [mnemo]. If there is no patch for [mnemo] in database
 * [db], the identity function is returned. *)
(*val get : string -> db -> (Type_internal.tannot Ast.exp -> Type_internal.tannot Ast.exp)*)
val get : string -> db -> (Type_check.tannot Ast.exp -> Type_check.tannot Ast.exp)


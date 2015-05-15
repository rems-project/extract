open Xml

(** Logic to extract instructions from ISA XML. *)

(** {2 Utility functions} *)

(** Match tags in a concise way *)

let (=~) ((uri, local), attr) s = local = s


(** @param l list of {!Xml.tree.D} elements (raw text)
 * @return list of the content of each element
 * @raise Failure if the list contains {!Xml.tree.E} nodes *)
let extract_data l = List.map (function D s -> s | E(((_, t), _), _) -> raise (Failure t)) l

(** Keep the head of list matching some tag *)
let rec take_while ?(acc=[]) tag l = match l with
  | E (t, c) :: xs when t =~ tag -> take_while ~acc:(c::acc) tag xs
  | _ -> List.rev acc, l

(** Drop the head of list until some tag is reached *)
let rec drop_until tag l = match l with
  | E (((_, t), _), c) :: xs when t <> tag ->
    Printf.eprintf "dropping <%s>\n" t;
    drop_until tag xs
  | D _ :: _ -> assert false
  | _ -> l

(** Peephole optimisation: match on two consecutive tags (separated by a
 * newline), erase the tag of the second one and replace its children.
 * @param prev first tag
 * @param pattern second tag
 * @param subst new children
 * @param l list of tags to perform peephole on *)
let rec peephole prev pattern subst l = match l with
  | (E (tag, c) as x) :: D "\n" :: E (tag', c') :: xs when tag =~ prev && tag' =~ pattern ->
    x :: peephole prev pattern subst (E ((("", subst), []) , c') :: xs)
  | x :: xs -> x :: peephole prev pattern subst xs
  | [] -> l

(** Flatten nested [<DIV>] elements *)
let rec flatten_div = function
  | E (tag, c) :: xs when tag =~ "DIV" ->
    (flatten_div c) @ flatten_div xs
  | x :: xs -> x :: flatten_div xs
  | [] -> []

(** Deeply collect elements matching some predicate in a tree *)
let collect f t =
  let rec aux acc t =
    let acc' = if f t then t::acc else acc in
    match t with
    | E (_, c) -> List.fold_left aux acc' c
    | D _ -> acc'
  in List.rev (aux [] t)

(** {2 Pruning}
 * Keep only relevant elements of instruction descriptions. *)

(** Tags we are interested in: instruction name, asm, pseudo-code and layout *)
let characteristic_tags = [ "Instruction-Head"; "Instruction-Form";
  "TABLE"; "Instruction-Description-Reg-List" ]
let relevant_tags = "code-example" :: characteristic_tags

let is_relevant t = match t with
  | D _ -> false
  | E (tag, _) -> List.exists (fun s -> tag =~ s) relevant_tags

(** An instruction element is an element containing elements with all the relevant tags
 * (except possibly code-example). *)
let is_instr_elem t = match t with
  | D _ -> false
  | E (tag, c) ->
      List.for_all
        (fun char_tag -> List.exists (function D _ -> false | E (t, _) -> t =~ char_tag) c)
        characteristic_tags

let extract_ton = Sys.getenv "EXTRACT_TON"
let extract_toff = Sys.getenv "EXTRACT_TOFF"
let extract_sub = Sys.getenv "EXTRACT_SUB"

let rec find_memtag_ton = function
  | E(((_, "underline-code"),_), [D extract_ton']) :: _ when extract_ton'=extract_ton -> true
  | D s :: E(((_, "Sub"),_), [D "tag"]) :: _
    when Str.string_match (Str.regexp ".*MEM$") s 0 -> true
  | _ :: xs -> find_memtag_ton xs
  | [] -> false

(** Rewrite function to normalize XML idiosyncrasies in a form suitable
 * for processing. *)
let rec cleanup t = match t with
  | E (((_, "Wingding-3"), []), [D "f"]) -> [D "←"]
  | E(((_, "underline-code"),_), [D extract_toff']) when extract_toff'=extract_toff->
      [D "bitone"]
  | E (((_, "Wingding-3"), []), [D "f (RA)"]) -> [D "← (RA)"]
  | E (((_, "Math-B"), []), [D "/"]) -> [D "⊕"]
  | E (((_, "Courier-New"), []), [D "-"]) -> [D "-"]
  | E (((_, "A"), [(_, "ID"), _]), []) -> []
  | E (((_, "Instruction-Form"), []), c)
    when List.for_all (function E _ -> false | D s -> String.trim s = "") c -> []
  | E (((uri, "CharFmt3"), []), c)
  | E (((uri, "Super-compressed"), []), c)
  | E (((uri, "Super-oblique"), []), c) -> [E (((uri, "Super"), []), c)]
  | E (((uri, "Sub-compressed"), []), c)
  | E (((uri, "FM-sub-"), []), c)
  | E (((uri, "Sub-oblique"), []), c) -> cleanup (E (((uri, "Sub"), []), c))
  | E (((uri, "Sub"), []), []) -> []
  | E (tag, [D extract_sub']) when tag =~ "Sub" && extract_sub'=extract_sub -> []
  | E (((_, "code-example"),_), l) when find_memtag_ton l -> []
  | E (((_, "Bold"), []), c) -> c
  | E (((_, "Bold-emphasis"), []), c) -> c
  | E (((uri, "FM-inst-name-page"), []), c)
  | E (((uri, "FM-inst-name-"), []), c) -> [E(((uri, "Instruction-Head"), []), c)]
  | E (((uri, "FM-inst-syntax-"), []), c) -> [E(((uri, "Instruction-Form"), []), c)]
  | E (((uri, "FM-inst-function-compact"), []), c)
  | E (((uri, "FM-inst-function-"), []), c) -> [E(((uri, "code-example"), []), c)]
  | E (((uri, "FM-inst-desc-pd-"), []), c) -> [E(((uri, "Instruction-Description-Reg-List"), []), c)]
  | E (((uri, "Instruction-Head"), []), [
    D name; D "["; D alternative; D "]"; D form
  ]) ->
    let name' = Printf.sprintf "%s [%s] %s" name alternative form in
    [E (((uri, "Instruction-Head"), []), [D name'])]
  | E (((uri, "FM-bit-center"), []), []) -> []
  | E (((uri, "Instruction-Bit-Number-Center"), []), c)
  | E (((uri, "Instruction-mini"), []), c)
  | E (((uri, "FM-bit-center"), []), c)
  | E (((uri, "Instruction-Bit-Number-Right"), []), c) -> cleanup (E (((uri, "Instruction-Bit-Number"), []), c))
  | E (((uri, "Instruction-Bit-Number"), []), [D s]) ->
    (* stupid bit numbers in tables which also mark the end of the range, not
     * only the beginnig *)
    let bits = Str.split (Str.regexp " +") s in
    (* sanity checks *)
    (match bits with
     | [ s ] | [ s; "31" ] | [ s; "127" ] ->
         (* in some tables, FM-bit-center contains hexadecimal digits. We
          * don't need those tables in practice, and we are dropping them
          * later, but at this stage, we process them too. *)
         assert(Str.string_match (Str.regexp "[A-F0-9]+") s 0);
         [E (((uri, "Instruction-Bit-Number"), []), [D s])]
     | [ s ; s' ] -> failwith s'
     | _ -> assert false)
  | E (((uri, "Register-Field"), []), c)
  | E (((uri, "FM-opcd-center"), []), c)
  | E (((uri, "FM-opcd-center-narrow"), []), c) -> cleanup (E (((uri, "Instruction-Fields"), []), c))
  | E (((uri, "Instruction-Fields"), []), [D "00"]) ->
    [
      E (((uri, "Instruction-Fields"), []), [D "0"]);
      E (((uri, "Instruction-Fields"), []), [D "0"])
    ]
  | E (((_, "CELL"), [(_, "ROWSPAN"), _;(_, "COLSPAN"), _]), c) -> c
  | E (((_, "TABLE"), []), rows) ->
    begin match rows with
      | [E (((_, "ROW"), []), r1); E (((_, "ROW"), []), r2)]
        when List.length r1 = List.length r2 -> [t]
      | _ -> []
    end
  | _ -> [t]

(** Collect relevant tags and normalize them. *)
let prune tree =
  let open Xml in
  let instr_tree = Xml.filter_tree cleanup [tree] |> List.map (collect is_instr_elem) |> List.concat in
  let relevant_only = List.map
      (function E (t, c) -> E (t, List.filter is_relevant c)| D _ -> assert false)
      instr_tree in
  (* check that we did not lose relevant tags in rewriting and filtering *)
  assert(List.for_all is_instr_elem relevant_only);
  relevant_only

(** {2 Instruction name} *)

(** Separate name, optional alternative and form: "A Long Name [alternative] XX-form". *)
let parse_name s =
  let open Str in
  assert(last_chars s 5 = "-form" || last_chars s 5 = "-Form" );
  let form, name =
    String.sub s 0 (String.length s - 5) |>
    split (regexp " +") |> List.rev |>
    (fun l -> List.hd l, String.concat " " (List.rev (List.tl l))) in
  let name, alternative =
    if string_match (regexp "^\\(.*\\) \\[ *\\(.*[^ ]\\) *\\]\\(.*\\)$") name 0
    then replace_matched "\\1\\3" name, (
      let alt = Str.matched_group 2 name in
      if  alt = "with Update"
      (* XXX [with Update] alternatives for Vector-Scalar
      instructions have disappeared in 2.07; the XML has been updated,
      except the name is wrong. The published spec is consistent,
      though, this is one of the many quirks of the XML we got. Anyway,
      ignore this alternative form since the binary format and mnemonics
      are not available in the XML. *)
  || alt.[0] = '&'
         (* XXX there is also a number of alternative names in
  * vector-scalar chapter which are not real alternatives, just a
          * variant with a dot suffix and a flag in the binary representation
          * that we deal with already. Funnily enough, they all start
          * with an ampersand. Ignore those as well. *)
    then None
       else Some (replace_matched "\\1 \\2\\3" name))
      else name, None in
  name, alternative, form

(** {2 Binary representation} *)

(** @param l list of rows of the table giving the binary layout
 * @return Binary representation and ifields of the instruction *)
let extract_repr l = match l with
  | [E (((_, "ROW"), _), r1); E (((_, "ROW"), _), r2)] ->
    let fields =
      take_while "Instruction-Fields" r1 |> fst |> List.flatten
      |> extract_data in
    let positions =
      take_while "Instruction-Bit-Number" r2 |> fst |> List.flatten
      |> extract_data |> List.map int_of_string in
    let repr = BinRep.build fields positions in
    let ifields = repr |> Xml.filter_map (function
      BinRep.Ifield (name, _) | BinRep.SplitIfield (name, _, _) -> Some name | _ -> None) in
    repr, ifields
  | _ -> failwith "unexpected row layout"

(** Discard non-standard mnemonics (annotated with Category or
 * P/P2) *)
let remove_category =
  let standalone_category = Str.regexp "^\\[\\(Category\\|POWER2? mnemonics?\\): .*\\]$" in
  let trailing_category = Str.regexp " *\\[Category: .*\\]$" in
  Xml.filter_map (fun s ->
  if Str.string_match standalone_category s 0 then
    None
  else
    Some (Str.global_replace trailing_category "" s))



(** {2 Pseudo-code parsing} *)

(** Ad-hoc fixes for all the bugs and shortcuts in pseudo-code *)
let fixup_code s =
  let fix_crbc s =
    (* XXX subscript BC is not parsed correctly for isel *)
    Str.global_replace (Str.regexp_string "CRBC+32")
      "CR STARTSub BC+32 ENDSub" s
  in
  let fix_parity s =
    (* XXX prtyd and following are completely broken:
       "%" instead of "*" and wrong "<sub>" *)
    Str.global_replace (Str.regexp_string "i STARTSub % ENDSub 8+7")
      "STARTSub i*8+7 ENDSub" s
  in
  let fix_mod32 s =
    (* XXX lswi/lswx use (mod 32) *)
    Str.global_replace (Str.regexp_string "r + 1 (mod 32)")
      "(r + 1) mod 32" s
  in
  let fix_mod64 s =
    (* XXX lsdx etc. use (mod 64) *)
    Str.global_replace (Str.regexp_string "i + 8 (mod 64)")
      "(i + 8) mod 64" s
  in
  let fix_times s =
    (* XXX fix some instances of x instead of × *)
    let s = Str.global_replace (Str.regexp_string "4xi") "4×i" s in
    let s = Str.global_replace (Str.regexp_string "i x 32") "i × 32" s in
    s
  in
  let fix_addg6s s =
    (* XXX fix dots in addg6s *)
    if s = "c ← STARTSuper 4 ENDSuper (dc STARTSub 0 ENDSub ) || STARTSuper 4 ENDSuper (dc STARTSub 1 ENDSub ) || ... || STARTSuper 4 ENDSuper (dc STARTSub 15 ENDSub ) "
    then "c ← " ^ String.concat " || " (List.map (fun i -> Printf.sprintf "STARTSuper 4 ENDSuper (dc STARTSub %d ENDSub )" i) [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15])
    else
      (* XXX fix (RA) and (RB) in addg6s *)
      Str.global_replace (Str.regexp_string "RA STARTSub 4×i:63 ENDSub + RB STARTSub 4×i:63 ENDSub")
        "(RA) STARTSub 4×i:63 ENDSub + (RB) STARTSub 4×i:63 ENDSub" s
  in
  let fix_mtcrf s =
    (* XXX fix dots in mtcrf *)
    if s = "mask ← STARTSuper 4 ENDSuper (FXM STARTSub 0 ENDSub ) || STARTSuper 4 ENDSuper (FXM STARTSub 1 ENDSub ) || ... STARTSuper 4 ENDSuper (FXM STARTSub 7 ENDSub ) "
    then "mask ← " ^ String.concat " || " (List.map (fun i -> Printf.sprintf "STARTSuper 4 ENDSuper (FXM STARTSub %d ENDSub )" i) [0;1;2;3;4;5;6;7])
    else s
  in
  let fix_blcr s =
    (* XXX fix missing parenthese in blcr *)
    if s = "ctr_ok ← BO STARTSub 2 ENDSub | ((CTR STARTSub M:63 ENDSub ≠ 0) ⊕ BO STARTSub 3 ENDSub "
    then s^")" else s
  in
  let fix_assign_font s =
    (* XXX fix missing annotation to recognise " f " as an assignment arrow *)
    Str.global_replace (Str.regexp_string " f ") " ← " s
  in
  let fix_clamp_parens s =
    (* XXX fix missing parens in vpkswss *)
    if s = "VRT STARTSub i:i+15 ENDSub ← Clamp(EXTS((VRA) STARTSub i × 2:i × 2+31 ENDSub , -2 STARTSuper 15 ENDSuper , 2 STARTSuper 15 ENDSuper -1)16:31 "
    then "VRT STARTSub i:i+15 ENDSub ← Clamp(EXTS((VRA) STARTSub i × 2:i × 2+31 ENDSub) , -2 STARTSuper 15 ENDSuper , 2 STARTSuper 15 ENDSuper -1)16:31 "
    else if s = "VRT STARTSub i+64:i+79 ENDSub ← Clamp(EXTS((VRB) STARTSub i × 2:i × 2+31 ENDSub , -2 STARTSuper 15 ENDSuper , 2 STARTSuper 15 ENDSuper -1)16:31 "
    then "VRT STARTSub i+64:i+79 ENDSub ← Clamp(EXTS((VRB) STARTSub i × 2:i × 2+31 ENDSub), -2 STARTSuper 15 ENDSuper , 2 STARTSuper 15 ENDSuper -1)16:31 "
    else s
  in
  (* order matters! *)
  s |> fix_assign_font |> fix_parity |> fix_mod32 |> fix_mod64 |> fix_times |>
          fix_addg6s |> fix_mtcrf |> fix_crbc |> fix_blcr |> fix_clamp_parens

(** Compute indentation level, based on [<ws>] and [<ts>] tags
 * introduced in {!Split} and fix the resulting code. Also introduce
 * STARTSub/ENDSub tokens when subscripts are present in xml.
 * @return indentation level (leading spaces) and cleaned-up lined of code *)
let code_line_to_string code =
  let open Buffer in
  let b = create 10 in
  let level = ref 0 in
  let rec fill_buf e = match e with
    | D s -> add_string b s; add_char b ' '
    | E (tag, []) when tag =~ "ws" ->
      incr level
    | E (tag, []) when tag =~ "ts" ->
      (* assume tab = 2 spaces --- seems to be correct in practice *)
      level := !level + 2
    (* I don't really know what to do with FM-symbol- etc., they are empty most of
     * the time and don't seem significant the rest of it. *)
    | E (tag, l) when tag =~ "FM-symbol-" || tag =~ "FM-helv-" -> List.iter fill_buf l
    | E (((_, tag), _), [D s]) when tag = "Sub" || tag = "Super" ->
      add_string b "START"; add_string b tag; add_char b ' ';
      add_string b s; add_char b ' ';
      add_string b "END"; add_string b tag; add_char b ' ';
    | E (((_, tag), _), _) ->
      Printf.eprintf "unexpected tag %s\n" tag
  in
  let rec merge_tags t l = match t, l with
    | E ((("", tag), []), [D s]), (E ((("", tag'), []), [D s']) :: xs)
      when tag = tag' ->
      E ((("", tag), []), [D (s ^ " " ^ s')]) :: xs
    | _ -> t :: l
  in
  List.fold_right merge_tags code [] |> List.iter fill_buf;
  let s = contents b |> fixup_code in
  (!level, s)

(** Interpret indentation information to reconstruct meaningful blocks.
 * This is a huge, hackish heuristics driven by the actual pseudo-code.
 * There is no real logic in it. It seems to work. *)

type block_context = BThen | BElse | BLoop | BRoot type level_stack = int * block_context

let make_blocks lines =
  let context : level_stack Stack.t = Stack.create () in
  let has word s = Str.string_match (Str.regexp_case_fold ("^.*"^word)) s 0 in
  let begins word s = Str.string_match (Str.regexp_case_fold ("^[ ]*"^word^" ")) s 0 in
  let ends word s = Str.string_match (Str.regexp_case_fold (".*"^word^"[ ]*$")) s 0 in
  let rec aux l =
    match l with
    | (l1, ("if (RS) STARTSub i ENDSub = 1 then " as s1)) :: (l2, ("n ← n+1 " as s2)) :: xs when l1 = l2 ->
      (* XXX fix bogus indentation for popcntd *)
      aux ((l1, s1) :: (l2 + 1, s2) :: xs)
    | (level, line) :: xs ->
      let clevel, ccontext = Stack.top context in
      if level < clevel
      then begin
        ignore(Stack.pop context);
        let close =
          if ccontext = BElse || ccontext = BLoop ||
             (ccontext = BThen && not(begins "else" line))
          then "END;"
          else "END"
        in close :: aux l
      end else if begins "if" line then begin
        if Str.string_match (Str.regexp_case_fold "^if.*then.*else.*$") line 0 then begin
          (* trivial case on a single line *)
          line :: ";" :: aux xs
        end else if ends "then" line then begin
          Stack.push (level+1, BThen) context;
          line :: "BEGIN" :: aux xs
        end else if has "then" line then begin
          (* the indentation is sometimes crazy:
             if ... then ...
                         ...
                    else ...
                         ...
             with minor mistakes, of course (else is sometimes 1 space too
             far, hence the "3 +" below).
             Try to be smart without breaking everything.
          *)
          let level' = 3 + level + Str.search_forward (Str.regexp "then") line 0 in
          Stack.push (level', BThen) context;
          (Str.replace_first (Str.regexp_string "then") "then BEGIN" line) ::
          ";" :: aux xs
        end else begin
          (* "then" is on the next line *)
          line :: aux xs
        end
      end else if begins "then" line then begin
        Stack.push (level+1, BThen) context;
        if ends "then" line (* alone on its line *)
        then line :: "BEGIN" :: aux xs
        else "then BEGIN" :: (Str.string_after line 4) :: ";" :: aux xs
      end else if begins "else" line then begin
        Stack.push (level+1, BElse) context;
        if ends "else" line (* alone on its line *)
        then line :: "BEGIN" :: aux xs
        else if begins "else if" line then begin
          (* we treat "else if" as if there were no "else", just keeping the
           * same level, removing the "else" context and treating the line
           * starting with the "if" *)
          ignore(Stack.pop context);
          "else" :: aux ((level, (Str.string_after line 5)) :: xs)
        end else begin
          "else BEGIN" :: Str.string_after line 5 :: ";" :: aux xs
        end
      end else if begins "do" line || begins "For" line then begin
        Stack.push (level+1, BLoop) context;
        line :: "BEGIN" :: aux xs
      end else begin
        line :: ";" :: aux xs
      end
    | [] ->
      (* close every open context *)
      if Stack.is_empty context
      then []
      else
        let close = if snd (Stack.pop context) = BRoot then "END" else "END;" in
        close :: aux []
  in
  Stack.push (0, BRoot) context;
  String.concat " " ("BEGIN" :: aux lines)

(** Work-around line-wrapping: merge lines starting with "   ||" with
 * the previous one, and lines ending with & with the next one. *)
let merge_lines lines =
  let merged =
    let is_wrapped (level, line) pred_line =
      let chars = ["&";"|";"||";"←"] in
      let r = Printf.sprintf "\\(%s\\)" (String.concat "\\|" (List.map Str.quote chars)) in
        ((Str.string_match (Str.regexp r) line 0) ||
         (Str.string_match (Str.regexp (".* "^r^" *$")) pred_line 0))  in
    List.fold_left
      (fun acc ((level, line) as x) ->
        match acc with
        | (l, s) :: xs when is_wrapped x s ->
            (l, s ^ line) :: xs
        | _ -> x :: acc)
      []
      lines in
  List.rev merged

(** Call {!PseudoLexer} and {!PseudoParser} to parse pseudo-code. *)
let parse_pseudo code =
  let lexbuf = Lexing.from_string code in
  (* Printf.eprintf "Parsing: %s\n" code;*)
  try
    PseudoParser.main PseudoLexer.token lexbuf
  with
  | Failure _ -> Printf.eprintf "Lexer error: %s\n" code; []
  | Parsing.Parse_error -> Printf.eprintf "PseudoParser error: %s\n" code; []
  | e -> Printexc.print_backtrace stderr;
    Printf.eprintf "exception: %s\n" (Printexc.to_string e); []

(** Merge consecutive lines, remove empty or bogus lines, reconstruct
 * blocks and parse. *)
let parse_code code =
  code |> merge_lines
  |> List.filter (fun (_, s) -> s <> "")
  |> List.filter (fun (_, s) ->
      (* XXX remove two lines, ori and xori, which are treated as
       * instructions by mistake, as well as 0x8000_0000 ÷ -1,
       * <anything> ÷ 0, etc. *)
      let r = Str.regexp "^\\(\\(.?ori\\)\\|\\(^<anything>\\)\\|\\(^0x.* ÷ - 1\\)\\)" in
      not(Str.string_match r s 0))
  |> make_blocks |> parse_pseudo

(** {2 Gluing things together} *)

(** Wrappers to provide backtraces in case of errors *)

let try_parse_code code =
  try
    parse_code code
  with e ->Printexc.print_backtrace stderr;
    Printf.eprintf "%s\n" (Printexc.to_string e); []

let try_parse_mnemo m =
  try
    Mnemo.parse m
  with e ->Printexc.print_backtrace stderr;
    Printf.eprintf "%s\n" (Printexc.to_string e);
    Mnemo.({ name = "FAILED s"; dot = false; params = []; flags = [] })

(** Build {!Instruction.t} internal representation from XML *)

let parse_instr instr =
  let parse_table t = t |> List.flatten |> extract_repr in
  let parse_mnemo m = m |> List.flatten |> extract_data |> remove_category |> List.map try_parse_mnemo  in
  (** Parse a single instruction - except when it contains an
   * alternative, in which case it is in fact two instructions merged
   * into one, and we need to duplicate it *)
  let parse_single l =
    (* XXX drop useless blocks, printing a warning to help debugging *)
    let l = drop_until "Instruction-Head" l in
    let name, l = take_while "Instruction-Head" l in
    let name = name |> List.flatten |> extract_data |> remove_category |> String.concat " " in
    let mnemonics, l = take_while "Instruction-Form" l in
    let table, l = take_while "TABLE" l in
    (* in case of alternatives, there are two series of mnemo/binary
     * repr *)
    let alt_mnemonics, l = take_while "Instruction-Form" l in
    let alt_table, l = take_while "TABLE" l in
    let code, l = take_while "code-example" l in
    let special_regs, l = take_while "Instruction-Description-Reg-List" l in
    let code = code |> List.map code_line_to_string in
    let pseudo = try_parse_code code in
    let name, alt_name, form = parse_name name in
    let repr, ifields = parse_table table in
    let mnemonics = parse_mnemo mnemonics in
    let special_regs = [] in
    (* TODO some special regs containt <Sub>, and we need to parse
     * them in more detail anyway.
       if name = "Move to Condition Register from XER" (* XXX fix special register for mcrxr *)
       then ["CR field BF"; "XER 32:35"]
       else special_regs |> List.flatten |> extract_data [] in *)
    let instr = Instruction.({
        name = name;
        form = form;
        mnemonics = mnemonics;
        repr = repr;
        ifields = ifields;
        code = code;
        pseudo = pseudo;
        special_regs = special_regs;
      }) in
    Printf.eprintf "Built instruction %s\n" name;

    (* Now, deal with the alternative, if any. *)

    (* XXX Note that many alternative instructions contain pseudo-pseudo
     * code to switch on the form:
     *   if “xsnmsubasp” then do etc.
     * We should really parse this and perform constant-folding, to get
     * a different pseudo code for each. We don't bother at the moment.
     * *)
    match alt_name with
    | None when alt_table <> [] && alt_mnemonics <> [] && form = "XX3" ->
        (* XXX some vector-scalar instructions have an alternative form
         * that swaps two ifields. Eg. xsnmsubasp/xsnmsubmsp. This
         * alternative form has its own mnemonics and binary
         * representation. One good way to deal with it would be to
         * actually keep a single instruction and swap the relevant
         * ifields; it is only a decoding issue, at some fundamental
         * level. However, because of the rigid naming of ifields in
         * "forms", architects seem to like thinking of them as
         * two instructions, or as one instruction but with "switches"
         * in the pseudo-code depending on the form. You get the idea...
         * Anyway, to keep our peace of mind, we duplicate it, and
         * append "Swapped" to the name of the alternative form, to
         * distinguish them. Eeek! *)
        let name = Printf.sprintf "%s Swapped" name in
        let repr, ifields = parse_table alt_table in
        let mnemonics = parse_mnemo alt_mnemonics in
        let alt_instr = Instruction.({instr with name; repr; mnemonics; ifields}) in
        Printf.eprintf "Built (alternative, made up) instruction %s\n" name;
        [instr; alt_instr], l
    | None when alt_table <> [] && alt_mnemonics <> [] && name = "Wait" ->
        (* XXX idem for "wait" instruction, with phased-in and out *)
        let name = Printf.sprintf "%s Phased-Out" name in
        let repr, ifields = parse_table alt_table in
        let mnemonics = parse_mnemo alt_mnemonics in
        let _alt_instr = Instruction.({instr with name; repr; mnemonics; ifields}) in
        Printf.eprintf "Built (alternative, made up) instruction %s\n" name;
        (* XXX ignore phased out instruction because Sail generation
         * does not like having two instructions with the same mnemonic *)
        [instr; (*alt_instr*)], l
    | None ->
        assert(alt_table = []);
        assert(alt_mnemonics = []);
        [instr], l
    | Some name ->
        let repr, ifields = parse_table alt_table in
        let mnemonics = parse_mnemo alt_mnemonics in
        let alt_instr = Instruction.({instr with name; repr; mnemonics; ifields}) in
        Printf.eprintf "Built (alternative) instruction %s\n" name;
        [instr; alt_instr], l in

  let rec parse_many acc = function
    | [] -> List.rev acc
    | l ->
      let i, l' = parse_single l in
      parse_many (i @ acc) l' in
  match instr with
  | E (tag, l) when tag =~ "XML" -> parse_many [] l
  | _ -> failwith "instruction container is not XML"

let parse_instrs l = l |> List.map parse_instr |> List.flatten

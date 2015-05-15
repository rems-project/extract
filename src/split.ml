open Xml

(** {b Step one:} Split ISA book into individual chapters. *)


(**/**)
(* Utility functions, for backwards-compatibility (copy-pasted from
 * recent versions of OCaml's stdlib). *)
let (|>) x f = f x
let rec iteri i f = function
    [] -> ()
  | a::l -> f i a; iteri (i + 1) f l
let iteri f l = iteri 0 f l
let is_space = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let trim s =
  let len = String.length s in
  let i = ref 0 in
  while !i < len && is_space (String.unsafe_get s !i) do
    incr i
  done;
  let j = ref (len - 1) in
  while !j >= !i && is_space (String.unsafe_get s !j) do
    decr j
  done;
  if !i = 0 && !j = len - 1 then
    s
  else if !j >= !i then
    String.sub s !i (!j - !i + 1)
  else
    ""

(**/**)

(** {2 Fixing input XML} *)

(* We do a minor amount of high-level, structural XML fixing at this
 * stage. Much more extensive fixes, in particular for broken
 * pseudo-code, is done in {!Extract}.
*)

(** Instructions where [<Body>] needs to be [<Instruction-Form>] *)
let fix_instr_form = [
  "pgfId-2368238"; (* oris *)
  "pgfId-996034" ; (* crxor *)
]

(** Instructions where [<Body>] needs to be [<code-example>] *)
let fix_code_example = [
  "pgfId-996815"; (* subfic *)
]

(** Fix some broken tags, and insert explicit [<ws>] and [<ts>] instead
 * of whitespaces in [<code-example>] blocks. This is necessary, or at
 * least easier, because whitespace is significant for pseudo-code, and
 * hard to preserve when we pretty-print the intermediate files.
 * Similarly, split Instruction-Form with line returns into several
 * tags, and fix crazily broken Instruction-Form for Wait instruction. *)
let rec fix_xml t = match t with
  | E (((uri, "code-example-compressed"), []), c) -> fix_xml (E (((uri, "code-example"), []), c))
  | E (((_, "code-example"), _) as tag,
                                   (* erase newline after <code-example> *)
                                   (D "\n") ::
                                   (* as well as <A ID="..." /> *)
                                   (E (((_, "A"), [(_, "ID"), _]), [])) ::
                                   (* and make leading whitespace of the first data block explicit *)
                                   (D s) :: c) ->
    assert(s.[0] = '\n');
    let l = ref [] in
    (* start loop from 1: skip line return inherited from <A> above *)
    let i = ref 1 in
    while (!i < String.length s && (s.[!i] = ' ' || s.[!i] = '\t')) do
      l := (E ((("", if s.[!i] = ' ' then "ws" else "ts"), []), [])) :: !l;
      i := !i + 1;
    done;
    let c' = !l @ [D (Str.string_after s !i)] @ c in
    [E (tag, c')]
  | E (tag, ((D "\n" :: E (((_, "A"), [(_, "ID"), id]), []) :: _) as c))
    when List.mem id fix_instr_form ->
    [E ((("", "Instruction-Form"), []), c)]
  | E (tag, ((D "\n" :: E (((_, "A"), [(_, "ID"), id]), []) :: _) as c))
    when List.mem id fix_code_example ->
    fix_xml (E ((("", "code-example"), []), c))
  (* special-case for particularly broken Wait instruction *)
  | E (((uri, "Instruction-Form"), []), [
      D "\n";
      E (((_, "A"), [(_, "ID"), _]), []);
      D "\n";
      E (((_, "Bold"), []), [D "\n[Category: "]);
      D category]) ->
        assert(category.[0] = '\n');
        let c = Str.string_after category 1 in
        [E (((uri, "Instruction-Form"), []), [D ("[Category: "^c)])]
  (* general case *)
  | E (((uri, "Instruction-Form"), []), c) ->
      List.map (function
        | D s
        | E (((_, "Bold"), []), [D s]) ->
            List.map (fun f -> E(((uri, "Instruction-Form"), []), [D f])) (Str.split (Str.regexp "\n") s)
        | e -> [])
      c |> List.concat
  | _ -> [t]

(** {2 Splitting into chapters} *)

(** [split name l] splits the list [l] into sub-lists the first elements
 * of which are [name] tags. *)
let split names l =
  let add_elem (cur, acc) e = match e with
    | E(((_, t),_), c) when List.mem t names -> ([e], (List.rev cur) :: acc)
    | _ -> (e :: cur, acc) in
  let cur, acc = List.fold_left add_elem  ([], []) l in
  List.rev ((List.rev cur) :: acc)


let split_chapters xml =
  let books = split ["Title"] xml in
  List.map (split ["Title-Chapter-"; "Title-Appendix-"]) books


(** {2 Cleaning-up chapter titles to produce filenames} *)

(* Ubuntu with an encrypted filesystem seems to have a roughly
   144-character filename limit which the Category strings
   exceed for Chapter 9, so we trim those out *)
  
let up_to_first_bracket s = 
  try
    String.sub s 0 (String.index s '[')
  with
    Not_found -> s

let get_title xml =
  let s = 
  match List.find (function E _ -> true | _ -> false) xml with
  | D _ -> assert false
  | E(_, title) -> begin
      filter_map (function
          | E _ -> None
          | D s -> match trim s with
            | "" -> None
            | s' -> Some s') title
      |> String.concat ""
    end
in
  trim (up_to_first_bracket s)

(** {2 Main function} *)

let do_split xmlfile dir =
  let ic = open_in xmlfile in
  let i = Xmlm.make_input ~strip:false (`Channel ic) in
  let tag, xml = match in_tree i with
    | E(tag, xml) -> tag, xml
    | D _ -> failwith "badly formed xml" in
  let output_chapter booknum chapternum xml' =
    let name =
      Printf.sprintf "%s/book%d-chapter%d-%s.xml"
        dir booknum chapternum (get_title xml') in
    let oc = open_out name in
    let o = Xmlm.make_output ~indent:None (`Channel oc) in
    out_tree o (E(tag, xml')) in
  split_chapters xml
  |> List.map (List.map (filter_tree fix_xml))
  |> iteri (fun booknum -> iteri (output_chapter booknum))


let () = do_split Sys.argv.(1) Sys.argv.(2)

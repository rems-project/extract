(** {b Step Two:} Parse individual chapters and generate Litmus and Sail code. *)

module LG = LitmusGen

open Instruction

let () = Printexc.record_backtrace true

(** {2 Utility functions} *)

module StringSet = Set.Make(struct let compare = Pervasives.compare; type t = string end)

let (|>) x f = f x

let list_to_set = List.fold_left (fun s x -> StringSet.add x s) StringSet.empty

let with_file instrs file f =
  let chan = open_out_gen [Open_append; Open_creat] 0o666 file in
  List.iter (f chan) instrs;
  close_out chan



let internal_string = Sys.getenv "EXTRACT_INTERNAL" 

let internal_data = Str.split (Str.regexp_string "-") internal_string

let internal i = List.exists (fun m -> List.mem m.Mnemo.name internal_data) i.mnemonics



(** {2 Validation} *)

(** Check that ifields in binary representation and parameters of
 * mnemonics match. *)
let validate_mnemo_ops i =
  let map f s = StringSet.fold (fun e s -> StringSet.add (f e) s) s StringSet.empty in
  let norm = map String.lowercase in
  let get_params m s = List.fold_left (fun s (_, e) -> StringSet.add e s) s m.Mnemo.params in
  let get_flags m s = List.fold_left (fun s (e, _) -> StringSet.add e s) s m.Mnemo.flags in
  let get_params_flags m = StringSet.empty |> get_params m |> get_flags m |> norm in
  let check_mnemo m = StringSet.equal (get_params_flags m) (i.ifields |> list_to_set |> norm) in
  List.for_all check_mnemo i.mnemonics


(* Check that we can find every Ifield operand as an ifield in the pseudocode,
 * except flags *)
let validate_ifield_ops i =
  Printf.eprintf "validating ifields of %s\n" i.name;
  let map f s = StringSet.fold (fun e s -> StringSet.add (f e) s) s StringSet.empty in
  let no_flags ops = StringSet.diff ops (list_to_set ["oe";"rc"]) in
  let norm = map String.lowercase in
  let ifields = Footprint.infer_ifield_type i.pseudo |> List.map fst |> list_to_set |> norm in
  let () = 
    StringSet.iter (Printf.eprintf "missing ifield:%s\n") 
      (StringSet.diff (i.ifields |> list_to_set |> norm |> no_flags) ifields)
  in
  StringSet.equal ifields (i.ifields |> list_to_set |> norm |> no_flags)


(** {2 Sail code generation} *)

(** Use long AST names in Sail code *)
let longAstNames = ref false

(** Pretty-print Sail code for instruction to a channel. *)
(* Although this function is unlikely to be a bottleneck in our
 * generation, it's a bit stupid to call it twice per instruction as we
 * do currently (once in dump_instr and once directly). We might cache
 * the result in Instruction.t. *)
let dump_sail patch_db chan i =
  let print ast = Pretty_print_sail.pp_ast chan ast; output_char chan '\n' in
  let name = if !longAstNames then begin
      i.name
      |> Str.global_replace (Str.regexp_string "+") "Plus"
      |> Str.global_replace (Str.regexp_string " -") "Minus" (* eg. -Infinity *)
      |> Str.global_replace (Str.regexp_string "-") "" (* eg. Floating-Point *)
      |> Str.global_replace (Str.regexp_string " ") ""
    end else if ((List.hd i.mnemonics).Mnemo.name = "addic") && ((List.hd i.mnemonics).Mnemo.dot) 
      then String.capitalize ((List.hd i.mnemonics).Mnemo.name ^ "Dot")
      else String.capitalize (List.hd i.mnemonics).Mnemo.name
  in
  let mnemo = if ((List.hd i.mnemonics).Mnemo.name = "addic") && 
                  ((List.hd i.mnemonics).Mnemo.dot) 
    then (List.hd i.mnemonics).Mnemo.name ^ "." 
    else (List.hd i.mnemonics).Mnemo.name in
  let patch = Patch.get mnemo patch_db in
  let sail = SailGen.make_instr name i.repr i.pseudo patch in
  print sail

(** {2 Litmus code generation} *)

(** Check that all the mnemonics for a given instruction give the same
 * AST node, and output the first one. *)
let to_ast map chan i =
  if i.mnemonics = [] then failwith i.name;
  let cstrs = List.map (LG.gen_ast (map i)) i.mnemonics in
  let cstr = List.hd cstrs in
  if not (List.for_all (fun s -> s = cstr) cstrs)
  then begin
    Printf.eprintf "Different ast nodes:";
    List.iter (Printf.eprintf "%s; ") cstrs;
    Printf.eprintf "\n";
  end;
(* SS: turned off the assertion checking

   Currently the only ones that fail are dcbt and dcbtst, which have
   "server-mode" and "embedded-mode" versions with arguments in
   opposite orders. Didn't find a easy way to filter out the embedded
   version, but conveniently, the server version comes first in the file!!! *)
(*  assert(List.for_all (fun s -> s = cstr) cstrs); *)
  Printf.fprintf chan "%s\n" cstr

(** Iterate a function over all the mnemonics of an instruction and output the results *)
let generic_make map f chan i =
  let print s = Printf.fprintf chan "%s\n" s in
  let fn = fun m -> print (f (map i) m) in
  (* XXX for dcbt and dcbtst, just use the first one, since the second one is 
     category embedded mode *)
  let fst_mnemo = List.hd i.mnemonics in
  match String.lowercase fst_mnemo.name with
    | "dcbt" | "dcbtst" -> fn fst_mnemo
    | _ -> List.iter fn i.mnemonics


(** Translate to Sail-friendly instruction *)
let to_trans_sail map chan i =
  let trans_frag = LG.gen_trans_sail (map i) i in
  Printf.fprintf chan "%s\n" trans_frag

let to_herdtools_ast_to_shallow_ast map chan i =
  let trans_frag = LG.gen_herdtools_ast_to_shallow_ast (map i) i in
  Printf.fprintf chan "%s\n" trans_frag

(** Translate from Sail instructions back to litmus data *)
let to_trans_out_sail map chan i =
  let trans_frag = LG.gen_sail_trans_out (map i) i in
  Printf.fprintf chan "%s\n" trans_frag

let to_shallow_ast_to_herdtools_ast map chan i =
  let trans_frag = LG.gen_shallow_ast_to_herdtools_ast (map i) i in
  Printf.fprintf chan "%s\n" trans_frag

(** Generate all boilerplate files for Litmus *)
let gen_litmus gendir instrs map =
  with_file instrs (gendir^"/ast.gen") (to_ast map);
  with_file instrs (gendir^"/lexer.gen") (generic_make map LG.gen_lex);
  with_file instrs (gendir^"/parser.gen") (generic_make map LG.gen_parse);
  with_file instrs (gendir^"/tokens.gen") (generic_make map LG.gen_token);
  with_file instrs (gendir^"/pretty.gen") (generic_make map LG.gen_pp);
  with_file instrs (gendir^"/fold.gen") (generic_make map LG.gen_fold);
  with_file instrs (gendir^"/map.gen") (generic_make map LG.gen_map);
  with_file instrs (gendir^"/compile.gen") (generic_make map LG.gen_compile);
  with_file instrs (gendir^"/trans_sail.gen") (to_trans_sail map);
  with_file instrs (gendir^"/herdtools_ast_to_shallow_ast.gen") (to_herdtools_ast_to_shallow_ast map);
  with_file instrs (gendir^"/sail_trans_out.gen") (to_trans_out_sail map);
  with_file instrs (gendir^"/shallow_ast_to_herdtools_ast.gen") (to_shallow_ast_to_herdtools_ast map)

(** {2 Glue everything together} *)

(** List of mnemonics to export. Set it by putting a comma-separated
 * list of mnemonics in the environment variable EXPORT_MNEMO. *)
let export_mnemo = ref []

(** If set to true, then export_mnemo is interpreted as a list of
 * mnemonics {b not} to export instead. There is no command-line switch
 * for this, you need to change the source code. *)
let export_exclude_mode = false

(** Pretty-print human-readable debug information about an instruction *)
let dump_instr patch_db chan i =
  let print s = Printf.fprintf chan "%s\n" s in
  let print_indent (i, s) =
    Printf.fprintf chan "%s%s\n"
      (String.make i ' ') s in
  let field_type l f =
    try Footprint.ifield_type_to_string (List.assoc (String.lowercase f) l)
    with Not_found ->
      if f = "OE" || f = "Rc" then "FLAG" else "???" in
  let print_field tl = 
    let open BinRep in function
    | Opcode (i, p) ->
        Printf.fprintf chan "Opcode %d %s\n" i (pos_to_string p)
    | Reserved p ->
        Printf.fprintf chan "Reserved %s\n" (pos_to_string p)
    | Ifield (s, p) -> 
        Printf.fprintf chan "Ifield %s [%s] %s\n" s (field_type tl s)
        (pos_to_string p)
    | SplitIfield (s, p, p') -> 
        Printf.fprintf chan "Split Ifield %s [%s] %s-%s\n" s (field_type tl s)
        (pos_to_string p) (pos_to_string p') in
  print i.name;
  print (i.form^"-form");
  List.iter print (List.map Mnemo.to_string i.mnemonics);
  List.iter (print_field (Footprint.infer_ifield_type i.pseudo)) i.repr;
  if not (validate_mnemo_ops i) then
    print "Mismatch between mnemonics and representation!";
  List.iter print_indent i.code;
  (* TODO: print "Special registers altered";
     List.iter print i.special_regs; *)
  (match i.pseudo with
   | [] -> print "Parsing failed."
   | _ -> print "\nParsing succeeded. Translating to Sail."; dump_sail patch_db chan i);
  print "--------------------"


(** Extract and convert from XML, print some statistics and generate
 * files *)
let extract xmlfile gendir =
  let ic = open_in xmlfile in
  let oc = open_out (xmlfile ^ ".out") in
  let i = Xmlm.make_input ~strip:true (`Channel ic) in
  let o = Xmlm.make_output ~indent:(Some 2) (`Channel oc) in
  let t =  Xml.in_tree i in
  let t' = XML_fixes.prune t in
  Xml.out_tree o (List.hd t');
  close_in ic; close_out oc;
  let instrs = XML_fixes.parse_instrs t' in
  let instrs = List.filter (function i -> not (internal i)) instrs in
  let map i = 
    (* Argh! Lmw and Stmw treat RT/RS respectively as a loop index, causing
       inference to assume immediate type. Special case, in the absence of
       anything more principled *)
    let extra = match i.name with
    | "Load Multiple Word" -> [("rt", Footprint.RegOutFrom)]
    | "Store Multiple Word" -> [("rs", Footprint.RegInFrom)]
    | _ -> [] in
    extra @ (Footprint.infer_ifield_type i.pseudo) in
  let good_instrs, bad_instrs = List.partition validate_ifield_ops instrs in
  let parsed_instr = List.filter (function i -> i.pseudo <> []) instrs in
  let sail_instrs =
    if !export_mnemo = [] then instrs else
    List.filter (fun i ->
      (if export_exclude_mode then not else fun x -> x)
      (List.exists (fun m -> List.mem m.Mnemo.name !export_mnemo) i.mnemonics)) instrs in
  (* rebuild the patch database for each xml chapter file - no big deal
   * but a bit wasteful. *)
  let patch_db = Patch.load_db "sail/patches.txt" in
  Printf.eprintf "Total: %d instructions, %d parsed and %d analysed\nAnalyse failed for:\n"
    (List.length instrs)
    (List.length parsed_instr)
    (List.length good_instrs);
(* PS hackery start *)
  (* printing all non-internal instructions, one per line, with all the mnemonics and status for each*)
  let myinstrs = ref 0 in
  let myparsed = ref 0 in  (* for consistency check wrt Gabriel code *)
  let mygood = ref 0 in
  let myinternal = ref 0 in
  let l = String.length "divdeu divdeu divdeuo divdeuo" in
  let mnemos_of i = String.concat " " (List.map (fun m -> m.Mnemo.name) i.mnemonics) in
  let max_mnemo_length = List.fold_right max (List.map (fun i -> String.length (mnemos_of i)) instrs) 0 in
  let pad s = if String.length s < max_mnemo_length then s ^ String.make (max_mnemo_length - String.length s) ' ' else s in
  List.iter 
    (fun i -> 
      if internal i then 
(*  (if internal then (myinternal := 1 + !myinternal;Printf.eprintf "   internal , ") else Printf.eprintf "notinternal , "); *)
        ()
      else
        begin
          myinstrs := 1 + !myinstrs;
          Printf.eprintf "PS: %s: " xmlfile;
          Printf.eprintf "%s , " (pad (mnemos_of i));
          (if List.mem i parsed_instr then (myparsed := 1 + !myparsed;Printf.eprintf "   parsed , ") else Printf.eprintf "notparsed , ");
          
          (if List.mem i good_instrs then (mygood := 1 + !mygood;Printf.eprintf "   analysed , ") else Printf.eprintf "notanalysed , ");
          Printf.eprintf "\n"
        end)
    instrs;
  Printf.eprintf "PS: %s: Total: %d instructions, %d parsed and %d analysed\n"
    xmlfile
    (!myinstrs)
(*    (!myinternal)*)
    (!myparsed)
    (!mygood);

(* PS hackery end *)
  List.iter (fun i -> Printf.eprintf "%s\n" i.name) bad_instrs;
  with_file instrs (gendir^"/extract.txt") (dump_instr patch_db);
  with_file sail_instrs (gendir^"/extract.sail") (dump_sail patch_db);
  gen_litmus gendir instrs map


(** Print backtraces to help debugging *)
let wrap xml gen =
  Printf.eprintf "******* %s *******\n" xml ;
  try extract xml gen with
  | e -> Printexc.print_backtrace stderr;
         Printf.eprintf "exception: %s\n" (Printexc.to_string e)


let () =
  begin try
    let mnemo_string = Sys.getenv "EXPORT_MNEMO" in
    Printf.eprintf "Export restricted to: %s\n" mnemo_string;
    export_mnemo := Str.split (Str.regexp ",") mnemo_string;
  with Not_found -> () end;
  for i = 2 to Array.length Sys.argv - 1 do 
    wrap Sys.argv.(i) Sys.argv.(1)
  done

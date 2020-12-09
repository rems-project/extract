module SG = SailGen

type action =
    Insert of Ast.typ_aux Ast.exp list (* add before instruction *)
  | Replace of Ast.typ_aux Ast.exp list (* replace instruction *)
  | Append of Ast.typ_aux Ast.exp list (* add after instruction *)

(** The database associates each mnemonic to a list of position and
 * action to perform at that position (insertion or deletion). *)
type db = (string, (int * action) list) Hashtbl.t

let is_comment s = s = "" || s.[0] = '#'

let rec get_lines ic acc =
  match (try Some(input_line ic) with End_of_file -> None)
  with
  | Some l when is_comment l -> get_lines ic acc
  | Some l -> get_lines ic (l::acc)
  | None -> List.rev acc

let check_mnemo s =
  if not (Str.string_match (Str.regexp "[a-z]+") s 0) then
    failwith ("malformed mnemonic in patch file: "^s)

let exp_list_to_exp = function
  | [e] -> e
  | b -> SG.exp_block b

let parse_action chan a = match a.[0] with
  | 'd' -> Scanf.sscanf a "d %d %!" (fun pos -> (pos, Replace []))
  | 'r' -> Scanf.sscanf a "r %d : %s@!" (fun pos e ->
      Printf.fprintf chan "parsing replace patch: %s\n" e;
      let exps = Sail_lib.parse_exps e in
      (pos, Replace exps))
  | 'i' -> Scanf.sscanf a "i %d : %s@!" (fun pos e ->
      Printf.fprintf chan "parsing insert patch: %s\n" e;
      let exps = Sail_lib.parse_exps e in
      (pos, Insert exps))
  | 'a' -> Scanf.sscanf a "a %d : %s@!" (fun pos e ->
      Printf.fprintf chan "parsing append patch: %s\n" e;
      let exps = Sail_lib.parse_exps e in
      (pos, Append exps))
  | _ -> failwith "unknown action"

let action_cmp (p, a) (p', a') =
  let abstract = function Insert _ -> -1 | Replace _ -> 0 | Append _ -> 1 in
  compare (p, abstract a) (p', abstract a')

let load_db file =
  let h = Hashtbl.create 10 in
  let add (pos, action) mnemo =
    check_mnemo mnemo;
    let l = try Hashtbl.find h mnemo with Not_found -> [] in
    (* preserve order *)
    let l' = List.stable_sort action_cmp (l@[(pos,action)]) in
    Hashtbl.replace h mnemo l' in
  let ic = open_in file in
  let oc = open_out "logs/patches.log" in
  let lines = get_lines ic [] in
  let rec parse : string list -> unit = function
    | mnemos :: action :: rest ->
      let mnemo_list = Str.split (Str.regexp " *, *") mnemos in
      let a = parse_action oc action in
      List.iter (add a) mnemo_list;
      parse rest
    | [] -> ()
    | [_] -> failwith ("odd number of lines in "^file) in
  parse lines;
  close_in ic;
  close_out oc;
  h

let apply_patch mnemo actions ast =
  (* helper function to add Append patches after instruction #current *)
  let rec add_append current = function
    | (next, (p, Append el) :: a, acc) when current = p ->
      add_append current (next, a, (*List.rev_append*) List.append el acc)
    | state -> state in
  (* walk simultaneously through the ast and list of actions, count
   * instruction number, and apply actions when needed *)
  let rec walk counter actions block =
    let rec walker state expr = match state with
      (* sanity check *)
      | (c, ((p, _) :: _), _) when c > p ->
        failwith (Printf.sprintf "missed patch on instruction %d" p)
      (* first, iterate until every insert is done *)
      | (c, ((p, Insert el) :: actions), acc) when c = p ->
        walker (c, actions, List.rev_append el acc) expr
      (* then, go deeper if necessary to update counter,
       * replace if needed, and finally process append actions *)
      | (c, ((p, Replace el) :: actions), acc) when c = p ->
	let (next_append, actions_append, acc_append) = add_append c (c,actions,[]) in
        let next, actions, expr' = recurse (c+1) actions_append expr in
        if expr' <> expr then
          failwith "deleting or replacing a patched sub-expression";
	(next, actions, List.append acc_append (List.rev_append el acc))
        (*add_append c (next, actions, List.rev_append el acc)*)
      | (c, actions, acc) ->
	let (next_append, actions, acc_append) = add_append c (c,actions,[]) in
        let next, actions, expr = recurse (c+1) actions expr in
        (*add_append c (next, actions, expr :: acc)*)
	(next, actions, List.rev_append acc_append (expr :: acc))
    in
    let (counter, actions, block) = List.fold_left walker (counter, actions, []) block in
    counter, actions, List.rev block
  (* helper function to recurse down conditionals, etc. *)
  and recurse counter actions expr = match expr with
    | Ast.E_aux(Ast.E_block block, ta) ->
      let (counter, actions, block) = walk counter actions block in
      counter, actions, Ast.E_aux(Ast.E_block block, ta)
    | Ast.E_aux(Ast.E_nondet block, ta) ->
      let (counter, actions, block) = walk counter actions block in
      counter, actions, Ast.E_aux(Ast.E_nondet block, ta)
    | Ast.E_aux(Ast.E_if (cond, e_then, e_else), ta) ->
      let (counter, actions, e_then) = walk counter actions [e_then] in
      let (counter, actions, e_else) = walk counter actions [e_else] in
      counter, actions, Ast.E_aux(
        Ast.E_if (cond, exp_list_to_exp e_then, exp_list_to_exp e_else),
        ta)
    | Ast.E_aux(Ast.E_for(a,b,c,d,e,body), ta) ->
      let (counter, actions, body) = walk counter actions [body] in
      counter, actions, Ast.E_aux(Ast.E_for (a,b,c,d,e,exp_list_to_exp body), ta)
    | _ -> counter, actions, expr
  in
  (* make a block of the body of the function to patch, in case it's a
   * single instruction *)
  let block = match ast with
    | Ast.E_aux(Ast.E_block b, _) -> b
    | e -> [e] in
  (* perform walk *)
  let pprint ast =
    let b = Buffer.create 50 in
    Sail_lib.Pretty.pp_exp b (exp_list_to_exp ast);
    Buffer.contents b in
  let chan = open_out ("logs/"^mnemo^"-patch.log") in
  Printf.eprintf "Patching %s\n" mnemo;
  Printf.fprintf chan "Before:\n%s\n***\n" (pprint block);
  let (_counter, actions, block) = walk 1 actions block in
  if actions <> [] then failwith "remaining actions";
  Printf.fprintf chan "After:\n%s\n***\n" (pprint block);
  close_out chan;
  (* wrap up result in a block if necessary *)
  exp_list_to_exp block

let get mnemo db =
  try apply_patch mnemo (Hashtbl.find db mnemo)
  with Not_found -> (fun x -> x)


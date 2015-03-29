open Mnemo

let token m = (String.uppercase m.name)^(if m.dot then "DOT" else "")

let gen_token _map m = Printf.sprintf "%%token %s" (token m)

let gen_lex _map m = Printf.sprintf "    \"%s%s\", %s;"
    m.name (if m.dot then "." else "") (token m)

let gen_constr m =
  let has_oe = List.mem ("OE", true) m.flags in
  let has_aa = List.mem ("AA", true) m.flags in
  let has_lk = List.mem ("LK", true) m.flags in
  (* XXX addic. and addic are 2 different instructions! *)
  let n = m.name ^ (if m.name = "addic" && m.dot then "dot" else "") in
  let l = String.length n - 1 in
  (* SS: made it a *polymorphic* variant *)
  "`P" ^ String.lowercase (
    if has_oe then begin
      assert (n.[l] = 'o');
      String.sub n 0 l;
    end else
      let minus_a = if has_aa then begin 
        assert (n.[l] = 'a');
        String.sub n 0 l;
      end else n in
      let l = String.length minus_a - 1 in
      if has_lk then begin
        assert (minus_a.[l] = 'l');
        String.sub minus_a 0 l;
      end else minus_a
        )


let assoc_nocase k l =
  try List.assoc (String.lowercase k) l with Not_found ->
    (* intercept failure and attempt name-based type inference optimistically *)
    begin
      match (String.lowercase k) with
      | "ra" | "rb" | "rs" | "rt" -> Footprint.RegInOutImm
      | _ -> Imm
    end
    (* Used to be: *)
    (* failwith ("assoc_nocase: cannot find "^k) *)

let is_input f map = match assoc_nocase f map with
  | Footprint.RegIn | Footprint.RegInOut
  | Footprint.RegInImm | Footprint.RegInOutImm -> true
  | Footprint.RegInFrom | Footprint.RegOutFrom
  | Footprint.RegOut | Footprint.Imm | Footprint.RegOutImm -> false


let is_plain_input f map = match assoc_nocase f map with
  | Footprint.RegIn | Footprint.RegInOut -> true
  | Footprint.RegInFrom | Footprint.RegOutFrom
  | Footprint.RegInImm | Footprint.RegInOutImm
  | Footprint.RegOut | Footprint.Imm | Footprint.RegOutImm -> false


let is_output f map = match assoc_nocase f map with
  | Footprint.RegOut | Footprint.RegInOut
  | Footprint.RegOutImm | Footprint.RegInOutImm -> true
  | Footprint.RegInFrom | Footprint.RegOutFrom
  | Footprint.RegIn | Footprint.Imm | Footprint.RegInImm -> false


let is_imm f map = assoc_nocase f map = Footprint.Imm
let is_inpregfrom f map = assoc_nocase f map = Footprint.RegInFrom
let is_outregfrom f map = assoc_nocase f map = Footprint.RegOutFrom


let field_to_parse f map =
  (* special case for BF and FXM, allowed to be written crX for extended mnemonics *)
  if f = "BF" then (assert (is_imm f map); "crindex") else
  if f = "FXM" then (assert (is_imm f map); "crmask") else
  (* special case for DS field, which is a byte offset in assembly, but
   * a word offset in the binary representation - cf. section 3.3.1 of
   * Power ISA 2.07 *)
  if f = "DS" then (assert (is_imm f map); "ds") else
    (* collapse idx into k, they are the same anyway *)
  if is_imm f map then "k" else "reg"

let field_to_var = String.uncapitalize
let field_to_pp map (_, f) =
  (* special cases - see comments in field_to_parse above for details *)
  (* Note: not pretty-printing FXM to crX, it is too error-prone *)
  if f = "DS" then (assert (is_imm f map); Printf.sprintf "(pp_ds %s)" (field_to_var f)) else
  if is_imm f map then field_to_var f
  else Printf.sprintf "(pp_reg %s)" (field_to_var f)

let field_to_ppformat f map =
  if is_imm f map then "%d" else "%s"

let make_flag_name flag = match flag with
  | "Rc" -> "CR0" (* XXX this should be CR1 for floating-point chapter *)
  | "OE" -> "SOOV"
  (* XXX the following are not currently supported in litmus *)
  | "AA" | "LK" -> flag
  | _ -> failwith ("bug: make_flag_name: unknown "^flag)

let make_flag (f, b) =
  Printf.sprintf "%s%s" (if b then "Set" else "DontSet") (make_flag_name f)

let gen_parse map m =
  let tokens, params = Buffer.create 10, Buffer.create 10 in
  let pos = ref 2 in
  let add_params s =
    if Buffer.length params > 0 then Buffer.add_string params ",";
    Buffer.add_string params s in
  let param_to_string (sep, f) =
    let f' = field_to_parse f map in
    match sep with
    | NoSep ->
      Buffer.add_string tokens " "; Buffer.add_string tokens f';
      add_params (Printf.sprintf "$%d" !pos);
      pos := !pos + 1
    | Comma ->
      Buffer.add_string tokens " COMMA "; Buffer.add_string tokens f';
      add_params (Printf.sprintf "$%d" (!pos + 1));
      pos := !pos + 2
    | Paren ->
      Buffer.add_string tokens " LPAR ";
      Buffer.add_string tokens f';
      Buffer.add_string tokens " RPAR";
      add_params (Printf.sprintf "$%d" (!pos + 1));
      pos := !pos + 3 in
  List.iter (fun c -> add_params (make_flag c)) m.flags;
  List.iter param_to_string m.params;
  match Buffer.contents params with
  | "" ->
    Printf.sprintf "  | %s%s\n    { %s }"
      (token m)
      (Buffer.contents tokens)
      (gen_constr m)
  | params ->
    Printf.sprintf "  | %s%s\n    { %s (%s) }"
      (token m)
      (Buffer.contents tokens)
      (gen_constr m)
      params

let gen_ast_frag map m =
  let make_flag (f, _) = "set"^(String.lowercase (make_flag_name f)) in
  let param_to_string (_, f) = field_to_parse f map in
  let flags = List.map make_flag m.flags in
  let params = List.map param_to_string m.params in
  (flags @ params)

let gen_ast map m =
  let flags_params = gen_ast_frag map m in
  match String.concat "*" (flags_params) with
  | "" -> Printf.sprintf "  | %s" (gen_constr m)
  | params -> Printf.sprintf "  | %s of %s" (gen_constr m) params


let gen_pattern m =
  let param_to_string (_, f) = field_to_var f in
  let flags = List.map make_flag m.flags in
  let params = List.map param_to_string m.params in
  match String.concat "," (flags@params) with
  | "" -> Printf.sprintf "| %s ->" (gen_constr m)
  | params -> Printf.sprintf "| %s (%s) ->" (gen_constr m) params


let gen_pp map m =
  let pp (sep, f) = param_to_string (sep, field_to_ppformat f map) in
  let pp_string = String.concat "" (List.map pp m.params) in
  let pp_params = String.concat " " (List.map (field_to_pp map) m.params) in
  Printf.sprintf "%s sprintf \"%s %s\" %s"
    (gen_pattern m)
    (name_to_string m)
    pp_string
    pp_params

let gen_fold map m =
  let fold acc (_, f) =
    if is_imm f map then acc
    else if is_inpregfrom f map 
    then Printf.sprintf "List.fold_right fold_reg (regs_interval %s) (%s)"
        (field_to_var f) acc
    else if is_outregfrom f map 
    then Printf.sprintf "List.fold_right fold_reg (regs_interval %s) (%s)"
        (field_to_var f) acc
    else Printf.sprintf "fold_reg %s (%s)" (field_to_var f) acc in
  let fold_string = List.fold_left fold "y_reg, y_sreg" m.params in
  Printf.sprintf "%s %s"
    (gen_pattern m)
    fold_string

let gen_map map m =
  (* hmm, copy-paste from gen_pattern mostly *)
  let param_to_string (_, f) =
    let v = field_to_var f in
    if is_imm f map || is_inpregfrom f map || is_outregfrom f map then v
    else "map_reg "^v in
  let flags = List.map make_flag m.flags in
  let params = List.map param_to_string m.params in
  match String.concat "," (flags@params) with
  | "" -> Printf.sprintf "%s %s" (gen_pattern m) (gen_constr m)
  | params -> Printf.sprintf "%s %s(%s)" (gen_pattern m) (gen_constr m) params


let index k l =
  let rec aux n = function
    | x :: xs when x = k -> n
    | x :: xs -> aux (n+1) xs
    | [] -> raise Not_found
  in aux 0 l


(* HERE BE DRAGONS --- hmm, or rather RegInImm.  We have to take care of GPR0
 * being treated specially by some instructions.  This is dealt with by the fact
 * that it is then treated as an immediate in the pseudocode --- and it only
 * happens for RA, really.  Its type is then RegInImm.  Note that RegInOutImm is
 * never used in practice, and RegOutImm is used for RT in lswx, but this
 * instruction is broken anyway. *)
let gen_compile map m =
  let inputs = List.filter (fun (_, f) -> is_input f map) m.params in
  let plain_inputs = List.filter (fun (_, f) -> is_plain_input f map)
      m.params in
  let outputs = List.filter (fun (_, f) -> is_output f map) m.params in
  let imms = List.filter (fun (_, f) -> is_imm f map) m.params in
  let inp_reg_from = List.filter (fun (_, f) -> is_inpregfrom f map) m.params in
  let out_reg_from = List.filter (fun (_, f) -> is_outregfrom f map) m.params in
  let pp_imm (_, f) =
    (* DS must be pretty-printed as a byte offset for GCC *)
    if f = "DS" then Printf.sprintf "(%s lsr 2)" (field_to_var f)
    else field_to_var f in
  let memo_string special_ra inputs f =
    let s =
      if (snd f) = "RA" && special_ra then
        "0"
      else if List.mem f inputs || List.mem f inp_reg_from then
        Printf.sprintf "^i%d" (index f (inputs @ inp_reg_from))
      else if List.mem f outputs || List.mem f out_reg_from then
        Printf.sprintf "^o%d" (index f (outputs @ out_reg_from))
      else if List.mem f imms then
        "%i"
      else assert false
    in
    match fst f with
    | NoSep -> s
    | Comma -> Printf.sprintf ",%s" s
    | Paren -> Printf.sprintf "(%s)" s in
  if inputs = plain_inputs then begin
    Printf.sprintf "%s\n    \
                    { empty_ins with\n    \
                    memo=sprintf \"%s %s\" %s;\n    \
                    inputs=[%s];\n    \
                    outputs=[%s]; }::k"
      (gen_pattern m)

      (name_to_string m)
      (String.concat ""   (List.map (memo_string false inputs) m.params))
      (String.concat " "  (List.map pp_imm imms))

      (String.concat "; " (List.map (fun (_, f) -> field_to_var f) inputs))
      (String.concat "; " (List.map (fun (_, f) -> field_to_var f) outputs))
  end else begin
    (* Here we build the input list conditionnally *)
    Printf.sprintf "%s\n    \
                    { empty_ins with\n    \
                    memo= if rA = A.Ireg A.GPR0\n      \
                    then sprintf \"%s %s\" %s\n      \
                    else sprintf \"%s %s\" %s;\n    \
                    inputs=\n      \
                    (if rA = A.Ireg A.GPR0 then [%s] else [%s])%s;\n    \
                    outputs=[%s]%s; }::k"

      (gen_pattern m)

      (name_to_string m)
      (String.concat ""   (List.map (memo_string true plain_inputs)
                             m.params))
      (String.concat " "  (List.map pp_imm imms))

      (name_to_string m)
      (String.concat ""   (List.map (memo_string false inputs) m.params))
      (String.concat " "  (List.map pp_imm imms))

      (String.concat "; " (List.map (fun (_, f) -> field_to_var f) plain_inputs))
      (String.concat "; " (List.map (fun (_, f) -> field_to_var f) inputs))

      (if List.length inp_reg_from = 0 then "" else
      " @ " ^
      (String.concat "@ " (List.map (fun (_, f) -> "(A.regs_interval " ^ field_to_var f ^ ")") inp_reg_from)))


      (String.concat "; " (List.map (fun (_, f) -> field_to_var f) outputs))

      (if List.length out_reg_from = 0 then "" else
       " @ " ^ 
      (String.concat "@ " (List.map (fun (_, f) -> "(A.regs_interval " ^ field_to_var f ^ ")") out_reg_from)))
        
  end


open Instruction

let gen_trans_sail map i = 
  if i.mnemonics = [] then failwith i.name;
  let m = List.hd i.mnemonics in (* hoping those are same, checked elsewhere *)

  (* hmm, the mnemonics and the decoded instruction have 
     flags and parameters in different orders *)
  (* First, the mnemonic way *)
  let flags_params = gen_ast_frag map m in
  (* add arg number to disambiguate *)
  let fps = List.mapi (fun i f -> Printf.sprintf "%s%d" f i) flags_params in 
  
  (* then a function to convert a decoded field in that kind of list *)
  let find_param_num n =
    let ifield = String.lowercase (List.nth i.ifields n) in
    let raw_flag_params = 
      (List.map fst m.Mnemo.flags) @ (List.map snd m.Mnemo.params) in
    let rec find_pos_in_lst look_for n lst =
      match lst with
      | [] -> begin Printf.eprintf "%s:Did not find %s\n" m.name ifield; List.iter (Printf.eprintf "%s; ") raw_flag_params; Printf.eprintf "\n"; raise Not_found end
      | h :: l -> 
          if look_for = (String.lowercase h) 
          then n else find_pos_in_lst look_for (n+1) l
    in
    find_pos_in_lst ifield 0 raw_flag_params in

  (* Now we can generate the function to convert parameters *)
  let fields = List.filter (function BinRep.Ifield _ -> true | _ -> false) 
      (List.sort BinRep.compare_pos i.repr) in
  let pp_fields =
    List.mapi
      (fun idx f -> match f with
        | (BinRep.Ifield (n,(_,1))) ->
            let fld = 
              try List.nth fps (find_param_num idx) 
              with Not_found -> 
                Printf.sprintf "(trans_%s_%s " (String.lowercase (List.nth i.ifields idx)) (String.concat "_" flags_params) ^
                (String.concat " " fps) ^ 
                ")"
            in
            let field_name = try List.nth flags_params (find_param_num idx) 
            with Not_found -> "PLACEHOLDER" in
            let conv_fld =
              match field_name with
              | "setcr0" -> Printf.sprintf "(trans_cr0 %s)" fld 
              | "setsoov" -> Printf.sprintf "(trans_soov %s)" fld
              | "setaa" -> Printf.sprintf "(trans_aa %s)" fld
              | "setlk" -> Printf.sprintf "(trans_lk %s)" fld
              | _ -> fld in
            Printf.sprintf "(\"%s\", IInt.Bit, IImp.num_to_bits 1 IInt.Bitv (Big_int_Z.big_int_of_int %s))" n conv_fld
        | (BinRep.Ifield (n,(_,sz))) ->
            let fld = 
              try List.nth fps (find_param_num idx) 
              with Not_found -> 
                Printf.sprintf "(trans_%s_%s " (String.lowercase (List.nth i.ifields idx)) (String.concat "_" flags_params) ^
                (String.concat " " fps) ^ 
                ")"
            in
            let ifield = List.nth i.ifields idx in
            if is_imm ifield map 
            then Printf.sprintf "(\"%s\", IInt.Bvector (Some %d), IImp.num_to_bits %d IInt.Bitv (Big_int_Z.big_int_of_int %s))" 
                n sz sz fld
                (* for registers *)
            else Printf.sprintf "(\"%s\", IInt.Bvector (Some 5), IImp.num_to_bits 5 IInt.Bitv (Big_int_Z.big_int_of_int (int_of_reg %s)))" 
                n fld
            
        | _ -> Printf.eprintf "what type?\n"; assert false)
     fields in      

  let sailname = (* special case for addicdot *)
    if ((List.hd i.mnemonics).Mnemo.name = "addic") && ((List.hd i.mnemonics).Mnemo.dot) 
    then String.capitalize ((List.hd i.mnemonics).Mnemo.name ^ "Dot")
    else String.capitalize (List.hd i.mnemonics).Mnemo.name
  in

  (* printing it all out *)

  (* 1. the instruction name *)
  Printf.sprintf "  | %s" (gen_constr m) ^

  (* 2. the arguments (in mnemonic, aka litmus order) *)
  (match fps with
  | [] -> ""
  | fps -> "(" ^ (String.concat ", " fps) ^ ")") ^

  (* 3. now for the conversion *)
  " -> \n" ^

  (* 3.1. The Sail name *)
  Printf.sprintf "      (\"%s\",\n" sailname ^

  (* 3.2. The field conversions *) 
  "       [" ^ (String.concat ";\n        " pp_fields) ^ "],\n" ^

  (* 3.3. The list of "base effects", always empty for litmus *)
  "       [(* always empty base effects*)]\n" ^
  "      )"

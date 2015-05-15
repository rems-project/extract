{
open PseudoParser
open Lexing
exception Eof
let remove_underscore = Str.global_replace (Str.regexp "_") ""
let hex_to_bit s =
  let b = Buffer.create 10 in
  let tr = function
    | '0' -> "0000" | '1' -> "0001"
    | '2' -> "0010" | '3' -> "0011"
    | '4' -> "0100" | '5' -> "0101"
    | '6' -> "0110" | '7' -> "0111"
    | '8' -> "1000" | '9' -> "1001"
    | 'a' | 'A' -> "1010" | 'b' | 'B' -> "1011"
    | 'c' | 'C' -> "1100" | 'd' | 'D' -> "1101"
    | 'e' | 'E' -> "1110" | 'f' | 'F' -> "1111"
    | _ -> ""
  in
  String.iter (fun c -> Buffer.add_string b (tr c)) s;
  Buffer.contents b
}
let to31 = ['0'-'9'] | ['1'-'2']['0'-'9'] | '3'['0'-'1']
let to63 = ['0'-'9'] | ['1'-'5']['0'-'9'] | '6'['0'-'3']

rule token = parse
    [' ' '\t' '\n']     { token lexbuf }     (* skip blanks *)
  | eof            { EOF }
  | ['0'-'9''_']+     { INT(int_of_string(lexeme lexbuf)) }
  | "0x"(['0'-'9''_''a'-'f''A'-'F']+ as h)     { BITFIELD(hex_to_bit h) }
  | "0b"(['0'-'1''_']+ as b)     { BITFIELD(remove_underscore b) }
  | '+'            { PLUS }
  | "+tea"         { PLUS }
  | '-'            { MINUS }
  | "×"            { TIMES }
  | "*"            { TIMES }
  | "∞"            { TIMES }
  | "÷"            { DIV }
  | "mod"          { MOD }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | ','            { COMMA }
  | ':'            { COLON }
  | '<'            { LT }
  | "≤"            { LE }
  | "≥"            { GE }
  | '>'            { GT }
  | "< STARTSuper u ENDSuper" { LTU }
  | "> STARTSuper u ENDSuper" { GTU }
  (* Symbols *)
  | "←"            { ASSIGN }
  | "iea"          { IEA }
  | "64-bit mode"  { IDENT("mode64bit") }
  | "Big-Endian byte ordering" { IDENT("bigendianmode") }
  | "||"           { CONCAT }
  | "¬"            { NOT }
  | "|"            { OR }
  | "&"            { AND }
  | "≡"            { EQUIV }
  | "⊕"            { XOR }
  | "XOR"          { XOR } (* XXX you know, why use just one symbol after all? *)
  | "="            { EQ }
  | "≠"            { NEQ }

  (* XXX stuff not (really) supported in parser at the moment *)
  | "?"            { QUESTION } (* for ternary operator ? : *)
  | "<<"           { SHIFTL } (* fill with zeroes *)
  | ">>"           { SHIFTR } (* copy signed bit *)
  | ">>ui"           { SHIFTRUI } (* fill with zeroes *)
  | ">> STARTSub ui ENDSub"           { SHIFTRUI } (* fill with zeroes *)
  | "<<<"           { SHIFTLROT } (* XXX the same as ROTL64 but for arbitrary length? *)

  | "+int"         { PLUS } (* XXX need to distinguish *)
  | "+ STARTSub int ENDSub" { PLUS } (* XXX idem *)
  | "∞si"            { TIMES } (* XXX idem *)
  | "∞ui"            { TIMES } (* XXX idem *)

  | "undefined"    { UNDEF }
  | "see Book III-S" { TRAP } (* XXX *)
  (* sup/sub *)
  | "STARTSub"     { LSUB }
  | "ENDSub"       { RSUB }
  | "STARTSuper"     { LSUP }
  | "ENDSuper"       { RSUP }
  (* functions *)
  | "ABS"    { FUNC(lexeme lexbuf) }
  | "DOUBLE" { FUNC(lexeme lexbuf) }
  | "SINGLE" { FUNC(lexeme lexbuf) }
  | "EXTS"   { FUNC(lexeme lexbuf) }
  | "MASK"   { FUNC(lexeme lexbuf) }
  | "DEC_TO_BCD" { FUNC(lexeme lexbuf) }
  | "BCD_TO_DEC" { FUNC(lexeme lexbuf) }
  | "length" { FUNC(lexeme lexbuf) }
  | "carry_out" { FUNC(lexeme lexbuf) }
  | "RoundToSPInt"['A'-'Z']['a'-'z']+ { FUNC(lexeme lexbuf) }
  | "Convert"['A'-'Z''a'-'z']+ { FUNC(lexeme lexbuf) }
  | ['A'-'Z''a'-'z''0'-'9']+"Estimate"['A'-'Z''a'-'z']+ { FUNC(lexeme lexbuf) }
  | "Clamp" { FUNC(lexeme lexbuf) }
  | "Chop" { FUNC(lexeme lexbuf) }
  | "ROTL"   { ROTL }
  | "DCR"   { DCR }
  | "GPR"   { GPR }
  | "SPR"   { SPR }
  | "MEM"   { MEM }
  | "MEM"   { MEM_DECORATED }
  (* Special *)
  | "TRAP" { TRAP }
  (* Control-flow *)
  | "if" { IF }
  | "If" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | ';'    { SEQ }    (* added by extract *)
  | "BEGIN" { BEGIN } (* added by extract *)
  | "END"   { END }   (* added by extract *)
  | "do" { DO }
  | "For" { DO }      (* again, Nature loves diversity *)
  | "while" { WHILE }
  | "leave" { LEAVE }
  | "to"    { TO }
  | "by"    { BY }
  (* Instruction fields *)
  | "AA"    { IFIELD(lexeme lexbuf) }
  | "AX"    { IFIELD(lexeme lexbuf) }
  | "A"    { IFIELD(lexeme lexbuf) }
  | "BA"    { IFIELD(lexeme lexbuf) }
  | "BB"    { IFIELD(lexeme lexbuf) }
  | "BC"    { IFIELD(lexeme lexbuf) }
  | "BD"    { IFIELD(lexeme lexbuf) }
  | "BF"    { IFIELD(lexeme lexbuf) }
  | "BFA"    { IFIELD(lexeme lexbuf) }
  | "BH"    { IFIELD(lexeme lexbuf) }
  | "BI"    { IFIELD(lexeme lexbuf) }
  | "BO"    { IFIELD(lexeme lexbuf) }
  | "BT"    { IFIELD(lexeme lexbuf) }
  | "BX"    { IFIELD(lexeme lexbuf) }
  | "B"    { IFIELD(lexeme lexbuf) }
  | "CT"    { IFIELD(lexeme lexbuf) }
  | "CX"    { IFIELD(lexeme lexbuf) }
  | "C"    { IFIELD(lexeme lexbuf) }
  | "D"    { IFIELD(lexeme lexbuf) }
  | "DCM"    { IFIELD(lexeme lexbuf) }
  | "DCR"    { IFIELD(lexeme lexbuf) }
  | "DGM"    { IFIELD(lexeme lexbuf) }
  | "DM"    { IFIELD(lexeme lexbuf) }
  | "DQ"    { IFIELD(lexeme lexbuf) }
  | "DS"    { IFIELD(lexeme lexbuf) }
  | "DUI"    { IFIELD(lexeme lexbuf) }
  | "DUIS"    { IFIELD(lexeme lexbuf) }
  | "E"    { IFIELD(lexeme lexbuf) }
  | "EH"    { IFIELD(lexeme lexbuf) }
  | "FLM"    { IFIELD(lexeme lexbuf) }
  | "FRA"    { IFIELD(lexeme lexbuf) }
  | "FRAp"    { IFIELD(lexeme lexbuf) }
  | "FRB"    { IFIELD(lexeme lexbuf) }
  | "FRBp"    { IFIELD(lexeme lexbuf) }
  | "FRC"    { IFIELD(lexeme lexbuf) }
  | "FRS"    { IFIELD(lexeme lexbuf) }
  | "FRSp"    { IFIELD(lexeme lexbuf) }
  | "FRT"    { IFIELD(lexeme lexbuf) }
  | "FRTp"    { IFIELD(lexeme lexbuf) }
  | "FXM"    { IFIELD(lexeme lexbuf) }
  | "IH"    { IFIELD(lexeme lexbuf) }
  | "L"    { IFIELD(lexeme lexbuf) }
  | "LEV"    { IFIELD(lexeme lexbuf) }
  | "LI"    { IFIELD(lexeme lexbuf) }
  | "LK"    { IFIELD(lexeme lexbuf) }
  | "MB"|"mb" { IFIELD(lexeme lexbuf) } (* XXX lowercase but not split: different instruction forms  and meaning *)
  | "ME"|"me" { IFIELD(lexeme lexbuf) } (* XXX lowercase but not split: different instruction forms  and meaning *)
  | "MO"    { IFIELD(lexeme lexbuf) }
  | "NB"    { IFIELD(lexeme lexbuf) }
  | "OC"    { IFIELD(lexeme lexbuf) }
  | "OPCD"    { IFIELD(lexeme lexbuf) }
  | "OE"    { IFIELD(lexeme lexbuf) }
  | "PMRN"    { IFIELD(lexeme lexbuf) }
  | "R"    { IFIELD(lexeme lexbuf) }
  | "RA"    { IFIELD(lexeme lexbuf) }
  | "RB"    { IFIELD(lexeme lexbuf) }
  | "Rc"    { IFIELD(lexeme lexbuf) }
  | "RMC"    { IFIELD(lexeme lexbuf) }
  | "RS"    { IFIELD(lexeme lexbuf) }
  | "RSp"    { IFIELD(lexeme lexbuf) }
  | "RT"    { IFIELD(lexeme lexbuf) }
  | "RTp"    { IFIELD(lexeme lexbuf) }
  | "S"    { IFIELD(lexeme lexbuf) }
  | "SH"    { IFIELD(lexeme lexbuf) }
  | "SHB"    { IFIELD(lexeme lexbuf) }
  | "SHW"    { IFIELD(lexeme lexbuf) }
  | "SI"    { IFIELD(lexeme lexbuf) }
  | "SIM"    { IFIELD(lexeme lexbuf) }
  | "SP"    { IFIELD(lexeme lexbuf) }
  | "SPR" { IFIELD(lexeme lexbuf) }
  | "SR"    { IFIELD(lexeme lexbuf) }
  | "SX"    { IFIELD(lexeme lexbuf) }
  | "T"    { IFIELD(lexeme lexbuf) }
  | "TBR"    { IFIELD(lexeme lexbuf) }
  | "TE"    { IFIELD(lexeme lexbuf) }
  | "TH"    { IFIELD(lexeme lexbuf) }
  | "TO"    { IFIELD(lexeme lexbuf) }
  | "TX"    { IFIELD(lexeme lexbuf) }
  | "U"    { IFIELD(lexeme lexbuf) }
  | "UI"    { IFIELD(lexeme lexbuf) }
  | "UIM"    { IFIELD(lexeme lexbuf) }
  | "VRA"    { IFIELD(lexeme lexbuf) }
  | "VRB"    { IFIELD(lexeme lexbuf) }
  | "VRC"    { IFIELD(lexeme lexbuf) }
  | "VRS"    { IFIELD(lexeme lexbuf) }
  | "VRT"    { IFIELD(lexeme lexbuf) }
  | "W"    { IFIELD(lexeme lexbuf) }
  | "WC"    { IFIELD(lexeme lexbuf) }
  | "XO"    { IFIELD(lexeme lexbuf) }
  (* Registers *)
  | "CIA"    { REG(lexeme lexbuf) }
  | "NIA"    { REG(lexeme lexbuf) }
  | "CR"    { REG(lexeme lexbuf) }
  | "LR"    { REG(lexeme lexbuf) }
  | "CTR"    { REG(lexeme lexbuf) }
  | "GPR" to31    { REG(lexeme lexbuf) }
  | "XER"    { REG(lexeme lexbuf) }
  | "VRSAVE"    { REG(lexeme lexbuf) }
  | "SPRG" ['4'-'7']    { REG(lexeme lexbuf) }
  | "FPR0" to31    { REG(lexeme lexbuf) }
  | "FPSCR"    { REG(lexeme lexbuf) }
  | "VR" to63    { REG(lexeme lexbuf) }
  | "Accumulator"    { REG(lexeme lexbuf) }
  | "SPEFSCR"    { REG(lexeme lexbuf) }
  (* Free variables - must be after the keywords above *)
  (* assume no mixed case here, to enable parsing of RAn+1 *)
  | ['a'-'z' '_']+ { IDENT(lexeme lexbuf) }
  | ['A'-'Z' '_']+ { IDENT(lexeme lexbuf) }

%{
open Pseudo

let make_sup n expr = match expr with
  | Int 0 -> Bitfield (String.make n '0')
  | Int 1 -> Bitfield (String.make n '1')
  | _ -> Binop(Replicate, expr, Int n)
;;

let make_exp n m =
  if n <> 2 then failwith (Printf.sprintf "unexpected exponentiation: %d^%d" n m)
  else
  let s = String.make (m+1) '0' in
  s.[0] <- '1';
  Bitfield s
;;

let make_concat e1 e2 = match e1, e2 with
  | Concat el, Concat el' -> Concat (el @ el')
  | Concat el, e -> Concat (el @ [e])
  | e, Concat el -> Concat (e :: el)
  | _, _ -> Concat [e1; e2]
%}

%token <int> INT
%token <string> BITFIELD
%token <string> IDENT
%token ASSIGN IEA
%token LPAREN RPAREN
%token EOF
%token UNDEF
%token LT GT LE GE LTU GTU
%token CONCAT NOT OR EQUIV XOR AND EQ NEQ
%token MOD
%token LSUB RSUB LSUP RSUP
%token <string> IFIELD
%token <string> REG

%token QUESTION SHIFTL SHIFTR SHIFTRUI SHIFTLROT

%token TRAP

%token BEGIN SEQ END
%token IF THEN ELSE
%token DO WHILE LEAVE TO BY

%token COMMA COLON

%token <string> FUNC
%token ROTL
%token DCR
%token GPR
%token SPR
%token MEM MEM_DECORATED

%token PLUS MINUS TIMES DIV

/* Precedences go increasing, so "then" < "else" */
%nonassoc THEN
%nonassoc ELSE


/* Resolve shift/reduce conflict for ifield dereference:
  (RA) should be interpreted as a dereference of RA, not as
  RA with spurious parens. */
%nonassoc IFIELD
%nonassoc RPAREN


%start main
%type <Pseudo.block> main
%%
main:
    block EOF { $1 }
;
block:
    instr { [ $1 ] }
  | BEGIN instr_list END { $2 }
;
instr_list: instr_list_elem { List.rev $1 }
;
instr_list_elem:
    instr SEQ { [ $1 ] }
  | instr_list_elem instr SEQ { $2 :: $1 }
;

instr:
  | lval sub_opt ASSIGN expr { Assign(($1, $2), $4, Regular) }
  | lval sub_opt ASSIGN LSUB IEA RSUB expr { Assign(($1, $2), $7, Iea) }
  | IF expr THEN block { If($2, $4, []) }
  | IF expr THEN block ELSE block { If($2, $4, $6) }
  | DO WHILE expr block { DoWhile($3, $4) }
  | DO IDENT ASSIGN expr TO expr do_step block { For($2, Inc, SimpleInit $4, $6, $7, $8) }
  | DO IDENT EQ expr TO expr do_step block { For($2, Inc, SimpleInit $4, $6, $7, $8) }
  | TRAP                   { Trap }
  | LEAVE                  { Leave }
  | func { Expr $1 }
;

do_step:
  | { Int 1 }
  | BY expr { $2 }
;

lval:
    IFIELD { Ifield($1) }
  | IDENT  { Var($1) }
  | REG    { Reg(RLit($1)) }
  | DCR LPAREN expr RPAREN { Reg(DCR($3)) }
  | GPR LPAREN expr RPAREN { Reg(GPR($3)) }
  | SPR LPAREN expr RPAREN { Reg(SPR($3)) }
  | MEM LPAREN expr COMMA expr RPAREN { Mem($3, $5) }
;

subscript:
  | plus_sub { $1 }
;

plus_sub:
  | times_sub { $1 }
  | plus_sub PLUS times_sub          { Binop(Plus, $1, $3) }
  | plus_sub MINUS times_sub         { Binop(Minus, $1, $3) }
;

times_sub:
  | atomic_sub { $1 }
  | times_sub TIMES atomic_sub         { Binop(Times, $1, $3) }
  | times_sub DIV atomic_sub           { Binop(Div, $1, $3) }
;

atomic_sub:
    INT                     { Int $1 }
  | lval                    { Lval($1) }
  | LPAREN subscript RPAREN      { $2 }
;

comma_exprs:
  | expr { [$1] }
  | expr COMMA comma_exprs { $1 :: $3 }
;

expr:
  | or_expr { $1 }
;

or_expr:
  | and_expr { $1 }
  | or_expr OR and_expr    { Binop(Or, $1, $3) }
;

and_expr:
  | eq_expr { $1 }
  | and_expr AND eq_expr            { Binop(And, $1, $3) }
  | and_expr EQUIV eq_expr            { (* EQUIV is defined in terms of XOR and NOT *)
                              Binop(Xor, $1, Unop (Not, $3)) }
  | and_expr XOR eq_expr            { Binop(Xor, $1, $3) }
;

eq_expr:
  | concat_expr { $1 }
  | eq_expr EQ  concat_expr            { Binop(Eq, $1, $3) }
  | eq_expr NEQ concat_expr            { Binop(Neq, $1, $3) }
  | eq_expr LT concat_expr            { Binop(Lt, $1, $3) }
  | eq_expr GT concat_expr            { Binop(Gt, $1, $3) }
  | eq_expr LE concat_expr            { Binop(Le, $1, $3) }
  | eq_expr GE concat_expr            { Binop(Ge, $1, $3) }
  | eq_expr LTU concat_expr            { Binop(Ltu, $1, $3) }
  | eq_expr GTU concat_expr            { Binop(Gtu, $1, $3) }
;

concat_expr:
  | shift_expr { $1 }
  | concat_expr CONCAT plus_expr        { make_concat $1 $3 }
;

shift_expr:
  | plus_expr { $1 }
  /* XXX FIXME - also not sure about precedence */
  | plus_expr SHIFTLROT plus_expr { Binop(Shiftl, $1, $3) }
  | plus_expr SHIFTL plus_expr { Binop(Shiftl, $1, $3) }
  | plus_expr SHIFTR plus_expr { Binop(Shiftr, $1, $3) }
  | plus_expr SHIFTRUI plus_expr { Binop(Shiftr, $1, $3) }
;

plus_expr:
  | times_expr { $1 }
  | plus_expr PLUS  times_expr          { Binop(Plus, $1, $3) }
  | plus_expr MINUS times_expr         { Binop(Minus, $1, $3) }
;

times_expr:
  | mod_expr { $1 }
  | times_expr TIMES mod_expr         { Binop(Times, $1, $3) }
  | times_expr DIV   mod_expr         { Binop(Div, $1, $3) }
;

mod_expr:
  | not_expr { $1 }
  | not_expr MOD not_expr            { Binop(Mod, $1, $3) }
;

not_expr:
  | sup_expr { $1 }
  | MINUS not_expr { (* XXX Uminus fails for SailGen *) Binop(Minus, Int 0, $2) }
  | NOT not_expr   { Unop(Not, $2) }
;

sup_expr:
  | sub_expr { $1 }
  | INT LSUP INT RSUP { make_exp $1 $3 }
  | LSUP INT RSUP sub_expr     { make_sup $2 $4 }
;

sub_expr:
  | func_expr { $1 }
  | func_expr LSUB sub RSUB       { Sub($1, $3) }
  /* XXX Sometimes, LSUB / RSUB is missing - try and guess it if it's a
  range - this rule does trigger conflicts, unlike sub_opt, because it
  is not possible to distinguish the end of the second subscript from
  the next operator surrounding the sub_expr. */
  | func_expr subscript COLON subscript         { Sub ($1, Range($2, $4)) }
;

func_expr:
  | starifield_expr { $1 }
  | func { $1 }
;

func:
  | FUNC LPAREN comma_exprs RPAREN { Func ($1, $3) }
  | IDENT LPAREN comma_exprs RPAREN
          {
            Printf.eprintf "Parsing unknown function: %s\n" $1;
            Func ($1, $3)
          }
  | ROTL LSUB INT RSUB LPAREN expr COMMA expr RPAREN
          { match $3 with
            | 64 -> Func("ROTL", [$6; $8])
            | 32 -> Func("ROTL", [make_concat $6 $6; $8])
            | _  -> failwith "Unexpected ROTL subscript"
          }
;

starifield_expr:
  | atomic_expr { $1 }
  | LPAREN IFIELD RPAREN    { StarIfield($2) }
;

atomic_expr:
    INT                     { (* XXX fix common parse errors *)
                              match $1 with
                              | 320 -> make_sup 32 (Int 0)
                              | 310 -> make_sup 31 (Int 0)
                              | 630 -> make_sup 63 (Int 0)
                              | 960 -> make_sup 96 (Int 0)
                              | i -> Int i
                            }
  | BITFIELD                { Bitfield $1 }
  | lval                    { Lval($1) }
  | UNDEF                   { Undef }
  | LPAREN expr RPAREN      { $2 }
;

sub_opt:
  | LSUB sub RSUB           { Some $2}
  /* XXX Sometimes, LSUB / RSUB is missing - try and guess it if it's a
  range - note that this rule does not trigger any conflict because
  sub_opt is always followed by an ASSIGN which helps finding the end of
  the subscript */
  | subscript COLON subscript         { Some (Range($1, $3)) }
  |                         { None }
;
sub:
    subscript                    { Bit($1) }
  | subscript COLON subscript         { Range($1, $3) }
;

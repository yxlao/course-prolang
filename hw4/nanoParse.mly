%{
(* See this for a tutorial on ocamlyacc
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano
%}

%token <int> Num
%token TRUE FALSE
%token <string> Id
%token EOF
%token LET
%token REC
%token EQ
%token IN
%token FUN
%token ARROW
%token IF
%token THEN
%token ELSE

%start exp
%type <Nano.expr> exp

%%

exp:
  | TRUE                       { True }
  | FALSE                      { False }

  | LET Id EQ exp IN exp       { Let ($2, $4, $6) }
  | LET REC Id EQ exp IN exp   { Letrec ($3, $5, $7) }
  | FUN Id ARROW exp           { Fun ($2, $4) }
  | IF exp THEN exp ELSE exp   { If ($2, $4, $6) }

  | Num                        { Const $1 }
  | Id                         { Var $1 }

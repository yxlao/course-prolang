%{
(* See this for a tutorial on ocamlyacc
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano
%}

%token <int> Num
%token TRUE FALSE
%token <string> Id
%token EOF

%start exp
%type <Nano.expr> exp

%%

exp:
  | TRUE                       { True }
  | FALSE                      { False }
  | Num                        { Const $1 }
  | Id                         { Var $1 }

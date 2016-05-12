exception MLFailure of string

type binop = 
      Plus 
    | Minus 
    | Mul 
    | Div 
    | Eq 
    | Ne 
    | Lt 
    | Le 
    | And 
    | Or          
    | Cons

type expr =   
      Const of int 
    | True   
    | False      
    | NilExpr
    | Var of string    
    | Bin of expr * binop * expr 
    | If  of expr * expr * expr
    | Let of string * expr * expr 
    | App of expr * expr 
    | Fun of string * expr    
    | Letrec of string * expr * expr

type value =  
      Int of int		
    | Bool of bool          
    | Closure of env * string option * string * expr 
    | Nil                    
    | Pair of value * value     

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
      Int i -> 
        Printf.sprintf "%d" i
    | Bool b -> 
        Printf.sprintf "%b" b
    | Closure (evn,fo,x,e) -> 
        let fs = match fo with None -> "Anon" | Some fs -> fs in
          Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
    | Pair (v1,v2) -> 
        Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
    | Nil -> 
        "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
    "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True -> 
        "true" 
    | False -> 
        "false"
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
          (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
          (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
          x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
          x (exprToString e1) (exprToString e2)
    | NilExpr -> "[]"

(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
                | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

let lookup (x,evn) = match listAssoc (x,evn) with
  | Some x -> x
  | None -> raise (MLFailure (Printf.sprintf "variable not bound: %s" x))

let rec eval (evn,e) = match e with
  | Const i -> Int i
  | Var s   -> lookup (s, evn)
  | Bin (e1,op,e2) ->
      begin
        match (eval(evn, e1), eval(evn, e2)) with
          | (Int x1, Int x2) -> 
              begin
                match op with
                  | Plus -> Int (x1 + x2)
                  | Minus -> Int (x1 - x2)
                  | Mul -> Int (x1 * x2)
                  | Div -> Int (x1 / x2)
                  | Eq -> Bool (x1 = x2)
                  | Ne -> Bool (x1 <> x2)
                  | Lt -> Bool (x1 < x2)
                  | Le -> Bool (x1 <= x2)
                  | _ -> raise (MLFailure "Invalid operator for 'Int op Int'")
              end
          | (Bool x1, Bool x2) ->
              begin
                match op with
                  | Eq -> Bool (x1 = x2)
                  | Ne -> Bool (x1 <> x2)
                  | And -> Bool (x1 && x2)
                  | Or -> Bool (x1 || x2)
                  | _ -> raise (MLFailure "Invalid operator for 'Bool op Bool'")
              end
          | _ -> raise (MLFailure "Invalid operands for binary ops")
      end
  | If (e1, e2, e3) -> 
      begin
        match eval(evn, e1) with
          | Bool true -> eval(evn, e2)
          | Bool false -> eval(evn, e3)
          | _ -> raise (MLFailure "Invalid value for if condition")
      end
  | Let (s, e1, e2) ->
      eval ((s, eval(evn, e1))::evn, e2)
  | Letrec (s, e1, e2) ->
      eval ((s, eval(evn, e1))::evn, e2)
  | Fun (s, e1) ->
      Closure (evn, None, s, e1)
  | App (e1, e2) ->
      let c = eval(evn, e1) in
      let v = eval(evn, e2) in
        begin
          match c with
            | Closure (c_evn, None, c_s, c_e) -> 
                eval ((c_s, v)::c_evn, c_e)
            | _ -> Nil (* need to add rec *)
        end
  | _ -> raise (MLFailure "Invalid expr type")


(**********************     Testing Code  ******************************)

(* Uncomment to test part (a) 

   let evn = [("z1",Int 0);("x",Int 1);("y",Int 2);("z",Int 3);("z1",Int 4)]

   let e1  = Bin(Bin(Var "x",Plus,Var "y"), Minus, Bin(Var "z",Plus,Var "z1"))

   let _   = eval (evn, e1)        (* EXPECTED: Nano.value = Int 0 *)

   let _   = eval (evn, Var "p")   (* EXPECTED:  Exception: Nano.MLFailure "variable not bound: p". *)

*)

(* Uncomment to test part (b) 

   let evn = [("z1",Int 0);("x",Int 1);("y",Int 2);("z",Int 3);("z1",Int 4)]

   let e1  = If(Bin(Var "z1",Lt,Var "x"),Bin(Var "y",Ne,Var "z"),False)

   let _   = eval (evn,e1)         (* EXPECTED: Nano.value = Bool true *)

   let e2  = If(Bin(Var "z1",Eq,Var "x"), 
   Bin(Var "y",Le,Var "z"),
   Bin(Var "z",Le,Var "y")
   )

   let _   = eval (evn,e2)         (* EXPECTED: Nano.value = Bool false *)

*)

(* Uncomment to test part (c) 

   let e1 = Bin(Var "x",Plus,Var "y")

   let e2 = Let("x",Const 1, Let("y", Const 2, e1)) 

   let _  = eval ([], e2)          (* EXPECTED: Nano.value = Int 3 *)

   let e3 = Let("x", Const 1, 
   Let("y", Const 2, 
   Let("z", e1, 
   Let("x", Bin(Var "x",Plus,Var "z"), 
   e1)
   )
   )
   )

   let _  = eval ([],e3)           (* EXPCETED: Nano.value = Int 6 *)

*)


(* Uncomment to test part (d) *)

let _ = eval ([], Fun ("x",Bin(Var "x",Plus,Var "x"))) 

(* EXPECTED: Nano.value = Closure ([], None, "x", Bin (Var "x", Plus, Var "x")) *)

let _ = eval ([],App(Fun ("x",Bin(Var "x",Plus,Var "x")),Const 3));;

(* EXPECTED: Nano.value = Int 6 *)

let e3 = Let ("h", Fun("y", Bin(Var "x", Plus, Var "y")), 
              App(Var "f",Var "h"))

let e2 = Let("x", Const 100, e3)

let e1 = Let("f",Fun("g",Let("x",Const 0,App(Var "g",Const 2))),e2) 

let _  = eval ([], e1)        
(* EXPECTED: Nano.value = Int 102 *)

(*

let _ = eval ([],Letrec("f",Fun("x",Const 0),Var "f"))
(* EXPECTED: Nano.value = Closure ([], Some "f", "x", Const 0) *)

*)

(* Uncomment to test part (e)

   let _ = eval ([], 
   Letrec("fac", 
   Fun("n", If (Bin (Var "n", Eq, Const 0), 
   Const 1, 
   Bin(Var "n", Mul, App(Var "fac",Bin(Var "n",Minus,Const 1))))),
   App(Var "fac", Const 10)))

   (* EXPECTED: Nano.value = Int 3628800 *)

*)

(* Uncomment to test part (f)

   let _ = eval ([],Bin(Const 1,Cons,Bin(Const 2,Cons,NilExpr)))

   (* EXPECTED: Nano.value = Pair (Int 1, Pair (Int 2, Nil)) *)

   let _ = eval ([],App(Var "hd",Bin(Const 1,Cons,Bin(Const 2,Cons,NilExpr))))

   (* EXPECTED: Nano.value = Int 1 *)

   let _ = eval ([],App(Var "tl",Bin(Const 1,Cons,Bin(Const 2,Cons,NilExpr))))

   (* EXPECTED: Nano.value = Pair (Int 2, Nil) *)

*)

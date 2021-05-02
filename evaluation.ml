(* 
                         CS 51 Final Project
                         MiniML -- Evaluation
*)

(* This module implements a small untyped ML-like language under
   various operational semantics.
 *)

open Expr ;;
  
(* Exception for evaluator runtime, generated by a runtime error in
   the interpreter *)
exception EvalError of string ;;
  
(* Exception for evaluator runtime, generated by an explicit `raise`
   construct in the object language *)
exception EvalException ;;

(*......................................................................
  Environments and values 
 *)

module type ENV = sig
    (* the type of environments *)
    type env
    (* the type of values stored in environments *)
    type value =
      | Val of expr
      | Closure of (expr * env)
   
    (* empty () -- Returns an empty environment *)
    val empty : unit -> env

    (* close expr env -- Returns a closure for `expr` and its `env` *)
    val close : expr -> env -> value

    (* lookup env varid -- Returns the value in the `env` for the
       `varid`, raising an `Eval_error` if not found *)
    val lookup : env -> varid -> value

    (* extend env varid loc -- Returns a new environment just like
       `env` except that it maps the variable `varid` to the `value`
       stored at `loc`. This allows later changing the value, an
       ability used in the evaluation of `letrec`. To make good on
       this, extending an environment needs to preserve the previous
       bindings in a physical, not just structural, way. *)
    val extend : env -> varid -> value ref -> env

    (* env_to_string env -- Returns a printable string representation
       of environment `env` *)
    val env_to_string : env -> string
                                 
    (* value_to_string ?printenvp value -- Returns a printable string
       representation of a value; the optional flag `printenvp`
       (default: `true`) determines whether to include the environment
       in the string representation when called on a closure *)
    val value_to_string : ?printenvp:bool -> value -> string
  end

module Env : ENV =
  struct
    type env = (varid * value ref) list
     and value =
       | Val of expr
       | Closure of (expr * env) ;;

    let empty () : env = [] ;;

    let close (exp : expr) (env : env) : value =
      Closure (exp, env) ;;

    let lookup (env : env) (varname : varid) : value =
      try !(List.assoc varname env)
      with _ -> raise (EvalError ("Variable \"" ^ varname ^ "\" does not exist in the environment")) ;;

    let extend (env : env) (varname : varid) (loc : value ref) : env =
      (varname, loc) :: (List.remove_assoc varname env) ;;

    let env_to_string (env : env) : string =
      let str = ref "" in 
      List.iter (fun (var,_) -> str := (!str ^ var ^ " = NEED TO COMPLETE THIS PART; ")) env ;
      "E{" ^ !str ^ "}" ;; 

    let value_to_string ?(printenvp : bool = true) (v : value) : string =
      match v with
      | Val exp -> "Val: " ^ (exp_to_concrete_string exp)
      | Closure (exp, env) -> 
        "Closure: " ^ (exp_to_concrete_string exp) ^ (
          if printenvp then " in env: " ^ (env_to_string env) ^ ")" 
          else ""
        ) ;;
  end
;;


(*......................................................................
  Evaluation functions

  Each of the evaluation functions below evaluates an expression `exp`
  in an environment `env` returning a result of type `value`. We've
  provided an initial implementation for a trivial evaluator, which
  just converts the expression unchanged to a `value` and returns it,
  along with "stub code" for three more evaluators: a substitution
  model evaluator and dynamic and lexical environment model versions.

  Each evaluator is of type `expr -> Env.env -> Env.value` for
  consistency, though some of the evaluators don't need an
  environment, and some will only return values that are "bare
  values" (that is, not closures). 

  DO NOT CHANGE THE TYPE SIGNATURES OF THESE FUNCTIONS. Compilation
  against our unit tests relies on their having these signatures. If
  you want to implement an extension whose evaluator has a different
  signature, implement it as `eval_e` below.  *)

(* The TRIVIAL EVALUATOR, which leaves the expression to be evaluated
   essentially unchanged, just converted to a value for consistency
   with the signature of the evaluators. *)
   
let eval_t (exp : expr) (_env : Env.env) : Env.value =
  (* coerce the expr, unchanged, into a value *)
  Env.Val exp ;;


(* Converts an Env.value to an expr for use in subst *)
let val_to_expr (v : Env.value) : expr = 
  match v with 
  | Val e -> e 
  | Closure (e, _) -> e ;;

(* Helper function for evaluating unop *)
let eval_unop (un : unop) (e : expr) : expr = 
  match un,e with 
  | Negate, Num i -> Num ((-1) * i)
  | Negate, Float f -> Float ((-1.0) *. f)
  | Not, Bool b -> Bool (not b)
  | _,_ -> raise (EvalError "invalid unop operation \nmake sure to check types") ;;

(* Helper function for evaluating binop *)
let eval_binop (bi : binop) (e1 : expr) (e2 : expr) : expr =
  match bi,e1,e2 with 
  | Plus, Num i1, Num i2 -> Num (i1 + i2)
  | Plus, Float f1, Float f2 -> Float (f1 +. f2)
  | Minus, Num i1, Num i2 -> Num (i1 - i2)
  | Minus, Float f1, Float f2 -> Float (f1 -. f2)
  | Times, Num i1, Num i2 -> Num (i1 * i2)
  | Times, Float f1, Float f2 -> Float (f1 *. f2)
  | Divide, Num i1, Num i2 -> Num (i1 / i2)
  | Divide, Float f1, Float f2 -> Float (f1 /. f2)
  | Modulo, Num i1, Num i2 -> Num (i1 mod i2)
  | Equals, Bool i1, Bool i2 -> if i1 = i2 then Bool true else Bool false
  | Equals, Num i1, Num i2 -> if i1 = i2 then Bool true else Bool false
  | Equals, Float f1, Float f2 -> 
    (* checks for near equality *)
    if abs_float (f1 -. f2) < 0.000001 then Bool true 
    else Bool false
  | LessThan, Num i1, Num i2 -> if i1 < i2 then Bool true else Bool false
  | LessThan, Float f1, Float f2 -> if f1 < f2 then Bool true else Bool false
  | LessThanOrEqual, Num i1, Num i2 -> if i1 <= i2 then Bool true else Bool false
  | LessThanOrEqual, Float f1, Float f2 -> if f1 <= f2 then Bool true else Bool false
  | GreaterThan, Num i1, Num i2 -> if i1 > i2 then Bool true else Bool false
  | GreaterThan, Float f1, Float f2 -> if f1 > f2 then Bool true else Bool false
  | GreaterThanOrEqual, Num i1, Num i2 -> if i1 >= i2 then Bool true else Bool false
  | GreaterThanOrEqual, Float f1, Float f2 -> if f1 >= f2 then Bool true else Bool false
  | _,_,_ -> raise (EvalError "invalid binop operation \nmake sure to check types") ;;


(* Used to keep track of which model to use *)
type model = Substitution | Dynamic | Lexical ;;
(* Semantics model we are currently using. 
   Set to Substitution by default. *)
let curr_mod = ref Substitution ;;


(* The UNIVERSAL evaluator -- essentially allows for any evaluation type *)
let rec eval_uni (exp : expr) (env : Env.env) : Env.value =
  match exp with 
  | Var v -> (
      match !curr_mod with 
      | Substitution -> raise (EvalError ("Unbound variable " ^ v))
      | Dynamic | Lexical -> 
        try 
          match Env.lookup env v with 
          | Env.Val new_exp -> Env.Val new_exp
          | Env.Closure (new_exp, new_env) -> eval_uni new_exp new_env
        with 
          Not_found -> raise (EvalError ("Unbound variable " ^ v))
    )
  | Num _ | Float _ | Bool _ -> Env.Val exp 
  | Unassigned | Raise -> raise EvalException 
  | Unop (un, e) -> Env.Val (eval_unop un (val_to_expr (eval_uni e env)))
  | Binop (bi, e1, e2) -> 
    Env.Val (eval_binop bi (val_to_expr (eval_uni e1 env)) (val_to_expr (eval_uni e2 env)))
  | Conditional (e1, e2, e3) -> (
      match eval_uni e1 env with 
      | Env.Val (Bool true) -> eval_uni e2 env
      | Env.Val (Bool false) -> eval_uni e3 env
      | _ -> raise (EvalError "expecting bool but received something else")
    )
  | Fun _ -> (
      match !curr_mod with 
      | Substitution | Dynamic -> Env.Val exp
      | Lexical -> Env.close exp env
    )
  | Let (v, e1, e2) -> (
      match !curr_mod with 
      | Substitution -> eval_uni (subst v (val_to_expr (eval_uni e1 env)) e2) env
      | Dynamic | Lexical -> let new_e1 = eval_uni e1 env in 
        eval_uni e2 (Env.extend env v (ref new_e1))
    )
  | Letrec (v, e1, e2) -> (
      match !curr_mod with 
      | Substitution -> let new_e1 = eval_uni (subst v (Letrec (v, e1, Var v)) e1) env in 
        eval_uni (subst v (val_to_expr new_e1) e2) env
      | Dynamic | Lexical ->
        let new_env = Env.extend env v (ref (Env.Val Unassigned)) in 
        let new_e1 = eval_uni e1 new_env in 
        (match new_e1 with 
        | Env.Val (Var _) -> raise (EvalError "hit variable")
        | _ -> eval_uni e2 (Env.extend new_env v (ref new_e1)))
    )
  | App (e1, e2) -> (
      match eval_uni e1 env with 
      | Env.Val (Fun (v, e3)) -> 
        if !curr_mod = Substitution 
          then eval_uni (subst v (val_to_expr (eval_uni e2 env)) e3) env
        else let new_val = ref (eval_uni e2 env) in 
          eval_uni e3 (Env.extend env v new_val)
      | Env.Closure (Fun (v, e3), old_env) -> 
          let new_val = ref (eval_uni e2 old_env) in 
          eval_uni e3 (Env.extend old_env v new_val)
      | _ -> raise (EvalError "expected function but received something else") 
    ) ;;


(* Each specific function *)
let eval_s (exp : expr) (env : Env.env) : Env.value =
  curr_mod := Substitution;
  eval_uni exp env ;;

let eval_d (exp : expr) (env : Env.env) : Env.value =
  curr_mod := Dynamic;
  eval_uni exp env ;;

let eval_l (exp : expr) (env : Env.env) : Env.value =
  curr_mod := Lexical;
  eval_uni exp env ;;




(* Connecting the evaluators to the external world. The REPL in
   `miniml.ml` uses a call to the single function `evaluate` defined
   here. Initially, `evaluate` is the trivial evaluator `eval_t`. But
   you can define it to use any of the other evaluators as you proceed
   to implement them. (We will directly unit test the four evaluators
   above, not the `evaluate` function, so it doesn't matter how it's
   set when you submit your solution.) *)

(* let evaluate = 
  match !current with 
  | Substitution -> eval_s
  | Dynamic -> eval_d
  | Lexical -> eval_l
  | Extended -> eval_e
  | Nil -> raise EvalException ;;  *)

let evaluate = 
  (* User defines the semantic type *)
  let preference = 
      print_string ("\nPlease enter a valid semantics model and press enter:\n" ^
        "(\"s\" for substitution, \"d\" for dynamic, \"l\" for lexical) \n" ^ 
        "Invalid inputs will result in Substitution being chosen. \n"); 
      read_line ()
  in 
  if preference = "d" then curr_mod := Dynamic 
  else if preference = "l" then curr_mod := Lexical;
  eval_uni ;;

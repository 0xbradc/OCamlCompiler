(* This file contains the original broken up eval functions.
   This is for reference purposes only. *)


(* The SUBSTITUTION MODEL evaluator -- to be completed *)
let eval_s (exp : expr) (_env : Env.env) : Env.value =
  let rec eval_s' (exp' : expr) : expr = 
    match exp' with 
    | Var v -> raise (EvalError ("Unbound variable " ^ v))
    | Num _ | Float _ | Bool _ | Fun -> exp'
    | Unassigned | Raise -> raise EvalException
    | Unop (un, e) -> eval_unop un (Env.Val (eval_s' e))
    | Binop (bi, e1, e2) -> eval_binop bi (Env.Val (eval_s' e1)) (Env.Val (eval_s' e2))
    | Conditional (e1, e2, e3) -> (
        match eval_s' e1 with 
        | Bool true -> (eval_s' e2) 
        | Bool false -> (eval_s' e3)
        | _ -> raise (EvalError "expecting bool but received something else")
      )
    | Fun _ -> exp'
    | Let (v, e1, e2) -> eval_s' (subst v (eval_s' e1) e2)
    | Letrec (v, e1, e2) -> 
      let new_e1 = eval_s' (subst v (Letrec (v, e1, Var v)) e1) in 
      eval_s' (subst v new_e1 e2)
    | App (e1, e2) -> 
      match eval_s' e1 with 
      | Fun (v, e) -> eval_s' (subst v (eval_s' e2) e)
      | _ -> raise (EvalError "non-function applied")
  in 
  Env.Val (eval_s' exp) ;;


(* The DYNAMICALLY-SCOPED ENVIRONMENT MODEL evaluator -- to be
   completed *)
let rec eval_d (exp : expr) (env : Env.env) : Env.value =
  match exp with 
  | Var v -> (
      try 
        match Env.lookup env v with 
        | Env.Val new_exp -> Env.Val new_exp
        | Env.Closure (new_exp, new_env) -> eval_d new_exp new_env
      with 
        Not_found -> raise (EvalError ("Unbound variable " ^ v))
    )
  | Num _ | Float _ | Bool _ | Fun _ -> Env.Val exp
  | Unassigned | Raise -> raise EvalException
  | Unop (un, e) -> Env.Val (eval_unop un (eval_d e env))
  | Binop (bi, e1, e2) -> Env.Val (eval_binop bi (eval_d e1 env) (eval_d e2 env))
  | Conditional (e1, e2, e3) -> (
      match eval_d e1 env with 
      | Env.Val (Bool true) -> eval_d e2 env
      | Env.Val (Bool false) -> eval_d e3 env
      | _ -> raise (EvalError "expecting bool but received something else")
    )
  | Let (v, e1, e2) -> 
    let new_e1 = eval_d e1 env in 
    eval_d e2 (Env.extend env v (ref new_e1))
  | Letrec (v, e1, e2) -> 
    let new_val = ref (Env.Val Unassigned) in
    let new_env = Env.extend env v new_val in 
    let new_e1 = eval_d e1 new_env in 
    (match new_e1 with 
    | Env.Val (Var _) -> raise (EvalError "hit variable")
    | _ -> new_val :=  new_e1; eval_d e2 new_env)
  | App (e1, e2) -> 
    let new_val = ref (eval_d e2 env) in 
    match eval_d e1 env with 
    | Env.Val (Fun (v, e3)) -> eval_d e3 (Env.extend env v new_val)
    | _ -> raise (EvalError "non-function applied") ;;


(* The LEXICALLY-SCOPED ENVIRONMENT MODEL evaluator -- optionally
   completed as (part of) your extension *)
let rec eval_l (exp : expr) (env : Env.env) : Env.value =
  match exp with 
  (* Same as eval_d *)
  | Var v -> (
      try 
        match Env.lookup env v with 
        | Env.Val new_exp -> Env.Val new_exp
        | Env.Closure (new_exp, new_env) -> eval_l new_exp new_env
      with 
        Not_found -> raise (EvalError ("Unbound variable " ^ v))
    )
  | Num _ | Float _ | Bool _ -> Env.Val exp
  | Unassigned | Raise -> raise EvalException
  | Unop (un, e) -> Env.Val (eval_unop un (eval_l e env))
  | Binop (bi, e1, e2) -> Env.Val (eval_binop bi (eval_l e1 env) (eval_l e2 env))
  | Conditional (e1, e2, e3) -> (
      match eval_l e1 env with 
      | Env.Val (Bool true) -> eval_l e2 env
      | Env.Val (Bool false) -> eval_l e3 env
      | _ -> raise (EvalError "expecting bool but received something else")
    )
  | Let (v, e1, e2) -> 
    let new_e1 = eval_l e1 env in 
    eval_l e2 (Env.extend env v (ref new_e1))
  | Letrec (v, e1, e2) -> 
    let new_val = ref (Env.Val Unassigned) in
    let new_env = Env.extend env v new_val in 
    let new_e1 = eval_l e1 new_env in 
    (match new_e1 with 
    | Env.Val (Var _) -> raise (EvalError "hit variable")
    | _ -> new_val :=  new_e1; eval_l e2 new_env)
  (* Special Cases *)
  | Fun _ -> Env.close exp env
  | App (e1, e2) -> 
    match eval_l e1 env with 
    | Env.Closure (Fun (v, e3), old_env) -> 
      let new_val = ref (eval_l e2 old_env) in 
      eval_l e3 (Env.extend old_env v new_val)
    | _ -> raise (EvalError "expected function but received something else") ;;


(* The EXTENDED evaluator -- if you want, you can provide your
   extension as a separate evaluator, or if it is type- and
   correctness-compatible with one of the above, you can incorporate
   your extensions within `eval_s`, `eval_d`, or `eval_l`. *)

let eval_e _ =
  failwith "eval_e not implemented" ;;

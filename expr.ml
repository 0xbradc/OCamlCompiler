(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Divide
  | Modulo
  | Equals
  | LessThan
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Float of float                       (* floats *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal ;;

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)
let rec free_vars (exp : expr) : varidset =
  match exp with
  | Var v -> SS.singleton v
  | Num _ | Float _ | Bool _ | Raise | Unassigned -> SS.empty
  | Unop (_, e) -> free_vars e
  | Binop (_, e1, e2) -> SS.union (free_vars e1) (free_vars e2)
  | Conditional (e1, e2, e3) ->
    SS.union (SS.union (free_vars e1) (free_vars e2)) (free_vars e3)
  | Fun (v, e) -> SS.remove v (free_vars e)
  | Let (v, e1, e2) -> SS.union (free_vars e1) (SS.remove v (free_vars e2)) 
  | Letrec (v, e1, e2) ->
    SS.union (SS.remove v (free_vars e1)) (SS.remove v (free_vars e2))
  | App (e1, e2) -> SS.union (free_vars e1) (free_vars e2) ;;
  

(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no variable names use the
   prefix "var". (Otherwise, they might accidentally be the same as a
   generated variable name.) *)

let new_varname : unit -> varid = 
  let count = ref ~-1 in 
  fun () -> incr count; "var" ^ (string_of_int !count) ;;


(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  (* Stored the free variables of repl for faster run-time *)
  let var_set = free_vars repl in 
  let rec subst' exp' = 
    match exp' with
    | Var v -> if v = var_name then repl else exp'
    | Num _ | Float _ | Bool _ | Raise | Unassigned -> exp'
    | Unop (u, e) -> Unop (u, subst' e)
    | Binop (b, e1, e2) -> Binop (b, subst' e1, subst' e2)
    | Conditional (e1, e2, e3) -> Conditional (subst' e1, subst' e2, subst' e3)
    | Fun (v, e) -> 
      if v = var_name then exp'
      (* Check if repl contains variable v *)
      else if SS.mem v var_set 
        then let n = new_varname () in Fun (n, subst' (subst v (Var n) e))
      else Fun (v, subst' e)
    | Let (v, e1, e2) ->
      if v = var_name then Let(v, subst' e1, e2)
      (* Check if repl contains variable v *)
      else if SS.mem v var_set 
        then let n = new_varname () in Let (n, subst' e1, subst' (subst v (Var n) e2))
      else Let (v, subst' e1, subst' e2)
    | Letrec (v, e1, e2) ->
      if v = var_name then Letrec(v, subst' e1, e2)
      (* Check if repl contains variable v *)
      else if SS.mem v var_set 
        then let n = new_varname () in Letrec (n, subst' (subst n (Var n) e1), subst' e2)
      else Letrec (v, subst' e1, subst' e2)
    | App (e1, e2) -> App (subst' e1, subst' e2)
  in 
  subst' exp ;;

     
(*......................................................................
  String representations of expressions
 *)
   
(* Helper to-string methods *)
let to_string_unop_concrete u : string = 
  match u with 
  | Negate -> "~-" ;;

let to_string_binop_concrete b : string = 
  match b with 
  | Plus -> " + "
  | Minus -> " - "
  | Times -> " * "
  | Divide -> " / "
  | Modulo -> " mod "
  | Equals -> " = "
  | LessThan -> " < " ;;


(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with
  | Var v -> v
  | Num i -> string_of_int i
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | Unop (un, e) -> (to_string_unop_concrete un) ^ (exp_to_concrete_string e)
  | Binop (bi, e1, e2) ->
    "(" ^ (exp_to_concrete_string e1) ^
    (to_string_binop_concrete bi) ^ 
    (exp_to_concrete_string e2) ^ ")"
  | App (e1, e2) -> (exp_to_concrete_string e1) ^ " " ^ (exp_to_concrete_string e2)
  | Conditional (e1, e2, e3) ->
    "if " ^ exp_to_concrete_string e1 ^
    " then " ^ exp_to_concrete_string e2 ^
    " else " ^ exp_to_concrete_string e3
  | Fun (v, e) -> "(fun " ^ v ^ " -> " ^ (exp_to_concrete_string e) ^ ")"
  | Let (v, e1, e2) ->
    "let " ^ v ^ " = " ^ (exp_to_concrete_string e1)
    ^ " in " ^ (exp_to_concrete_string e2)
  | Letrec (v, e1, e2) ->
    "let rec " ^ v ^ " = " ^ (exp_to_concrete_string e1)
    ^ " in " ^ (exp_to_concrete_string e2) ;;
     


(* Helper to-string methods *)
let to_string_unop_abstract u : string = 
  match u with 
  | Negate -> "Negate" ;;

let to_string_binop_abstract b : string = 
  match b with 
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Times -> "Times"
  | Divide -> "Divide"
  | Modulo -> "Modulo"
  | Equals -> "Equals"
  | LessThan -> "LessThan" ;;


(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with 
  | Var v -> "Var \"" ^ v ^ "\""
  | Num i -> "Num " ^ (string_of_int i)
  | Float f -> "Float " ^ (string_of_float f)
  | Bool b -> "Bool " ^ (string_of_bool b)
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | Unop (un, e) -> 
    "Unop (" ^ (to_string_unop_abstract un) ^ ", " ^ (exp_to_abstract_string e) ^ ")"
  | Binop (bi, e1, e2) ->
    "Binop (" ^ (to_string_binop_abstract bi) ^ ", " ^ (exp_to_abstract_string e1) 
    ^ ", " ^ (exp_to_abstract_string e2) ^ ")"
  | Conditional (e1, e2, e3) ->
    "Conditional (" ^ (exp_to_abstract_string e1) ^ ", " ^
    (exp_to_abstract_string e2) ^ ", " ^ (exp_to_abstract_string e3) ^ ")"
  | Fun (v, e) -> "Fun (\"" ^ v ^ "\", " ^ (exp_to_abstract_string e) ^ ")"
  | Let (v, e1, e2) ->
    "Let (\"" ^ v ^ "\", " ^
    (exp_to_abstract_string e1) ^ ", " ^ (exp_to_abstract_string e2) ^ ")"
  | Letrec (v, e1, e2) ->
    "Letrec (\"" ^ v ^ "\", " ^
    (exp_to_abstract_string e1) ^ ", " ^ (exp_to_abstract_string e2) ^ ")" 
  | App (e1, e2) ->
    "App (" ^ (exp_to_abstract_string e1) ^ 
    ", " ^ (exp_to_abstract_string e2) ^ ")";;

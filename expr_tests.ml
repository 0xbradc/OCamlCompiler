open CS51Utils ;;
open Absbook ;;
(* Make expr solution available for testing *)
open Expr ;;
(* Make evaluation solution available for testing *)
open Evaluation ;;


(* IS THERE A WAY TO IMPORT THIS? *)
(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;
type varidset = SS.t ;;



let test_exp_to_concrete_string () =
    unit_test ((exp_to_concrete_string (Var "x")) = "x") "Var x" ;
    unit_test ((exp_to_concrete_string (Num 1)) = "1") "Num 1" ;
    unit_test ((exp_to_concrete_string (Bool true)) = "true") "Bool true" ;
    unit_test ((exp_to_concrete_string Raise) = "Raise") "Raise" ;
    unit_test ((exp_to_concrete_string Unassigned) = "Unassigned") "Unassigned" ;
    unit_test ((exp_to_concrete_string (Unop(Negate, Num 1))) = "~-1") "Unop (Negate, (Num 1))" ;
    unit_test ((exp_to_concrete_string (Binop(Plus, Num 1, Num 2))) = "(1 + 2)") 
        "Binop (Plus (Num 1, Num 2))" ;
    unit_test ((exp_to_concrete_string (Conditional(Bool true, Num 1, Num 2)) = "if true then 1 else 2")) 
        "Conditional (Bool true, Num 1, Num 2)" ;
    unit_test ((exp_to_concrete_string (Fun("x", Num 1))) = "fun x -> 1") "Fun (x, Num 1)" ;
    unit_test ((exp_to_concrete_string (Let("x", Num 1, Num 2))) = "let x = 1 in 2") 
        "Let(x, Num 1, Num 2)" ;
    unit_test ((exp_to_concrete_string (Letrec("x", Num 1, Num 2))) = "let rec x = 1 in 2") 
        "Letrec(x, Num 1, Num 2)" ;;

  
let test_exp_to_abstract_string () =
    unit_test ((exp_to_abstract_string (Var "x")) = "Var x") "Var x" ;
    unit_test ((exp_to_abstract_string (Num 1)) = "Num 1") "Num 1" ;
    unit_test ((exp_to_abstract_string (Bool true)) = "Bool true") "Bool true" ;
    unit_test ((exp_to_abstract_string Raise) = "Raise") "Raise" ;
    unit_test ((exp_to_abstract_string Unassigned) = "Unassigned") "Unassigned" ;
    unit_test ((exp_to_abstract_string (Unop (Negate, Num 1))) = "Unop (Negate (Num 1))") 
        "Unop (Negate (Num 1))" ;
    unit_test ((exp_to_abstract_string (Binop (Plus, Num 1, Num 2))) = 
        "Binop (Plus (Num 1, Num 2))") "Binop (Plus (Num 1, Num 2))" ;
    unit_test ((exp_to_abstract_string (Conditional (Bool true, Num 1, Num 2)) = 
        "Conditional (Bool true, Num 1, Num 2)")) "Conditional (Bool true, Num 1, Num 2)" ;
    unit_test ((exp_to_abstract_string (Fun ("x", Num 1))) = "Fun (x, Num 1)") 
        "Fun (\"x\", Num 1)" ;
    unit_test ((exp_to_abstract_string (Let ("x", Num 1, Num 2))) = "Let (x, Num 1, Num 2)") 
        "Let (\"x\", Num 1, Num 2)" ;
    unit_test ((exp_to_abstract_string (Letrec ("x", Num 1, Num 2))) = "Letrec (x, Num 1, Num 2)") 
        "Letrec (\"x\", Num 1, Num 2)" ;;


let test_free_vars () =
    unit_test ((free_vars (Num 1)) = SS.empty) "Num 1" ;
    unit_test ((free_vars (Bool true)) = SS.empty) "Bool true" ;
    unit_test ((free_vars (Raise)) = SS.empty) "Raise" ;
    unit_test ((free_vars (Unassigned)) = SS.empty) "Unassigned" ;
    unit_test ((free_vars (Var "x")) = SS.singleton "x") "Var x" ;;


let test_subst () = 
    unit_test (true) "blank" ;;


let tests () = 
    print_string "exp_to_concrete_string tests\n";
    test_exp_to_concrete_string () ;
    print_newline () ;
    print_string "\nexp_to_abstract_string tests\n";
    test_exp_to_abstract_string () ;
    print_string "\nfree_vars tests\n";
    test_free_vars () ;
    print_string "\nsubst tests\n";
    test_subst () ;
    print_newline ();
    () ;;

let _ = tests () ;;
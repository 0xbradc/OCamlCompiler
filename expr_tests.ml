open CS51Utils ;;
open Absbook ;;
(* Make expr solution available for testing *)
open Expr ;;
(* Make evaluation solution available for testing *)
open Evaluation ;;


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
    unit_test ((exp_to_concrete_string (Fun("x", Num 1))) = "(fun x -> 1)") "Fun (x, Num 1)" ;
    unit_test ((exp_to_concrete_string (Let("x", Num 1, Num 2))) = "let x = 1 in 2") 
        "Let(x, Num 1, Num 2)" ;
    unit_test ((exp_to_concrete_string (Letrec("x", Num 1, Num 2))) = "let rec x = 1 in 2") 
        "Letrec(x, Num 1, Num 2)" ;
    unit_test ((exp_to_concrete_string (App ((Fun ("x", Var "x")), Num 2))) = "(fun x -> x) 2") 
        "Applying f to 2" ;;

  
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
        "Letrec (\"x\", Num 1, Num 2)" ;
    unit_test ((exp_to_abstract_string (App (Var "x", Num 2))) = "App (Var x, Num 2)") 
        "App (Var x, Num 2)" ;;


let test_new_varname () = 
    unit_test ((new_varname ()) = "var0") "var0" ;
    unit_test ((new_varname ()) = "var1") "var1" ;
    unit_test ((new_varname ()) = "var2") "var2" ;
    unit_test ((new_varname ()) = "var3") "var3" ;;


let test_subst () = 
    unit_test ((subst "x" (Var "y") (Var "x")) = (Var "y")) "Var x -> Var y" ;
    unit_test ((subst "x" (Num 1) (Num 1)) = (Num 1)) "Num 1" ;
    unit_test ((subst "x" (Bool true) (Bool true)) = (Bool true)) "Bool true" ;
    unit_test ((subst "x" Raise Raise) = Raise) "Raise" ;
    unit_test ((subst "x" Unassigned Unassigned) = Unassigned) "Unassigned" ;
    unit_test ((subst "x" (Var "y") (Unop (Negate, Var "x"))) = (Unop (Negate, Var "y")))
        "(Unop (Negate, Var x)) -> (Unop (Negate, Var y))" ;
    unit_test ((subst "x" (Var "y") (Binop (Plus, Num 1, Var "x"))) = (Binop (Plus, Num 1, Var "y")))
        "Binop (Plus, Num 1, Num 2))" ;
    unit_test ((subst "x" (Var "z") (Conditional (Bool true, Var "x", Var "y"))) = 
        (Conditional (Bool true, Var "z", Var "y"))) "Conditional (Bool true, Var z, Var y)" ;
    unit_test ((subst "x" (Var "x") (Fun ("x", Num 1))) = (Var "x"))
        "Fun (\"x\", Num 1)" ;
    unit_test ((subst "x" (Var "x") (Let ("x", Num 1, Num 2))) = (Var "x"))
        "Let (\"x\", Num 1, Num 2)" ;
    unit_test ((subst "x" (Var "x") (Letrec ("x", Num 1, Num 2))) = (Var "x"))
        "Letrec (\"x\", Num 1, Num 2)" ;
    unit_test ((subst "x" (Var "z") (App (Var "x", Var "y"))) = (App (Var "z", Var "y")))
        "App (Var \"x\", Var \"y\")" ;;


let test_eval_s () = 
    (* Using try-with because these are supposed to throw errors/exceptions *)
    try 
        let _ = eval_s (Var "y") (Env.empty ()) in 
        print_string "Var x is unbound FAILED\n" ;
    with | EvalError _ -> print_string "Var x is unbound passed\n" ;
    try 
        let _ = eval_s Raise (Env.empty ()) in
        print_string "Raise FAILED\n" ;
    with | EvalException -> print_string "Raise passed\n" ;
    try 
        let _ = eval_s Unassigned (Env.empty ()) in
        print_string "Unassigned FAILED\n" ;
    with | EvalException -> print_string "Unassigned passed\n";

    unit_test ((eval_s (Num 1) (Env.empty ())) = (Val (Num 1))) "Num 1" ;
    unit_test ((eval_s (Bool true) (Env.empty ())) = (Val (Bool true))) "Bool true" ;
    unit_test ((eval_s (Unop (Negate, Num 1)) (Env.empty ())) = (Val (Num ~-1)))
        "(Unop (Negate, Var x)) -> (Val (Num -1))" ;
    unit_test ((eval_s (Binop (Plus, Num 1, Num 10)) (Env.empty ())) = (Val (Num 11)))
        "Binop (Plus, Num 1, Num 10)) -> (Val (Num 11))" ;
    unit_test ((eval_s (Conditional (Bool true, Num 1, Num 2)) (Env.empty ())) = 
        (Val (Num 1))) "Conditional (Bool true, Var z, Var y)" ;
    unit_test ((eval_s (Fun ("x", Num 1)) (Env.empty ())) = (Val (Fun ("x", Num 1))))
        "Fun (\"x\", Num 1)" ;
    unit_test ((eval_s (Let ("x", Num 1, Var "x")) (Env.empty ())) = (Val (Num 1)))
        "Let (\"x\", Num 1, Num 2)" ;
    unit_test ((eval_s (Letrec ("x", Num 1, Var "x")) (Env.empty ())) = (Val (Num 1))) 
        "Letrec (\"x\", Binop (Minus, Var \"x\", Num 1), Num 2))" ;
    unit_test ((eval_s (App (Fun ("x", (Binop (Plus, Var "x", Num 4))), Num 4)) (Env.empty ())) 
        = (Val (Num 8))) "App (Fun (\"x\", Binop (Plus, Var \"x\", Num 4)), 4)" ;;


let tests () = 
    print_string "exp_to_concrete_string tests\n";
    test_exp_to_concrete_string () ;
    print_newline () ;
    print_string "\nexp_to_abstract_string tests\n";
    test_exp_to_abstract_string () ;
    print_string "\nnew_varname tests\n" ;
    test_new_varname () ;
    print_string "\nsubst tests\n";
    test_subst () ;
    print_string "\ntest_eval_s\n";
    test_eval_s () ;
    print_newline () ;
    () ;;

let _ = tests () ;;
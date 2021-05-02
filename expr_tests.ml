open CS51Utils ;;
open Absbook ;;
(* Make expr solution available for testing *)
open Expr ;;
(* Make evaluation solution available for testing *)
open Evaluation ;;

open Miniml ;;


(* Used for more efficient testing below *)
let test_lists (f : 'a -> 'b) (tester_lst : expr list) (answer_lst : 'b list) = 
    try List.iter2 
        (fun elem1 elem2 -> 
            unit_test ((f elem1) = elem2) (exp_to_abstract_string elem1)
        )
        tester_lst 
        answer_lst 
    with _ -> raise (EvalError "test list lengths do not match") ;;


let test_exp_to_concrete_string () =
    let tester_lst = [
        (Var "x");
        (Num 1);
        (Float 1.);
        (Bool true);
        (Raise);
        (Unassigned);
        (Unop (Negate, Num 1));
        (Binop(Plus, Num 1, Num 2));
        (Conditional(Bool true, Num 1, Num 2));
        (Fun ("x", Num 1));
        (Let("x", Num 1, Num 2));
        (Letrec("x", Num 1, Num 2));
        (App ((Fun ("x", Var "x")), Num 2))
    ] in 
    let answer_lst = [
        "x";
        "1";
        "1.";
        "true";
        "Raise";
        "Unassigned";
        "~-1";
        "(1 + 2)";
        "if true then 1 else 2";
        "(fun x -> 1)";
        "let x = 1 in 2";
        "let rec x = 1 in 2";
        "(fun x -> x) 2"
    ] in 
    test_lists exp_to_concrete_string tester_lst answer_lst ;;

  
let test_exp_to_abstract_string () =
    let tester_lst = [
        (Var "x");
        (Num 1);
        (Float 1.);
        (Bool true);
        (Raise);
        (Unassigned);
        (Unop (Negate, Num 1));
        (Binop(Plus, Num 1, Num 2));
        (Conditional(Bool true, Num 1, Num 2));
        (Fun ("x", Num 1));
        (Let("x", Num 1, Num 2));
        (Letrec("x", Num 1, Num 2));
        (App (Var "x", Num 2))
    ] in 
    let answer_lst = [
        "Var \"x\"";
        "Num 1";
        "Float 1.";
        "Bool true";
        "Raise";
        "Unassigned";
        "Unop (Negate, Num 1)";
        "Binop (Plus, Num 1, Num 2)";
        "Conditional (Bool true, Num 1, Num 2)";
        "Fun (\"x\", Num 1)";
        "Let (\"x\", Num 1, Num 2)";
        "Letrec (\"x\", Num 1, Num 2)";
        "App (Var \"x\", Num 2)"
    ] in 
    test_lists exp_to_abstract_string tester_lst answer_lst ;;


let test_new_varname () = 
    unit_test ((new_varname ()) = "var0") "var0" ;
    unit_test ((new_varname ()) = "var1") "var1" ;
    unit_test ((new_varname ()) = "var2") "var2" ;
    unit_test ((new_varname ()) = "var3") "var3" ;;


let test_free_vars () = 
    let tester_lst = [
        (Var "x");
        (Num 1);
        (Float 1.);
        (Bool true);
        (Raise);
        (Unassigned);
        (Unop (Negate, Var "x"));
        (Binop (Plus, Var "x", Var "y"));
        (Conditional (Var "x", Num 1, Num 2));
        (Fun ("x", Num 1));
        (Let ("x", Num 1, Var "x"));
        (Let ("x", Var "x", Num 2));
        (Let ("x", Num 1, Num 2));
        (Let ("x", Var "x", Var "x"));
        (Letrec ("x", Num 1, Var "x"));
        (Letrec ("x", Var "x", Num 2));
        (Letrec ("x", Num 1, Num 2));
        (App (Var "x", Num 1));
        (App (Num 1, Num 2));
        (str_to_exp "let f = fun x -> fun y -> if x < y then y else f y x in f 0 1 ;;")
    ]
    in 
    let answer_lst = 
        List.map vars_of_list [
            ["x"];
            [];
            [];
            [];
            [];
            [];
            ["x"];
            ["x"; "y"];
            ["x"];
            [];
            [];
            ["x"];
            [];
            ["x"];
            [];
            [];
            [];
            ["x"];
            [];
            ["f"]
        ] 
    in 
    test_lists free_vars tester_lst answer_lst ;;


let test_subst () =
    let tester_lst = [
        ((Var "y"), (Var "x"));
        ((Num 1), (Num 1));
        ((Float 1.), (Float 1.));
        ((Bool true), (Bool true));
        (Unassigned, Unassigned);
        (Raise, Raise);
        ((Var "y"), (Unop (Negate, Var "x")));
        ((Var "y"), (Binop (Plus, Num 1, Var "x")));
        ((Var "z"), (Conditional (Bool true, Var "x", Var "y")));
        ((Var "z"), (Fun ("x", Var "y"))); (* 1st fun if statement *)
        ((Var "y"), (Fun ("y", Var "y"))); (* 2nd fun if statement *)
        ((Var "z"), (Fun ("y", Var "x"))); (* 3rd fun if statement *)
        ((Var "y"), (Let ("x", Var "x", Var "x"))); (* 1st let if statement *)
        ((Var "y"), (Let ("y", Var "y", Var "y"))); (* 2nd let if statement *)
        ((Var "z"), (Let ("y", Var "x", Var "x"))); (* 3rd let if statement *)
        ((Var "y"), (Letrec ("x", Var "x", Var "x"))); (* 1st letrec if statement *)
        ((Var "y"), (Letrec ("y", Var "y", Var "y"))); (* 2nd letrec if statement *)
        ((Var "z"), (Letrec ("y", Var "x", Var "x"))); (* 3rd letrec if statement *)
        ((Var "z"), (App (Var "x", Var "y")))
    ] in 
    let answer_lst = [
        (Var "y");
        (Num 1);
        (Float 1.);
        (Bool true);
        Unassigned;
        Raise;
        (Unop (Negate, Var "y"));
        (Binop (Plus, Num 1, Var "y"));
        (Conditional (Bool true, Var "z", Var "y"));
        (Fun ("x", Var "y")); (* 1st fun if statement *)
        (Fun ("var4", Var "var4")); (* 2nd fun if statement *)
        (Fun ("y", Var "z")); (* 3rd fun if statement *)
        (Let ("x", Var "y", Var "x")); (* 1st let if statement *)
        (Let ("var5", Var "y", Var "var5")); (* 2nd let if statement *)
        (Let ("y", Var "z", Var "z")); (* 3rd let if statement *)
        (Letrec ("x", Var "y", Var "x")); (* 1st letrec if statement *)
        (Letrec ("var6", Var "y", Var "y")); (* 2ndrec let if statement *)
        (Letrec ("y", Var "z", Var "z")); (* 3rd letrec if statement *)
        (App (Var "z", Var "y"))
    ] in 
    try List.iter2 
        (fun (elem1a, elem1b) elem2 -> 
            unit_test 
                ((subst "x" elem1a elem1b) = elem2) 
                ((exp_to_abstract_string elem1b) ^ "\n --> " ^ (exp_to_abstract_string elem2))
        )
        tester_lst 
        answer_lst 
    with _ -> raise (EvalError "test list lengths do not match") ;;


let test_eval_s () = 
    (* Using try-with because these are supposed to throw errors/exceptions *)
    try let _ = eval_s (Var "x") (Env.empty ()) in print_string "Var is unbound FAILED\n" 
    with _ -> print_string "Var unbound passed\n" ;
    try let _ = eval_s Raise (Env.empty ()) in print_string "Raise FAILED\n" 
    with _ -> print_string "Raise passed\n" ;
    try let _ = eval_s Unassigned (Env.empty ()) in print_string "Unassigned FAILED\n" 
    with _ -> print_string "Unassigned passed\n";

    (* For these tests, I utilized the str_to_exp function to emulate command line input *)
    let tester_lst = [
        ("1 ;;", (Env.empty ()));
        ("1. ;;", (Env.empty ()));
        ("true ;;", (Env.empty ()));
        ("~-1 ;;", (Env.empty ()));
        ("1 + 10 ;;", (Env.empty ()));
        ("if true then 42 else 21 ;;", (Env.empty ()));
        ("fun x -> 1 ;;", (Env.empty ()));
        ("let x = 2 in x ;;", (Env.empty ()));
        ("let rec x = 3 in x ;;", (Env.empty ()));
        ("let rec f = fun x -> if x = 0 then 1 else x * f (x - 1) in f 3 ;;", (Env.empty ()));
        (* ("fun x -> x + 4 in 4 ;;", (Env.empty ())); *)
        ("let x = 1 in let f = fun y -> x + y in let x = 2 in f 3 ;;", (Env.empty ()));
        ("42 mod 10 ;;", (Env.empty ()))
    ] in 
    let answer_lst = [
        (Num 1);
        (Float 1.);
        (Bool true);
        (Num ~-1);
        (Num 11);
        (Num 42);
        (Fun ("x", Num 1));
        (Num 2);
        (Num 3);
        (Num 6);
        (* (Num 8); *)
        (Num 4);
        (Num 2)
    ] in 
    try List.iter2 
        (fun (elem1a, elem1b) elem2 -> 
            unit_test 
                ((eval_s (str_to_exp elem1a) elem1b) = (Env.Val elem2)) 
                (elem1a ^ "\n --> " ^ (exp_to_concrete_string elem2))
        )
        tester_lst 
        answer_lst 
    with _ -> raise (EvalError "eval_s failed for some reason") ;;


let test_eval_d () = 
    (* Using try-with because these are supposed to throw errors/exceptions *)
    try let _ = eval_d (Var "x") (Env.empty ()) in print_string "Var is unbound FAILED\n" 
    with _ -> print_string "Var unbound passed\n" ;
    try let _ = eval_d Raise (Env.empty ()) in print_string "Raise FAILED\n" 
    with _ -> print_string "Raise passed\n" ;
    try let _ = eval_d Unassigned (Env.empty ()) in print_string "Unassigned FAILED\n" 
    with _ -> print_string "Unassigned passed\n";

    let tester_lst = [
        ("1 ;;", (Env.empty ()));
        ("1. ;;", (Env.empty ()));
        ("true ;;", (Env.empty ()));
        ("~-1 ;;", (Env.empty ()));
        ("1 + 10 ;;", (Env.empty ()));
        ("if true then 42 else 21 ;;", (Env.empty ()));
        ("fun x -> 1 ;;", (Env.empty ()));
        ("let x = 2 in x ;;", (Env.empty ()));
        ("let rec x = 3 in x ;;", (Env.empty ()));
        ("let rec f = fun x -> if x = 0 then 1 else x * f (x - 1) in f 3 ;;", (Env.empty ()));
        (* ("fun x -> x + 4 in 4 ;;", (Env.empty ())); *)
        ("let x = 1 in let f = fun y -> x + y in let x = 2 in f 3 ;;", (Env.empty ()));
        ("42 mod 10 ;;", (Env.empty ()))
    ] in 
    let answer_lst = [
        (Num 1);
        (Float 1.);
        (Bool true);
        (Num ~-1);
        (Num 11);
        (Num 42);
        (Fun ("x", Num 1));
        (Num 2);
        (Num 3);
        (Num 6);
        (* (Num 8); *)
        (Num 5);
        (Num 2)
    ] in 
    try List.iter2 
        (fun (elem1a, elem1b) elem2 -> 
            unit_test 
                ((eval_d (str_to_exp elem1a) elem1b) = (Env.Val elem2))
                (elem1a ^ "\n --> " ^ (exp_to_concrete_string elem2))
        )
        tester_lst 
        answer_lst 
    with _ -> raise (EvalError "eval_d failed for some reason") ;;


let test_eval_l () = 
    (* Using try-with because these are supposed to throw errors/exceptions *)
    try let _ = eval_l (Var "x") (Env.empty ()) in print_string "Var is unbound FAILED\n" 
    with _ -> print_string "Var unbound passed\n" ;
    try let _ = eval_l Raise (Env.empty ()) in print_string "Raise FAILED\n" 
    with _ -> print_string "Raise passed\n" ;
    try let _ = eval_l Unassigned (Env.empty ()) in print_string "Unassigned FAILED\n" 
    with _ -> print_string "Unassigned passed\n";

    let tester_lst = [
        ("1 ;;", (Env.empty ()));
        ("1. ;;", (Env.empty ()));
        ("true ;;", (Env.empty ()));
        ("~-1 ;;", (Env.empty ()));
        ("1 + 10 ;;", (Env.empty ()));
        ("if true then 42 else 21 ;;", (Env.empty ()));
        ("fun x -> 1 ;;", (Env.empty ()));
        ("let x = 2 in x ;;", (Env.empty ()));
        ("let rec x = 3 in x ;;", (Env.empty ()));
        ("let rec f = fun x -> if x = 0 then 1 else x * f (x - 1) in f 3 ;;", (Env.empty ()));
        (* ("fun x -> x + 4 in 4 ;;", (Env.empty ())); *)
        ("let x = 1 in let f = fun y -> x + y in let x = 2 in f 3 ;;", (Env.empty ()));
        ("42 mod 10 ;;", (Env.empty ()))
    ] in 
    let answer_lst = [
        (Num 1);
        (Float 1.);
        (Bool true);
        (Num ~-1);
        (Num 11);
        (Num 42);
        (Fun ("x", Num 1));
        (Num 2);
        (Num 3);
        (Num 6);
        (* (Num 8); *)
        (Num 4);
        (Num 2)
    ] in 
    try List.iter2 
        (fun (elem1a, elem1b) elem2 -> 
            unit_test 
                ((eval_l (str_to_exp elem1a) elem1b) = (Env.Val elem2))
                (elem1a ^ "\n --> " ^ (exp_to_concrete_string elem2))
        )
        tester_lst 
        answer_lst 
    with _ -> raise (EvalError "eval_l failed for some reason") ;;


let tests () = 
    print_string "\nexp_to_concrete_string tests\n";
    test_exp_to_concrete_string () ;
    print_newline () ;
    print_string "\nexp_to_abstract_string tests\n";
    test_exp_to_abstract_string () ;
    print_string "\nnew_varname tests\n" ;
    test_new_varname () ;
    print_string "\nfree_vars tests\n" ;
    test_free_vars () ;
    print_string "\nsubst tests\n" ;
    test_subst () ;
    print_string "\neval_s tests\n" ;
    test_eval_s () ;
    print_string "\neval_d tests\n" ;
    test_eval_d () ;
    print_string "\neval_l tests\n" ;
    test_eval_l () ;
    print_newline () ;
    () ;;

let _ = tests () ;;
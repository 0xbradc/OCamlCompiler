(* This file was created to store the alternative user-friendly approach I thought of.
   Refer to section 10 "User Friendly Extension" for more information on this approach. *)


let evaluate = 
  let preference = 
      print_string (
        "\n\nPlease enter a valid semantics model and press enter:\n" ^
        "\"s\" for substitution, \"d\" for dynamic, \"l\" for lexical. \n" ^ 
        "(invalid inputs will result in Substitution being chosen) \n"
      ); 
      read_line ()
  in 
  if preference = "d" then (curr_mod := Dynamic; print_string "\nDYNAMIC MODEL\n\n")
  else if preference = "l" then (curr_mod := Lexical; print_string "\nLEXICAL MODEL\n\n")
  else print_string "\nSUBSTITUTION MODEL\n\n" ;
  eval_uni ;;
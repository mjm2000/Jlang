open Jumplang 
open Parser
open Lexer 
open Intr
open Printf

let values= read_j_file (Array.get Sys.argv 1) |> get_raw_lexes |> extract_lexems |> parser_f  
let () =

    let (assings,_,_) =  values in

    let rec print_assign assign_list = 
        match assign_list with
        |[] -> ();
        |assign::[] -> printf "main:\n"; assign_to_intr stdout assign
        |assign::xs -> 
                assign_to_intr stdout assign; print_assign xs
       
    in
    print_assign assings

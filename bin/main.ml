open Jumplang 
open Parser
open Lexer 
open Intr

let () =

    let (stmts,_,_) = read_j_file (Array.get Sys.argv 1) 
    |> get_raw_lexes 
    |> extract_lexems 
    |> parser_f in

    stmts
    |> stmt_to_intr 
  (*|> print_intrs stdout *)
    |> Assem.intr_to_x64
    |> Assem.print_x64 stdout 

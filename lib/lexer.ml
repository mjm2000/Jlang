
let  read_j_file  file = 
    let ic = open_in file in
    let rec read_j_file_ ls =
        try 
            let char1 = input_char ic in 
            read_j_file_ (char1::ls)                  
        with
        | End_of_file  ->  ls
    in

    List.rev (read_j_file_ [])

exception Str of string 
exception Char of string
(*redo*)
let get_next_lex ls  =
    let rec get_next_lexem ls  buffer s_quote d_quote = 
(*        
        (match (ls) with 
            |x::xs ->Printf.printf "%c,%b\n" x  d_quote  ;    
            |_-> ()
        );
*)
        match ls,buffer with
        |('\'' as x::xs),[] when not s_quote  ->  
              get_next_lexem xs (x::buffer) true d_quote
    
        |('\'' as x::xs),_ when s_quote ->  
              xs,(x::buffer)  
        |('\"' as x::xs),[] when not d_quote  ->  
              get_next_lexem xs (x::buffer) s_quote true
    
        |('\"' as x::xs),_ when d_quote ->  
               xs, (x::buffer)
        |([],_)when (d_quote ) -> 
                raise (Str "quote non matched" )
        |(x::xs,_)when (d_quote ) -> 
            get_next_lexem xs (x::buffer) s_quote d_quote
        |([],_)when (s_quote ) -> 
                raise (Char "quote non matched" )
        |(x::xs,_)when (s_quote ) -> 
            get_next_lexem xs (x::buffer) s_quote d_quote
        |' '::xs,[] ->
            get_next_lexem xs buffer d_quote s_quote   
        |' '::xs,_ ->
               xs, buffer
        |'{'::xs,[] ->
                xs,['{']
        |'{'::_,_ ->  
                ls, buffer
        |'}'::xs,[] ->
                xs,['}']
        |'}'::_,_ ->  
                ls, buffer
        |'.'::xs,[] ->
                xs,['.']
        |'.'::_,_ ->  
                ls, buffer
        |'('::xs,[] ->
                xs,['(']
        |'('::_,_ ->  
                ls, buffer
        |')'::xs,[] ->
                xs,[')']
        |')'::_,_ ->  
                ls, buffer
        |'>'::'='::xs,[] ->
                xs,['=';'>']
        |'>'::'='::_,_ ->  
                ls, buffer
        |'<'::'='::xs,[] ->
                xs,['=';'<']
        |'<'::'='::_,_ ->  
                ls, buffer
        |'|'::'>'::xs,[] ->
                xs,['>';'|']
        |'|'::'>'::_,_ ->  
                ls, buffer
        |'<'::'|'::xs,[] ->
                xs,['|';'<']
        |'<'::'|'::_,_ ->  
                ls, buffer
        |':'::'='::xs,[] ->
                xs,['=';':']
        |':'::'='::_,_ ->  
                ls, buffer
        |'<'::xs,[] ->
                xs,['<']
        |'<'::_,_ ->  
                ls, buffer
        |'>'::xs,[] ->
                xs,['>']
        |'>'::_,_ ->  
                ls, buffer
                
        |'+'::xs,[] ->
                xs,['+']
        |'+'::_,_ ->  
                ls, buffer
        |'='::xs,[] ->
                xs,['=']
        |'='::_,_ ->  
                ls, buffer
        |';'::xs,[] ->
                xs,[';']
        |';'::_,_ ->  
                ls, buffer
        |'d'::'e'::'f'::xs,[] ->
                xs, ['f';'e';'d']
        |'d'::'e'::'f'::_,_ ->
                ls, buffer
        |'e'::'n'::'d'::xs,[]->
            xs, ['d';'n';'e']
        |'e'::'n'::'d'::_,_ ->
                ls, buffer

        |('\n'::xs,[]) -> 
                xs, ['\n'] 
        |('\n'::_,_) -> 
                ls, buffer
        |(x::xs,_) -> 
               get_next_lexem xs (x::buffer) d_quote s_quote 
        | [],_ ->  ls, buffer
    in
    let stream,buf= (get_next_lexem ls [] false false) in
    stream, String.of_seq (List.to_seq(List.rev buf))

type tokens = 
    | IF  | LPARAM | RPARAM | COMMA | LBRAC | RBRAC  | RPIPE | LPIPE  
    | ARROW | ASSIGN   
    | LEQ | GEQ | EQ | LT | GT 
    | PLUS | MINUS | SLASH | TIMES
    | SEMICOLON | NEWLINE
    | DEF | END
    | FLOAT of float | INTEGER of int | STRING of string | IDEN of string | CHAR of char 

        

let match_reg reg str= Str.string_match (Str.regexp reg)  str 0 
let typify str = 
        match  (str) with
            | "if" ->  IF
            | "end" -> END
            | "("  ->  LPARAM
            | ")"  ->  RPARAM
            | "{"  ->  LBRAC
            | "}"  ->  RBRAC
            | "=>" ->  ARROW
            | ">=" ->  GEQ
            | "=" ->  EQ
            | "<=" ->  LEQ
            | ">"  ->  GT
            | "<"  ->  LT 
            | ":=" -> ASSIGN
            |"|>" -> RPIPE
            |"<|" -> LPIPE
            |"+" -> PLUS
            |"-" -> MINUS
            |"\\" -> SLASH
            |"*" -> TIMES 
            |"," -> COMMA
            |";" -> SEMICOLON
            |"def" -> DEF
            |"\n" -> NEWLINE
            |int_value when match_reg "^[0-9]+$" int_value -> 
                
                INTEGER ( Stdlib.int_of_string int_value)

            |float_value when match_reg "^[0-9]+\\.[0-9]+$" float_value -> 
                FLOAT (Stdlib.float_of_string float_value)
            |string_value when match_reg "^\".*\"$" string_value ->
                STRING (Str.replace_first (Str.regexp "\"\\(.*\\)\"" ) "\\1" string_value)

            |string_value when match_reg "^\'.\'$" string_value ->
                CHAR (String.get  (Str.replace_first (Str.regexp "\'\\(.\\)\'" ) "\\1" string_value) 0)
            | id_value -> IDEN(id_value) 

let stringify token = 
    match(token) with
        |IF      ->  "if"
        |LPARAM  ->  "(" 
        |RPARAM  ->  ")" 
        |LBRAC   ->  "{" 
        |RBRAC   ->  "}" 
        |ARROW   ->  "=>"
        |GEQ     ->  ">="
        |LEQ     ->  "<="
        |GT      ->  ">" 
        |LT      ->  "<" 
        |RPIPE   ->  "|>" 
        |LPIPE   ->  "<|" 
        |EQ      ->  "="
        |COMMA -> ","
        |ASSIGN -> ":="
        |PLUS -> "+"
        |MINUS -> "-"
        |SLASH -> "\\"
        |TIMES -> "*"
        |NEWLINE -> "newline"
        |SEMICOLON -> ";"
        |DEF -> "def"
        |END -> "end"
        |INTEGER(v) -> string_of_int v
        |FLOAT(v) -> string_of_float v
        |STRING(v) -> Printf.sprintf "\"%s\"" v 
        |CHAR(v) -> Printf.sprintf "%c" v
        |IDEN(v) -> v


let get_raw_lexes char_list = 
    let rec recur_lex ch_ls buf = 
        match (get_next_lex ch_ls) with 
        |([],end_val) -> end_val::buf
        |(stream,lex) -> recur_lex stream (lex::buf)
    in

    List.rev (recur_lex char_list [])
    

let extract_lexems str_list = List.map typify str_list 
let print_token x =  Printf.printf "%s;\n" (stringify x);()
 

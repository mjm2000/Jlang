
open Lexer
open Printf
exception OP of string

type op_type = ADD | DIV | SUB | MULT
type expr_value = Iden_Val of string | Int_Val of int | String_Val of string | Float_Val of float | Char_Val  of char 

type expr = OP of expr * op_type * expr 
            | FUNC_CALL of string * expr list
            | VALUE of expr_value 
            | ERROR

module VarMap = Map.Make(String)
type var_type = |Iden of string | Int | String | Float | Char  | Any
                

type symbol_entry = 
                    |Table of symbol_entry VarMap.t
                    |Variable of var_type
                    |Type of var_type list
                    |Function of var_type list * var_type *  symbol_entry VarMap.t 

type stmt = FUNC of string * string list * stmt list * expr
            | ASSIGN of string * expr

let rec sizeof value sym_tbl = match value with  
| Int_Val(_)  -> 4
| Float_Val(_) -> 4 
| Char_Val(_)  -> 1
| String_Val(s) -> String.length s
| Iden_Val(iden) ->
    let rec iden_size id = 
    match (VarMap.find id sym_tbl) with 
    |Variable(v) -> (match v with 
        |Iden(i) -> iden_size i 
        |Int -> 4 
        |String -> 100
        |Float -> 8
        |Char -> 1 
        |Any -> 8
        )
    |Type(ls) -> List.fold_left (fun  total x -> total + (iden_size x)) ls 0 
    |_ -> raise (OP "value")
    in
    iden_size iden 



let stringify_value value= match value with 
    Iden_Val (x)-> sprintf "%s" x
    | Int_Val(x)-> sprintf "%i" x
    | String_Val (x)-> sprintf "%s" x
    | Float_Val (x)-> sprintf "%f" x
    | Char_Val  (x)-> sprintf "%c" x
 
let print_exp  exp = match exp with  
            |OP(_,v,_)  ->
                    (match v with 
                        |ADD  -> printf  "add";
                        |SUB   -> printf "sub";
                        |DIV   -> printf "div";
                        |MULT  -> printf "mult";
                    );
            | FUNC_CALL(_) -> printf "func\n" ;
            | VALUE (value)-> printf "Value(%s)\n" (stringify_value value);
            | ERROR -> printf "error\n";;
let token_lit_to_value token = match token with 
            |IDEN(x)->  Iden_Val (x)
            |INTEGER(x)->  Int_Val(x)
            |STRING(x)->String_Val(x)
            |FLOAT(x)-> Float_Val(x)
            |CHAR(x)-> Char_Val(x)
            |_-> raise (OP "wrong token")

let parser_f terminals_list = 
    let rec expression terms = 
        let arguments terms = 
            let rec argument_r args terms =  match terms with
            |RPARAM::xs -> args,xs  
            |xs -> 
                let expr,rest = expression xs in
                argument_r (expr::args) rest  
            in
            argument_r [] terms
        in
        let factor factor_terms = match factor_terms with 
        |IDEN(v)::LPARAM::xs ->
                let arg_list,rest = arguments xs in
                FUNC_CALL(v,arg_list),rest
        |INTEGER(_) |FLOAT(_)|STRING(_)|CHAR(_)|IDEN(_) as v::xs ->

                (VALUE (token_lit_to_value v)),xs
        |LPARAM::xs -> 
                 (
                 match (expression xs) with 
                 |expr,RPARAM::rest ->expr,rest 
                 |_,rest->ERROR,rest
                  )
        |_::xs->
                ERROR,xs 
        |[] ->ERROR,[]

        in
        
        let term terms =
            let factor_term,factor_rest = factor terms in  

            let rec term_r input_terms output = match input_terms with
                |TIMES::xs-> 
                let factor_value,rest = factor xs in
                    term_r rest (OP(factor_value,MULT,output))
                |SLASH::xs-> 
                let factor_value,rest = factor xs in

                term_r rest (OP(factor_value,DIV,output))
                
                |xs -> 
                    output,xs
            in
            term_r factor_rest factor_term
        in
        

        let rec expression_r terms output=

            match terms with
                |PLUS::xs ->

                    let term_value,rest = term xs in 
                    
                    expression_r rest (OP(term_value,ADD,output) ) 
                |MINUS::xs ->
                    let term_value,rest = term xs in 
                    expression_r rest (OP(term_value,SUB,output) )
               (* |IDEN::LPARAM::xs-> *)
                |xs-> 

                        output,xs
        in
        let starting_term,starting_list = (match terms with
        |PLUS::xs -> term xs 
        |MINUS::xs -> term xs 
        |xs -> 
                term xs )
        in
        expression_r  starting_list starting_term
    in
    let add_symbol iden type_value sym_tbl = match sym_tbl with
    |Table(tbl)->  
            let new_tbl = VarMap.add iden (type_value) tbl in 
            Table(new_tbl)
    |Function(args,return,tbl) -> 
            let new_tbl = VarMap.add iden (type_value) tbl in
            Function(args,return,new_tbl)
    |_-> raise (OP "wrong append")
    in
    let get_table sym_tbl =  match sym_tbl with
            |Table(tbl) -> tbl
            |Function(_,_,tbl) -> tbl
            |_->raise (OP "variable has no symbol table")
    in
    let get_symbol iden sym_tbl = 
        let tbl = match sym_tbl with
            |Table(tbl) -> tbl
            |Function(_,_,tbl) -> tbl
            |_->raise (OP "variable has no symbol table")
        in
        match (VarMap.find_opt iden tbl) with
        |Some(Variable(x)) -> x
        |_->raise (OP "access error")
    in
    let get_func_call iden sym_tbl = 
        let tbl = match sym_tbl with
            |Table(tbl) -> tbl
            |Function(_,_,tbl) -> tbl
            |_->raise (OP "variable has no symbol table")
        in
        match (VarMap.find_opt iden tbl) with
        |Some(Function(args,return,_)) -> 
                args,return
        |_->raise (OP "function doesn't exist")
         
    in
    let rec type_check_expr expr sym_tbl = 
            
    match expr with
        |OP(l,op,r)->
            (match  (type_check_expr l sym_tbl),(type_check_expr r sym_tbl) with
            | (rv,r),(lv,l) when r = l -> (OP(rv,op,lv)),Any
            |(rv,Float),(lv,Int)-> OP(rv,op,(FUNC_CALL("int_to_float",[lv]))),Float
            |(rv,Int),(lv,Float)-> OP((FUNC_CALL("int_to_float",[rv])),op,lv),Float
            | (rv,Any),(lv,_)-> (OP(rv,op,lv)),Any
            | (rv,_),(lv,Any)-> (OP(rv,op,lv)),Any
            |_->raise (OP "wrong comparition")
            )
        |VALUE(v) ->    
            (
            match v with 
            |Int_Val(_) as value -> VALUE(value),Int  
            |Float_Val(_)   as value -> VALUE(value),Float
            |String_Val(_)  as value -> VALUE(value),String
            |Char_Val(_)    as value -> VALUE(value),Char
            |Iden_Val(i)     -> (VALUE(Iden_Val(i))),(get_symbol i sym_tbl)
            )
        |FUNC_CALL(iden,ls)->
            let decl_arg_types,return = get_func_call iden sym_tbl in
            let call_args,call_arg_types = List.fold_left 
            (fun return value -> 
                match ((type_check_expr value sym_tbl),return) with 
                |(expr,type_value),(val_list,type_list) -> (expr::val_list),(type_value::type_list)
            ) ([],[]) ls in
            let rec cmp_args call_args decl_args = 
                match call_args,decl_args with
                |(x)::xs,(y)::ys when x = y ->  cmp_args xs ys 
                |Iden(a)::xs,Iden(b)::ys  when a = b ->  cmp_args xs ys
                |Any::xs,_::ys ->  cmp_args xs ys
                |_::xs,Any::ys ->  cmp_args xs ys
                |[],[] -> true
                |_,_ -> false 
            in
            if (cmp_args decl_arg_types call_arg_types) then 
                FUNC_CALL(iden,call_args),return
            else raise (OP "wrong arg types")
        |ERROR -> raise (OP "error in expression")

    in
    
    let rec assign_list terms statements sym_tbl= 
        match  terms with
        |IDEN(v)::ASSIGN::xs ->
            let expr,rest = expression xs in
            let checked_expr,checked_type = type_check_expr expr sym_tbl in
            let appended_tble = add_symbol v (Variable(checked_type)) sym_tbl in
            assign_list rest ((ASSIGN(v,checked_expr))::statements) appended_tble
        |DEF::IDEN(v)::LPARAM::xs ->
            let rec argument_iden_r output arg_list ls = match ls,output with 
            |IDEN(v)::xs,Function(arg_types,return,f_table)  ->  
                let f = Function((Any::arg_types),return, (VarMap.add v (Variable (Any))  f_table)) in

                argument_iden_r f (v::arg_list) xs 

            |RPARAM::xs,f ->
               f,arg_list,xs
            |_ -> raise (OP "wrong args decl")
            in
            let f,args,rest= argument_iden_r (Function([],Any,(get_table sym_tbl))) [] xs in
            let stmts,rest,func_tbl =  assign_list rest [] f in
            let return_expr,rest = expression rest in
            let return_expr,return_type = type_check_expr return_expr func_tbl in
            (match f with 
            |Function(arg_types,_,func_tbl)->
                let f = Function(arg_types,return_type,func_tbl) in 
                let sym_tbl = add_symbol v f sym_tbl in 
                assign_list rest (FUNC(v, args, stmts,return_expr)::statements) sym_tbl
            |_->( raise (OP "not a func") )
            )    
        |NEWLINE::xs -> assign_list xs statements sym_tbl
        |[] -> statements,[],sym_tbl
        |rest -> (List.rev statements),rest,sym_tbl
    in
    
    (assign_list terminals_list [] (Table VarMap.empty))
    

let stringify_op v = (match v with 
            |ADD ->  "+";
            |SUB ->  "-";
            |DIV ->  "\\";
            |MULT -> "*";
);;
    


open Parser
open Printf
open Lexer

type reg_type = Temp of int | Arg of int  | Return of int | Const of tokens

type intr = |Load of reg_type * reg_type 
            |Bin_Insr of reg_type * reg_type * op_type * reg_type  
            |Call of string
            |Label of string * intr list
            
let rec stmt_to_intr stmts =
    let rec expr_to_intr expr incr = 
    match expr with
    
    |OP(VALUE l,v, VALUE r) ->  
            
        Bin_Insr(Temp(incr),Const(r),v,Const(l))::[]  
    |OP(VALUE l,v,r) ->
        Bin_Insr(Temp(incr),Temp(incr),v,Const(l)) 
        :: (expr_to_intr r (incr))
    |OP(l,v,VALUE r) ->
        Bin_Insr(Temp(incr),Temp(incr),v,Const(r)) 
        :: (expr_to_intr l (incr))
    |OP (l,v,r) -> 

        Bin_Insr(Temp(incr),Temp(incr),v,Temp(incr+1))
        :: (expr_to_intr l (incr)) 
        @  (expr_to_intr r (incr+1))
    |VALUE(v) ->
        Load(Temp(incr),Const(v))::[]

    |FUNC_CALL(iden,expr_list) -> 
        let lst,_= List.fold_left (fun ls_i expr -> 
            let ls,i = ls_i in
            (match (expr) with 
            |VALUE(x) -> Load(Arg(i),Const(x))::ls,i+1
            |any-> ( match (expr_to_intr any  (incr+i)) with
                |Bin_Insr(Temp(cur_i),_,_,_)  ::rest -> 
                        ((Load(Arg(i),Temp(cur_i))):: rest @ ls),(i+1)
                |_->[],i+1
            )
            )    
        ) ([],0) expr_list  in
        Call(iden) :: lst

    |ERROR -> 
            raise (OP "failed to print") 
    in
    let rec stmt_to_intr_r stmts output =
    match (stmts) with 
    |Parser.ASSIGN(k , v) :: xs -> 
        (match (expr_to_intr v 0) with  
        |Bin_Insr(Temp(i),_,_,_)::_ as all ->
            let value = (Load(Const(IDEN(k)),Temp(i))::all) in
            stmt_to_intr_r xs (value @ output) 
        |_ -> (raise (OP "wrong insr"))
        )
            
    |FUNC(name, args , stmts,_)::xs ->

            let args = List.mapi 
                (fun i v -> Load(Const(IDEN(v)), Arg(i)))
                args 
            in
            let intrs = stmt_to_intr stmts in
            stmt_to_intr_r xs (Label(name,args @ intrs)::output)
    |[] -> output
    in
    stmt_to_intr_r stmts []

let rec assign_to_intr file assign =
    let rec expr_to_intr expr incr = 
    (match expr with
    | OP(VALUE l,v, VALUE r) ->  
            

            fprintf file "\tt%i = %s %s %s\n" incr (stringify r) (stringify_op v) (stringify l);

    |OP(VALUE l,v,r) ->

            let _ = expr_to_intr r (incr) in 
            fprintf file "\tt%i = t%i %s %s\n" incr incr (stringify_op v) (stringify l);
    |OP(l,v,VALUE r) ->
        
            let _ = expr_to_intr l (incr) in 
            fprintf file "\tt%i = t%i %s %s\n" incr incr (stringify_op v) (stringify r);
 
    |OP (l,v,r) -> 
        let _ = expr_to_intr l (incr) , expr_to_intr r (incr+1) in
        
        fprintf file "\tt%i = t%i %s t%i\n" incr incr (stringify_op v) (incr+1);
    |VALUE(v) -> 
            fprintf file "\tt%i = %s\n" incr (stringify v); 
    |FUNC_CALL(iden,expr_list) -> 
        List.iteri (fun i expr -> 
            match (expr) with 
            |VALUE(x) -> fprintf file "\tv%i = %s\n"  i (stringify x);
            |any-> fprintf file "\tv%i = t%i\n" i (expr_to_intr any  (incr+i));
                
            ) expr_list;
            Printf.fprintf file "\tcall %s\n" iden;

    |ERROR -> 
            raise (OP "failed to print") );
    incr
    in
    match (assign) with 
    |Parser.ASSIGN(k , v) -> 
            let final_incr = expr_to_intr v 0 in
            Printf.fprintf file "\t%s = t%i\n" k final_incr;
    |FUNC(name, args , stmts,_) ->
            Printf.fprintf file "%s:\n" name;
            List.iteri 
                (fun i v -> Printf.fprintf file "\t%s = v%i\n" v i) 
                args
            ; 
            List.iter  (assign_to_intr file) stmts;
            Printf.fprintf file "\treturn\n\n"
    




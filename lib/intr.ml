open Parser
open Printf
open Lexer

type reg_type = Temp of int | Arg of int  | Return of int | Const of tokens

type intr = |Load of reg_type * reg_type 
            |Bin_Insr of reg_type * reg_type * op_type * reg_type  
            |Call of string
            |Label of string * intr list
            
let rec last_reg stmts = match stmts with
    |Load(Temp(i),_)::_ -> Temp(i+1)
    |Bin_Insr(Temp(i),_,_,_)::_ -> Temp(i+1)
    |_::xs-> last_reg xs 
    |[]-> Temp(0) 

let stmt_to_intr stmts =
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
        ) ([],0) expr_list in  
        
       Load(Temp(incr),Return(0))::Call(iden)::lst
         

    |ERROR -> 
            raise (OP "failed to print") 
    in
    let rec stmt_to_intr_r stmts output =
    match (stmts) with 
    |Parser.ASSIGN(k , v) :: xs -> 
        let new_output = match (expr_to_intr v 0) with  
            |Bin_Insr(Temp(i),_,_,_)::_ as all ->
                let value = (Load(Const(IDEN(k)),Temp(i))::all) in
                List.rev (value) @ output
            |all ->  all @ output 
        in 
        stmt_to_intr_r xs new_output 
            
    |FUNC(name, args , stmts,return)::xs ->
        let args = List.mapi 
            (fun i v -> Load(Const(IDEN(v)), Arg(i)))
            args 
        in

        let stmt_insrs = stmt_to_intr_r stmts [] in
          
        let last_reg,expr_insrs = match (expr_to_intr return 1 ) with 
            |Bin_Insr(x,_,_,_)::_ as rest->x, rest
            |Load(x,_)::_ as rest ->x,rest
            |_-> raise (OP "wrong last istr")
        in
        let return = Load(Return(0),last_reg) in
        let all_intrs = args @ stmt_insrs @ expr_insrs @ [return] 
        in
        let func_label = Label(name,all_intrs)
        in
        stmt_to_intr_r xs (func_label::output)

    |[] -> output
    in
     (stmt_to_intr_r  stmts [])


let print_intrs file insts = 
    let stringify_reg reg = match reg with
    | Temp(x)  -> sprintf "t%i"  x 
    | Arg (x)  -> sprintf "a%i"  x
    | Return(x)-> sprintf "v%i"  x
    | Const(x) -> sprintf "%s"  (stringify x)
    in
    let rec print_intr inst = match inst with 
        |Bin_Insr(res,l,op,r)-> 
            fprintf file "%s = %s %s %s\n"  
                (stringify_reg res)  
                (stringify_reg l) 
                (stringify_op op) 
                (stringify_reg r);
        |Load(res,send) ->
            fprintf file "%s = %s\n"  
                (stringify_reg res) 
                (stringify_reg send);
            
        |Label(title,ls) ->
            fprintf file "%s:\n" title;
            List.iter (fun x -> fprintf file "\t"; (print_intr x)) ls;
            fprintf file "\treturn\n";
        |Call(x) ->
                fprintf file "call %s\n" x;
        in
    List.iter print_intr insts;;

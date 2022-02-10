open Parser
open Printf
open Lexer

type reg_type = Temp of int | Arg of int  | Return of int
type reg_dag = Load of reg_type * tokens
              |Op_single_reg of reg_type * op_type * tokens 
(*
let rec stmt_to_dag  assign =
    let rec expr_to_dag expr output = 
    (match expr,output with
    
    |OP (l,v,r) -> 
        let v1,v2 = expr_to_dag l (incr), expr_to_dag r (incr+1) 
        
    |VALUE(v) -> 
            Val(v) 
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
*)


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
    




open Parser
open Intr
open Printf

type reg =  
    |RAX 
    |EAX 
    |AX 
    |AL 
    |RCX 
    |ECX 
    |CX 
    |CL 
    |RDX 
    |EDX 
    |DX 
    |DL 
    |RBX 
    |EBX 
    |BX 
    |BL 
    |RSI 
    |ESI 
    |SI 
    |SIL 
    |RDI 
    |EDI 
    |DI 
    |DIL 
    |RSP 
    |ESP 
    |SP 
    |SPL 
    |RBP of int
    |EBP 
    |BP 
    |BPL 
    |R8 
    |R8D 
    |R8W 
    |R8B 
    |R9 
    |R9D 
    |R9W 
    |R9B 
    |R10 
    |R10D 
    |R10W 
    |R10B 
    |R11 
    |R11D 
    |R11W 
    |R11B 
    |R12 
    |R12D 
    |R12W 
    |R12B 
    |R13 
    |R13D 
    |R13W 
    |R13B 
    |R14 
    |R14D 
    |R14W 
    |R14B 
    |R15 
    |R15D 
    |R15W 
    |R15B 
    |Number of int

type jump_type = JMP 

type assem_bin_insr = MOV |  X64_SUB  | X64_ADD  | X64_MUL | X64_DIV

type  assem_un_insr =  PUSH | POP

type x64_ops = |X64_BIN of  assem_bin_insr * reg * reg
               |X64_UN of assem_bin_insr * reg 
               |X64_Jump of jump_type * string
               |X64_Label of string

                

module StackMap = Map.Make(String)

type stack_iden = Table of  stack_iden StackMap.t | Stack_Pos of int 


let intr_to_x64 insts = 
    (*
    let int_to_reg i =  match i with
        |1->EAX 
        |2->ECX 
        |3->EDX 
        |4->EBX 
        |5->ESP 
        |6->EBP 
        |7->ESI 
        |8->EDI
        |_ -> EDI 
    in
    let reg_to_int i =  match i with
        |EAX ->1 
        |ECX ->2
        |EDX ->3
        |EBX ->4
        |ESP ->5
        |EBP ->6
        |ESI ->7
        |EDI ->8
        |_ -> -1
    in
    *)
    (*
    let next_reg reg reg_count = match reg with
        |Temp(_) when reg_count < 8  -> reg_count + 1 
        |Temp(_) -> 1 
        |_ -> raise (OP "wrong reg")
    in
    *)
    let op_type_to_bin op = match op with
        |ADD ->X64_ADD  
        |DIV ->X64_DIV 
        |SUB ->X64_SUB 
        |MULT->X64_MUL 
    in
    let execute_op l op r  = match op with
        |ADD -> l + r  
        |DIV -> l/r 
        |SUB -> l-r
        |MULT-> l*r
    in

    let stack_init input_list = 
        let rec stack_init_r input last_reg output = match input,output with
            |(Load(Const(Iden_Val(var)),Const(value))::xs),Table(tbl) ->
                let new_reg = (last_reg-(sizeof value)) in
                let appened_map = StackMap.add var (Stack_Pos(new_reg)) tbl in
                stack_init_r xs new_reg (Table(appened_map) )


            |Label(_,values)::xs,Table(tbl)->
                let table_stack,new_reg = stack_init_r values last_reg (Table tbl) in 
                stack_init_r xs new_reg table_stack 

            |(Bin_Insr(Const(Iden_Val(var)),Const(value),_,_)::xs),Table(tbl) ->
                let new_reg = (last_reg-(sizeof value)) in
                let appened_map = StackMap.add var (Stack_Pos(new_reg)) tbl in
                stack_init_r xs new_reg (Table(appened_map))
            |[],_ -> output , last_reg 
            |_::xs,_-> stack_init_r xs last_reg output
        in
        stack_init_r input_list 0 (Table(StackMap.empty) )
    in
    let stack,_ = stack_init insts in
    let get_stack_location iden tbl = match tbl with
    |Table(x) -> (match (StackMap.find iden x) with Stack_Pos(x)->x | _-> raise (OP "line136") )
    |_->raise (OP "stack lookup called in wrong place")
    in 

    let rec intr_to_x64_r insts output = 
    match insts with
    
    |Load(Const(Iden_Val(d)),Const(Int_Val(s)))::xs -> 
        let stack_pos = get_stack_location d stack in  
        let new_inst = (X64_BIN(MOV,Number(s),(RBP (stack_pos)))) in
        intr_to_x64_r xs (new_inst::output)
    |Load(Const(Iden_Val(d)),Const(Iden_Val(s)))::xs -> 
        let dest = get_stack_location d stack in  
        let src = get_stack_location s stack in
        let new_inst = (X64_BIN(MOV,(RBP(src)),(RBP (dest)))) in
        intr_to_x64_r xs (new_inst::output)
    |Load(Temp(_),Const(Int_Val(s)))::xs->
        let new_inst = (X64_BIN(MOV,Number(s),EAX)) in 
        intr_to_x64_r xs (new_inst::output)

    |Label(title,sub_insts)::xs->
        let f_output= intr_to_x64_r sub_insts ((X64_Label (title))::output)  
        in
        intr_to_x64_r xs f_output

    |(Bin_Insr(Const(Iden_Val(d)),Const(Int_Val(l)),op, Const(Int_Val(r))))::xs ->
        let stack_pos = get_stack_location d stack in
        let ex_op = execute_op l op r in 
        let new_inst = X64_BIN(MOV,Number(ex_op),RBP(stack_pos)) in
        intr_to_x64_r xs (new_inst::output) 
    |(Bin_Insr(Const(Iden_Val(_)),_,_, _))::xs ->
        intr_to_x64_r xs (output)

    |Bin_Insr(Temp(d_i),Temp(r_i),op,Const(Int_Val(v)))::xs when d_i == r_i -> 
            intr_to_x64_r  xs (X64_BIN((op_type_to_bin op),Number(v),EAX)::output)
    |[]-> output
    |xs->
        print_intrs stdout xs; 
        output
        
    in
    intr_to_x64_r insts []



let stringify_x64_op op = match op with
    |X64_ADD  -> "add"
    |X64_DIV  -> "div"
    |X64_SUB  -> "sub"
    |X64_MUL  -> "mul"
    |MOV -> "mov" 


let stringify_reg reg = match reg with
|RAX -> "%rax"
|EAX -> "%eax"
|AX  -> "%ax"
|AL  -> "%al"
|RCX -> "%rcx"
|ECX -> "%ecx"
|CX  -> "%cx"
|CL  -> "%cl"
|RDX -> "%rdx"
|EDX -> "%edx"
|DX  -> "%dx"
|DL  -> "%dl"
|RBX -> "%rbx"
|EBX -> "%ebx"
|BX  -> "%bx"
|BL  -> "%bl"
|RSI -> "%rsi"
|ESI -> "%esi"
|SI  -> "%si"
|SIL -> "%sil"
|RDI -> "%rdi"
|EDI -> "%edi"
|DI  -> "%di"
|DIL -> "%dil"
|RSP -> "%rsp"
|ESP -> "%esp"
|SP  -> "%sp"
|SPL -> "%spl"
|RBP(i) -> Printf.sprintf "%i(rbp)" i 
|EBP -> "%ebp"
|BP  -> "%bp"
|BPL -> "%bpl"
|R8  -> "%r8"
|R8D -> "%r8d"
|R8W -> "%r8w"
|R8B -> "%r8b"
|R9  -> "%r9"
|R9D -> "%r9d"
|R9W -> "%r9w"
|R9B -> "%r9b"
|R10 -> "%r10"
|R10D-> "%r10d" 
|R10W-> "%r10w" 
|R10B-> "%r10b" 
|R11 -> "%r11"
|R11D-> "%r11d" 
|R11W-> "%r11w" 
|R11B-> "%r11b" 
|R12 -> "%r12"
|R12D-> "%r12d" 
|R12W-> "%r12w" 
|R12B-> "%r12b" 
|R13 -> "%r13"
|R13D-> "%r13d" 
|R13W-> "%r13w" 
|R13B-> "%r13b" 
|R14 -> "%r14"
|R14D-> "%r14d" 
|R14W-> "%r14w" 
|R14B-> "%r14b" 
|R15 -> "%r15"
|R15D-> "%r15d" 
|R15W-> "%r15w" 
|R15B-> "%r15b" 
|Number(i) -> Printf.sprintf "$%i" i

let stringify_jmp jmp = match jmp with
|JMP-> "jmp"


let rec print_x64 file ls= match ls with
    |X64_BIN(op,l,r)::xs -> 
        fprintf file "\t%s   %s,%s\n" (stringify_x64_op op) (stringify_reg l) (stringify_reg r);
        print_x64 file xs  
    |X64_UN (op,value)::xs-> 
        fprintf file "\t%s   %s" (stringify_x64_op op) (stringify_reg value);
        print_x64 file xs
    |X64_Jump(jmp,location)::xs -> 
        fprintf file "\t%s   %s\n" (stringify_jmp jmp) location;
        print_x64 file xs 
    |X64_Label(str)::xs-> 
        fprintf file "%s:\n" str;
        print_x64 file xs
    |[] ->  ()

open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string
(* a label                         *) | LABEL of string
(* unconditional jump              *) | JMP   of string                                                                                                                
(* conditional jump                *) | CJMP  of string * string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(*
     val eval_insn : config -> insn -> config
 *)
 let eval_insn (stack, conf) insn =
     let (state, stdin, stdout) = conf in match insn with
     | BINOP (op) -> (
         match stack with
             | x :: y :: ss -> ((Expr.apply_op op y x) :: ss, conf)
             | _            -> failwith "There is no enough element on the stack"
     )
     | CONST (n)  -> (n :: stack, conf)
     | READ       -> (
         match stdin with
             | i :: is -> (i :: stack, (state, is, stdout))
             | _       -> failwith "Stdin is empty"
     )
     | WRITE      -> (
         match stack with
             | x :: ss -> (ss, (state, stdin, stdout @ [x]))
             | _       -> failwith "Stack is empty"
     )
     | LD (x)     -> (state x :: stack, conf)
     | ST (x)     -> (
         match stack with
             | v :: ss -> (ss, (Expr.update x v state, stdin, stdout))
             | _       -> failwith "Stack is empty"
     )
     | _          -> failwith "Unsupported instruction"

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                         
let rec eval env conf prog = match prog with
    | (LABEL _) :: ps     -> eval env conf ps
    | (JMP l) :: _        -> eval env conf (env#labeled l)
    | (CJMP (f, l)) :: ps -> (
        let (stack, cnf) = conf in
        match stack with
            | x :: ss -> (
                if f = "z" && x == 0 || f = "nz" && x != 0 then
                    eval env (ss, cnf) (env#labeled l)
                else
                    eval env (ss, cnf) ps
            )
            | _       -> failwith "Stack is empty"
    )
    | insn :: ps -> eval env (eval_insn conf insn) ps
    | _  -> conf

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], (Expr.empty, i, [])) p in o


let label_generator =
    object
        val mutable counter = 0

        method next =
            counter <- counter + 1;
            "l" ^ string_of_int counter
    end

let rec print_list = function
    [] -> ()
    | e::l -> print_string(show_insn e) ; print_string "\n" ; print_list l

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let rec compile stmt =
(*    let _ = print_string (Stmt.show_t stmt); print_string "\n\nc" in*)
    (*
        val compile_expr : Language.Expr.t -> prg
     *)
    let rec compile_expr expr = match expr with
        | Expr.Const (n)        -> [CONST n]
        | Expr.Var (x)          -> [LD x]
        | Expr.Binop (op, x, y) -> (compile_expr x) @ (compile_expr y) @ [BINOP op]
    in
    let rec compile_if stmt label_end = match stmt with
        | Stmt.Seq (a, b)       -> (compile a) @ (compile_if b label_end)
        | Stmt.If (expr, a, b)  ->
            let label_else = label_generator#next in
            (compile_expr expr) @
            [CJMP ("z", label_else)] @
            (compile_if a label_end) @
            [JMP label_end] @
            [LABEL label_else] @
            (compile b)
        | _                     -> compile stmt
    in
    let cmp = match stmt with
    | Stmt.Read (x)         -> [READ; ST x]
    | Stmt.Write (expr)     -> (compile_expr expr) @ [WRITE]
    | Stmt.Assign (x, expr) -> (compile_expr expr) @ [ST x]
    | Stmt.Seq (a, b)       -> (compile a) @ (compile b)
    | Stmt.Skip             -> []
    | Stmt.While (expr, a)  ->
        let label_cond = label_generator#next in
        let label_body = label_generator#next in
        [JMP label_cond; LABEL label_body] @
        (compile a) @
        [LABEL label_cond] @
        (compile_expr expr) @
        [CJMP ("nz", label_body)]
    | Stmt.Repeat (a, expr)  ->
        let label_cond = label_generator#next in
        let label_body = label_generator#next in
        [LABEL label_body] @
        (compile a) @
        [LABEL label_cond] @
        (compile_expr expr) @
        [CJMP ("z", label_body)]
    | Stmt.If (expr, a, b) ->
        let label_end = label_generator#next in
        (compile_if stmt label_end) @
        [LABEL label_end]
    in
(*    let _ = print_string "\n ----------------- \n"; print_list cmp; print_string "\n ----------------- \n" in*)
    cmp

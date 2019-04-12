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
(* conditional jump                *) | CJMP  of string * string
(* begins procedure definition     *) | BEGIN of string list * string list
(* end procedure definition        *) | END
(* calls a procedure               *) | CALL  of string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
 *)
type config = (prg * State.t) list * int list * Stmt.config

(*
     val eval_insn : config -> insn -> config
 *)
 let eval_insn (cs, stack, conf) insn =
     let (state, stdin, stdout) = conf in match insn with
     | BINOP (op) -> (
         match stack with
             | x :: y :: ss -> (cs, (Expr.to_func op y x) :: ss, conf)
             | _            -> failwith "There is no enough element on the stack"
     )
     | CONST (n)  -> (cs, n :: stack, conf)
     | READ       -> (
         match stdin with
             | i :: is -> (cs, i :: stack, (state, is, stdout))
             | _       -> failwith "Stdin is empty"
     )
     | WRITE      -> (
         match stack with
             | x :: ss -> (cs, ss, (state, stdin, stdout @ [x]))
             | _       -> failwith "Stack is empty"
     )
     | LD (x)     -> (cs, (State.eval state x) :: stack, conf)
     | ST (x)     -> (
         match stack with
             | v :: ss -> (cs, ss, (State.update x v state, stdin, stdout))
             | _       -> failwith "Stack is empty"
     )
     | _          -> failwith "Unsupported instruction"
(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)
let rec eval env ((cs, stack, cnf) as conf) prog =
    match prog with
    | (LABEL _) :: ps     -> eval env conf ps
    | (JMP l) :: _        -> eval env conf (env#labeled l)
    | (CJMP (f, l)) :: ps -> (
        match stack with
            | x :: ss -> (
                if f = "z" && x == 0 || f = "nz" && x != 0 then
                    eval env (cs, ss, cnf) (env#labeled l)
                else
                    eval env (cs, ss, cnf) ps
            )
            | _       -> failwith "Stack is empty"
    )
    | (CALL l) :: ps      -> (
        let (state, _, _) = cnf in
        eval env ((ps, state) :: cs, stack, cnf) (env#labeled l)
    )
    | (BEGIN (arg_names, local_names)) :: ps -> (
        let (state, stdin, stdout) = cnf in
        let scope = State.push_scope state (arg_names @ local_names) in
        let args = List.map (fun x -> ST x) arg_names in
        eval env (cs, stack, (scope, stdin, stdout)) (args @ ps)
    )
    | (END) :: _ -> (
        match cs with
            | (ps, state') :: cs' -> (
                let (state, stdin, stdout) = cnf in
                eval env (cs', stack, (State.drop_scope state state', stdin, stdout)) ps
            )
            | _                 -> conf
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
  let (_, _, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], [], (State.empty, i, [])) p in o

let label_generator =
    object
        val mutable counter = 0

        method next =
            counter <- counter + 1;
            "l" ^ string_of_int counter
    end

(* Stack machine compiler

     val compile : Language.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let rec compile_block stmt =
    (*
        val compile_expr : Language.Expr.t -> prg
     *)
    let rec compile_expr expr = match expr with
        | Expr.Const (n)        -> [CONST n]
        | Expr.Var (x)          -> [LD x]
        | Expr.Binop (op, x, y) -> (compile_expr x) @ (compile_expr y) @ [BINOP op]
    in
    let rec compile_if stmt label_end = match stmt with
        | Stmt.Seq (a, b)       -> (compile_block a) @ (compile_if b label_end)
        | Stmt.If (expr, a, b)  ->
            let label_else = label_generator#next in
            (compile_expr expr) @
            [CJMP ("z", label_else)] @
            (compile_if a label_end) @
            [JMP label_end] @
            [LABEL label_else] @
            (compile_block b)
        | _                     -> compile_block stmt
    in
    match stmt with
    | Stmt.Read (x)         -> [READ; ST x]
    | Stmt.Write (expr)     -> (compile_expr expr) @ [WRITE]
    | Stmt.Assign (x, expr) -> (compile_expr expr) @ [ST x]
    | Stmt.Seq (a, b)       -> (compile_block a) @ (compile_block b)
    | Stmt.Skip             -> []
    | Stmt.While (expr, a)  ->
        let label_cond = label_generator#next in
        let label_body = label_generator#next in
        [JMP label_cond; LABEL label_body] @
        (compile_block a) @
        [LABEL label_cond] @
        (compile_expr expr) @
        [CJMP ("nz", label_body)]
    | Stmt.Repeat (a, expr)  ->
        let label_cond = label_generator#next in
        let label_body = label_generator#next in
        [LABEL label_body] @
        (compile_block a) @
        [LABEL label_cond] @
        (compile_expr expr) @
        [CJMP ("z", label_body)]
    | Stmt.If (expr, a, b) ->
        let label_end = label_generator#next in
        (compile_if stmt label_end) @
        [LABEL label_end]
    | Stmt.Call (f, args) ->
        let args_rev = List.concat(List.map compile_expr args) in
        let args = List.rev args_rev in
        args @ [CALL f]

let compile_def (f, (arg_names, local_names, stmt)) =
    [LABEL f; BEGIN (arg_names, local_names)] @
    (compile_block stmt) @
    [END]

let compile (defs, stmt) =
    let main = compile_block stmt in
    let defs = List.concat (List.map compile_def defs) in
    main @
    [END] @
    defs

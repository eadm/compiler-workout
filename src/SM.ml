open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

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

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)
let rec eval config prg = List.fold_left eval_insn config prg

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Language.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)
let rec compile stmt =
    (*
        val compile_expr : Language.Expr.t -> prg
    *)
    let rec compile_expr expr = match expr with
        | Expr.Const (n)        -> [CONST n]
        | Expr.Var (x)          -> [LD x]
        | Expr.Binop (op, x, y) -> (compile_expr x) @ (compile_expr y) @ [BINOP op]
    in
    match stmt with
    | Stmt.Read (x)         -> [READ; ST x]
    | Stmt.Write (expr)     -> (compile_expr expr) @ [WRITE]
    | Stmt.Assign (x, expr) -> (compile_expr expr) @ [ST x]
    | Stmt.Seq (a, b)       -> (compile a) @ (compile b)

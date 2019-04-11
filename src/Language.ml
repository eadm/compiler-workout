(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Apply raw binary operation to arguments
          val apply_op : string -> int -> int -> int
    *)
    let apply_op raw_op x y =
      (*
        Convert int to bool

        val of_int : int -> bool
      *)
      let of_int i = i != 0 in
      (*
        Convert bool to int

        val to_int : bool -> int
      *)
      let to_int b = if b then 1 else 0 in
      (*
        Applies compare operation and convert it result to int

        val compare_op : (int -> int -> bool) -> int -> int -> int
      *)
      let compare_op op x y = to_int (op x y) in
      (*
        Applies boolean operation and convert it result to int

        val compare_op : (bool -> bool -> bool) -> int -> int -> int
      *)
      let boolean_op op x y = to_int (op (of_int x) (of_int y)) in
      let op = match raw_op with
      | "+"  -> ( + )
      | "-"  -> ( - )
      | "*"  -> ( * )
      | "/"  -> ( / )
      | "%"  -> (mod)
      | "<"  -> compare_op ( <  )
      | "<=" -> compare_op ( <= )
      | ">"  -> compare_op ( >  )
      | ">=" -> compare_op ( >= )
      | "==" -> compare_op ( == )
      | "!=" -> compare_op ( != )
      | "&&" -> boolean_op ( && )
      | "!!" -> boolean_op ( || )
      | _    -> failwith (Printf.sprintf "Unknown operation %s" raw_op)
      in
      op x y

    (* Expression evaluator

          val eval : state -> t -> int

       Takes a state and an expression, and returns the value of the expression in
       the given state.
     *)
    let rec eval state expr = match expr with
      | Const (n)        -> n
      | Var (x)          -> state x
      | Binop (op, x, y) -> apply_op op (eval state x) (eval state y)

    let parse_op op = ostap (- $(op)), (fun x y -> Binop (op, x, y))
    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    ostap (                                      
      expr:
        !(Ostap.Util.expr
          (fun x -> x)
          (Array.map (fun (assoc, operations) -> assoc, List.map parse_op operations)
            [|
              `Lefta, ["!!"];
              `Lefta, ["&&"];
              `Nona , ["<="; "<"; ">="; ">"; "=="; "!="];
              `Lefta, ["+"; "-"];
              `Lefta, ["*"; "/"; "%"];
            |]
          )
          primary
        );

      primary: n:DECIMAL {Const n} | x:IDENT {Var x} | -"(" expr -")"
    )
    
  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of t * Expr.t with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

         val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval ((state, stdin, stdout) as conf) stmt = match stmt with
      | Read (x)         -> (
        match stdin with
          | i :: is -> (Expr.update x i state, is, stdout)
          | _       -> failwith "Stdin is empty"
      )
      | Write (expr)     -> (state, stdin, stdout @ [Expr.eval state expr])
      | Assign (x, expr) -> (Expr.update x (Expr.eval state expr) state, stdin, stdout)
      | Seq (a, b)       -> eval (eval conf a) b
      | Skip             -> conf
      | If (expr, a, b)  -> eval conf (if (Expr.eval state expr) != 0 then a else b)
      | While (expr, a)  -> if (Expr.eval state expr) != 0 then eval (eval conf a) stmt else conf
      | Repeat (a, expr) -> (
        let (state', stdin', stdout') as conf' = eval conf a in
        if (Expr.eval state' expr) != 0 then conf' else eval conf' stmt
      )
                               
    (* Statement parser *)
    ostap (
      stmt:
        "read" "(" x:IDENT ")" {Read (x)} |
        "write" "(" expr:!(Expr.expr) ")" {Write (expr)} |
        "skip" {Skip} |
        "if" expr:!(Expr.expr) "then" s1:parse s2:elsebranch {If(expr, s1, s2)} |
        "while" expr:!(Expr.expr) "do" s1:parse "od" {While(expr, s1)} |
        "for" s1:parse "," expr:!(Expr.expr) "," s2:parse "do" s3:parse "od" {Seq (s1, While (expr, Seq (s3, s2)))} |
        "repeat" s1:parse "until" expr:!(Expr.expr) {Repeat(s1, expr)} |

        x:IDENT ":=" expr:!(Expr.expr) {Assign (x, expr)};

      elsebranch:
        "fi" {Skip} |
        "else" s1:parse "fi" {s1} |
        "elif" expr:!(Expr.expr) "then" s1:parse s2:elsebranch {If(expr, s1, s2)};

      parse: a:stmt ";" b:parse {Seq (a, b)} | stmt
    )
      
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o

(* Top-level parser *)
let parse = Stmt.parse                                                     

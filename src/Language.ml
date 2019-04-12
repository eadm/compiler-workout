(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators

(* States *)
module State =
  struct
                                                                
    (* State: global state, local state, scope variables *)
    type t = {g : string -> int; l : string -> int; scope : string list}

    (* Empty state *)
    let empty =
      let e x = failwith (Printf.sprintf "Undefined variable: %s" x) in
      {g = e; l = e; scope = []}

    (* Update: non-destructively "modifies" the state s by binding the variable x 
       to value v and returns the new state w.r.t. a scope
    *)
    let update x v s =
      let u x v s = fun y -> if x = y then v else s y in
      if List.mem x s.scope then {s with l = u x v s.l} else {s with g = u x v s.g}

    (* Evals a variable in a state w.r.t. a scope *)
    let eval s x = (if List.mem x s.scope then s.l else s.g) x

    (* Creates a new scope, based on a given state *)
    let push_scope st xs = {empty with g = st.g; scope = xs}

    (* Drops a scope *)
    let drop_scope st st' = {st' with g = st.g}

  end
    
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
      
    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)                                                       
    let to_func op =
      let bti   = function true -> 1 | _ -> 0 in
      let itb b = b <> 0 in
      let (|>) f g   = fun x y -> f (g x y) in
      match op with
      | "+"  -> (+)
      | "-"  -> (-)
      | "*"  -> ( * )
      | "/"  -> (/)
      | "%"  -> (mod)
      | "<"  -> bti |> (< )
      | "<=" -> bti |> (<=)
      | ">"  -> bti |> (> )
      | ">=" -> bti |> (>=)
      | "==" -> bti |> (= )
      | "!=" -> bti |> (<>)
      | "&&" -> fun x y -> bti (itb x && itb y)
      | "!!" -> fun x y -> bti (itb x || itb y)
      | _    -> failwith (Printf.sprintf "Unknown binary operator %s" op)    
    
    let rec eval st expr =      
      match expr with
      | Const n -> n
      | Var   x -> State.eval st x
      | Binop (op, x, y) -> to_func op (eval st x) (eval st y)

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    ostap (                                      
      expr:
	  !(Ostap.Util.expr 
             (fun x -> x)
	     (Array.map (fun (a, s) -> a, 
                           List.map  (fun s -> ostap(- $(s)), (fun x y -> Binop (s, x, y))) s
                        ) 
              [|                
		`Lefta, ["!!"];
		`Lefta, ["&&"];
		`Nona , ["=="; "!="; "<="; "<"; ">="; ">"];
		`Lefta, ["+" ; "-"];
		`Lefta, ["*" ; "/"; "%"];
              |] 
	     )
	     primary);
      
      primary:
        n:DECIMAL {Const n}
      | x:IDENT   {Var x}
      | -"(" expr -")"
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
    (* loop with a post-condition       *) | Repeat of t * Expr.t
    (* call a procedure                 *) | Call   of string * Expr.t list with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = State.t * int list * int list 

    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The 
       environment supplies the following method

           method definition : string -> (string list, string list, t)

       which returns a list of formal parameters and a body for given definition
    *)
    let rec eval env ((state, stdin, stdout) as conf) stmt = match stmt with
      | Read (x)         -> (
        match stdin with
          | i :: is -> (State.update x i state, is, stdout)
          | _       -> failwith "Stdin is empty"
      )
      | Write (expr)     -> (state, stdin, stdout @ [Expr.eval state expr])
      | Assign (x, expr) -> (State.update x (Expr.eval state expr) state, stdin, stdout)
      | Seq (a, b)       -> eval env (eval env conf a) b
      | Skip             -> conf
      | If (expr, a, b)  -> eval env conf (if (Expr.eval state expr) != 0 then a else b)
      | While (expr, a)  -> if (Expr.eval state expr) != 0 then eval env (eval env conf a) stmt else conf
      | Repeat (a, expr) -> (
        let (state', stdin', stdout') as conf' = eval env conf a in
        if (Expr.eval state' expr) != 0 then conf' else eval env conf' stmt
      )
      | Call (f, args)  -> (
        let (arg_names, local_names, f_stmt) = env#definition f in
        let args = List.map (Expr.eval state) args in
        let scope = State.push_scope state (arg_names @ local_names) in
        let f_state = List.fold_left2 (fun state' x v -> State.update x v state') scope arg_names args in
        let (state', stdin', stdout') = eval env (f_state, stdin, stdout) f_stmt in
        (State.drop_scope state' state, stdin', stdout')
      )
                                
    (* Statement parser *)
    ostap (
      stmt:
        "read" "(" x:IDENT ")" {Read (x)} |
        "write" "(" expr:!(Expr.expr) ")" {Write (expr)} |
        "skip" {Skip} |
        "if" expr:!(Expr.expr) "then" s1:parse s2:elsebranch {If (expr, s1, s2)} |
        "while" expr:!(Expr.expr) "do" s1:parse "od" {While (expr, s1)} |
        "for" s1:parse "," expr:!(Expr.expr) "," s2:parse "do" s3:parse "od" {Seq (s1, While (expr, Seq (s3, s2)))} |
        "repeat" s1:parse "until" expr:!(Expr.expr) {Repeat (s1, expr)} |
        f:IDENT "(" args:(!(Expr.expr))* ")" { Call (f, args) } |

        x:IDENT ":=" expr:!(Expr.expr) {Assign (x, expr)};

      elsebranch:
        "fi" {Skip} |
        "else" s1:parse "fi" {s1} |
        "elif" expr:!(Expr.expr) "then" s1:parse s2:elsebranch {If(expr, s1, s2)};

      parse: a:stmt ";" b:parse {Seq (a, b)} | stmt
    )
      
  end

(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

    let default v opt = match opt with
      | Some x -> x
      | None   -> v

    ostap (                                      
      parse:
        "fun" f:IDENT "(" arg_names:(IDENT)* ")" local_names:(-"local" (IDENT)*)? "{"
          stmt:!(Stmt.parse)
        "}" { (f, (arg_names, default [] local_names, stmt)) }
    )

  end
    
(* The top-level definitions *)

(* The top-level syntax category is a pair of definition list and statement (program body) *)
type t = Definition.t list * Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval (defs, body) i =
  let module M = Map.Make (String) in
  let m        = List.fold_left (fun m ((name, _) as def) -> M.add name def m) M.empty defs in  
  let _, _, o  = Stmt.eval (object method definition f = snd @@ M.find f m end) (State.empty, i, []) body in o

(* Top-level parser *)
let parse = ostap (!(Definition.parse)* !(Stmt.parse))

type const =
    Int of int 
  | Str of string 
  | Float of float 


  type expr =
    Plus of expr * expr        (* means a + b *)
  | Minus of expr * expr       (* means a - b *)
  | Multi of expr * expr       (* means a * b *)
  | Divide of expr * expr      (* means a / b *)
  | Value of string            (* "x", "y", "n", etc. *)
  | Cons of const
  ;;

type bCommand;;

type constr = 
    And of bCommand * bCommand (* && *)
  | Sequence of bCommand * bCommand (* ; *)
  | Or of bCommand * bCommand (* || *)

  type fu = 
      FunctionInto of string * (* параметр функции * *) bCommand
    | Name of string

type bCommand =
  Construction of constr
| Expression of expr
| FunctionOuto of fu
| NonRecognized of string
;;

let rec to_string_expr e =
  match e with
    Plus (left, right)   -> "(" ^ (to_string_expr left) ^ " + " ^ (to_string_expr right) ^ ")"
  | Minus (left, right)  -> "(" ^ (to_string_expr left) ^ " - " ^ (to_string_expr right) ^ ")"
  | Multi (left, right)  -> "(" ^ (to_string_expr left) ^ " * " ^ (to_string_expr right) ^ ")"
  | Divide (left, right) -> "(" ^ (to_string_expr left) ^ " / " ^ (to_string_expr right) ^ ")"
  | Value v -> v
  | Cons c -> "const"
  ;;

let rec to_string_bCommand c =
  match c with
    Construction (constr) -> "construction"
    | Expression (ex) -> to_string_expr ex
    | FunctionOuto (f) -> "function"
    | NonRecognized (s) -> s
    ;;

print_string (to_string_bCommand (Expression(Multi(Value "3", Plus(Value "5", Value "5")))));;
type expr;;
type cmd;;
type pipe;;
type fu;;
type mfor;;
type forList;;
type enumFor;;
type mcase;;
type mif;;
type mwhile;;
type muntil;;
type compList;;
type redirect;;
type word;;
type wordAssign;;
type arifm;;
type cons;;
type logOp;;
type pipeOp;;

type bCommand = 
  LogicalExpression of expr
  | Pipeline of pipe
  | Command of cmd
  | Function of fu
  | For of mfor
  | Case of mcase
  | If of mif
  | While of mwhile
  | Until of muntil
  ;;
type logOp = And | Or | Great | Less | EcGreat | EcLess | Ecual;; (*логические выражения, как их парсить? например обратно 2 раза pattern match?*)

type expr =
  (*надо ли еще 1 expr отдельно*)
  LogicalExpression of logOp * expr * expr
  | Pipeline of pipe
  | Command of cmd
  | Function of fu
  | For of mfor
  | Case of mcase
  | If of mif
  | While of mwhile
  | Until of muntil
  ;;
type pipeOp = And | Or | Dpoint;; (*склейки пайпы || && ;*)
type pipe = (*не работает надо чет придумать*)
   Pipline of pipeOp * pipe * pipe
  | Command of cmd
  | Function of fu
  | For of mfor
  | Case of mcase
  | If of mif
  | While of mwhile
  | Until of muntil
  ;;
type cmd =
  (*WHAT A FU*K *)
  Name of string * arifm * redirect (* name command, parametrs, redirection*)
  |NameN of string * redirect (* name command, redirection*)
  | Arifm of arifm
  ;;
type arifm =
  Ecual of word * arifm          (* means a = b or a = 4*)
  | Plus of arifm * arifm        (* means a + b *)
  | Minus of arifm * arifm       (* means a - b *)
  | Multi of arifm * arifm       (* means a * b *)
  | Divide of arifm * arifm      (* means a / b *)
  | Cons of cons           (* "x", "y", "n", etc. *)
  ;;
type fu =
  Funct of string * string * compList
  ;;
type compList =
  LogicalExpression of expr
  | Pipeline of pipe
  | Command of cmd
  | Function of fu
  | For of mfor
  | Case of mcase
  | If of mif
  | While of mwhile
  | Until of muntil
  ;;
type mfor =
  For of forList * compList
  ;;
type forList = 
  Arg of word * enumFor
  ;;
type enumFor =
  Listm of word
  | Str of string
;; 
type mcase =
  CaseItem of word * compList
  ;;
type mif =
  Clause of compList * compList * compList (*if then else*)
  ;;
type mwhile =
  Clause of compList * compList (*clause, todoList*)
  ;;
type muntil =
  Clause of compList * compList (*clause, todoList*)
  ;;
type redirect =
  Redirect of string * string * string
  ;;
type word =
  Cons of cons
  ;;
type cons =
  Int of int
  | Float of float
  | String of string
;;
(*type wordref =

;;*)

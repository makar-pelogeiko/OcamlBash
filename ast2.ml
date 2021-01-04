(* lazy stream -------------------------------------------------------------- *)

module LazyStream = struct
  type 'a t = Cons of 'a * 'a t Lazy.t | Nil

  let of_stream stream =
    let rec next stream =
      try Cons(Stream.next stream, lazy (next stream))
      with Stream.Failure -> Nil
    in
    next stream

  let of_function f =
    let rec next f =
      match f () with
      | Some x -> Cons(x, lazy (next f))
      | None -> Nil
    in
    next f

  let of_string str = str |> Stream.of_string |> of_stream
  let of_channel ic = ic |> Stream.of_channel |> of_stream
end

(* utilities ---------------------------------------------------------------- *)

let implode l = String.concat "" (List.map (String.make 1) l)

let explode s =
  let l = ref [] in
  String.iter (fun c -> l := c :: !l) s;
  List.rev !l

let (%) f g = fun x -> g (f x)

let parse parser input =
  match parser input with
  | Some(res, _) -> Some res
  | None -> None

(* primitives --------------------------------------------------------------- *)

type 'token input = 'token LazyStream.t
type ('token, 'result) monad = ('result * 'token input) option
type ('token, 'result) parser = 'token input -> ('result * 'token input) option

let return x input = Some(x, input)

let (>>=) x f =
  fun input ->
  match x input with
  | Some(result', input') -> f result' input'
  | None -> None

let (let*) = (>>=)

let (<|>) x y =
  fun input ->
  match x input with
  | Some _ as ret -> ret
  | None -> y input

let rec scan x input =
  match x input with
  | Some(result', input') -> LazyStream.Cons(result', lazy (scan x input'))
  | None -> LazyStream.Nil

let mzero _ = None

let any = function
  | LazyStream.Cons(token, input') -> Some(token, Lazy.force input')
  | LazyStream.Nil -> None

let satisfy test =
  any >>= (fun res -> if test res then return res else mzero)

let eof x = function
  | LazyStream.Nil -> Some(x, LazyStream.Nil)
  | _ -> None

(* derived combinators ------------------------------------------------------ *)

let (=>) x f = x >>= fun r -> return (f r)
let (>>) x y = x >>= fun _ -> y
let (<<) x y = x >>= fun r -> y >>= fun _ -> return r
let (<~>) x xs = x >>= fun r -> xs >>= fun rs -> return (r :: rs)

let rec choice = function
  | [] -> mzero
  | h :: t -> h <|> choice t

let rec count n x =
  if n > 0
  then x <~> count (n - 1) x
  else return []

let between op ed x = op >> x << ed

let option default x = x <|> return default
let optional x = option () (x >> return ())

let rec skip_many x = option () (x >>= fun _ -> skip_many x)
let skip_many1 x = x >> skip_many x

let rec many x = option [] (x >>= fun r -> many x >>= fun rs -> return (r :: rs))
let many1 x = x <~> many x

let sep_by1 x sep = x <~> many (sep >> x)
let sep_by x sep = sep_by1 x sep <|> return []

let end_by1 x sep = sep_by1 x sep << sep
let end_by x sep = end_by1 x sep <|> return []

let chainl1 x op =
  let rec loop a =
    (op >>= fun f -> x >>= fun b -> loop (f a b)) <|> return a
  in
  x >>= loop
let chainl x op default = chainl1 x op <|> return default

let rec chainr1 x op =
  x >>= fun a -> (op >>= fun f -> chainr1 x op => f a) <|> return a
let chainr x op default = chainr1 x op <|> return default

(* singletons --------------------------------------------------------------- *)

let exactly x = satisfy ((=) x)
let one_of  l = satisfy (fun x -> List.mem x l)
let none_of l = satisfy (fun x -> not (List.mem x l))
let range l r = satisfy (fun x -> l <= x && x <= r)

(* char parsers ------------------------------------------------------------- *)

let space     = one_of [' '; '\t'; '\r'; '\n']
let spaces    = skip_many space
let newline   = exactly '\n'
let tab       = exactly '\t'
let upper     = range 'A' 'Z'
let lower     = range 'a' 'z'
let digit     = range '0' '9'
let letter    = lower  <|> upper
let alpha_num = letter <|> digit
let hex_digit = range 'a' 'f' <|> range 'A' 'F' <|> digit
let oct_digit = range '0' '7'

(* lex helper --------------------------------------------------------------- *)

let lexeme x = spaces >> x

let token s =
  let rec loop s i =
    if i >= String.length s
    then return s
    else exactly s.[i] >> loop s (i + 1)
  in
  lexeme (loop s 0)
(*............................*)
(*............................*)
(*............................*)
(*............................*)
(*............................*)
(*............................*)
(*............................*)
(*........AST....................*)

type const =
  Int of int
  | Float of float
  | String of string
;;
type word =
  ConstW of const
  ;;
type redirect =
  Redirect of string * string * string
;;
type arifm =
  Ecual of string * arifm          (* means a = b or a = 4*)
  | Plus of arifm * arifm        (* means a + b *)
  | Minus of arifm * arifm       (* means a - b *)
  | Multi of arifm * arifm       (* means a * b *)
  | Divide of arifm * arifm      (* means a / b *)
  | Const of const           (* "x", "y", "n", etc. *)
  ;;
type cmd =
  (*WHAT A FU*K *)
  Name of string * string list * redirect (* name command, parametrs, redirection*)
  |NameN of string * redirect (* name command, redirection*)
  | Arifm of arifm
  ;;
  type enumFor =
    Listm of word
    | Str of string
  ;; 
  type forList = 
    Arg of word * enumFor
    ;;
  
type logOp = And | Or | Great | Less | EcGreat | EcLess | Ecualy;; (* как их прикрутить?логические выражения, как их парсить? например обратно 2 раза pattern match?*)
type expr =
  Logexpr of logOp * arifm * arifm (*логические выражения*)

type pipeOp = And | Or | Dpoint;; (*склейки пайпы || && ;*)
type pipe = (*не работает надо чет придумать*)
   Pipline of pipeOp * pipe * pipe
  | Command of cmd
  | Function of string * string * pipe (*name, parametrs, operators*)
  | For of forList * pipe (*аргумент и само тело фора надо еще думать над ним*)
  | Case of word * pipe (*не совсем знаю механизм кейсов в баше*)
  | If of expr * pipe * pipe (*if then else*)
  | While of expr * pipe (*clause, todoList*)
  | Until of expr * pipe (*clause, todoList*)
  | LogicalExpression of expr (*может или не может тут быть??*)
  ;;

(*...........AST.................*)
(*............................*)
(*............................*)
(*............................*)
(*............................*)


(* эта функция применяет парсер к строке *)
let apply p s = parse p (LazyStream.of_string s)
(* тут производится парсинг целых чисел и операция + - * / *)
let digits = spaces >> many1 digit => implode
  let integer = digits => int_of_string
  let temp_integer c = (Const ( Int (int_of_string c)))(*промежуточный перевод*)
  let some_integer = digits => temp_integer(*возвращает int  в нужном формате и остается парсером*)
  let floater = digits >>= fun fir -> token "." >> digits >>= fun sec -> return(float_of_string (fir ^ "." ^ sec))
  let some_floater = digits >>= fun fir -> token "." >> digits >>= fun sec -> return(Const (Float (float_of_string (fir ^ "." ^ sec))))
;;

let reserved =
  [ "true"; "false"; "if"; "else"; "for"; "while"; "public"; "const"; "static"
  ; "int"; "bool"; "string"; "void"; "char"; "null"; "new"; "this"; "base"
  ; "vitual"; "override"; "abstract"; "namespace"; "using"; "do"; "return"
  ; "continue"; "brake"; "class" ]

(*returns string which is not in reserved*)
let ident =
  spaces >> (letter <~> many alpha_num) => implode
  >>= function s when List.mem s reserved -> mzero | s -> return s
(*had to be like ident but точки тоже включает в слово*) 
let ident_p =
  spaces >> (letter <~> many (alpha_num <|> (one_of ['.']) )) => implode
  >>= function s when List.mem s reserved -> mzero | s -> return s
 (*had to be like ident, but with points.*) 
;;
let some_ident =
  spaces >> (letter <~> many alpha_num) => implode
  >>= function s when List.mem s reserved -> mzero | s -> return (Const (String s))
let pars_const =
  (some_floater <|> some_integer <|> some_ident)

let parens = between (token "(") (token ")")
let add_op = token "+" >> return (fun x y -> Plus (x, y))
let sub_op = token "-" >> return (fun x y -> Minus (x, y))
let multi_op = token "*" >> return (fun x y -> Multi (x, y))
let div_op = token "/" >> return (fun x y -> Divide (x, y))

let rec arifm input = chainl1 term (add_op <|> sub_op) input
and term input = chainl1 factor (multi_op <|> div_op) input
and factor input = (parens arifm <|> pars_const) input
;;

let rec arifm_to_string ex =
  match ex with
  Plus (l, r) -> "Plus(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"
  | Minus (l, r) -> "Minus(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"
  | Multi (l, r) -> "Multi(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"
  | Divide (l, r) -> "Divide(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"           
  | Const (Int c) -> string_of_int c 
  | Const (String d)-> d
  | Const (Float d)-> string_of_float d 
  | Ecual (str, ar) -> "Ecual(" ^ str ^ " , " ^ arifm_to_string ar ^ ")"

let arifmOption_to_string d =
  match d with
  None -> "None"
  | Some x -> arifm_to_string x
  ;;


print_string "//////////////////\n";;
let (str:string) = "(3+5)*4.1 + Path";;
print_string (str ^ " = ");;
print_string (arifmOption_to_string (apply arifm str));;
(*конец парсинга arifm, но бужет еще функция all_arifm*)

(*та самая all_arifm которая парсит арифм и Ecual*)
let rec all_arifm input = (ecualer <|> arifm) input
 and ecualer input =
  (ident >>= fun variable ->
  token "=" >>
  arifm >>= fun arifmer ->
  return (Ecual(variable, arifmer)) ) input
;;

let (str:string) = "sss = (3+5)*4 + 10";;
print_string ("\n" ^ str ^ " <-> ");;
print_string (arifmOption_to_string (apply all_arifm str));;


print_string "\n example of ident_p: ";
match apply ident_p "sd.sd." with
None -> print_string "None"
|Some (x) -> print_string x
;;

let name = ident >>= fun name -> return (name)
(*
let atom = (ident => (fun s -> String s))
       <|> (integer => (fun x -> Int x))
;;
*)
let rec cmd_pars input = (name_p <|> nameN_p <|> arifm_p) input
and name_p input =
  (ident         >>= fun nam ->
  (many ident_p)        >>= fun arg ->
  ((with_redirF nam arg) <|> (with_redirL nam arg) <|> (return (Name (nam, arg, Redirect("", "", ""))) )) ) input
    and with_redirF nam arg input = 
      (token ">"    >> (* > *)
      ident_p        >>= fun red ->
      (
      match arg with
        [] -> return (NameN (nam, Redirect(red, "", ""))) 
      | a::s -> return (Name (nam, arg, Redirect(red, "", "")))
      ) )input
    and with_redirL nam arg input = 
      (token "<"    >> (* < *)
      ident_p        >>= fun red ->
      (
      match arg with
        [] -> return (NameN (nam, Redirect("", red, ""))) 
      | a::s -> return (Name (nam, arg, Redirect("", red, "")))
      ) )input
and nameN_p input =
  (ident         >>= fun nam ->
   ((with_redirF nam []) <|> (with_redirL nam []) <|> (return (NameN (nam, Redirect("", "", ""))) )) ) input
and arifm_p input =
  (all_arifm       >>= fun ar ->
   return (Arifm (ar))) input
;;

(*function to convert*)
let cmd_to_string d =
  let rec list_string = function 
  [] -> ""
  | e::l -> e ^ " " ^ list_string l
  in
  let rec temp ex =
    match ex with
      Name (s, li, Redirect(fir, sec, thi)) -> "Name(" ^ s ^" , " ^ list_string li ^ " , " ^ "Redirect(" ^ fir ^ " , " ^ sec ^ " , " ^ thi ^ "))"
    | NameN (s, Redirect(fir, sec, thi)) -> "NemeN(" ^ s ^" , "  ^ "Redirect(" ^ fir ^ " , " ^ sec ^ " , " ^ thi ^ "))"
    | Arifm (a) -> arifm_to_string a
    in 
  match d with
  None -> "None"
  | Some x -> temp x
;;
(*тест парсера cmd*)
let c_str = "cat file.txt > my.txt";;
let file = apply cmd_pars c_str;;
print_string ("\n example of cmd_parse with " ^ c_str ^ ": ");;
print_string (cmd_to_string (file));;
print_string "\n";;
;;


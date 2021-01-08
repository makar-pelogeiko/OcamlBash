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
  | Variable of string

type word =
  ConstW of const

type redirect =
  Redirect of string * string * string

type stroperator =
  Leng of const (*${#string}*)
  | PicFromPos of const * const (*${string:position}*)
  | PicFromPosLeng of const * const *const (*${string:position:length}*)
  | CutBegLess of const * const (*${string#substring}*)
  | CutEndLess of const * const (*${string%substring}*)
  | CutBegMore of const * const (*${string##substring}*)
  | CutEndMore of const * const (*${string%%substring}*)
  | ReplaceFirst of const * const * const (*${string/pattern/replacement}*)
  | ReplaceAll of const * const *const (*${string//pattern/replacement}*)
  | ReplaceBeg of const * const * const (*${string/#pattern/replacement}*)
  | ReplaceEnd of const * const * const (*${string/%pattern/replacement}*)


type enumFor =
  Listm of word
  | Str of string
 
type forList = 
  Arg of word * enumFor

type arconst =
  | Const of const
  | SubFn of arifm (*$(...)*)   
and arifm =
  Ecual of string * arifm          (* means a = b or a = 4*)
  | Plus of arifm * arifm        (* means a + b *)
  | Minus of arifm * arifm       (* means a - b *)
  | Multi of arifm * arifm       (* means a * b *)
  | Divide of arifm * arifm      (* means a / b *)
  | AriConst of arconst               (* "x", "y", "n", etc. *)
  | AndAr of arifm * arifm         (*a && b *)
  | OrAr of arifm * arifm          (*a || b *)
  | GreatAr of arifm * arifm       (*a > b *)
  | LessAr of arifm * arifm        (*a < b *) 
  | EcGreatAr of arifm * arifm     (*a >= b *)
  | EcLessAr of arifm * arifm      (*a =< b *)
  | EcualyAr of arifm * arifm      (*a == b *) 
  | StringOp of stroperator
  | CallFunction of string * arifm list * redirect (*name, parametrs*)  
  (*| SubFn of arifm (*$(...)*)*)
  | Braces of arifm (*${...}*)

and cmd =
  (*Name of string * string list * redirect (* name command, parametrs, redirection*)*)
  (*| NameN of string * redirect (* name command, redirection*)*)
  | Arifm of arifm (*в основном вызов функций и присваивание*)
  

and pipeOp = And | Or | Dpoint | Redi (*склейки пайпы || && ;*)
and pipe = (*не работает надо чет придумать*)
   Pipline of pipeOp * pipe * pipe
  (*| Command of cmd*)
  | For of forList * pipe (*аргумент и само тело фора надо еще думать над ним*)
  | Case of word * pipe (*не совсем знаю механизм кейсов в баше*)
  | IfElse of arifm * pipe * pipe (*if then else*)
  | While of arifm * pipe (*clause, todoList*)
  | Until of arifm * pipe (*clause, todoList*)
  | Expression of arifm (*может или не может тут быть??*)

type declFunct = 
  Method of string * pipe 

type  bCommand = 
  | Pipe of pipe
  | DeclFunct of declFunct (*kosiak*)
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
  let temp_integer c = (Int (int_of_string c))(*промежуточный перевод*)
  let some_integer = digits => temp_integer(*возвращает int  в нужном формате и остается парсером*)
  let floater = digits >>= fun fir -> token "." >> digits >>= fun sec -> return(float_of_string (fir ^ "." ^ sec))
  let some_floater = digits >>= fun fir -> token "." >> digits >>= fun sec -> return(Float (float_of_string (fir ^ "." ^ sec)))

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

let some_ident =
  spaces >> (letter <~> many (alpha_num <|> (one_of ['.']) )) => implode
  >>= function s when List.mem s reserved -> mzero | s -> return (String s)

let pars_vari =
  (token "$" >> ident >>= fun str -> return(Variable (str)) )
let pars_const =
  (some_floater <|> some_integer <|> some_ident)
let pars_const_arifm =
  pars_const >>= fun ar -> return(Const (ar))
let pars_const_vari =
  (pars_vari <|> pars_const)
let pars_const_vari_arifm =
  pars_const_vari >>= fun ar -> return(AriConst(Const (ar)))
let parse_word =
  ident >>= fun str -> return (ConstW (String (str)))

(*
type stroperator =
  Leng of const (*${#string}*)
  | PicFromPos of const * const (*${string:position}*)
  | PicFromPosLeng of const * const *const (*${string:position:length}*)
  | CutBegLess of const * const (*${string#substring}*)
  | CutEndLess of const * const (*${string%substring}*)
  | CutBegMore of const * const (*${string##substring}*)
  | CutEndMore of const * const (*${string%%substring}*)
  | ReplaceFirst of const * const * const (*${string/pattern/replacement}*)
  | ReplaceAll of const * const *const (*${string//pattern/replacement}*)
  | ReplaceBeg of const * const * const (*${string/#pattern/replacement}*)
  | ReplaceEnd of const * const * const (*${string/%pattern/replacement}*)
*)
let rec pars_stroperator input = 
  (replaceEnd <|> replaceBeg <|> replaceAll <|> replaceFirst <|> cutEndMore <|> cutBegMore <|> cutEndLess <|> cutBegLess <|> picFromPosLeng <|> picFromPos <|> leng) input
      and replaceEnd input =
        (token "$" >> token "{" >> pars_const_vari >>= fun str -> token "/" >> token "%" >> pars_const_vari >>= fun patrn -> token "/" >> pars_const_vari >>= fun rep -> token "}" >> return (ReplaceEnd(str, patrn, rep)) ) input
      and replaceBeg input =
        (token "$" >> token "{" >> pars_const_vari >>= fun str -> token "/" >> token "#" >> pars_const_vari >>= fun patrn -> token "/" >> pars_const_vari >>= fun rep -> token "}" >> return (ReplaceBeg(str, patrn, rep)) ) input
      and replaceAll input =
        (token "$" >> token "{" >> pars_const_vari >>= fun str -> token "/" >> token "/" >> pars_const_vari >>= fun patrn -> token "/" >> pars_const_vari >>= fun rep -> token "}" >> return (ReplaceAll(str, patrn, rep)) ) input
      and replaceFirst input =
        (token "$" >> token "{" >> pars_const_vari >>= fun str -> token "/" >> pars_const_vari >>= fun patrn -> token "/" >> pars_const_vari >>= fun rep -> token "}" >> return (ReplaceFirst(str, patrn, rep)) ) input
      and cutEndMore input =
        (token "$" >> token "{" >> pars_const_vari >>= fun str -> token "%" >> token "%" >> pars_const_vari >>= fun substr -> token "}" >> return (CutEndMore(str, substr)) ) input
      and cutBegMore input =
        (token "$" >> token "{" >> pars_const_vari >>= fun str -> token "#" >> token "#" >> pars_const_vari >>= fun substr -> token "}" >> return (CutBegMore(str, substr)) ) input
      and cutEndLess input =
        (token "$" >> token "{" >> pars_const_vari >>= fun str -> token "%" >> pars_const_vari >>= fun substr -> token "}" >> return (CutEndLess(str, substr)) ) input
      and cutBegLess input =
        (token "$" >> token "{" >> pars_const_vari >>= fun str -> token "#" >> pars_const_vari >>= fun substr -> token "}" >> return (CutBegLess(str, substr)) ) input
      and picFromPosLeng input =
        (token "$" >> token "{" >> pars_const_vari >>= fun str -> token ":" >> pars_const_vari >>= fun pos -> token ":" >> pars_const_vari >>= fun leng -> token "}" >> return (PicFromPosLeng(str, pos, leng)) ) input
      and picFromPos input =
        (token "$" >> token "{" >> pars_const_vari >>= fun str -> token ":" >> pars_const_vari >>= fun pos -> token "}" >> return (PicFromPos(str, pos)) ) input
      and leng input =
        (token "$" >> token "{" >>  token "#" >> ident >>= fun str -> token "}" >> return (Leng(String(str))) ) input
let pars_stroperator_arifm =
  pars_stroperator >>= fun strop -> return(StringOp (strop))


let parens = between (token "(") (token ")")
let money_parens propars = token "$" >> (between (token "(") (token ")") propars) >>= fun arg -> return (AriConst(SubFn (arg)))
let money_braces propars = token "$" >> ((between (token "{") (token "}")) propars) (*надо добавить обработку ${array[$i]}*)
let add_op = token "+" >> return (fun x y -> Plus (x, y))
let sub_op = token "-" >> return (fun x y -> Minus (x, y))
let multi_op = token "*" >> return (fun x y -> Multi (x, y))
let div_op = token "/" >> return (fun x y -> Divide (x, y))

let and_op = token "&&" >> return (fun x y -> AndAr (x, y))
let or_op = token "||" >> return (fun x y -> OrAr (x, y))
let great_op = token ">" >> return (fun x y -> GreatAr (x, y))
let less_op = token "<" >> return (fun x y -> LessAr (x, y))
let ecGreat_op = token ">=" >> return (fun x y -> EcGreatAr (x, y))
let ecLess_op = token "=<" >> return (fun x y -> EcLessAr (x, y))
let ecualy_op = token "==" >> return (fun x y -> EcualyAr (x, y))
;;
let rec arifm_not_call input = chainl1 arifmeticNOT (and_op <|> or_op <|> ecGreat_op <|> ecLess_op <|> ecualy_op) input
and arifmeticNOT input = chainl1 termNOT (add_op <|> sub_op) input
and termNOT input = chainl1 factorNOT (multi_op <|> div_op) input
and factorNOT input = (money_parens arifm <|> parens arifm_not_call <|> pars_stroperator_arifm <|> pars_const_vari_arifm) input
and arifm input = chainl1 arifmetic (and_op <|> or_op <|> ecGreat_op <|> ecLess_op <|> great_op <|> less_op <|> ecualy_op) input
and arifmetic input = chainl1 term (add_op <|> sub_op) input
and term input = chainl1 factor (multi_op <|> div_op) input
and factor input = (money_parens arifm <|> parens arifm <|> pars_stroperator_arifm <|> pars_callFunction <|> pars_const_vari_arifm) input
and pars_callFunction input = call_func input
and call_func input =
  (ident         >>= fun nam ->
  (many (money_parens arifm <|> arifm_not_call))   >>= fun arg ->
  ((with_redirF nam arg) <|> (with_redirL nam arg) <|> (return (CallFunction (nam, arg, Redirect("", "", ""))) )) ) input
    and with_redirF nam arg input = 
      (token ">"    >> (* > *)
      ident_p        >>= fun red ->
      (
      match arg with
        [] -> return (CallFunction (nam, [], Redirect(red, "", ""))) 
      | a::s -> return (CallFunction (nam, arg, Redirect(red, "", "")))
      ) )input
    and with_redirL nam arg input = 
      (token "<"    >> (* < *)
      ident_p        >>= fun red ->
      (
      match arg with
        [] -> return (CallFunction (nam, [], Redirect("", red, ""))) 
      | a::s -> return (CallFunction (nam, arg, Redirect("", red, "")))
      ) )input


let rec arifm_to_string ex =
  let rec arlist_string = function
    | [] -> ""
    | e :: l -> arifm_to_string e ^ " " ^ arlist_string l in
  match ex with
  Plus (l, r) -> "Plus(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"
  | Minus (l, r) -> "Minus(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"
  | Multi (l, r) -> "Multi(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"
  | Divide (l, r) -> "Divide(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"           
  | AriConst (Const(Int c)) -> string_of_int c 
  | AriConst (Const(String d))-> d
  | AriConst (Const(Float d))-> string_of_float d
  | AriConst (Const(Variable v)) -> "Vari( " ^ v ^ ")" 
  | Ecual (str, ar) -> "Ecual(" ^ str ^ " , " ^ arifm_to_string ar ^ ")"
  | AndAr (l, r)     -> "And(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")" (*a && b *)
  | OrAr (l, r)      -> "Or(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")" (*a || b *)
  | GreatAr (l, r)   -> "Great(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"(*a > b *)
  | LessAr (l, r)    -> "Less(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"(*a < b *) 
  | EcGreatAr (l, r) -> "EcGreat(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"  (*a >= b *)
  | EcLessAr (l, r)  ->"EcLess(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"    (*a =< b *)
  | EcualyAr (l, r)  ->"EcualY(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"    (*a == b *)
  | StringOp (ReplaceAll (s, p, r)) -> "ReplaceAll(" ^ arifm_to_string (AriConst(Const(s))) ^ ", " ^ arifm_to_string (AriConst(Const(p))) ^ ", " ^ arifm_to_string (AriConst(Const(r))) ^ ")"
  | StringOp (d) -> "some string operation"
  | AriConst (SubFn(ari)) -> "Ari(SubFn(" ^ arifm_to_string ari ^ "))"
  | CallFunction (str, arlst, Redirect(fir, sec, thir)) -> "CallFunction(" ^ str ^ ", " ^ arlist_string arlst ^ ", " ^ "Redirect(" ^ fir ^ ", " ^ sec ^ ", " ^ thir ^ "))"
  | Braces (_) -> "braces"  

let arifmOption_to_string d =
  match d with
  None -> "None"
  | Some x -> arifm_to_string x
  ;;

print_string "//////(3+5)==4.1 + $Path + ${stssring//passsttern/repssslacement}////////////\n";;
let (str:string) = "echo $(2+ echo ss.txt) < mm.txt";;
print_string (str ^ " = ");;
print_string (arifmOption_to_string (apply arifm str));;
(*EcualY(Plus(3 , 5) , Plus(Plus(4.1 , Vari( Path)) , ReplaceAll(stssring, passsttern, repssslacement)))*)
(*конец парсинга arifm, но бужет еще функция all_arifm*)

(*та самая all_arifm которая парсит только  Ecual*)
let rec all_arifm input = (ecualer) input
 and ecualer input =
  (ident >>= fun variable ->
  token "=" >>
  arifm >>= fun arifmer ->
  return (Ecual(variable, arifmer)) ) input
;;

let (str:string) = "sss = rrr + 3";;
print_string ("\n" ^ str ^ " <-> ");;
print_string (arifmOption_to_string (apply (all_arifm <|> arifm) str));;


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

(*
(*тест парсера cmd*)
let c_str = "cat file.txt > my.txt";;
let file = apply cmd_pars c_str;;
print_string ("\n example of cmd_parse with " ^ c_str ^ ": ");;
print_string (cmdOption_to_string (file));;
print_string "\n";;
;;
(*поехали в pipe*)
*)
let rec parse_pipe input =
  ( (parse_pipeline <|> single_pipe) >>= fun par -> return (par)) input
and single_pipe input =
  ((while_pars <|> ifelse_pars <|> expr_pars) >>= fun par -> return (par)) input
and parse_pipeline input =
  ((p_dpoint <|> p_pipe) >>= fun par -> return(par)) input
and p_dpoint input = 
  (single_pipe >>= fun fir 
  -> token ";" >> single_pipe >>= fun sec 
  -> return (Pipline (Dpoint, fir, sec))) input
and p_pipe input = 
  (single_pipe >>= fun fir 
  -> token "|" >> single_pipe >>= fun sec 
  -> return (Pipline (Redi, fir, sec))) input
      and expr_pars input =
        ((all_arifm <|> arifm) >>= fun ar -> return (Expression (ar))) input        
      and while_pars input =
        (token "while"   >> (* while *)
        arifm         >>= fun pred ->
        token "do" >> (* do *)
        parse_pipe     >>= fun act ->
        token "done"    >>
        return (While (pred, act))) input
      and ifelse_pars input =
        (token "if"   >> (* if *)
        arifm         >>= fun pred ->
        token "then" >> (* then *)
        parse_pipe   >>= fun thn ->
        token "else" >> (* else *)
        parse_pipe   >>= fun els ->
        token "fi"    >>
        return (IfElse (pred, thn, els))) input
     (* and function_pars input = 
        (ident        >>= fun nam ->
        token "("     >>
        (many ident)  >>= fun arg ->
        token ")"     >>
        token "{"     >> (* { *)
        parse_pipe    >>= fun act ->
        token "}"     >>
        return (Function (nam, arg, act))) input  
      *)
;;
let rec pipe_to_string pip =
  (*let rec list_string = function 
  [] -> ""
  | e::l -> e ^ " " ^ list_string l
  in*)
  match pip with
  Pipline (Dpoint, l, r) -> "Pipeline(Dpoint, " ^ pipe_to_string l ^ ", " ^ pipe_to_string r ^ ")"
  | Pipline (Redi, l, r) -> "Pipeline(Redi, " ^ pipe_to_string l ^ ", " ^ pipe_to_string r ^ ")"
  | Expression (ar) -> "Expression(" ^ arifm_to_string ar ^ ")"
  | While (pred, p) -> "While(" ^ arifm_to_string pred ^ ", " ^ pipe_to_string p ^ ")"
  | IfElse (ar, l, r) -> "IFElse(" ^ arifm_to_string ar ^ ", " ^ pipe_to_string l ^ ", " ^ pipe_to_string r ^ ")"
  | _ -> "i do not know"

let pipeOption_to_string pip =
  match pip with
   None -> "None"
  | Some (p) -> pipe_to_string p
;;
(*тест пайпов*)
let pp_str = "test () {pwd file.txt > my.txt; ss= 10+2}"
let p_str = "while 10 < 2 do pwd file.txt > my.txt; ss= 10+2 done";;
let p_file = apply parse_pipe p_str;;
print_string ("\n example of pipe_parse with " ^ p_str ^ ": ");;
print_string (pipeOption_to_string (p_file));;
print_string "\n";;
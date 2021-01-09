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
type enumFor =
  Listm of string
  | Str of string
 
type forList = 
  Arg of string * enumFor

type vari =
  | SimpleVari of string (*пересечение с const Variable*)
  | ArrayVari of string * arifm (*обращение к массиву*)
  | ArrayDecl of string (*объявление массива и присваивание ему чего-то*)
  | Braces of vari (*${ss}, но не ${$s}*)

and const =
  | Int of int
  | Float of float
  | String of string
  | Variable of vari

and redirect =
  Redirect of string * string * string

and stroperator =
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

and arifm =
  | Plus of arifm * arifm        (* means a + b *)
  | Minus of arifm * arifm       (* means a - b *)
  | Multi of arifm * arifm       (* means a * b *)
  | Divide of arifm * arifm      (* means a / b *)
  | Const of const               (* "x", "y", "n", etc. *)
  | AndAr of arifm * arifm         (*a && b *)
  | OrAr of arifm * arifm          (*a || b *)
  | GreatAr of arifm * arifm       (*a > b *)
  | LessAr of arifm * arifm        (*a < b *) 
  | EcGreatAr of arifm * arifm     (*a >= b *)
  | EcLessAr of arifm * arifm      (*a =< b *)
  | EcualyAr of arifm * arifm      (*a == b *) 
and brExpan =
  InBracExpan of word list (*{a,b,c}*)
and word =
  | WString of const
  | WbrExp of brExpan
  | WStringCont of const * word
  | WbrExpCont of brExpan * word
and arg = 
  | SubFn of expr (*$(...)*)
  | StringOp of stroperator (*${#string}*)
  | Word of word (*{a,b,c}, или string*) (*КОСЯК: ВСТАВИТЬ СЮДА ПЕРЕМЕННЫЕ*)
  | Subarifm of arifm (*$((2+2))*)
and expr =
  | Eqal of vari * arg list (* a=b НО: смотри в листок внимательно*)
  | CallFunction of string * arg list * redirect (*name, parametrs*)

and pipeOp = And | Or | Dpoint | Redi (*склейки пайпы || && ;*)
and pipe = (*не работает надо чет придумать*)
   | Expression of expr (*может или не может тут быть??*)
   | IfElse of expr * pipeConveyor * pipeConveyor (*if then else*)
   | While of expr * pipeConveyor (*clause, todoList*)
 (* | For of forList * pipe (*аргумент и само тело фора надо еще думать над ним*)
  | Case of word * pipe (*не совсем знаю механизм кейсов в баше*)
  *)
  (*| Until of arifm * pipe (*clause, todoList*)
  *)
and pipeConveyor =
  | Pipline of pipeOp * pipe * pipeConveyor
  | SigPipe of pipe

type declFunct = 
  Method of string * pipeConveyor 
(*тут надо еще шаманить, но оно должно работать*)
type  bCommand = 
  | PipeConv of pipeConveyor
  | DeclFunct of declFunct
type bCmdConv =
  | BConv of bCommand * bCmdConv (*конвеер функций и кода*)
  | BSigCmd of bCommand (*функция либо код*)
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
let bash_ident =
  many alpha_num => implode
  >>= function s when List.mem s reserved -> mzero | s -> return s
let ident =
  spaces >> (letter <~> many alpha_num) => implode
  >>= function s when List.mem s reserved -> mzero | s -> return s
(*had to be like ident but точки тоже включает в слово*) 
let ident_p =
  spaces >> (letter <~> many (alpha_num <|> (one_of ['.']) )) => implode
  >>= function s when List.mem s reserved -> mzero | s -> return s
 (*had to be like ident, but with points.*) 

let some_ident =
  (many (alpha_num <|> (one_of ['.']) )) => implode
  >>= function s when List.mem s reserved -> mzero | s -> return (String s)
(* тест some_ident чтоб было топроно как в баш
let stringer ww =
  match ww with 
  None -> "NOne"
  | Some (String(x)) -> "String(" ^ x ^ ")"
  | _ -> "shit"

let d = print_string (stringer (apply some_ident "s.s. s."))
*)
let parens = between (token "(") (token ")")
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
(*--------------------------------------------------------------------------------------------------------------------*)
(*Парсим const vari-без объявления массива arifm*)

let rec pars_vari input = (pars_bracesEx <|> pars_array <|> pars_Simplevari)input
  and pars_bracesEx input =
    (token "$" >> (between (token "{") (token "}") pars_variNot) >>= fun var -> return(Braces(var)) ) input
  and pars_Simplevari input =
    (token "$" >> bash_ident >>= fun str -> return(SimpleVari(str))) input
  and pars_array input=
    (bash_ident >>= fun str -> (between (token "[") (token "]") arifm) >>= fun index -> return (ArrayVari(str, index))) input
  and pars_variNot input =
  (pars_array <|> pars_SimplevariNot) input
  and pars_SimplevariNot input =
    (bash_ident >>= fun str -> return(SimpleVari(str))) input
  (*----------*)
and vari_to_const input =
  (pars_vari >>= fun var -> return (Variable(var))) input
and pars_const input =
  (vari_to_const <|> some_floater <|> some_integer <|> some_ident) input
and pars_const_arifm input =
  (pars_const >>= fun ar -> return(Const (ar)) )input

and arifm input = chainl1 arifmetic (and_op <|> or_op <|> ecGreat_op <|> ecLess_op <|> great_op <|> less_op <|> ecualy_op) input
and arifmetic input = chainl1 term (add_op <|> sub_op) input
and term input = chainl1 factor (multi_op <|> div_op) input
and factor input = (parens arifm <|> pars_const_arifm) input

let vari_eqal input = (*используем в парсере expr, для приравнивания*)
  ((bash_ident >>= fun str -> return(SimpleVari(str))) <|> pars_array) input
let rec vari_to_string var =
  match var with
  | SimpleVari (x) -> "SimpleVari(" ^ x ^ ")" 
  | ArrayVari (name, index) -> "ArryVari(" ^ name ^ ", " ^ arifm_to_string index ^ ")"
  | ArrayDecl (name) -> "ArrayDeclar(" ^ name ^ ")"
  | Braces (v) -> "Barces(" ^ vari_to_string v ^ ")"
and const_to_string ct =
  match ct with
  | Int c -> "INT:" ^ string_of_int c 
  | String d-> "STR:" ^ d
  | Float d-> "FLOAT:" ^ string_of_float d
  | Variable v -> "Vari( " ^ vari_to_string v ^ ")" 
and arifm_to_string ex =
  (*let rec arlist_string = function
    | [] -> ""
    | e :: l -> arifm_to_string e ^ " " ^ arlist_string l in
  *)
  match ex with
  Plus (l, r) -> "Plus(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"
  | Minus (l, r) -> "Minus(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"
  | Multi (l, r) -> "Multi(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"
  | Divide (l, r) -> "Divide(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"           
  | Const ct -> "Const(" ^ const_to_string ct ^ ")"
  | AndAr (l, r)     -> "And(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")" (*a && b *)
  | OrAr (l, r)      -> "Or(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")" (*a || b *)
  | GreatAr (l, r)   -> "Great(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"(*a > b *)
  | LessAr (l, r)    -> "Less(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"(*a < b *) 
  | EcGreatAr (l, r) -> "EcGreat(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"  (*a >= b *)
  | EcLessAr (l, r)  ->"EcLess(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"    (*a =< b *)
  | EcualyAr (l, r)  ->"EcualY(" ^ arifm_to_string l ^ " , " ^ arifm_to_string r ^ ")"    (*a == b *)
let constOption_to_string d =
  match d with
  None -> "None"
  | Some x -> const_to_string x
  ;;

print_string "//////CONST+VARI TEST////////////\n";;
let (str:string) = "${aa[1+${bb[2]}]}";;
print_string (str ^ " = ");;
print_string (constOption_to_string (apply pars_const str));;

let arifmOption_to_string d =
  match d with
  None -> "None"
  | Some x -> arifm_to_string x
  ;;

print_string "\n//////ARIFM TEST////////////\n";;
let (str:string) = "(3+5)==4.1 + ${$Path[1]}";;
print_string (str ^ " = ");;
print_string (arifmOption_to_string (apply arifm str));;


(*-----------------------Парсим WORD--------------------------------------------------------*)
let rec pars_word input = 
  (wbrExpCont <|> wStringCont <|> wString <|> wbrExp) input
  and wString input =
    (pars_const >>= fun con -> return(WString(con))) input
  and wbrExp input =
    ( token "{" >> inBraceExp >>= fun brac -> token "}" >> return(WbrExp(brac)) ) input
  and wStringCont input =
    ( pars_const >>= fun con -> pars_word_NoConst >>= fun wor -> return(WStringCont(con, wor)) ) input
  and wbrExpCont input =
    ( token "{" >> inBraceExp >>= fun barc -> token "}" >> pars_word_NoBr >>= fun wor -> return(WbrExpCont(barc, wor)) ) input
  and pars_word_NoConst input = 
  (wbrExpCont <|> wbrExp) input
  and pars_word_NoBr input = 
  (wStringCont <|> wString) input
  and inBraceExp input =
    ( (many1 (pars_word >>= fun elem -> token "," >> return(elem)) >>= fun lst -> pars_word >>= fun last -> return(InBracExpan (lst @ [last])))
      <|> 
      (pars_word >>= fun only -> return(InBracExpan([only]))) 
    ) input
let rec word_string wor =
  match wor with
  | WString (c) -> "WString(" ^ const_to_string c ^ ")"
  | WbrExp (brac) -> "WbrExp(" ^ brexp_to_string brac ^ ")"
  | WStringCont (con, brac) -> "WStringCont(" ^ const_to_string con ^ "| " ^ word_string brac ^ ")"
  | WbrExpCont (brac, con) -> "WbrExpCont(" ^ brexp_to_string brac ^ "|" ^ word_string con ^ ")"
  and brexp_to_string bra =
    let rec wordlst_to_string = function
    | [] -> ""
    | e :: l -> word_string e ^ ", " ^ wordlst_to_string l in
    match bra with
    InBracExpan (ls) -> wordlst_to_string ls
let option_word_string arg =
  match arg with
  | None -> "None"
  | Some x -> word_string x

  (*ssss{${sds[1]},sss,}dsdsd*) (*не парсит это, тк после запятой пустая строка, он не парсит пустую строку*)
let ff = print_string "\n//////WORD TEST////////////\n";;
let (str:string) = "ssss{${sds[1]},sss,}dsdsd";; 
print_string (str ^ " = ");;
let ddddd = print_string (option_word_string (apply pars_word str));;

(*-----------------------------Парсим stroperator----------------------------------------------------------------------------*)
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
        (token "$" >> token "{" >> pars_const >>= fun str -> token "/" >> token "%" >> pars_const >>= fun patrn -> token "/" >> pars_const >>= fun rep -> token "}" >> return (ReplaceEnd(str, patrn, rep)) ) input
      and replaceBeg input =
        (token "$" >> token "{" >> pars_const >>= fun str -> token "/" >> token "#" >> pars_const >>= fun patrn -> token "/" >> pars_const >>= fun rep -> token "}" >> return (ReplaceBeg(str, patrn, rep)) ) input
      and replaceAll input =
        (token "$" >> token "{" >> pars_const >>= fun str -> token "/" >> token "/" >> pars_const >>= fun patrn -> token "/" >> pars_const >>= fun rep -> token "}" >> return (ReplaceAll(str, patrn, rep)) ) input
      and replaceFirst input =
        (token "$" >> token "{" >> pars_const >>= fun str -> token "/" >> pars_const >>= fun patrn -> token "/" >> pars_const >>= fun rep -> token "}" >> return (ReplaceFirst(str, patrn, rep)) ) input
      and cutEndMore input =
        (token "$" >> token "{" >> pars_const >>= fun str -> token "%" >> token "%" >> pars_const >>= fun substr -> token "}" >> return (CutEndMore(str, substr)) ) input
      and cutBegMore input =
        (token "$" >> token "{" >> pars_const >>= fun str -> token "#" >> token "#" >> pars_const >>= fun substr -> token "}" >> return (CutBegMore(str, substr)) ) input
      and cutEndLess input =
        (token "$" >> token "{" >> pars_const >>= fun str -> token "%" >> pars_const >>= fun substr -> token "}" >> return (CutEndLess(str, substr)) ) input
      and cutBegLess input =
        (token "$" >> token "{" >> pars_const >>= fun str -> token "#" >> pars_const >>= fun substr -> token "}" >> return (CutBegLess(str, substr)) ) input
      and picFromPosLeng input =
        (token "$" >> token "{" >> pars_const >>= fun str -> token ":" >> pars_const >>= fun pos -> token ":" >> pars_const >>= fun leng -> token "}" >> return (PicFromPosLeng(str, pos, leng)) ) input
      and picFromPos input =
        (token "$" >> token "{" >> pars_const >>= fun str -> token ":" >> pars_const >>= fun pos -> token "}" >> return (PicFromPos(str, pos)) ) input
      and leng input =
        (token "$" >> token "{" >>  token "#" >> ident >>= fun str -> token "}" >> return (Leng(String(str))) ) input
;;

(*
and arg = 
  | Subarifm of arifm (*$((2+2))*)
  | SubFn of expr (*$(...)*)
  | StringOp of stroperator (*${#string}*)
  | Word of word (*{a,b,c}, или string*) (*КОСЯК: ВСТАВИТЬ СЮДА ПЕРЕМЕННЫЕ*)
and expr =
  | Eqal of vari * arg (* a=b НО: смотри в листок внимательно*)
  | CallFunction of string * arg list * redirect (*name, parametrs*)
*)
(*---------------------------------Парсим expr-------------------------------------------------------------------------*)
let rec pars_arg input =
  (sub_arifm <|> sub_fn <|> string_op <|> word_p) input
  and sub_arifm input =
    (token "$((" >> arifm >>= fun ar -> token "))" >> return (Subarifm(ar))) input
  and sub_fn input =
    (token "$(" >> pars_expr >>= fun fn -> token ")" >> return (SubFn(fn))) input
  and string_op input =
    (pars_stroperator >>= fun strop -> return(StringOp (strop)))input
  and word_p input =
    (pars_word >>= fun wd -> return(Word(wd))  )input
and pars_expr input = (*<<<<<<<<<<<<<<<<*)
  (eqal_e <|> call_e)input
  and eqal_e input =
    (array_decl_eq <|> single_eqal) input
    and single_eqal input =
      (vari_eqal >>= fun variable -> token "=" >> pars_arg >>= fun ag -> return (Eqal(variable, [ag]))) input
    and array_decl input =
      (bash_ident >>= fun str -> return(ArrayDecl(str))) input
    and array_decl_eq input =
      ( array_decl >>= fun massiv -> token "=(" >> pars_list_arg_array >>= fun args -> token ")" >> return(Eqal(massiv, args)) ) input
    and pars_list_arg_array input =
      ( pars_arg >>= fun beg -> (many1 (space >> spaces >> pars_arg)) >>= fun lst -> return (beg::lst) ) input
  and call_e input =
     (bash_ident >>= fun nam -> (many (space >> spaces >> pars_arg)) (*pars_list_arg*) >>= fun arglst -> 
      ((with_redirF nam arglst) <|> (with_redirL nam arglst) <|> (return (CallFunction (nam, arglst, Redirect("", "", ""))) )) ) input
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

let rec arg_to_string ar =
  match ar with
  | Subarifm (x) -> "Subarifm(" ^ arifm_to_string x ^ ")" (*$((2+2))*)
  | SubFn (ex) -> "SubFn(" ^ expr_to_string ex ^ ")" (*$(...)*)
  | StringOp (str) -> "StrngOP" ^ "did not done" ^ ")" (*${#string}*)
  | Word (wd) -> "Word(" ^ word_string wd ^ ")"(*{a,b,c}, или string*)
and expr_to_string ex =
  match ex with
  | Eqal (var, ar) -> "Eqal(" ^ vari_to_string var ^ ","^ arglst_to_string ar ^")" (* a=b НО: смотри в листок внимательно*)
  | CallFunction (str, arglst, Redirect(fir, sec, thir)) -> "CallFunction(" ^ str ^ ", "^ "Listarg("^ arglst_to_string arglst ^ ")Redirect(" ^fir ^", "^ sec^", " ^ thir ^"))"  (*name, parametrs*)
and arglst_to_string = function
| [] -> ""
| a::lis -> arg_to_string a ^ ", " ^ arglst_to_string lis
let option_expr_string = function
  | None -> "NONE"
  | Some (x) -> expr_to_string x
let ff = print_string "\n//////EXPR+ARG TEST////////////\n";;
(*cat .w123.txt > txt.txt*)
let (str:string) = "ss=(111 33 ww $(cat .w123.txt))";;
print_string (str ^ " <-> ");;
let ddddd = print_string (option_expr_string (apply pars_expr str));;

(*
and pipeOp = And | Or | Dpoint | Redi (*склейки пайпы || && ;*)
and pipe = (*не работает надо чет придумать*)
   | Expression of expr (*может или не может тут быть??*)
   | IfElse of expr * pipeConveyor * pipeConveyor (*if then else*)
   | While of expr * pipeConveyor (*clause, todoList*)
and pipeConveyor =
  | Pipline of pipeOp * pipe * pipeConveyor
  | SigPipe of pipe
*)
(*-------------------------------------Парсим PIPE--------------------------------------------------------------------*)

let rec pars_pipeConv input =
  ( (parse_pipeline <|> single_pipe_conveyr) >>= fun par -> return (par)) input
and single_pipe input =
  ((while_pars <|> ifelse_pars <|> expr_pipe) >>= fun par -> return (par)) input
and single_pipe_conveyr input =
  ((while_pars <|> ifelse_pars <|> expr_pipe) >>= fun par -> return (SigPipe(par))) input
and parse_pipeline input =
  ((p_dpoint <|> p_pipe) >>= fun par -> return(par)) input
and p_dpoint input = 
  (single_pipe >>= fun fir 
  -> spaces >> token ";" >> spaces >> pars_pipeConv >>= fun sec 
  -> return (Pipline (Dpoint, fir, sec))) input
and p_pipe input = 
  (single_pipe >>= fun fir 
  -> spaces >> token "|" >> spaces >> pars_pipeConv >>= fun sec 
  -> return (Pipline (Redi, fir, sec))) input
      and expr_pipe input =
        (pars_expr >>= fun ar -> return (Expression (ar))) input        
      and while_pars input =
        (token "while"   >> (* while *)
        spaces >> token "[" >>
        pars_expr         >>= fun pred ->
        spaces >> token "]" >>
        token "do" >> (* do *)
        pars_pipeConv     >>= fun act ->
        token "done"    >>
        return (While (pred, act))) input
      and ifelse_pars input =
        (token "if"   >> (* if *)
        pars_expr        >>= fun pred ->
        token "then" >> (* then *)
        pars_pipeConv   >>= fun thn ->
        token "else" >> (* else *)
        pars_pipeConv   >>= fun els ->
        token "fi"    >>
        return (IfElse (pred, thn, els))) input

let rec pipe_to_string pip =
  (*let rec list_string = function 
  [] -> ""
  | e::l -> e ^ " " ^ list_string l
  in*)
  match pip with
  | Expression (ar) -> "Expression(" ^ expr_to_string ar ^ ")"
  | While (pred, p) -> "While(" ^ expr_to_string pred ^ ", " ^ pipeConveyor_to_string p ^ ")"
  | IfElse (ar, l, r) -> "IFElse(" ^ expr_to_string ar ^ ", " ^ pipeConveyor_to_string l ^ ", " ^ pipeConveyor_to_string r ^ ")"
and pipeConveyor_to_string conv =
  match conv with
  | SigPipe (p) -> "SigPipe(" ^ pipe_to_string p ^ ")"
  | Pipline (Dpoint, l, r) -> "Pipeline(Dpoint, " ^ pipe_to_string l ^ ", " ^ pipeConveyor_to_string r ^ ")"
  | Pipline (Redi, l, r) -> "Pipeline(Redi, " ^ pipe_to_string l ^ ", " ^ pipeConveyor_to_string r ^ ")"
  | _ -> "did not done yet"
let option_pipeConveyr_string pip =
  match pip with
   None -> "None"
  | Some (p) -> pipeConveyor_to_string p
;;
(*while [ss=s] do pwd file.txt > my.txt; ss= 10+2 done*)
let ff = print_string "\n//////PIPE TEST////////////\n";;

let (str:string) = "echo www.txt ; cat w123.txt | echo yra";;
print_string (str ^ "\n <-> \n");;
let pp_str = "test () {pwd file.txt > my.txt; ss= 10+2}"
let ddddd = print_string (option_pipeConveyr_string (apply pars_pipeConv str));;

(*
type declFunct = 
  Method of string * pipeConveyor 
*)
(*-------------------------------------Парсим DECLFUNTION----------------------------------------------------------*)
let pars_declFunction input =
   (bash_ident  >>= fun nam ->
    spaces >> token "("  >>
    token ")" >> spaces  >>
    token "{" >> spaces >> 
    pars_pipeConv    >>= fun act ->
    spaces >> token "}"     >>
    return (Method (nam, act))) input

let declFunction_to_string dec =
  match dec with
  Method (name, act) -> "DeclFunct(" ^ name ^ ">" ^ pipeConveyor_to_string act ^ ")"
let option_declFunction_string = function
| None -> "NONE"
| Some (x) -> declFunction_to_string x

let ff = print_string "\n//////DeclFUNCTION TEST////////////\n";;

let (str:string) = "myfunction (){ echo www.txt ; cat w123.txt | echo yra }";;
print_string (str ^ "\n <-> \n");;
let ddddd = print_string (option_declFunction_string (apply pars_declFunction str));;

(*
type  bCommand = 
  | Pipe of pipe
  | DeclFunct of declFunct
type bCmdConv =
  | BConv of bCommand * bCmdConv (*конвеер функций и кода*)
  | BSigCmd of bCommand (*функция либо код*)
*)

(*------------------------------Парсим BCOMMAND и BCMDCONV--------------------------------------------------------*)

let rec pars_bComdConv input =
  (b_conv <|> bCmd_sig_cmd) input
  and b_sig_cmd input =
    (b_func <|> b_pipeConv) input
  and bCmd_sig_cmd input =
    (b_sig_cmd >>= fun bcmd -> return (BSigCmd(bcmd)) ) input
  and b_func input =
    (spaces >> pars_declFunction >>= fun fn -> return(DeclFunct(fn)) ) input
  and b_pipeConv input =
    (spaces >> pars_pipeConv >>= fun conv -> return (PipeConv(conv)) ) input
  and b_conv input =
  (b_sig_cmd >>= fun bcmd -> space >> pars_bComdConv >>= fun conv -> return(BConv(bcmd, conv)) ) input

let bCommand_to_string = function
| PipeConv x -> pipeConveyor_to_string x
| DeclFunct x -> declFunction_to_string x
let rec bCmdConv_to_string bcmd =
  match bcmd with 
  | BSigCmd x -> "BsigCmd(" ^ bCommand_to_string x ^ ")"
  | BConv (x, y) -> "BConv(" ^ bCommand_to_string x ^"<>"^ bCmdConv_to_string y ^ ")"
let option_bCmdConv_string = function
| None -> "NONE"
| Some x -> bCmdConv_to_string x


let ff = print_string "\n//////BCmd+BCommand TEST////////////\n";;
let (str:string) = "myfn (){echo eeee} echo rrr";; 
print_string (str ^ "\n <-> \n");;
let sds = apply pars_bComdConv str

let ddddd = print_string (option_bCmdConv_string (apply pars_bComdConv str));;

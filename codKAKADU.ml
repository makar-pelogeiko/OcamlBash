type expr =
  | Const of int
  | Plus of expr * expr
  | Slash of expr * expr
  | Asterisk of expr * expr
  | Var of string


let eval from_env : expr -> int option =
  let (>>=) x f = match x with None -> None | Some x -> f x in
  let return x = Some x in
  let rec helper = function
  | Const n -> return n
  | Plus  (l,r) -> helper l >>= fun l -> helper r >>= fun r -> return (l+r)
  | Asterisk (l,r) -> helper l >>= fun l -> helper r >>= fun r -> return (l * r)
  | Slash (l,r) ->
      helper r >>= fun r ->
        if r=0 then None
        else helper l >>= fun l -> return (l/r)
  | Var s -> from_env s
  in
  helper;;



  let int_string e =
    match e with
    Some x -> string_of_int x
    | None -> "none"
    ;;
  print_string (int_string (eval (fun s -> Some (int_of_string s)) (Plus (Const 1, Asterisk (Const 3, Const 3)))))

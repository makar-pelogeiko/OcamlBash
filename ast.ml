let rec fuu n =
  (
  if n > 0 then fuu (n - 1)
  else n
  );;

(*

print_string "Prime Partitions Program";;
*)
print_string "sddsd";;
Printf.printf "%d\n" (fuu 3000000);;
print_int (3 + 4+9);;

type exc = string

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end
module type MONADFAIL = sig
  include MONAD

  val fail : string -> 'a t
end

module type TRANSFORMER = sig
  type 'a m
  type 'a t

  val promote : 'a m -> 'a t
  val observe : 'a t -> 'a m
end

module type MAKE_TRANSFORMER = functor (M : MONADFAIL) -> sig
  include TRANSFORMER with type 'a m = 'a M.t
  include MONADFAIL with type 'a t := 'a t
end
module Identity : MONADFAIL with type 'a t = 'a = struct
  type 'a t = 'a

  let return x = x
  let ( >>= ) x f = f x
  let fail = failwith
end;;
(*----------------------------------------------------------------------------------*)

print_int (3 + 4+9);;
module RAISE (M : MONADFAIL) : sig
  include TRANSFORMER with type 'a m = 'a M.t
  include MONADFAIL with type 'a t := 'a t
end = struct
  (*
  Законы R1
  Законы O3
*)
  type 'a m = 'a M.t

  type 'a t =
    | Return : 'a -> 'a t
    | Bind : 'b t * ('b -> 'a t) -> 'a t
    | Raise : exc -> 'a t
    | Promote : 'a m -> 'a t

  let promote m = Promote m
  let return x = Return x
  let ( >>= ) x f = Bind (x, f)
  let fail x = Raise x

  let observe = function
    | Return a -> M.return a
    | Raise exc -> failwith exc
    | Promote m -> m
    | Bind (_x, _f) -> failwith "???"

  let rec observe = function
    | Return a -> M.return a
    | Raise exc -> M.fail exc
    | Promote m -> m
    | Bind (Return a, k) -> observe (k a)
    | Bind (Raise s, _) -> M.fail s
    | Bind (Promote m, k) -> M.( >>= ) m (fun x -> observe (k x))
    | Bind (Bind (m, k1), k2) -> observe (Bind (m, fun a -> k1 a >>= k2))

  (* Эта штука завершается, потому что первый аргумент Bind становится меньше *)

  (* Формально Bind (Return a, return) =/= Return a
       но с точки зрения наблюдателя -- да
     observe (Bind (Return a, return)) === observe (Return a)
  *)

  module N : sig end = struct
    (* Переход к следующему шагу *)
    let rec simplifier : 'a t -> 'a t = function
      | Return a -> Return a
      | Bind (Return a, k) -> simplifier (k a)
      | Bind (Bind (m, k1), k2) -> simplifier (m >>= fun a -> k1 a >>= k2)
      | Raise exc | Bind (Raise exc, _) -> Raise exc
      | Promote m ->
          Promote m (* Что эквивалентно promote m >>= return *)
      | Bind (Promote m, k) -> Promote m >>= fun x -> simplifier (k x)
  end
end
[@@warning "-37-32"]

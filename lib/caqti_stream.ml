module type S = sig
  type +'a future

  type ('a, 'err) t = unit -> ('a, 'err) node future
  and ('a, 'err) node =
    | Nil
    | Err of 'err
    | Cons of 'a * ('a, 'err) t

  val fold :
    f: ('a -> 'state -> 'state) ->
    ('a, 'err) t ->
    'state ->
    ('state, 'err) result future

  val fold_s :
    f: ('a -> 'state -> ('state, 'err) result future) ->
    ('a, 'err) t ->
    'state ->
    ('state, 'err) result future

  val iter_s :
    f:('a -> (unit, 'err) result future) ->
    ('a, 'err) t ->
    (unit, 'err) result future

  val to_rev_list : ('a, 'err) t -> ('a list, 'err) result future

  val to_list : ('a, 'err) t -> ('a list, 'err) result future
end

module type FUTURE = sig
  type +'a future

  val (>>=) : 'a future -> ('a -> 'b future) -> 'b future
  val (>|=) : 'a future -> ('a -> 'b) -> 'b future
  val return : 'a -> 'a future
end

module Make(X : FUTURE) : S with type 'a future := 'a X.future = struct
  open X

  let (>>=?) res_future f = res_future >>= function Ok a -> f a | Error _ as r -> return r
  let (>|=?) res_future f =
    res_future >>= function
    | Ok a -> return @@ Ok (f a)
    | Error _ as r -> return r

  type ('a, 'err) t = unit -> ('a, 'err) node future
  and ('a, 'err) node =
    | Nil
    | Err of 'err
    | Cons of 'a * ('a, 'err) t

  let rec fold ~f t state =
    t () >>= function
    | Nil -> return (Ok state)
    | Err err -> return (Error err)
    | Cons (a, t') -> fold ~f t' (f a state)

  let rec fold_s ~f t state =
    t () >>= function
    | Nil -> return (Ok state)
    | Err err -> return (Error err)
    | Cons (a, t') -> f a state >>=? fold_s ~f t'

  let rec iter_s ~f t =
    t () >>= function
    | Nil -> return (Ok ())
    | Err err -> return (Error err)
    | Cons (a, t') -> f a >>=? fun () -> iter_s ~f t'

  let to_rev_list t = fold ~f:List.cons t []

  let to_list t = to_rev_list t >|=? List.rev
end

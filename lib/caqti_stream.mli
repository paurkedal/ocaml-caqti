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

module Make (X : FUTURE) : S with type 'a future := 'a X.future

include Caqti_miou.System
include Caqti_miou
include Caqti_miou_unix

module Fiber = struct
  type 'a t = 'a

  let return = Fun.id
  let catch fn exnc = try fn () with exn -> exnc exn
  let fail = raise
  let bind x f = f x

  module Infix = struct
    let (>>=) x f = f x
    let (>|=) x f = f x
    let (>>=?) = Result.bind
    let (>|=?) x f = Result.map f x
  end
end

module Alcotest_cli =
  Testlib.Make_alcotest_cli
    (Alcotest.Unix_platform)
    (Alcotest_engine.Monad.Identity)

module List_result_fiber = struct
  open Fiber.Infix

  let rec iter_s f = function
   | [] -> Fiber.return (Ok ())
   | x :: xs -> f x >>=? fun () -> iter_s f xs
end

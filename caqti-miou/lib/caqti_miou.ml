open Caqti_platform
module Stream = System.Stream
module System = System

type stdenv = unit
type switch = System.Switch.t

module Pool = Caqti_platform.Pool.Make (System) (System.Alarm)
module Loader = Caqti_platform.Driver_loader.Make (System)

module type CONNECTION =
  Caqti_connection_sig.S
    with type 'a fiber := 'a
     and type ('a, 'e) stream := ('a, 'e) Stream.t

type connection = (module CONNECTION)

include Connector.Make (System) (Pool) (Loader)

let or_fail = function
  | Ok x -> x
  | Error (#Caqti_error.t as err) -> raise (Caqti_error.Exn err)

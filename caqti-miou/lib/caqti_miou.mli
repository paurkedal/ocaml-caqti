type stdenv = unit
type switch

module Stream : Caqti_stream_sig.S with type 'a fiber := 'a

module Pool : sig
  include Caqti_pool_sig.S with type 'a fiber := 'a

  val create :
    ?config: Caqti_pool_config.t ->
    ?check: ('a -> (bool -> unit) -> unit) ->
    ?validate: ('a -> bool) ->
    ?log_src: Logs.Src.t ->
    sw: switch ->
    stdenv: stdenv ->
    (unit -> ('a, 'e) result) -> ('a -> unit) ->
    ('a, 'e) t
end

module type CONNECTION =
  Caqti_connection_sig.S
    with type 'a fiber := 'a
     and type ('a, 'e) stream := ('a, 'e) Stream.t

include
  Caqti_connect_sig.S
    with type 'a fiber := 'a
     and type 'a with_switch := sw:switch -> 'a
     and type 'a with_stdenv := stdenv:stdenv -> 'a
     and type ('a, 'e) stream := ('a, 'e) Stream.t
     and type ('a, 'e) pool := ('a, 'e) Pool.t
     and type connection = (module CONNECTION)

val or_fail : ('a, [< Caqti_error.t ]) result -> 'a

(**/**) (* for private use by caqti-eio.unix *)
module System : Caqti_platform.System_sig.S
    with type 'a Fiber.t = 'a
     and type Switch.t = switch
     and type stdenv = unit
     and module Stream = Stream
     and type Net.tcp_flow = Miou_unix.file_descr

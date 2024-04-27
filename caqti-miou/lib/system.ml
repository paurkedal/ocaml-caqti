open Caqti_platform

type Caqti_error.msg +=
  | Msg_unix of Unix.error * string * string
  | Invalid_arg of string

let () =
  let pp ppf = function
    | Msg_unix (err, f, v) -> Fmt.pf ppf "%s(%s): %s" f v (Unix.error_message err)
    | _ -> assert false in
  Caqti_error.define_msg ~pp [%extension_constructor Msg_unix]

let () =
  let pp ppf = function
    | Invalid_arg msg -> Fmt.string ppf msg
    | _ -> assert false in
  Caqti_error.define_msg ~pp [%extension_constructor Invalid_arg]

external reraise : exn -> 'a = "%reraise"

module Fiber = struct
  type 'a t = 'a

  module Infix = struct
    let ( >>= ) x f = f x
    let ( >|= ) x f = f x
  end

  let return x = x
  let catch f g = try f () with exn -> g exn
  let finally f finally = Fun.protect ~finally f
  let cleanup f g = try f () with exn -> g (); raise exn
end

module Stream = Caqti_platform.Stream.Make (Fiber)

module Semaphore = struct
  type t = {
    mutable value: int;
    mutable wakeups: int;
    mutex: Miou.Mutex.t;
    condition: Miou.Condition.t;
  }

  let create () =
    {
      value= 1;
      wakeups= 0;
      mutex= Miou.Mutex.create ();
      condition= Miou.Condition.create ();
    }

  let acquire t =
    Miou.Mutex.protect t.mutex @@ fun () ->
    while t.value <= 0 do
      Miou.Condition.wait t.condition t.mutex
    done;
    t.value <- t.value - 1

  let release t =
    Miou.Mutex.protect t.mutex @@ fun () ->
    t.value <- t.value + 1;
    Miou.Condition.signal t.condition
end

module Log = struct
  type 'a log = 'a Logs.log

  let err ?(src = Logging.default_log_src) = Logs.err ~src
  let warn ?(src = Logging.default_log_src) = Logs.warn ~src
  let info ?(src = Logging.default_log_src) = Logs.info ~src
  let debug ?(src = Logging.default_log_src) = Logs.debug ~src
end

type stdenv = unit

module Sequencer = struct
  type 'a t = 'a * Miou.Mutex.t

  let create v = (v, Miou.Mutex.create ())
  let enqueue (v, m) f = Miou.Mutex.protect m (fun () -> f v)
end

type _ Effect.t += Spawn : (unit -> unit) -> unit Effect.t

module Switch = struct
  type hook = (unit -> unit) Miou.Sequence.node
  type t = { orphans: unit Miou.orphans; hooks: (unit -> unit) Miou.Sequence.t }

  let peek : type a. a Miou.Sequence.t -> a Miou.Sequence.node =
   fun hooks ->
    let exception Node of a Miou.Sequence.node in
    try
      Miou.Sequence.iter_node ~f:(fun node -> raise (Node node)) hooks;
      invalid_arg "peek"
    with Node node -> node

  let release hooks =
    List.iter (fun fn -> fn ()) (Miou.Sequence.to_list hooks);
    Miou.Sequence.drop hooks

  let rec terminate orphans =
    match Miou.care orphans with
    | None -> ()
    | Some None -> Miou.yield (); terminate orphans
    | Some (Some prm) -> (
        match Miou.await prm with
        | Ok () -> terminate orphans
        | Error _exn -> terminate orphans)

  let run fn =
    let orphans = Miou.orphans () in
    let hooks = Miou.Sequence.create () in
    let open Effect.Deep in
    let retc = Fun.id in
    let exnc = reraise in
    let effc : type c. c Effect.t -> ((c, 'r) continuation -> 'r) option =
      function
      | Spawn fn ->
          let _ = Miou.call_cc ~orphans fn in
          Some (fun k -> continue k ())
      | _ -> None
    in
    let handler = { retc; exnc; effc } in
    match match_with fn { orphans; hooks } handler with
    | value -> terminate orphans; release hooks; value
    | exception exn -> terminate orphans; release hooks; raise exn

  let on_release_cancellable { hooks; _ } fn =
    Miou.Sequence.(add Left) hooks fn

  let remove_hook hook = Miou.Sequence.remove hook

  let check { orphans; _ } =
    match Miou.care orphans with
    | None -> ()
    | Some None -> ()
    | Some (Some prm) -> (
        match Miou.await prm with Ok () -> () | Error _exn -> ())
end

module Alarm = struct
  type t = Miou.Condition.t * Miou.Mutex.t

  let schedule ~sw:{ Switch.orphans; _ } ~stdenv:_ t fn =
    let t_now = Mtime_clock.now () in
    let mutex = Miou.Mutex.create () and condition = Miou.Condition.create () in
    let delay =
      if Mtime.is_later t ~than:t_now then 0.0
      else Mtime.Span.to_float_ns (Mtime.span t t_now) *. 1e-9
    in
    let sleeper = Miou.call_cc @@ fun () -> Miou_unix.sleep delay; `Continue in
    let canceller =
      Miou.call_cc @@ fun () ->
      Miou.Condition.wait condition mutex;
      `Cancel
    in
    let _prm =
      Miou.call_cc ~orphans @@ fun () ->
      match Miou.await_first [ sleeper; canceller ] with
      | Ok `Continue -> fn ()
      | Ok `Cancel -> ()
      | Error _exn -> ()
    in
    (condition, mutex)

  let unschedule (condition, mutex) =
    Miou.Mutex.protect mutex @@ fun () -> Miou.Condition.signal condition
end

let async ~sw:_ fn = Effect.perform (Spawn fn)
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

module Net = struct
  module Sockaddr = struct
    type t = Unix.sockaddr

    let unix v = Unix.ADDR_UNIX v
    let tcp (addr, port) = Unix.ADDR_INET (Ipaddr_unix.to_inet_addr addr, port)
  end

  let getaddrinfo ~stdenv:() host port =
    let opts = Unix.[ AI_SOCKTYPE SOCK_STREAM ] in
    match
      Unix.getaddrinfo (Domain_name.to_string host) (string_of_int port) opts
    with
    | lst -> Ok (List.map (fun ai -> ai.Unix.ai_addr) lst)
    | exception Not_found -> Ok []
    | exception Unix.Unix_error (err, f, v) ->
        error_msgf "%s(%s): %s" f v (Unix.error_message err)

  let convert_io_exception = function
    | Unix.Unix_error (err, f, v) -> Some (Msg_unix (err, f, v))
    | _ -> None

  type tcp_flow = Miou_unix.file_descr
  type tls_flow = |

  module Socket = struct
    type t = Miou_unix.file_descr

    let output_char fd chr = Miou_unix.write fd (String.make 1 chr) 0 1
    let output_string fd str = Miou_unix.write fd str 0 (String.length str)
    let flush _ = ()

    let input_char fd =
      let buf = Bytes.make 1 '\000' in
      let len = Miou_unix.read fd buf 0 1 in
      if len = 0 then raise End_of_file else Bytes.get buf 0

    let really_input fd buf off len =
      let rec go off = function
        | 0 -> ()
        | len ->
            let len' = Miou_unix.read fd buf off len in
            go (off + len') (len - len')
      in
      go off len

    let close fd = Miou_unix.close fd
  end

  let socket = function
    | Unix.ADDR_UNIX _ -> Error (Invalid_arg "Caqti_miou.Net.connect_tcp")
    | Unix.ADDR_INET (inet_addr, _) when Unix.is_inet6_addr inet_addr ->
        Ok (Miou_unix.tcpv6 ())
    | _ -> Ok (Miou_unix.tcpv4 ())

  let connect_tcp ~sw:_ ~stdenv:_ sockaddr =
    let ( >>= ) = Result.bind in
    socket sockaddr >>= fun socket ->
    match Miou_unix.connect socket sockaddr with
    | () -> Ok socket
    | exception Unix.Unix_error (err, f, v) ->
        Miou_unix.close socket;
        Error (Msg_unix (err, f, v))
    | exception exn -> Miou_unix.close socket; raise exn

  let tcp_flow_of_socket fd = Some fd

  let socket_of_tls_flow : sw:_ -> tls_flow -> Socket.t =
   fun ~sw:_ -> function _ -> .

  module type TLS_PROVIDER =
    Caqti_platform.System_sig.TLS_PROVIDER
      with type 'a fiber := 'a
       and type tcp_flow := tcp_flow
       and type tls_flow := tls_flow

  let tls_providers _ = []
  let register_tls_provider _ = ()
end

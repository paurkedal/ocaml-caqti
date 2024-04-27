include Caqti_miou.System

module Unix = struct
  type file_descr = Miou_unix.file_descr

  let wrap_fd f fd = f (Miou_unix.of_file_descr ~non_blocking:true fd)

  exception Timeout
  let or_raise = function Ok v -> v | Error exn -> raise exn

  let poll ~stdenv:() ?(read= false) ?(write= false) ?timeout fd =
    let fn () = match read, write with
      | false, false -> (false, false, false)
      | true, true ->
        let reader = Miou.call_cc @@ fun () ->
          Miou_unix.(blocking_read (to_file_descr fd));
          (true, false, false) in
        let writer = Miou.call_cc @@ fun () ->
          Miou_unix.(blocking_write (to_file_descr fd));
          (false, true, false) in
        Miou.await_first [ reader; writer ] |> or_raise
      | true, false ->
        Miou_unix.(blocking_read (to_file_descr fd)); (true, false, false)
      | false, true ->
        Miou_unix.(blocking_write (to_file_descr fd)); (false, true, false) in
    match timeout with
    | None -> fn ()
    | Some t ->
      let sleep = Miou.call_cc @@ fun () -> Miou_unix.sleep t; raise Timeout in
      match Miou.await_first [ sleep; Miou.call_cc fn ] with
      | Ok v -> v
      | Error Timeout -> (false, false, true)
      | Error exn -> raise exn
end

module Preemptive = struct
  let detach f x = Miou.await_exn @@ Miou.call_cc @@ fun () -> f x
  let run_in_main fn = fn ()
end

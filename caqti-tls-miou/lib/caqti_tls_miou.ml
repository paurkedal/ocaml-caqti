(* Copyright (C) 2023--2024  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

open Caqti_miou_unix

module TLS_provider = struct
  type tls_config = Tls.Config.client
  let tls_config_key = Caqti_tls.Config.client

  module Flow = struct
    type t =
      { fd : Tls_miou_unix.t
      ; queue : (char, Bigarray.int8_unsigned_elt) Ke.Rke.Weighted.t }
    type bigstring =
      (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

    type 'a fiber = 'a

    external get_int32 : bigstring -> int -> int32 = "%caml_bigstring_get32"

    let blit_to_bytes src src_off dst dst_off len =
      let len0 = len land 3 in
      let len1 = len asr 2 in

      for i = 0 to len1 - 1 do
        let i = i * 4 in
        let v = get_int32 src (src_off + i) in
        Bytes.set_int32_ne dst (dst_off + i) v
      done;
      for i = 0 to len0 - 1 do
        let i = (len1 * 4) + i in
        let v = Bigarray.Array1.get src (src_off + i) in
        Bytes.set dst (dst_off + i) v
      done

    let flush t =
      Ke.Rke.Weighted.compress t.queue;
      match Ke.Rke.Weighted.N.peek t.queue with
      | [] -> ()
      | ba :: _ ->
          let rec split acc ba off len =
            if len > 0 then begin
              let max = min len 0x7ff in
              let buf = Bytes.create max in
              blit_to_bytes ba off buf 0 max;
              split (Bytes.unsafe_to_string buf :: acc) ba (off + max) (len - max)
            end else List.rev acc in
          let len = Bigarray.Array1.dim ba in
          let sstr = split [] ba 0 len in
          List.iter (Tls_miou_unix.write t.fd) sstr;
          Ke.Rke.Weighted.N.shift_exn t.queue len

    let output_char t chr =
      match Ke.Rke.Weighted.push t.queue chr with
      | Some () -> ()
      | None -> flush t; Ke.Rke.Weighted.push_exn t.queue chr

    external set_int8 : bigstring -> int -> int -> unit = "%caml_ba_set_1"
    external set_int32 : bigstring -> int -> int32 -> unit
      = "%caml_bigstring_set32"

    let blit src src_off dst dst_off len =
      let len0 = len land 3 in
      let len1 = len asr 2 in

      for i = 0 to len1 - 1 do
        let i = i * 4 in
        let v = String.get_int32_ne src (src_off + i) in
        set_int32 dst (dst_off + i) v
      done;
      for i = 0 to len0 - 1 do
        let i = (len1 * 4) + i in
        let v = String.get_uint8 src (src_off + i) in
        set_int8 dst (dst_off + i) v
      done

    let output_string t str =
      let rec go str off len =
        if len > 0 then
          let max = min (Ke.Rke.Weighted.available t.queue) len in
          if max > 0 then begin
            let _ = Ke.Rke.Weighted.N.push t.queue
              ~blit ~length:String.length ~off ~len:max str in
            if Ke.Rke.Weighted.available t.queue = 0
            then flush t;
            go str (off + max) (len - max)
          end else begin flush t; go str off len end in
      go str 0 (String.length str)

    let input_char { fd; _ } =
      let buf = Bytes.make 1 '\000' in
      let len = Tls_miou_unix.read fd buf in
      if len = 0 then raise End_of_file else Bytes.get buf 0

    let really_input { fd; _ } buf off len =
      if len > 0
      then Tls_miou_unix.really_read fd buf ~off ~len

    let close { fd; _ } = Tls_miou_unix.close fd
  end

  let start_tls ~config ?host tcp_flow =
    let fd = Tls_miou_unix.client_of_fd config ?host tcp_flow in
    let v =
      { Flow.fd; queue= fst (Ke.Rke.Weighted.create ~capacity:0x1000 Bigarray.char) } in
    Ok (System.OCaml ((module Flow), v))
end

let () = System.Net.register_tls_provider (module TLS_provider)

(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

(** Signatures. *)

open Caqti_describe
open Caqti_metadata
open Caqti_query

(** Parameter encoding functions. *)
module type PARAM = sig
  type t

  val null : t
  (** For SQL, [null] is [NULL]. *)

  val option : ('a -> t) -> 'a option -> t
  (** [option f None] is [null] and [option f (Some x)] is [f x]. *)

  val bool : bool -> t
  (** Constructs a boolean parameter. *)

  val int : int -> t
  (** Constructs an integer parameter. The remote end may have a different
      range. For SQL, works with all integer types. *)

  val int32 : int32 -> t
  (** Constructs an integer parameter. The remote end may have a different
      range. For SQL, works with all integer types. *)

  val int64 : int64 -> t
  (** Constructs an integer parameter. The remote end may have a different
      range. For SQL, works with all integer types. *)

  val float : float -> t
  (** Constructs a floating point parameter. The precision of the storage
      may be different from that of the OCaml [float]. *)

  val text : string -> t
  (** Given an UTF-8 encoded text, constructs a textual parameter with
      backend-specific encoding. *)

  val octets : string -> t
  (** Constructs a parameter from an arbitrary [string].  For SQL, the
      parameter is compatible with the [BINARY] type. *)

  val date : CalendarLib.Date.t -> t
  (** Construct a parameter representing a date. *)

  val utc : CalendarLib.Calendar.t -> t
  (** Construct a parameter representing an UTC time value.  Selecting a
      time zone suitable for an end-user is left to the application. *)

  val other : string -> t
  (** A backend-specific value. *)
end

(** Tuple decoding functions.

    These functions extracts and decodes components from a returned tuple.
    The first argument is the index, starting from 0.  The conversion
    performed are the inverse of the same named functions of {!PARAM}, so the
    documentation is not repeated here.

    {b Note!}  Calls to these functions are only valid during a callback
    from one of the query execution functions.  Returning a partial call or
    embedding the call in a returned monad leads to undefined behaviour. *)
module type TUPLE = sig
  type t
  val is_null : int -> t -> bool
  val option : (int -> t -> 'a) -> int -> t -> 'a option
  val bool : int -> t -> bool
  val int : int -> t -> int
  val int32 : int -> t -> int32
  val int64 : int -> t -> int64
  val float : int -> t -> float
  val text : int -> t -> string
  val octets : int -> t -> string
  val date : int -> t -> CalendarLib.Date.t
  val utc : int -> t -> CalendarLib.Calendar.t
  val other : int -> t -> string
end

module type REPORT = sig
  type t
  val returned_count : (t -> int) option
  val affected_count : (t -> int) option
end

module type WRAPPER = functor (Tuple : TUPLE) -> functor (Report : REPORT) ->
sig
  type 'a callback
  type queried
  type reported
  val on_query : query -> queried
  val on_report : queried -> Report.t -> reported
  val on_tuple : 'a callback -> reported -> Tuple.t -> 'a
end

(** The main API as provided after connecting to a resource.  In addition to
    these components, the connection will provide
    {[
      module Param : PARAM
      module Report : REPORT
      module Tuple : TUPLE
    ]} *)
module type CONNECTION_BASE = sig

  type 'a io
  (** The IO monad for which the module is specialized. *)

  type param
  (** An abstract type for a query parameter.  Only the backend knows the
      actual type. *)

  type tuple
  (** An abstract type for a tuple passed by a backend to callbacks during
      query execution. *)

  type 'a callback
  (** The form of callbacks.  For the default connection functions, this type
      is [tuple -> 'a]. *)

  val uri : Uri.t
  (** The connected URI. *)

  val backend_info : backend_info
  (** Various metadata about the backend which provides the connection. *)

  val disconnect : unit -> unit io
  (** Calling [disconnect ()] closes the connection to the database and frees
      up related resources. *)

  val validate : unit -> bool io
  (** For internal use by {!Caqti_pool}.  Tries to ensure the validity of the
      connection and must return [false] if unsuccessful. *)

  val check : (bool -> unit) -> unit
  (** For internal use by {!Caqti_pool}.  Called after a connection has been
      used.  [check f] must call [f ()] exactly once with an argument
      indicating whether to keep the connection in the pool or discard it. *)

  val describe : query -> querydesc io
  (** Returns a description of parameters and returned tuples.  What is
      returned may be limited by what the underlying library supports.  The
      number of paratemers and tuple components should be correct, but the
      types may be [`Unknown]. *)

  (** {3 Querying } *)

  val exec : query -> param array -> unit io
  (** [exec q params] executes a query [q(params)] which is not expected to
      return anything. *)

  val find : query -> 'a callback -> param array -> 'a io
  (** [find_e q params] executes [q(params)] which is expected to return
      exactly one tuple. *)

  val find_opt : query -> 'a callback -> param array -> 'a option io
  (** [find q params] executes a query [q(params)] which is expected to return
      at most one tuple. *)

  val fold : query -> ('a -> 'a) callback -> param array -> 'a -> 'a io
  (** [fold q f params acc] executes [q(params)], composes [f] over the
      resulting tuples in order, and applies the composition to [acc]. *)

  val fold_s : query -> ('a -> 'a io) callback -> param array -> 'a -> 'a io
  (** [fold_s q f params acc] executes [q(params)], forms a threaded
      composition of [f] over the resulting tuples, and applies the
      composition to [acc]. *)

  val iter_p : query -> unit io callback -> param array -> unit io
  (** [fold_p q f params] executes [q(params)] and calls [f t] in the thread
      monad in parallel for each resulting tuple [t].  A certain backend may
      not implement parallel execution, in which case this is the same as
      {!iter_s}. *)

  val iter_s : query -> unit io callback -> param array -> unit io
  (** [fold_s q f params] executes [q(params)] and calls [f t] sequentially in
      the thread monad for each resulting tuple [t] in order. *)

end

(** The signature of pools of reusable resources.  We use it to keep pools of
    open database connections.  A generic implementation is found in
    {!Caqti_pool}, and instantiations are available for each cooperative
    thread monad. *)
module type POOL = sig
  type 'a io
  type 'a t
  val create :
	?max_size: int ->
	?check: ('a -> (bool -> unit) -> unit) ->
	?validate: ('a -> bool io) ->
	(unit -> 'a io) -> ('a -> unit io) -> 'a t
  val size : 'a t -> int
  val use : ?priority: float -> ('a -> 'b io) -> 'a t -> 'b io
  val drain : 'a t -> unit io
end

(** The IO monad and system utilities used by backends.  Note that this
    signature will likely be extended due requirements of new backends. *)
module type SYSTEM = sig

  type 'a io
  val (>>=) : 'a io -> ('a -> 'b io) -> 'b io
  val return : 'a -> 'a io
  val fail : exn -> 'a io
  val catch : (unit -> 'a io) -> (exn -> 'a io) -> 'a io
  val join : unit io list -> unit io

  module Mvar : sig
    type 'a t
    val create : unit -> 'a t
    val store : 'a -> 'a t -> unit
    val fetch : 'a t -> 'a io
  end

  module Unix : sig
    type file_descr
    val wrap_fd : (file_descr -> unit io) -> Unix.file_descr -> unit io
    val wait_read : file_descr -> unit io
    val wait_write : file_descr -> unit io
  end

  module Log : sig
    val error_f : query -> ('a, unit, string, unit io) format4 -> 'a
    val warning_f : query -> ('a, unit, string, unit io) format4 -> 'a
    val info_f : query -> ('a, unit, string, unit io) format4 -> 'a
    val debug_f : query -> ('a, unit, string, unit io) format4 -> 'a
    val debug_query_enabled : unit -> bool
    val debug_query : query_info -> string list -> unit io
    val debug_tuple_enabled : unit -> bool
    val debug_tuple : string list -> unit io
  end

  module Preemptive : sig
    val detach : ('a -> 'b) -> 'a -> 'b io
    val run_in_main : (unit -> 'a io) -> 'a
  end

end

(** The part of {!CAQTI} which is implemented by backends. *)
module type CAQTUS = sig
  type 'a io
  module Wrap (Wrapper : WRAPPER) : sig
    module type CONNECTION = sig
      module Param : PARAM
      module Tuple : TUPLE
      module Report : REPORT
      include CONNECTION_BASE
	 with type 'a io = 'a io
	  and type param = Param.t
	  and type tuple = Tuple.t
	  and type 'a callback = 'a Wrapper (Tuple) (Report).callback
    end
    val connect : Uri.t -> (module CONNECTION) io
  end
end

(** Abstraction of the connect function over the concurrency monad. *)
module type CAQTUS_FUNCTOR =
  functor (System : SYSTEM) -> CAQTUS with type 'a io := 'a System.io

(** The connect functions as exposed to application code through the
    concurrency implementations:

    - [Caqti_lwt] which is provided in the [caqti.lwt] package
      if caqti was built with [--enable-lwt].
    - [Caqti_async] which is provided in the [caqti.async] package
      if caqti was built with [--enable-async]. *)
module type CAQTI = sig
  module System : SYSTEM

  module Pool : POOL with type 'a io := 'a System.io
  (** This is an instantiation of {!Caqti_pool} for the chosen thread monad. *)

  module type CONNECTION = sig
    module Param : PARAM
    module Tuple : TUPLE
    module Report : REPORT
    include CONNECTION_BASE
       with type 'a io = 'a System.io
	and type param = Param.t
	and type tuple = Tuple.t
	and type 'a callback = Tuple.t -> 'a
  end

  val connect : Uri.t -> (module CONNECTION) System.io
  (** Establish a single connection to a database.  This must only be used by
      one thread at a time, cooperative or not. *)

  val connect_pool : ?max_size: int -> Uri.t -> (module CONNECTION) Pool.t
  (** Create a pool of connections which can be shared among multiple
      cooperative threads run from the main system thread. *)

  module Wrap (Wrapper : WRAPPER) : sig
    module type CONNECTION = sig
      module Param : PARAM
      module Tuple : TUPLE
      module Report : REPORT
      include CONNECTION_BASE
	 with type 'a io = 'a System.io
	  and type tuple = Tuple.t
	  and type 'a callback = 'a Wrapper (Tuple) (Report).callback
    end
    val connect : Uri.t -> (module CONNECTION) System.io
  end
end

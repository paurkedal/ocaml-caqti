(* Copyright (C) 2014--2017  Petter A. Urkedal <paurkedal@gmail.com>
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
open Caqti_query

(** Parameter encoding functions. *)
module type PARAM = sig
  type t
  (** An abstract type for a query parameter.  Only the backend knows the
      actual type. *)

  val null : t
  (** A designated value to replace a missing parameter. For SQL, [null] is
      [NULL]. *)

  val option : ('a -> t) -> 'a option -> t
  (** [option f None] is [null] and [option f (Some x)] is [f x]. *)

  val bool : bool -> t
  (** Constructs a boolean parameter. If the database does not have booleans, an
      integer value of 0 for false and 1 for true is used. *)

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
  (** Constructs a floating point parameter. Note that the precision in the
      database may be different from that of the OCaml [float]. If conversion to
      string is requried by the backend, this is done with the [%.*g] format
      specifier, which may incur a small loss of precision, as well. *)

  val string : string -> t
  (** Given an UTF-8 encoded text, constructs a textual parameter with
      backend-specific encoding. *)

  val bytes : bytes -> t
  (** Constructs a parameter from an arbirary octet string.  For SQL, the
      parameter is compatible with the [BINARY] type. *)

  val date_string : string -> t
  (** Construct a date paramater from a string using ISO 8601 format. *)

  val date_tuple : int * int * int -> t
  (** Construct a date parameter from the year, month, and day of the month,
      using the literal enumeration. I.e. Epoch is [(1970, 1, 1)]. *)

  val date_cl : CalendarLib.Date.t -> t
  (** Construct a parameter representing a date. *)

  val utc_float : float -> t
  (** Construct a parameter representing date and time in the UTC time zone,
      converted from a float representing the number of non-leap seconds passed
      since 1970-01-01T00:00:00Z. *)

  val utc_string : string -> t
  (** Create a parameter for an UTC timestamp field from a UTC time string in
      ISO 8601 format. *)

  val utc_cl : CalendarLib.Calendar.t -> t
  (** Construct a parameter representing an date and time in the UTC time
      zone. *)
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
  (** An abstract type for a tuple passed by a backend to callbacks during
      query execution. *)

  val length : t -> int

  val is_null : int -> t -> bool
  val option : (int -> t -> 'a) -> int -> t -> 'a option
  val bool : int -> t -> bool
  val int : int -> t -> int
  val int32 : int -> t -> int32
  val int64 : int -> t -> int64
  val float : int -> t -> float
  val string : int -> t -> string
  val bytes : int -> t -> bytes
  val date_string : int -> t -> string
  val date_tuple : int -> t -> int * int * int
  val date_cl : int -> t -> CalendarLib.Date.t
  val utc_float : int -> t -> float
  val utc_string : int -> t -> string
  val utc_cl : int -> t -> CalendarLib.Calendar.t
end

(** The main API as provided after connecting to a resource. *)
module type CONNECTION = sig

  module Param : PARAM
  (** Interface for constructing query parameters. *)

  module Tuple : TUPLE
  (** Interface for extracting result tuples. *)

  type 'a io
  (** The IO monad for which the module is specialized. *)

  val uri : Uri.t
  (** The URI used to connect to the database. *)

  val driver_info : Caqti_driver_info.t
  (** Information about the database driver which provides this connection. *)

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

  val describe : (query -> querydesc io) option
  (** Returns a description of parameters and returned tuples.  What is
      returned may be limited by what the underlying library supports.  The
      number of paratemers and tuple components should be correct, but the
      types may be [`Unknown]. *)

  (** {3 Querying} *)

  val exec : query -> Param.t array -> unit io
  (** [exec q params] executes a query [q(params)] which is not expected to
      return anything. *)

  val find : query -> (Tuple.t -> 'a) -> Param.t array -> 'a io
  (** [find_e q params] executes [q(params)] which is expected to return
      exactly one tuple. *)

  val find_opt : query -> (Tuple.t -> 'a) -> Param.t array -> 'a option io
  (** [find q params] executes a query [q(params)] which is expected to return
      at most one tuple. *)

  val fold : query -> (Tuple.t -> 'a -> 'a) -> Param.t array -> 'a -> 'a io
  (** [fold q f params acc] executes [q(params)], composes [f] over the
      resulting tuples in order, and applies the composition to [acc]. *)

  val fold_s : query -> (Tuple.t -> 'a -> 'a io) -> Param.t array -> 'a -> 'a io
  (** [fold_s q f params acc] executes [q(params)], forms a threaded
      composition of [f] over the resulting tuples, and applies the
      composition to [acc]. *)

  val iter_p : query -> (Tuple.t -> unit io) -> Param.t array -> unit io
  (** [fold_p q f params] executes [q(params)] and calls [f t] in the thread
      monad in parallel for each resulting tuple [t].  A certain backend may
      not implement parallel execution, in which case this is the same as
      {!iter_s}. *)

  val iter_s : query -> (Tuple.t -> unit io) -> Param.t array -> unit io
  (** [fold_s q f params] executes [q(params)] and calls [f t] sequentially in
      the thread monad for each resulting tuple [t] in order. *)

  (** {3 Transactions} *)

  val start : unit -> unit io
  (** Starts a transaction if supported by the underlying database, otherwise
      does nothing. *)

  val commit : unit -> unit io
  (** Commits the current transaction if supported by the underlying database,
      otherwise does nothing. *)

  val rollback : unit -> unit io
  (** Rolls back a transaction if supported by the underlying database,
      otherwise does nothing. *)
end

(** The connect functions as exposed to application code through the
    concurrency implementations:

    - [Caqti_lwt] provided by caqti-lwt.
    - [Caqti_async] provided by caqti-async. *)
module type CAQTI = sig
  type 'a io

  module Pool : Caqti_pool_sig.S with type 'a io := 'a io
  (** This is an instantiation of {!Caqti_pool} for the chosen thread monad. *)

  module type CONNECTION = CONNECTION with type 'a io = 'a io

  val connect : Uri.t -> (module CONNECTION) io
  (** Establish a single connection to a database.  This must only be used by
      one thread at a time, cooperative or not. *)

  val connect_pool : ?max_size: int -> Uri.t -> (module CONNECTION) Pool.t
  (** Create a pool of connections which can be shared among multiple
      cooperative threads run from the main system thread. *)

  module type CONNECTION_V2 =
    Caqti_connection_sig.S with type 'a io := 'a io

  val connect_v2 : Uri.t -> (module CONNECTION_V2) io
  val connect_pool_v2 : ?max_size: int -> Uri.t -> (module CONNECTION_V2) Pool.t
end

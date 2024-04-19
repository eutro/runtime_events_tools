(** Write files in Fuchsia trace format: https://fuchsia.dev/fuchsia-src/reference/tracing/trace-format *)

module type Writer = sig
  type t

  val string : t -> ?len:int -> string -> unit
  val le_uint64 : t -> Int64.t -> unit
end

module Make : functor (W : Writer) -> sig
  type t

  type arg =
    [ `Unit
    | `Int64 of int64
    | `Pointer of int64
    | `Koid of int64
    | `String of string ]

  type args = (string * arg) list
  type thread = { pid : int64; tid : int64 }

  val of_writer : W.t -> t

  val instant_event :
    ?args:args ->
    t ->
    name:string ->
    thread:thread ->
    category:string ->
    ts:int64 ->
    unit

  val counter_event :
    id:int64 ->
    ?args:args ->
    t ->
    name:string ->
    thread:thread ->
    category:string ->
    ts:int64 ->
    unit

  val duration_begin :
    ?args:args ->
    t ->
    name:string ->
    thread:thread ->
    category:string ->
    ts:int64 ->
    unit

  val duration_end :
    ?args:args ->
    t ->
    name:string ->
    thread:thread ->
    category:string ->
    ts:int64 ->
    unit

  val user_object :
    ?args:args -> t -> name:string -> thread:thread -> int64 -> unit

  val kernel_object :
    ?args:args -> t -> name:string -> [ `Thread ] -> int64 -> unit

  val thread_wakeup : ?args:args -> t -> cpu:int -> ts:int64 -> int64 -> unit
end

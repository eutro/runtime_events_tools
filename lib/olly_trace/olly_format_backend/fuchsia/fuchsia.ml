open Bit_writer

(* Reference: https://fuchsia.dev/fuchsia-src/reference/tracing/trace-format

   Summary:

   {ul
   {- The whole file to emit is a trace archive, (section Archives), a
   sequence of concatenated trace {i records}}
   {- Timestamps are 64-bit ticks; the trace initialisation record
   describes the number of ticks per second.}
   {- We currently store:
   {ul
     {- Named Spans (custom or runtime-internal)}
     {- Counters (custom or runtime-internal)}
     {- Instants (custom only)}}}

   {- Event names are commonly repeated, so we emit string records
   so that we don't have to write them inline}

   {- Similarly, we emit a string record whenever we run into a new
   [ring_id]}}
*)
type trace = {
  mutable ring_max : int;
  string_table : (string, int) Hashtbl.t;
  mutable next_stringref : int;
  writer : Bit_writer.t;
}

let create ~filename =
  {
    ring_max = 0;
    string_table = Hashtbl.create 32;
    next_stringref = 1;
    writer = Bit_writer.create (open_out_bin filename);
  }

let close t =
  pad_to_word t.writer;
  Bit_writer.close t.writer

let num_words num_bytes =
  let divf = num_bytes / 8 in
  if num_bytes mod 8 == 0 then divf else divf + 1

let ticks_per_second = 1000_000_000
(* nanoseconds, conveniently what RTE gives us already *)

let magic_number = 0x0016547846040010L
let write_magic_number_record t = write_int64 t.writer magic_number

let write_init_record t =
  write_int64 t.writer
    Word.((* type = init (1) ; size = 2 words *)
          bits 0 3 ~v:1 -- bits 4 15 ~v:2);
  write_int64 t.writer (Int64.of_int ticks_per_second)

let write_provider_info_record t ~name ~id =
  let name_len = String.length name in
  let name_words = num_words name_len in
  let size = 1 + name_words in
  write_int64 t.writer
    Word.(
      (* type = meta (0) ; size = 2 *)
      bits 0 3 ~v:0 -- bits 4 15 ~v:size
      (* metadata type = provider info (0) *)
      -- bits 16 19 ~v:0
      (* provider id, arbitrary token *)
      -- bits 20 51 ~v:id
      -- bits 52 59 ~v:name_len);
  write_string t.writer name;
  pad_to_word t.writer

let write_section_record t ~id =
  write_int64 t.writer
    Word.(
      (* type = meta (0) ; size = 1 *)
      bits 0 3 ~v:0 -- bits 4 15 ~v:1
      (* metadata type = 2 *)
      -- bits 16 19 ~v:0
      (* provider id *)
      -- bits 20 51 ~v:id)

exception Bad_utf8

let write_string_record t idx str =
  if not (String.is_valid_utf_8 str) then raise Bad_utf8;
  let len_bytes = String.length str in
  let size = num_words len_bytes + 1 in
  write_int64 t.writer
    Word.(
      (* type = string (1) ; size = N words *)
      bits 0 3 ~v:2 -- bits 4 15 ~v:size
      (* string index *)
      -- bits 16 30 ~v:idx
      (* string length *)
      -- bits 32 46 ~v:len_bytes);
  write_string t.writer str;
  pad_to_word t.writer

let write_thread_record t idx ~pid ~tid =
  write_int64 t.writer
    Word.(
      (* type = thread (3) ; size = 3 words *)
      bits 0 3 ~v:3 -- bits 4 15 ~v:3 -- bits 16 23 ~v:idx);
  write_int64 t.writer pid;
  write_int64 t.writer tid

let get_string t str =
  try Hashtbl.find t.string_table str
  with Not_found ->
    let sid = t.next_stringref in
    t.next_stringref <- sid + 1;
    write_string_record t sid str;
    Hashtbl.add t.string_table str sid;
    sid

let write_kernel_object_record t koid ~ty ~name ?process () =
  let has_proc = Option.is_some process in
  let process_sref = if has_proc then get_string t "process" else 0 in
  let name_sref = get_string t name in
  let size = if has_proc then 4 else 2 in
  write_int64 t.writer
    Word.(
      (* type = 7 ; size = N words *)
      bits 0 3 ~v:7 -- bits 4 15 ~v:size -- bits 16 23 ~v:ty
      -- bits 24 39 ~v:name_sref);
  write_int64 t.writer koid;
  if has_proc then (
    write_int64 t.writer
      Word.(
        (* type = koid argument (8) ; size = 2 words *)
        bits 0 3 ~v:8 -- bits 4 15 ~v:2
        (* argument name = "process "*)
        -- bits 16 31 ~v:process_sref);
    write_int64 t.writer (Option.get process))

let add_ring_id t =
  let rid_i = t.ring_max in
  let rid = Int64.of_int rid_i in
  let pid = rid and tid = Int64.(add rid 1L) in
  let thread_name = Printf.sprintf "Ring_id %d" rid_i in
  (* no KOR for process, (as JS Tracing does), Perfetto will just call it "Process" *)
  write_kernel_object_record t tid ~ty:2 ~name:thread_name ~process:pid ();
  write_thread_record t (rid_i + 1) ~pid ~tid;
  t.ring_max <- t.ring_max + 1

let get_ring_thread_idx t ring_id =
  while ring_id >= t.ring_max do
    add_ring_id t
  done;
  ring_id + 1

let write_event_record_start t ~ring_id ~category ~name ~ts ~ty ~nargs
    ~extra_size =
  let tref = get_ring_thread_idx t ring_id in
  let catref = get_string t category in
  let nameref = get_string t name in
  let size = 2 + extra_size (* header, timestamp, extra *) in
  write_int64 t.writer
    Word.(
      (* type = event (4) ; len = ?? words *)
      bits 0 3 ~v:4 -- bits 4 15 ~v:size
      -- bits 16 19 ~v:ty (* event type *)
      -- bits 20 23 ~v:nargs (* number of arguments *)
      -- bits 24 31 ~v:tref (* thread *)
      -- bits 32 47 ~v:catref (* category *)
      -- bits 48 63 ~v:nameref
      (* name *));
  write_int64 t.writer ts

let write_instant_event t ~ring_id ~category ~name ~ts =
  write_event_record_start t ~ring_id ~category ~name ~ts ~ty:0 ~nargs:0
    ~extra_size:0

let write_counter_event t ~ring_id ~category ~name ~ts ~count =
  let extra_size = 3 (* 2 for argument, 1 for counter id *) in
  let vref = get_string t "v" in
  write_event_record_start t ~ring_id ~category ~name ~ts ~ty:1 ~nargs:1
    ~extra_size;
  (* v = <count> sint64 argument *)
  write_int64 t.writer
    Word.(
      (* argtype = sint64 (3) ; size = 2 *)
      bits 0 3 ~v:3 -- bits 4 15 ~v:2 -- bits 16 31 ~v:vref);
  write_int64 t.writer (Int64.of_int count);
  write_int64 t.writer 0L (* counter id = 0 *)

let write_duration_begin_event t ~ring_id ~category ~name ~ts =
  write_event_record_start t ~ring_id ~category ~name ~ts ~ty:2 ~nargs:0
    ~extra_size:0

let write_duration_end_event t ~ring_id ~category ~name ~ts =
  write_event_record_start t ~ring_id ~category ~name ~ts ~ty:3 ~nargs:0
    ~extra_size:0

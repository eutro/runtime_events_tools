open Olly_format_backend

module Writer = struct
  type t = { buf : Buffer.t; oc : out_channel }

  let string w ?len s =
    match len with
    | None -> Buffer.add_string w.buf s
    | Some len -> Buffer.add_substring w.buf s 0 len

  let le_uint64 w x = Buffer.add_int64_le w.buf x

  let create ~filename ?(size = 8192) () =
    let buf = Buffer.create size in
    let oc = open_out_bin filename in
    { buf; oc }

  let flush w =
    Buffer.output_buffer w.oc w.buf;
    Buffer.reset w.buf

  let maybe_flush w = if Buffer.length w.buf > 4096 then flush w

  let close w =
    flush w;
    close_out w.oc
end

module Trace = Fuchsia.Write.Make (Writer)

let name = "fuchsia"
let description = "Perfetto"

type trace = { writer : Writer.t; trace : Trace.t }

let create ~filename =
  let writer = Writer.create ~filename () in
  let trace = Trace.of_writer writer in
  for i = 0 to 127 do
    let id = Int64.of_int i in
    Trace.kernel_object trace
      ~args:[ ("process", `Koid id) ]
      ~name:(Printf.sprintf "Ring %d" i)
      `Thread id
  done;
  { writer; trace }

let close trace = Writer.close trace.writer

let emit trace evt =
  let open Event in
  let rid = Int64.of_int evt.ring_id in
  let thread : Trace.thread = { pid = rid; tid = rid }
  and category = "PERF"
  and { ts; name; _ } = evt in
  (match evt.kind with
  | SpanBegin | SpanEnd ->
      let write =
        if evt.kind = SpanBegin then Trace.duration_begin
        else Trace.duration_end
      in
      write trace.trace ~args:[] ~thread ~category ~name ~ts
  | Counter value ->
      Trace.counter_event trace.trace ~thread ~category ~name ~ts ~id:0L
        ~args:[ ("v", `Int64 (Int64.of_int value)) ]
  | Instant ->
      Trace.instant_event trace.trace ~args:[] ~thread ~category ~name ~ts
  | _ -> ());
  Writer.maybe_flush trace.writer

open Fuchsia

let name = "fuchsia"
let description = "Perfetto"

type trace = Fuchsia.trace

let create ~filename =
  let t = Fuchsia.create ~filename in
  write_magic_number_record t;
  write_provider_info_record t ~name:"olly_trace" ~id:1;
  write_section_record t ~id:1;
  write_init_record t;
  t

let close = Fuchsia.close

let emit t evt =
  let open Olly_format_backend.Event in
  let category = "PERF" and { ring_id; ts; name; _ } = evt in
  match evt.kind with
  | SpanBegin | SpanEnd ->
      let write =
        if evt.kind = SpanBegin then write_duration_begin_event
        else write_duration_end_event
      in
      write t ~ring_id ~category ~name ~ts
  | Counter value ->
      write_counter_event t ~ring_id ~category ~name ~ts ~count:value
  | Instant -> write_instant_event t ~ring_id ~category ~name ~ts
  | _ -> ()

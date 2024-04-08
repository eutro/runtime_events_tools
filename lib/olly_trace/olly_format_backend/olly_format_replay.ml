open Olly_format_backend.Event

let name = "replay"
let description = "olly --replay"

type trace = out_channel

let create ~filename = Out_channel.open_text filename
let close = Out_channel.close
let enum_to_int = Olly_rte_shim.Tabling.enum_to_int

let print_tag oc tag =
  match tag with
  | Unrecognised -> Printf.fprintf oc "unrecognised"
  | Lifecycle x -> Printf.fprintf oc "{lifecycle: %d}" (enum_to_int x)
  | Runtime_phase x -> Printf.fprintf oc "{runtime_phase: %d}" (enum_to_int x)
  | Runtime_counter x ->
      Printf.fprintf oc "{runtime_counter: %d}" (enum_to_int x)
  | Alloc -> Printf.fprintf oc "alloc"
  | Lost_events -> Printf.fprintf oc "lost_events"
  | _ ->
      if not (Olly_rte_shim.Custom_events.print_tag oc tag) then
        Printf.fprintf oc "unrecognised"

let print_kind oc kind =
  match kind with
  | SpanBegin -> Printf.fprintf oc "span_begin"
  | SpanEnd -> Printf.fprintf oc "span_end"
  | Instant -> Printf.fprintf oc "instant"
  | Counter i -> Printf.fprintf oc "{counter: %d}" i
  | IntArray is ->
      Printf.fprintf oc "{int_array: [%s]}"
        (String.concat "," @@ Array.to_list @@ Array.map string_of_int is)
  | MaybeInt i ->
      Printf.fprintf oc "{maybe_int: %t}" (fun oc ->
          match i with
          | None -> Printf.fprintf oc "no"
          | Some i -> Printf.fprintf oc "%d" i)
  | _ -> Printf.fprintf oc "unrecognised"

let emit oc evt =
  Printf.fprintf oc "- {ring: %d, ts: %Ld, name: %s, tag: %a, kind: %a}\n"
    evt.ring_id evt.ts evt.name print_tag evt.tag print_kind evt.kind

let scan_tag tag =
  match tag with
  | "unrecognised" -> Unrecognised
  | "alloc" -> Alloc
  | "lost_events" -> Lost_events
  | _ -> (
      match
        List.find_map
          (fun (s, f) ->
            try Some (Scanf.sscanf tag s f) with Scanf.Scan_failure _ -> None)
          [
            ("{lifecycle: %d}%!", fun i -> Lifecycle (Obj.magic i));
            ("{runtime_phase: %d}%!", fun i -> Runtime_phase (Obj.magic i));
            ("{runtime_counter: %d}%!", fun i -> Runtime_counter (Obj.magic i));
          ]
      with
      | Some x -> x
      | None ->
          Option.value ~default:Unrecognised
            (Olly_rte_shim.Custom_events.scan_tag tag))

let scan_kind kind =
  match kind with
  | "span_begin" -> SpanBegin
  | "span_end" -> SpanEnd
  | "instant" -> Instant
  | "unrecognised" -> NoValue
  | _ -> (
      try
        Scanf.sscanf kind "{%s@: %s@}%!" (fun key value ->
            match key with
            | "counter" -> Scanf.sscanf value "%d" (fun i -> Counter i)
            | "int_array" ->
                Scanf.sscanf value "[%s@]" (fun x ->
                    IntArray
                      (Array.map int_of_string @@ Array.of_list
                     @@ String.split_on_char ',' x))
            | "maybe_int" ->
                MaybeInt
                  (match value with
                  | "no" -> None
                  | _ -> Some (int_of_string value))
            | _ -> NoValue)
      with Scanf.Scan_failure _ -> NoValue)

let delimited_value k ic =
  let rec recurse depth buf ic : string =
    Scanf.bscanf ic "%[^{}]%c" (fun pre_delim delim ->
        Buffer.add_string buf pre_delim;
        Buffer.add_char buf delim;
        match delim with
        | '{' -> recurse (depth + 1) buf ic
        | '}' ->
            if depth > 0 then recurse (depth - 1) buf ic
            else Buffer.contents buf
        | _ -> failwith "unreachable")
  in
  let s =
    Scanf.bscanf ic "%0c" (function
      | '{' ->
          let buf = Buffer.create 16 in
          Scanf.bscanf ic "{" ();
          Buffer.add_char buf '{';
          recurse 0 buf ic
      | _ -> Scanf.bscanf ic "%[^,}]" Fun.id)
  in
  k s

let parse ~line =
  try
    Scanf.sscanf line "- {ring: %d, ts: %Ld, name: %s@, tag: %r, kind: %r}%!"
      (delimited_value scan_tag) (delimited_value scan_kind)
      (fun ring_id ts name tag kind -> { ring_id; ts; name; tag; kind })
  with End_of_file -> raise (Scanf.Scan_failure "unexpected EOF")

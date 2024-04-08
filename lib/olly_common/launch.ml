open Olly_rte_shim
open Event

let lost_events ring_id num =
  Printf.eprintf "[ring_id=%d] Lost %d events\n%!" ring_id num

type subprocess = {
  alive : unit -> bool;
  cursor : Runtime_events.cursor;
  close : unit -> unit;
}

let split_args exec_args =
  match exec_args with
  | [ arg ] ->
      if Sys.file_exists arg then [ arg ] else String.split_on_char ' ' arg
  | args -> args

type alive_mode = Kill | Wait

exception Could_not_attach of string

let attach_process dir pid alive_mode =
  let cursor = Runtime_events.create_cursor (Some (dir, pid)) in
  let alive =
    match alive_mode with
    | Wait -> (
        fun () ->
          match Unix.waitpid [ Unix.WNOHANG ] pid with
          | 0, _ -> true
          | p, _ when p = pid -> false
          | _, _ -> assert false)
    | Kill -> (
        fun () ->
          try
            Unix.kill pid 0;
            true
          with Unix.Unix_error (Unix.ESRCH, _, _) -> false)
  and close () = Runtime_events.free_cursor cursor in
  { alive; cursor; close }

let exec_process exec_args =
  let argsl = split_args exec_args in
  let executable_filename = List.hd argsl in

  (* TODO Set the temp directory. We should make this configurable. *)
  let tmp_dir = Filename.get_temp_dir_name () |> Unix.realpath in
  let env =
    Array.append
      [|
        "OCAML_RUNTIME_EVENTS_START=1";
        "OCAML_RUNTIME_EVENTS_DIR=" ^ tmp_dir;
        "OCAML_RUNTIME_EVENTS_PRESERVE=1";
      |]
      (Unix.environment ())
  in
  let child_pid =
    Unix.create_process_env executable_filename (Array.of_list argsl) env
      Unix.stdin Unix.stdout Unix.stderr
  in
  Unix.sleepf 0.1;
  let child = attach_process tmp_dir child_pid Wait in
  let delete_buffer () =
    (* We need to remove the ring buffers ourselves because we told
       the child process not to remove them *)
    let ring_file =
      Filename.concat tmp_dir (string_of_int child_pid ^ ".events")
    in
    Unix.unlink ring_file
  in
  let close =
    let { close; _ } = child in
    fun () -> Fun.protect ~finally:delete_buffer close
  in
  { child with close }

let collect_events child callbacks =
  (* Read from the child process *)
  while child.alive () do
    Runtime_events.read_poll child.cursor callbacks None |> ignore;
    Unix.sleepf 0.1 (* Poll at 10Hz *)
  done;
  (* Do one more poll in case there are any remaining events we've missed *)
  Runtime_events.read_poll child.cursor callbacks None |> ignore

type consumer_config = {
  handler : shim_callback;
  init : unit -> unit;
  cleanup : unit -> unit;
}

let empty_config =
  { handler = (fun _ -> ()); init = (fun () -> ()); cleanup = (fun () -> ()) }

type attach_mode =
  | Child of string list
  | Replay of string
  | Attach of string * int

type common_args = { src_table_path : string option; attach_mode : attach_mode }

let our_handler (k : shim_callback) (evt : event) =
  match evt.tag with
  | Lost_events -> (
      match evt.kind with Counter num -> lost_events evt.ring_id num | _ -> ())
  | _ -> k evt

let make_shim_callback src_table_path handler =
  let map_names =
    match src_table_path with
    | None -> Construct.builtin_names
    | Some path ->
        Tabling.tabled_names_and_tags
          ~actual:(Tabling.parse_from_yaml_file path)
          ~builtin:Construct.builtin_name_table
  in
  our_handler (map_names handler)

let child_common child cb =
  Fun.protect ~finally:child.close (fun () ->
      let callbacks = Construct.make_callbacks cb in
      collect_events child callbacks)

let launch_child exec_args cb = child_common (exec_process exec_args) cb
let launch_attach dir pid cb = child_common (attach_process dir pid Kill) cb

let replay_lines cb ic =
  let rec loop () =
    let line = input_line ic in
    let evt = Olly_format_replay.parse ~line in
    cb evt;
    loop ()
  in
  try loop () with End_of_file -> ()

let launch_replay file cb = In_channel.with_open_text file (replay_lines cb)

let olly config opts =
  config.init ();
  let cb = make_shim_callback opts.src_table_path config.handler in
  Fun.protect ~finally:config.cleanup (fun () ->
      (match opts.attach_mode with
      | Child args -> launch_child args
      | Attach (dir, pid) -> launch_attach dir pid
      | Replay file -> launch_replay file)
        cb)

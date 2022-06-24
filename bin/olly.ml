module H = Hdr_histogram
module Ts = Runtime_events.Timestamp
open Cmdliner

let print_percentiles hist =
  let ms ns = ns /. 1000000. in
  Printf.eprintf "\n";
  Printf.eprintf "GC latency profile:\n";
  Printf.eprintf "#[Mean (ms):\t%.2f,\t Stddev (ms):\t%.2f]\n"
    (H.mean hist |> ms) (H.stddev hist |> ms);
  Printf.eprintf "#[Min (ms):\t%.2f,\t max (ms):\t%.2f]\n"
    (float_of_int (H.min hist) |> ms) (float_of_int (H.max hist) |> ms);

  Printf.eprintf "\n";
  let percentiles = [| 25.0; 50.0; 60.0; 70.0; 75.0; 80.0; 85.0; 90.0; 95.0;
                       96.0; 97.0; 98.0; 99.0; 99.9; 99.99; 99.999; 99.9999;
                       100.0 |]
  in
  Printf.eprintf "Percentile \t Latency (ms)\n";
  Fun.flip Array.iter percentiles (fun p -> Printf.eprintf "%.4f \t %.2f\n" p
    (float_of_int (H.value_at_percentile hist p) |> ms))

let olly ~runtime_begin ~runtime_end ~cleanup ~init exec_args =
  let executable_filename, args =
    match String.split_on_char ' ' exec_args with
    | [] -> assert false
    | x::xs -> x,xs
  in
  let args = Array.of_list args in

  (* Set the temp directory. We should make this configurable. *)
  let tmp_dir = Filename.get_temp_dir_name ()  ^ "/" in
  let env = Array.append [| "OCAML_RUNTIME_EVENTS_START=1";
                            "OCAML_RUNTIME_EVENTS_DIR=" ^ tmp_dir;
                            "OCAML_RUNTIME_EVENTS_PRESERVE=1" |]
                          (Unix.environment ())
  in
  let child_pid =
    Unix.create_process_env executable_filename args env
                            Unix.stdin Unix.stdout Unix.stderr
  in

  init ();
  (* Read from the child process *)
  Unix.sleep 1;
  let cursor = Runtime_events.create_cursor (Some (tmp_dir, child_pid)) in
  let callbacks = Runtime_events.Callbacks.create ~runtime_begin ~runtime_end () in
  let child_alive () = match Unix.waitpid [ Unix.WNOHANG ] child_pid with
    | (0, _) -> true
    | (p, _) when p = child_pid -> false
    | (_, _) -> assert(false) in
  while child_alive () do begin
    Runtime_events.read_poll cursor callbacks None |> ignore;
    Unix.sleepf 0.1 (* Poll at 10Hz *)
  end done;

  (* Do one more poll in case there are any remaining events we've missed *)
  Runtime_events.read_poll cursor callbacks None |> ignore;

  (* Now we're done, we need to remove the ring buffers ourselves because we
      told the child process not to remove them *)
  let ring_file =
    Filename.concat tmp_dir (string_of_int child_pid ^ ".events")
  in
  Unix.unlink ring_file;
  cleanup ()

let trace trace_filename exec_args =
  let trace_file = open_out trace_filename in
  let ts_to_us ts = Int64.(div (Ts.to_int64 ts) (of_int 1000)) in
  let runtime_begin ring_id ts phase =
      Printf.fprintf trace_file "{\"name\": \"%s\", \"cat\": \"PERF\", \"ph\":\"B\", \"ts\":%Ld, \"pid\": %d, \"tid\": %d},\n" (Runtime_events.runtime_phase_name phase) (ts_to_us ts) ring_id ring_id;
      flush trace_file
  in
  let runtime_end ring_id ts phase =
    Printf.fprintf trace_file "{\"name\": \"%s\", \"cat\": \"PERF\", \"ph\":\"E\", \"ts\":%Ld, \"pid\": %d, \"tid\": %d},\n" (Runtime_events.runtime_phase_name phase) (ts_to_us ts) ring_id ring_id;
    flush trace_file
  in
  let init () =
    (* emit prefix in the tracefile *)
    Printf.fprintf trace_file "["
  in
  let cleanup () = close_out trace_file in
  olly ~runtime_begin ~runtime_end ~init ~cleanup exec_args

let latency exec_args =
  let current_event = Hashtbl.create 13 in
  let hist =
    H.init ~lowest_discernible_value:10
            ~highest_trackable_value:10_000_000_000
            ~significant_figures:3
  in
  let runtime_begin ring_id ts phase =
    match Hashtbl.find_opt current_event ring_id with
    | None -> Hashtbl.add current_event ring_id (phase, Ts.to_int64 ts)
    | _ -> ()
  in
  let runtime_end ring_id ts phase =
    match Hashtbl.find_opt current_event ring_id with
    | Some (saved_phase, saved_ts) when (saved_phase = phase) ->
        Hashtbl.remove current_event ring_id;
        let latency =
          Int64.to_int (Int64.sub (Ts.to_int64 ts) saved_ts)
        in
        assert (H.record_value hist latency)
    | _ -> ()
  in
  let init = Fun.id in
  let cleanup () = print_percentiles hist in
  olly ~runtime_begin ~runtime_end ~init ~cleanup exec_args

let help man_format cmds topic =
  match topic with
  | None -> `Help (`Pager, None) (* help about the program. *)
  | Some topic ->
      let topics = "topics" :: cmds in
      let conv, _ = Cmdliner.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
      match conv topic with
      | `Error e -> `Error (false, e)
      | `Ok t when t = "topics" -> List.iter print_endline topics; `Ok ()
      | `Ok t when List.mem t cmds -> `Help (man_format, Some t)
      | `Ok _t ->
          let page = (topic, 7, "", "", ""), [`S topic; `P "Say something";] in
          `Ok (Cmdliner.Manpage.print man_format Format.std_formatter page)

let () =
  (* Help sections common to all commands *)

  let help_secs = [
    `S Manpage.s_common_options;
    `P "These options are common to all commands.";
    `S "MORE HELP";
    `P "Use $(mname) $(i,COMMAND) --help for help on a single command.";`Noblank;
    `S Manpage.s_bugs; `P "Check bug reports at http://bugs.example.org.";]
  in

  (* Commands *)

  let sdocs = Manpage.s_common_options in

  let exec_args p =
    let doc = "Executable (and its arguments) to trace. If the executable takes
              arguments, wrap quotes around the executable and its arguments.
              For example, olly '<exec> <arg_1> <arg_2> ... <arg_n>'."
    in
    Arg.(required & pos p (some string) None & info [] ~docv:"EXECUTABLE" ~doc)
  in

  let trace_cmd =
    let trace_filename =
      let doc = "Target trace file name." in
      Arg.(required & pos 0 (some string) None & info [] ~docv:"TRACEFILE" ~doc)
    in
    let man = [
      `S Manpage.s_description;
      `P "Save the runtime trace in Chrome trace format.";
      `Blocks help_secs; ]
    in
    let doc = "Save the runtime trace in Chrome trace format." in
    let info = Cmd.info "trace" ~doc ~sdocs ~man in
    Cmd.v info Term.(const trace $ trace_filename $ exec_args 1)
  in

  let latency_cmd =
    let man = [
      `S Manpage.s_description;
      `P "Report the GC latency profile.";
      `Blocks help_secs; ]
    in
    let doc = "Report the GC latency profile." in
    let info = Cmd.info "latency" ~doc ~sdocs ~man in
    Cmd.v info Term.(const latency $ exec_args 0)
  in

  let help_cmd =
    let topic =
      let doc = "The topic to get help on. $(b,topics) lists the topics." in
      Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
    in
    let doc = "Display help about olly and olly commands." in
    let man =
      [`S Manpage.s_description;
      `P "Prints help about olly commands and other subjects…";
      `Blocks help_secs; ]
    in
    let info = Cmd.info "help" ~doc ~man in
    Cmd.v info
      Term.(ret (const help $ Arg.man_format $ Term.choice_names $ topic))
  in

  let main_cmd =
    let doc = "An observability tool for OCaml programs" in
    let info = Cmd.info "olly" ~doc ~sdocs in
    Cmd.group info [trace_cmd; latency_cmd; help_cmd]
  in

  exit (Cmd.eval main_cmd)

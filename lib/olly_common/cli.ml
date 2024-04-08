open Cmdliner

let help_secs =
  [
    `S Manpage.s_common_options;
    `P "These options are common to all commands.";
    `S "MORE HELP";
    `P "Use $(mname) $(i,COMMAND) --help for help on a single command.";
    `Noblank;
    `S Manpage.s_bugs;
    `P
      "Check bug reports at \
       http://github.com/tarides/runtime_events_tools/issues.";
  ]

let sdocs = Manpage.s_common_options

let help man_format cmds topic =
  match topic with
  | None -> `Help (`Pager, None) (* help about the program. *)
  | Some topic -> (
      let topics = "topics" :: cmds in
      let conv, _ = Cmdliner.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
      match conv topic with
      | `Error e -> `Error (false, e)
      | `Ok t when t = "topics" ->
          List.iter print_endline topics;
          `Ok ()
      | `Ok t when List.mem t cmds -> `Help (man_format, Some t)
      | `Ok _t ->
          let page =
            ((topic, 7, "", "", ""), [ `S topic; `P "Say something" ])
          in
          `Ok (Manpage.print man_format Format.std_formatter page))

let replay_option =
  let doc =
    "Read an olly replay trace, as generated with `olly trace --format=replay`."
  in
  Arg.(
    value & opt (some non_dir_file) None & info [ "replay" ] ~docv:"FILE" ~doc)

let attach_option =
  let doc =
    "Attach to an external OCaml process, with the given pid, writing runtime \
     events in the given directory. The user is responsible for ensuring that \
     this process has started runtime events, either using \
     `Runtime_events.start`, or by setting `OCAML_RUNTIME_EVENTS_START=1` \
     before running. See https://v2.ocaml.org/manual/runtime-tracing.html."
  in
  Arg.(
    value
    & opt (some (pair ~sep:':' dir int)) None
    & info [ "attach"; "a" ] ~docv:"DIRECTORY:PID" ~doc)

let exec_args p =
  let doc =
    "Executable (and its arguments) to trace. If the executable takes\n\
    \              arguments, they may be specified as trailing arguments, or \
     as a single space-separated argument.\n\
    \              For example, olly '<exec> <arg_1> <arg_2> ... <arg_n>'."
  in
  Arg.(value & pos_right (p - 1) string [] & info [] ~docv:"EXECUTABLE" ~doc)

let src_table_args =
  let doc =
    "Load a runtime events name table for event translation, for forwards \
     compatibility with newer OCaml versions.\n\
     See `olly-gen-tables`."
  in
  Arg.(
    value & opt (some non_dir_file) None & info [ "table" ] ~docv:"PATH" ~doc)

let common_args p =
  let combine src_table_path replay attach exec_args : Launch.common_args =
    let attach_mode : Launch.attach_mode =
      let exception Failed of string in
      try
        match (replay, attach, exec_args) with
        | None, None, (_ :: _ as args) -> Child args
        | Some file, None, [] -> Replay file
        | None, Some (dir, pid), [] -> Attach (dir, pid)
        | None, None, [] ->
            raise (Failed "no EXECUTABLE, --replay, or --attach specified")
        | _, _, _ ->
            raise
              (Failed
                 "more than one of EXECUTABLE, --replay, or --attach specified")
      with Failed s ->
        Printf.eprintf "error: %s" s;
        exit Cmd.Exit.cli_error
    in
    { src_table_path; attach_mode }
  in
  Term.(
    const combine $ src_table_args $ replay_option $ attach_option $ exec_args p)

let main name commands =
  let help_cmd =
    let topic =
      let doc = "The topic to get help on. $(b,topics) lists the topics." in
      Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
    in
    let doc = "Display help about olly and olly commands." in
    let man =
      [
        `S Manpage.s_description;
        `P "Prints help about olly commands and other subjects…";
        `Blocks help_secs;
      ]
    in
    let info = Cmd.info "help" ~doc ~man in
    Cmd.v info
      Term.(ret (const help $ Arg.man_format $ Term.choice_names $ topic))
  in

  let main_cmd =
    let doc = "An observability tool for OCaml programs" in
    let info = Cmd.info name ~doc ~sdocs in
    Cmd.group info (commands @ [ help_cmd ])
  in

  exit (Cmd.eval main_cmd)

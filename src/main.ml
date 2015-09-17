(*
 * Module resolution for EcmaScript 6
 * (c) 2012 Andreas Rossberg
 *)

let trace_flag = ref false
let dump_flag = ref false
let no_translate_flag = ref false

let trace_phase name file =
  if !trace_flag then print_endline ("-- [" ^ file ^ "] " ^ name)

let load file =
  let f = open_in file in
  let size = in_channel_length f in
  let source = String.create size in
  really_input f source 0 size;
  close_in f;
  source

let parse name source =
  let lexbuf = Lexing.from_string source in
  lexbuf.Lexing.lex_curr_p <-
    {lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = name};
  try Parser.prog Lexer.token lexbuf with Source.Error (region, s) ->
    let region' = if region <> Source.nowhere_region then region else
      {Source.left = Lexer.convert_pos lexbuf.Lexing.lex_start_p;
       Source.right = Lexer.convert_pos lexbuf.Lexing.lex_curr_p} in
    raise (Source.Error (region', s))

let () = Syntax.recursive := 
  (fun file ->
    trace_phase "Loading..." file;
    let source = load file in
    trace_phase "Parsing..." file;
    let prog = parse file source in
    trace_phase "Parsed." file;
    prog)

let process file =
  if !dump_flag then trace_flag := true;
  try
    trace_phase "Loading..." file;
    let source = load file in
    trace_phase "Parsing..." file;
    let prog = parse file source in
    trace_phase "Resolving..." file;
    let typ = Resolve.resolve prog in
    if !dump_flag then
      print_endline (Types.string_of_row (Types.asModT typ));
    if not !no_translate_flag then begin
      trace_phase "Translating..." file;
      let javascript = Translate.translate prog in
      print_string javascript
    end;
    trace_phase "Done." file
  with Source.Error (at, s) ->
    prerr_endline (Source.string_of_region at ^ ": " ^ s);
    trace_phase "Aborted." file;
    exit 1

let usage = "Usage: jsmodules [option] file"
let argspec = Arg.align
[
  "-h", Arg.Set Translate.harmony_flag, " output Harmony-only let declarations";
  "-t", Arg.Set no_translate_flag, " resolve only";
  "-d", Arg.Set dump_flag, " dump types (implies -v)";
  "-v", Arg.Set trace_flag, " verbose output";
]

let () =
  Printexc.record_backtrace true;
  try
    Arg.parse argspec process usage
  with exn ->
    flush stdout;
    prerr_endline
      (Sys.argv.(0) ^ ": uncaught exception " ^ Printexc.to_string exn);
    Printexc.print_backtrace stderr;
    exit 2

open Horse.Lexer
open Printf

let read_file filename =
  let chan = open_in filename in
  let len = in_channel_length chan in
  let content = really_input_string chan len in
  close_in chan;
  content

let () =
  let filename =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else (
      prerr_endline "Usage: horse <filename.hr>";
      exit 1)
  in
  let source = read_file filename in
  let lex = new lexer source in

  let rec loop () =
    match lex#consume with
    | Ok token ->
        printf "%s -> " (to_string token#typeof);
        if token#typeof = EOF then () else loop ()
    | Error msg ->
        eprintf "Lexer error: %s\n" msg;
        exit 1
  in
  loop ();
  printf "DONE\n"


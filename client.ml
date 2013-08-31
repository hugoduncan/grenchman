open Async.Std
open Core.Std
open Printf

let splice_args args =
  String.concat ~sep:"\" \"" (List.map args String.escaped)

let main_message ns form session =
  [("session", session);
   ("op", "eval");
   ("id", Uuid.to_string (Uuid.create ()));
   ("ns", ns);
   ("code", form)]

let main cwd root ns form port =
  let message = main_message ns form in
  Nrepl.new_session "127.0.0.1" port message Repl.handler;
  never_returns (Scheduler.go ())

let port_err msg =
  eprintf "%s" msg;
  Pervasives.exit 1

let repl_port env_var filename port_err_msg =
  match Sys.getenv env_var with
    | Some port -> port
    | None -> match Sys.file_exists filename with
              | `Yes -> In_channel.read_all filename
              | `No | `Unknown -> port_err port_err_msg

let rec find_root cwd original =
  match Sys.file_exists (String.concat ~sep:Filename.dir_sep
                           [cwd; "project.clj"]) with
    | `Yes -> cwd
    | `No | `Unknown -> if (Filename.dirname cwd) = cwd then
        original
      else
        find_root (Filename.dirname cwd) original

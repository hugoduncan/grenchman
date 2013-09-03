open Async.Std
open Core.Std

let port_err =
  "Couldn't read port from .nrepl-port or LEIN_REPL_PORT.\n
If Leiningen is not running, launch `lein repl :headless' from the
project directory and try again.\n"

let rec repl_port port_file jarpath repl_ns =
  let filename = match port_file with
    | Some s -> s
    | None -> let cwd = Sys.getcwd () in
              let root = Client.find_root cwd cwd in
              String.concat ~sep:Filename.dir_sep [root; ".nrepl-port"] in
  let javapath () = match Sys.getenv "CLJMAIN_JAVA" with
    | Some s -> s
    | None -> "java" in
  let main_ns () = match repl_ns with
    | Some s -> s
    | None -> "nrepl.main" in
  let rec wait_for filename max_iter =
    Printf.eprintf "Waiting for %s...\n%!" filename;
    match Sys.file_exists filename with
    | `Yes -> ()
    | `No | `Unknown -> if max_iter > 0 then
                          begin
                            Thread.delay 1.;
                            wait_for filename (max_iter - 1)
                          end
                        else
                          begin
                            Printf.eprintf "%s\n%!" "Repl start timed out.";
                            Pervasives.exit 1
                          end in
  match Client.repl_port "CLJMAIN_PORT" filename with
  | Some p -> p
  | None -> match (jarpath,port_file) with
            | (Some jp, Some pf) ->
               Printf.printf "launch %s %s %s\n%!" (javapath ()) jp (main_ns ());
               let pid = Jarlaunch.launch
                           jp ~java:(javapath ()) ~main_ns:(main_ns ())
                           ~args:[|"--port-file"; pf |] in
               Printf.eprintf "Started %d\n%!" pid;
               wait_for pf 20;
               repl_port port_file None None
            | _ -> Printf.eprintf "%s\n%!"
                     ("Could not find repl to connect to, and jarpath and " ^
                        "port-file are not specified, so cannot start one");
                   Pervasives.exit 1

let lein_ns = "leiningen.core.main"

let main_form =
  Printf.sprintf "(do
                    (use '%s)
                    (require '[clojure.stacktrace :refer [print-cause-trace]])
                    (binding [*exit-process?* false]
                      (try (-main \"%s\")
                      (catch Exception e
                        (let [c (:exit-code (ex-data e))]
                          (when-not (and (number? c) (zero? c))
                            (print-cause-trace e)))))))"

let do_main ~port ~port_file ~jarpath ~repl_ns ~ns ~args =
  if ! Sys.interactive then () else
    let port = match port with
      | Some x -> x
      | None -> repl_port port_file jarpath repl_ns in
    let cmd = main_form ns (Client.splice_args args) in
    Client.main ns cmd port

let command =
  Command.basic
    ~summary:"Run clojure main commands against a REPL."
    Command.Spec.(
      empty
      +> flag "-p" (optional int) ~doc:"Port to connect to"
      +> flag "-f" (optional string) ~doc:"port file for repl server"
      +> flag "-j" (optional string) ~doc:"jar file path for repl server"
      +> flag "-n" (optional string) ~doc:"namespace for repl server main"
      +> anon ("namspace" %: string)
      +> anon (sequence ("args" %: string)))
    (fun port port_file jarpath repl_ns namespace args () ->
     do_main ~port ~port_file ~jarpath ~repl_ns ~ns:namespace ~args)

let () =
  Command.run command

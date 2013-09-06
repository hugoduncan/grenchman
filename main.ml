open Async.Std
open Core.Std

(* This module is for running in-project -main functions; it does not *)
(* act as grenchman's own entry point. *)

(* Launch an nREPL server based on a jar file *)
let rec launch filename jarpath repl_ns =
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
  Printf.printf "launch %s %s %s\n%!" (javapath ()) jarpath (main_ns ());
  let pid = Jarlaunch.launch
              jarpath ~java:(javapath ()) ~main_ns:(main_ns ())
              ~args:[|"--port-file"; filename |] in
  Nrepl.debug ("Started " ^ (string_of_int pid));
  wait_for filename 20

let port_err =
  "Couldn't read port from .nrepl-port or GRENCH_PORT.\n
If Leiningen is not running, launch `lein repl :headless' from the
project directory and try again.\n"

let rec repl_port port_file jarpath repl_ns =
  let filename = match port_file with
    | Some s -> s
    | None -> let cwd = Sys.getcwd () in
              let root = Client.find_root cwd cwd in
              String.concat ~sep:Filename.dir_sep [root; ".nrepl-port"] in
  match Client.repl_port "GRENCH_PORT" filename with
  | Some p -> p
  | None -> match (jarpath,port_file) with
            | (Some jp, Some pf) ->
               launch pf jp repl_ns;
               repl_port port_file None None
            | _ -> Printf.eprintf "%s%!" port_err;
    Pervasives.exit 1

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

(* a fold function to split a list on a "--" element *)
let splitf (args,others) e =
    match others with
    | None -> if e = "--" then
                (args, Some [])
              else
                (List.append args [e], None)
    | Some l -> (args, Some (List.append l [e]))

let extra_args = ref []

let command =
  Command.basic
    ~summary:"Run clojure main commands against a REPL."
    Command.Spec.(
      empty
      +> flag "--port" ~aliases:["-p"] (optional int)
              ~doc:"Port to connect to"
      +> flag "--port-file" ~aliases:["-f"] (optional string)
              ~doc:"port file for repl server"
      +> flag "--jar" ~aliases:["-j"] (optional string)
              ~doc:"jar file path for repl server"
      +> flag "--namespace" ~aliases:["-n"] (optional string)
              ~doc:"namespace for repl server main"
      +> anon ("namspace" %: string)
      +> anon (sequence ("args" %: string)))
   (fun port port_file jarpath repl_ns namespace args () ->
    let args = (List.append args (! extra_args)) in
    do_main ~port ~port_file ~jarpath ~repl_ns ~ns:namespace ~args)

(* Run a clojure main *)
let main args =
  let (args,others) = List.fold args ~init:([],None) ~f:splitf in
  (match others with
   | Some others ->  extra_args := others
   | None -> extra_args := []);
  Command.run command ~argv:(List.cons "cljmain" args)
              ~version:"grenchman cljmain 0.1.0"

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

let lein_port_err =
  "Couldn't read port from ~/.lein/repl-port or LEIN_REPL_PORT.\n
If Leiningen is not running, launch `lein repl :headless' from outside a
project directory and try again.\n"

let lein_repl_port () =
  let filename = String.concat
                   ~sep:Filename.dir_sep [(Sys.getenv_exn "HOME");
                                          ".lein"; "repl-port"] in
  repl_port "LEIN_REPL_PORT" filename lein_port_err

let lein_ns = "leiningen.core.main"

let main_form = sprintf "(binding [*cwd* \"%s\", *exit-process?* false]
                           (System/setProperty \"leiningen.original.pwd\" \"%s\")

                           (defmethod leiningen.core.eval/eval-in :default
                             [project form]
                             (leiningen.core.eval/eval-in
                               (assoc project :eval-in :nrepl) form))
                           (defmethod leiningen.core.eval/eval-in :trampoline
                             [& _] (throw (Exception. \"trampoline disabled\")))

                           (try (-main \"%s\")
                             (catch clojure.lang.ExceptionInfo e
                               (let [c (:exit-code (ex-data e))]
                                 (when-not (and (number? c) (zero? c))
                                   (throw e))))))"

let lein_main cwd root args =
  let port = Int.of_string (lein_repl_port ()) in
  main cwd root lein_ns (main_form root cwd (splice_args args)) port

let usage = "usage: grench TASK [ARGS]...

A replacement launcher for running Leiningen tasks.
See `grench help' to list tasks."

let () =
  if ! Sys.interactive then () else
    let cwd = Sys.getcwd () in
    let root = find_root cwd cwd in
    match Sys.argv |> Array.to_list |> List.tl with
      | None | Some ["--grench-help"] -> printf "%s\n%!" usage
      | Some ["--leiningen-version"] | Some ["--lein-version"] ->
        lein_main root cwd ["version"]
      | Some ["--version"] | Some ["version"] | Some ["-v"] ->
        printf "Grenchman 0.1.0\n%!"
      | Some ["repl"] ->
         lein_main root cwd ["run"; "-m"; "clojure.main/main"; "-r"]
      | Some args -> lein_main root cwd args

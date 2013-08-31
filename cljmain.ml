open Async.Std
open Core.Std
open Printf

let port_err =
  "Couldn't read port from .nrepl-port or LEIN_REPL_PORT.\n
If Leiningen is not running, launch `lein repl :headless' from the
project directory and try again.\n"

let repl_port root =
  let filename = String.concat
                   ~sep:Filename.dir_sep [root; ".nrepl-port"] in
  Client.repl_port "LEIN_REPL_PORT" filename port_err

let lein_ns = "leiningen.core.main"

let main_form = sprintf "(do
                           (use '%s)
                           (binding [*exit-process?* false]
                             (try (-main \"%s\")
                               (catch clojure.lang.ExceptionInfo e
                                 (let [c (:exit-code (ex-data e))]
                                   (when-not (and (number? c) (zero? c))
                                     (throw e)))))))"

let do_main ~port ~ns ~args =
  if ! Sys.interactive then () else
    let port = (match port with
      | Some x -> x
      | None -> let cwd = Sys.getcwd () in
                let root = Client.find_root cwd cwd in
                repl_port root) in
    let cmd = main_form ns (Client.splice_args args) in
    Client.main ns cmd port

let command =
  Command.basic
    ~summary:"Run clojure main commands against a REPL"
    Command.Spec.(
      empty
      +> flag "-p" (optional int) ~doc:"Port to connect to"
      +> anon ("namspace" %: string)
      +> anon (sequence ("args" %: string)))
    (fun port namespace args () -> do_main ~port ~ns:namespace ~args)

let () =
  Command.run command

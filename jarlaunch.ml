open Unix
open Core.Std
open Printf

let launch jarfile ~java ~main_ns ~args =
  Printf.printf "%s -cp %s clojure.main -m %s\n%!" java jarfile main_ns;
  match fork () with
  | 0 -> (* code run only in the child *)
     ignore (setsid ());
     close stdout;
     close stderr;
     close stdin;
     execvp java (Array.append
                    [| java; "-cp"; jarfile; "clojure.main"; "-m"; main_ns|]
                    args)
  | pid -> pid (* code run only in the parent *)

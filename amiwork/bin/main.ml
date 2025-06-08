open Fibra
(* open Unix *)

let sleep_and_get_value sec =
  let a = Fibra.wake_by_val (fun wake ->
    ignore (Thread.create (fun () ->
      Unix.sleepf sec;
      wake 42
    ) ())
  ) in a + 1
let main () =
  let task name () =
    Printf.printf "start %s\n%!" name;
    let a = sleep_and_get_value 2.0 in
    Printf.printf "end %d\n%!" a
  in

  let a = Fibra.async (task "A") in
  let b = Fibra.async (task "B") in
  Fibra.await a;
  Fibra.await b

  let _ = Fibra.run main
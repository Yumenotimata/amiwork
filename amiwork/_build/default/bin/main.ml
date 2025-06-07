open Fibra
(* open Unix *)

let sleep_async _ =
  Fibra.wake_by (fun wake ->
    (* ignore (Thread.create (fun () ->
      Printf.printf "hello\n%!";
      Unix.sleepf sec;
      wake ()
    ) ()) *)
    Printf.printf "hello\n%!";
    wake ()
  )

let main () =
  let task name () =
    Printf.printf "start %s\n%!" name;
    let () = sleep_async 1.0 in
    Printf.printf "end %s\n%!" name
  in

  let a = Fibra.async (task "A") in
  let b = Fibra.async (task "B") in
  Fibra.await a;
  Fibra.await b

  let _ = Fibra.run main
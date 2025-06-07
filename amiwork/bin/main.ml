open Fibra

let main () =
  let task name () =
    Printf.printf "start %s\n%!" name;
    Fibra.wake_by(fun waker -> waker ())
    (* wake (Obj.magic 0) *)
  in 
  let pa = Fibra.async (task "a") in
  let _ = Fibra.async (task "b") in
  Fibra.await pa
let () = Fibra.run main
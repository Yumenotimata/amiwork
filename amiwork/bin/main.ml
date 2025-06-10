open Fibra

open Effect
open Effect.Deep

type _ Effect.t +=
  | Get : unit -> int Effect.t
  | Set : int -> unit Effect.t

let main () = 
  let state = ref 0 in
  let run f =
    try 
      f ();
      ()
    with
      | effect Get _, k -> 
          Printf.printf "get\n%!";
          continue k !state
      | effect Set i, k -> 
          state := i; 
          Printf.printf "set %d\n%!" !state;
          continue k ()

  in
  let prog () = 
    let state = perform @@ Get () in
    let p = Fibra.spawn (fun () -> Fibra.spawn @@ fun () -> perform @@ Set 1) in
    (* Fibra.await p; *)
    let state = perform @@ Get () in
    Printf.printf "state %d\n%!" state;
    ()
  in
  Fibra.run @@ fun () -> run @@ fun () -> Fibra.runner @@ fun () -> prog ()

let _ = main ()
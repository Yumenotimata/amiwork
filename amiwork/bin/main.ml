open Fibra

open Effect
open Effect.Deep

type _ Effect.t +=
  | Get : unit -> int Effect.t
  | Set : int -> unit Effect.t
  | AsyncGet : unit -> unit Fibra.promise Effect.t

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
      | effect AsyncGet i, k ->
          let p = Fibra.async(fun () -> Printf.printf "async get\n%!") in
          continue k p

  in
  let prog () = 
    let _ = perform (AsyncGet ()) in
    ()
  in
  Fibra.run @@ fun () -> run @@ fun () -> Fibra.runner @@ fun () -> prog ()

let _ = main ()
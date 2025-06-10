module Fibra = struct
  open Effect
  open Effect.Deep

  type _ Effect.t +=
    | Register : ((unit -> unit) -> unit) -> unit Effect.t

  let register f = perform (Register f)
  
  let run f =
    let hs : ((unit -> unit) -> unit) list ref = ref [] in
    try
      f ()
    with
      | effect Register h, k -> 
          hs := !hs @ [h];
          continue k () 
      | effect eff, k ->
          List.iter (fun h -> h (fun () -> continue k (perform eff))) !hs;
          ()
end

open Effect
open Effect.Deep

type _ Effect.t +=
  | Get : unit -> int Effect.t
  | Set : int -> unit Effect.t

let main () = 
  let state = ref 0 in
  let run = fun f ->
    try 
      f ();
      ()
    with
      | effect Get _, k -> 
          Printf.printf "get\n";
          continue k !state
      | effect Set i, k -> 
          state := i; 
          Printf.printf "set %d\n" !state;
          continue k ()
  in 
  Fibra.run @@ fun () ->
    Fibra.register run;
    perform (Set 3);
    Printf.printf "%d\n" (perform (Get ()));
    ()

let _ = main ()
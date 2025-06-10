



module Fibra = struct
  open Effect
  open Effect.Deep

  type _ Effect.t +=
    | Run : (unit -> unit) -> int Effect.t
    | Test : unit -> unit Effect.t

  let run : ((unit -> unit) -> unit) -> (unit -> unit) -> unit = fun h f ->
    let kqueue = ref [] in
    let higher () = () in
    let lower f =
      Printf.printf "lower run f \n%!";
      let a = perform (Run f) in
      Printf.printf "back\n%!";
      List.iter (fun f -> f ()) !kqueue;
      ()
    in

    let handle_higher f k = 
      try h (f); () with        
        | effect Test _, k' ->
            kqueue := !kqueue @ [fun () -> continue k' ()];
            Printf.printf "test handled\n%!";
            continue k 3
        | effect eff, k' ->
            Printf.printf "handle higher\n%!";
            (* continue k (Obj.magic @@ perform eff) *)

    in

    try lower f; () with
      | effect Run f, k ->
          Printf.printf "cathed Run\n%!";
          handle_higher f k;
  ()
end

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
          Printf.printf "get\n";
          continue k !state
      | effect Set i, k -> 
          state := i; 
          Printf.printf "set %d\n" !state;
          continue k ()
      | effect eff, k ->
          Printf.printf "unknown\n%!";
          let _ = perform eff in
          ()
  in 
  Fibra.run run @@ fun () ->
    (* perform (Fibra.Test ()); *)
    Printf.printf "run\n%!";
    let a = perform (Get ()) in
    Printf.printf "%d\n%!" a;
    perform (Fibra.Test ())

let _ = main ()
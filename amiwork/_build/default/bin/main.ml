



module Fibra = struct
  open Effect
  open Effect.Deep

  type _ Effect.t +=
    | Run : (unit -> unit) -> int Effect.t
    | Spawn : (unit -> unit) -> unit Effect.t
    | Runner : (unit -> unit) -> unit Effect.t

  let kqueue = ref []
  
  let runner f =
    (* perform @@ Runner f; *)
    kqueue := !kqueue @ [fun () -> f ()];
    match !kqueue with
      | [] -> failwith ""
      | tasks -> 
          kqueue := List.drop 1 !kqueue;
          let task = List.hd tasks in
          let _ = task () in
          Printf.printf "first task%!\n";
    Printf.printf "backed%!\n%!";
    match !kqueue with
      | [] -> failwith ""
      | tasks -> 
          let task = List.hd tasks in
          let _ = task () in
          Printf.printf "second task%!\n";
    Printf.printf "backed%!\n%!";
    ()


  let run f = 
    try
      f ();
    with
      | effect Spawn f, k ->
          Printf.printf "spawn\n%!";
          kqueue := !kqueue @ [fun () -> f ()];
          continue k ()
    
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

  in 
  let _ = Fibra.run @@ fun () -> run @@ fun () -> Fibra.runner @@ fun () ->
    Printf.printf "run\n%!";
    perform (Set 1);
    perform (Fibra.Spawn (fun () -> (perform (Set 2))));
    ()
  in ()

let _ = main ()
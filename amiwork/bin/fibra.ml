module type Fibra = sig
  type 'a promise
  type 'a level
  val async : (unit -> unit) -> unit promise
  val await : 'a promise -> 'a
  val spawn : (unit -> unit) -> unit
  val wake : (('a -> unit) -> unit) -> 'a
  val run : (unit -> unit) -> unit
  val runner : (unit -> unit) -> unit
end

module Fibra : Fibra = struct
  open Effect
  open Effect.Deep

  type 'a _promise = 
    | Waiting of ('a, unit) continuation list
    | Done of 'a

  type 'a promise = 'a _promise ref

  type 'a level =
    | Runtime of unit
    | User of 'a

  type _ Effect.t +=
    | Async : (unit -> unit) -> unit promise Effect.t
    | Await : 'a promise -> 'a Effect.t
    | Wake : (('a -> unit) -> unit) -> 'a Effect.t
    | Runner : (unit -> unit) -> unit Effect.t
    | Eff : (unit -> 'a) -> 'a level Effect.t
  
  let async f = perform (Async f)
  let await p = perform (Await p)
  let spawn f = ignore (async f)
  let wake f = perform (Wake f)

  let kqueue = Queue.create ()

  let runner f = 
    (* let _ = perform (Runner f) in *)
    Queue.push f kqueue;
    let rec scheduler () =
      if not (Queue.is_empty kqueue) then
        let task = Queue.pop kqueue in
        try
          Printf.printf "run tas\n%!";
          task ();
          Printf.printf "end task\n%!";
          if not (Queue.is_empty kqueue) then
            Printf.printf "kqueu is not empty\n%!";
          scheduler ();
        with
          | effect Async f, k ->
              Printf.printf "perform\n%!";
              let v = perform (Eff (fun () -> perform (Async f))) in
              Printf.printf "ta\n%!";
              begin match v with
                | User v ->
                    Printf.printf "user level\n%!";
                    continue k v
                | Runtime _ ->
                    Printf.printf "runtime\n%!";
              end;
          | effect Await p, k ->
              Printf.printf "perfoffrm\n%!";
              let v = perform (Eff (fun () -> perform (Await p))) in
              Printf.printf "tafff\n%!";
              begin match v with
                | User v -> continue k v
                | Runtime _ -> ();
              end;
        Printf.printf "next\n%!";
        scheduler ()
    in
    scheduler ();
    Printf.printf "runnerf\n%!"

  let rec run_promise : 'a promise -> (unit -> 'a) -> unit = fun pr f ->
    Printf.printf "run promise\n%!";
    let v = f () in
    match !pr with
      | Done _ -> failwith "already done"
      | Waiting ks ->
          pr := Done v;
          let schedule k = Queue.push (fun () -> continue k ()) kqueue in
          List.iter schedule ks

  let run f = 
    try 
      ignore @@ f ();
      Printf.printf "main run returne\n%!";
      ()
    with
      | effect Async f, k ->
          Printf.printf "async\n";
          let p = ref (Waiting []) in
          Queue.push (fun () -> ignore @@ run_promise p f) kqueue;
                (* Queue.push (fun () -> continue k' p) kqueue; *)
          continue k ((Obj.magic p));
      | effect Await p, k ->
          Printf.printf "await\n%!";
            begin match !p with
              | Done v -> continue k ((Obj.magic v))
              | Waiting l -> p := Waiting (k::l)
            end;
          (* continue k (User (Obj.magic p)); *)
      | effect Eff eff, k ->
          try
            let v = eff () in ()
          with
            | effect Runner f, k' -> 
                continue k (Runtime ());
            | effect Async f, k' ->
                Printf.printf "async\n";
                let p = ref (Waiting []) in
                Queue.push (fun () -> ignore @@ run_promise p f) kqueue;
                (* Queue.push (fun () -> continue k' p) kqueue; *)
                continue k (User (Obj.magic p));
            | effect Await p, k' ->
                Printf.printf "await\n%!";
                begin match !p with
                  | Done v -> continue k (User (Obj.magic v))
                  | Waiting l -> p := Waiting (k'::l)
                end;
                continue k (User (Obj.magic p));
    Printf.printf "Main.run returned\n%!";
    ()
end
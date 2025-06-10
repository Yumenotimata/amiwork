module type Fibra = sig
  type 'a promise
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

  type _ Effect.t +=
    | Async : (unit -> unit) -> unit promise Effect.t
    | Await : 'a promise -> 'a Effect.t
    | Wake : (('a -> unit) -> unit) -> 'a Effect.t
    | Runner : (unit -> unit) -> unit Effect.t
  
  let async f = perform (Async f)
  let await p = perform (Await p)
  let spawn f = ignore (async f)
  let wake f = perform (Wake f)

  let kqueue = Queue.create ()

  let runner f = 
    let _ = perform (Runner f) in
    Queue.push f kqueue;
    let rec scheduler () =
      if not (Queue.is_empty kqueue) then
        let task = Queue.pop kqueue in
        try
          task ();
        with
          | effect e, k ->
              let v = perform e in
              Printf.printf "ta\n%!";
              continue k v;
        Printf.printf "next\n%!";
        scheduler ()
    in
    scheduler ();
    Printf.printf "runner\n%!"

  let rec run_promise : 'a promise -> (unit -> 'a) -> unit = fun pr f ->
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
      | effect Runner f, k -> 
          continue k ();
      | effect Async f, k ->
          Printf.printf "async\n";
          let p = ref (Waiting []) in
          Queue.push (fun () -> ignore @@ run_promise p f) kqueue;
          continue k p;
      | effect Await p, k ->
          Printf.printf "await\n%!";
          begin match !p with
            | Done v -> continue k v
            | Waiting l -> p := Waiting (k::l)
          end;
          continue k (Obj.magic ());
    Printf.printf "Main.run returned\n%!";
    ()
end
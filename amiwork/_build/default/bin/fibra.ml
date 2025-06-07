module type Fibra = sig
  type 'a promise
  val async : (unit -> 'a) -> 'a promise
  val await : 'a promise -> 'a
  val run : (unit -> 'a) -> unit
  val wake_by : ((unit -> unit) -> unit) -> unit
end

module Fibra : Fibra = struct
  open Effect
  open Effect.Deep

  type 'a _promise =
    | Waiting of ('a, unit) continuation list
    | Done of 'a

  type 'a promise = 'a _promise ref

  type _ Effect.t +=
    | Async : (unit -> 'a) -> 'a promise Effect.t
    | Await : 'a promise -> 'a Effect.t
    | Wake : ((unit -> unit) -> unit) -> unit Effect.t

  let async f = perform (Async f)
  let await p = perform (Await p)
  let wake_by f = perform (Wake f)

  let q = Queue.create ()
  let m = Mutex.create ()

  let enqueue a = 
    Mutex.lock m;
    Queue.push a q;
    Mutex.unlock m
  
  let dequeue () =
    Mutex.lock m;
    let a = 
      if Queue.is_empty q then 
        None 
      else 
        Some (Queue.pop q)
    in
    Mutex.unlock m;
    a

  (* open Printf *)

let run main =
  let rec run_fibra : 'a. 'a promise -> (unit -> 'a) -> unit =
    fun pr f ->
      try 
        let v = f () in
        match !pr with
          | Done _ -> failwith ""
          | Waiting ks ->
              pr := Done v;
              let schedule w = 
                enqueue (fun () -> run_fibra (ref (Waiting [])) (fun () -> continue w v))
              in
              List.iter schedule ks;
      with
        | effect (Async f), k ->
            let p = ref (Waiting []) in
            let f' () = run_fibra p f in
            enqueue f';
            enqueue (fun () -> run_fibra (ref (Waiting [])) (fun () -> continue k p))
        | effect (Await p), k ->
          begin match !p with
            | Done v -> run_fibra (ref (Waiting [])) (fun () -> continue k v)
            | Waiting l ->
              p := Waiting (k::l);
              match dequeue () with
                | Some f -> f ()
                | None -> ()
          end
        | effect (Wake f), k ->
          let waker () =
            enqueue (fun () -> run_fibra (ref (Waiting [])) (fun () -> continue k ()))
          in
          f waker;
          match dequeue () with
            | Some f -> f ()
            | None -> ()

  in
  enqueue (fun () -> run_fibra (ref (Waiting [])) main);
  let rec scheduler () =
    match dequeue () with
      | Some f -> f (); scheduler ()
      | None -> scheduler ()
  in
  scheduler ()

end
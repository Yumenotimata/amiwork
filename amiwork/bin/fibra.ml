module type Fibra = sig
  type 'a promise
  val async : (unit -> 'a) -> 'a promise
  val await : 'a promise -> 'a
  val wake_by : ((unit -> unit) -> unit) -> unit
  val wake_by_val : (('a -> unit) -> unit) -> 'a
  val run : (unit -> 'a) -> 'a
  val exit : 'a -> 'a
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
    | WakeVal : (('a -> unit) -> unit) -> 'a Effect.t
    | Exit : 'a -> unit Effect.t

  let async f = perform (Async f)
  let await p = perform (Await p)
  let wake_by f = perform (Wake f)
  let wake_by_val f = perform (WakeVal f)
  let exit v = perform (Exit v); v

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

  let n_wakers = ref 0

  let run main =
    try 
      let rec run_fibra : 'a. 'a promise -> (unit -> 'a) -> unit =
        fun pr f ->
          try 
            let v = f () in
            match !pr with
              | Done _ -> failwith "already done"
              | Waiting ks ->
                  pr := Done v;
                  let schedule k = enqueue (fun () -> continue k v) in
                  List.iter schedule ks;
          with
            | effect (Async f), k ->
                let p = ref (Waiting []) in
                enqueue (fun () -> run_fibra p f);
                enqueue (fun () -> continue k p)
            | effect (Await p), k ->
                begin match !p with
                  | Done v -> enqueue (fun () -> continue k v)
                  | Waiting l -> p := Waiting (k::l)
                end
            | effect (Wake f), k ->
                n_wakers := !n_wakers + 1;
                let called = ref false in
                let waker () =
                  if not !called then
                    enqueue @@ fun () ->
                      n_wakers := !n_wakers - 1;
                      continue k ()
                in
                f waker
            | effect (WakeVal f), k ->
                n_wakers := !n_wakers + 1;
                let called = ref false in
                let waker v =
                  if not !called then
                    enqueue @@ fun () -> 
                      n_wakers := !n_wakers - 1;
                      continue k v;
                    called := true
                in
                f waker

      in
      (* Obj.magicではなくExitの値をoptionで返すべき *)
      let latest : 'a option ref = ref None in
      let rec scheduler () =
        match dequeue () with
          | Some f -> latest := Some (f ()); scheduler ()
          | None -> if !n_wakers > 0 then 
              scheduler () 
          else match !latest with 
            | Some v -> Obj.magic v 
            | None -> failwith ""
      in
      enqueue (fun () -> run_fibra (ref (Waiting [])) main);
      scheduler ()
    with
      | effect Exit v, _ -> Obj.magic v
end
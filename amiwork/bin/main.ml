open Fibra

module type Meta = sig
  type uid
  type route
  type device [@@deriving show]
  val issue : unit -> uid
  val connect : uid -> uid -> unit
  val run : (unit -> unit) -> device list
end

(*MEMO: なぜか抽象型だとrouteのフィールドにアクセスできない*)
module Meta = struct
  open Effect
  open Effect.Deep

  type uid = int [@@deriving show]
  type route = {
    p1 : uid;
    p2 : uid;
  }
  type device = {
    uid : uid;
    rx : bytes list;
  } [@@deriving show]
  
  type _ Effect.t +=
    | Create : uid Effect.t
    | Connect : uid * uid -> unit Effect.t
    | Send : uid * bytes list -> unit Effect.t
    | Recv : uid -> (bytes list Fibra.promise) Effect.t

  let counter = ref 0
  let issue () = 
    let uid = !counter in
    counter := !counter + 1;
    uid

  let connect p1 p2 = perform (Connect (p1, p2))
  let run f =
    let devices : device list ref = ref [] in
    let q = ref [] in
    let rtb : route list ref = ref [] in 
    try f (); !devices with
      | effect Connect (p1 , p2), k -> 
        if p1 != p2 then
          rtb := !rtb @ [{ p1; p2 }];
        continue k ()
      | effect Create, k -> 
          let uid = issue () in
          devices := !devices @ [{ uid; rx = [] }];
          continue k uid
      | effect Send (src, payload), k -> 
          let targets =
            List.filter_map (fun (rt : route) -> 
              if rt.p1 == src then 
                Some rt.p2
              else if rt.p2 == src then 
                Some rt.p1 
              else
                None
            ) !rtb
          in 
          devices := List.map (fun dev -> 
            if List.exists (fun p -> p == dev.uid) targets then 
              { dev with rx = dev.rx @ payload} 
            else 
              dev
          ) !devices;
          List.iter (fun (uid, waker) -> (
              List.iter (fun target -> (
                if uid == target then
                  Printf.printf "waker\n%!";
                  waker payload
              )) targets
          )) !q;
          continue k ()
      | effect Recv uid, k -> 
          let self = List.find (fun dev -> dev.uid == uid) !devices in
          begin match self.rx with
            | [] ->
                continue k (Fibra.async (fun () -> (
                  let bytes = Fibra.wake_by_val(fun waker -> q := !q @ [(uid, waker)]) in bytes
                )))
            | bytes -> 
                devices := List.map (fun dev -> if dev.uid == uid then { dev with rx = [] } else dev) !devices;
                continue k (Fibra.async (fun () -> bytes))
          end
end

module type Nic = sig
  type nic
  val create : unit -> nic
  val send : nic -> bytes list -> unit
  val recv : nic -> bytes list
end

module type Phy = sig
  module Nic : Nic
  val run : (unit -> unit) -> unit
end

module Phy = struct
  open Effect
  open Effect.Deep
  open Meta

  module Nic = struct
    type nic = {
      uid : Meta.uid;
    }
    let create () = { uid = perform Create }
    let send self payload = perform (Send (self.uid, payload))
    let recv self = Fibra.await (perform (Recv self.uid))
  end

  let run f =
    f ()  
end

module type Ethernet = sig
  type ethernet
  val create : unit -> ethernet
  val send : ethernet -> bytes list -> unit
  val recv : ethernet -> bytes list
end

module type Datalink = sig
  module Ethernet : Ethernet
end

module Datalink : Datalink = struct
  module Ethernet : Ethernet = struct
    type ethernet = {
      nic : Phy.Nic.nic;
    }
    let create () = { nic = Phy.Nic.create () }
    let send self payload = Phy.Nic.send self.nic payload
    let recv self = Phy.Nic.recv self.nic
  end
end

module type Ip = sig
  type addr
  type header
  type ip
  val create : unit -> ip
  val send : ip -> bytes list -> addr -> unit
end

module type Network = sig
  module Ip : Ip
  val run : (unit -> unit) -> unit
end

module Network : Network = struct
  open Effect
  open Effect.Deep

  type _ Effect.t +=
    | Send : (bytes list) * string -> unit Effect.t

  module Ip : Ip = struct
    type addr = string
    type header = {
      src : addr;
      dst : addr;
      payload : bytes list;
    }
    type ip = {
      eth : Datalink.Ethernet.ethernet
    }
    let create () = { eth = Datalink.Ethernet.create () }
    let send self payload dst = Datalink.Ethernet.send self.eth payload
    let recv self = Datalink.Ethernet.recv self.eth
  end

  let run f = 
    try f () with
      | effect Send (payload, dst), k -> ()
      | e -> raise e
end

let main () = 
  let nic0 = Phy.Nic.create () in
  let nic1 = Phy.Nic.create () in
  let nic2 = Phy.Nic.create () in
  Meta.connect nic0.uid nic1.uid;
  Meta.connect nic0.uid nic2.uid;
  let p1 = Fibra.async (fun () -> (
    (* Printf.printf "recv\n%!";
    let res = Phy.Nic.recv nic1 in
    Printf.printf "%s\n%!" (Bytes.to_string (Bytes.concat Bytes.empty res)); *)
  )) in
  let p2 = Fibra.async (fun () -> (
    Printf.printf "send\n%!";
    Phy.Nic.send nic0 [Bytes.make 10 'a'];
  )) in
  Fibra.await p1;
  Fibra.await p2


  (* Printf.printf "koko%!\n";
  let res = Phy.Nic.recv nic1 in
  Printf.printf "%s\n%!" (Bytes.to_string (Bytes.concat Bytes.empty res)) *)

let _ = 
  let devices = Fibra.run (fun () -> let a = Meta.run (fun () -> Phy.run main) in Fibra.exit a) in
  List.iter (fun (dev : Meta.device) -> Printf.printf "%s\n%!" (Meta.show_device dev)) devices;

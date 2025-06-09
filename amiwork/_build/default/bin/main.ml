(* open Fibra *)

module type Meta = sig
  type uid
  type route
  type device [@@deriving show]
  val issue : unit -> uid
  val connect : uid -> uid -> unit
  val run : (unit -> unit) -> unit
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

  let counter = ref 0
  let issue () = 
    let uid = !counter in
    counter := !counter + 1;
    uid

  let connect p1 p2 = perform (Connect (p1, p2))
  let run f =
    let devices : device list ref = ref [] in
    let rtb : route list ref = ref [] in 
    try f (); !devices with
      | effect Connect (p1 , p2), k -> 
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
          continue k ()
end

module type Nic = sig
  type nic
  val create : unit -> nic
  val send : nic -> bytes list -> unit
end

module type Phy = sig
  module Nic : Nic
  val run :  Meta.route list -> (unit -> unit) -> (Meta.device list)
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
  end

  let run rtb f = 
    let devices : device list ref = ref [] in
    f ()  
end

module type Ethernet = sig
  type ethernet
  val create : unit -> ethernet
  val send : ethernet -> bytes list -> unit
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
  val run : unit -> (unit -> unit) -> unit
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
  end

  let run rtb f = 
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
  Phy.Nic.send nic0 [Bytes.make 10 'a']

let _ = 
  let devices = Meta.run (fun () -> Phy.run [] main) in
  List.iter (fun (dev : Meta.device) -> Printf.printf "%s\n%!" (Meta.show_device dev)) devices;

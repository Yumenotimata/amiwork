(* open Fibra *)

module type Meta = sig
  type uid
  type route
  val issue : unit -> uid
end

(*MEMO: なぜか抽象型だとrouteのフィールドにアクセスできない*)
module Meta = struct
  type uid = int [@@deriving show]
  type route = {
    p1 : uid;
    p2 : uid;
  }
  let issue () = 0
end

module type Nic = sig
  type nic
  val create : unit -> nic
  val send : nic -> bytes list -> unit
end

module type Phy = sig
  module Nic : Nic
  type device [@@deriving show]
  val run :  Meta.route list -> (unit -> unit) -> (device list)
end

module Phy : Phy = struct
  open Effect
  open Effect.Deep
  open Meta
  
  type _ Effect.t +=
    | Create : Meta.uid Effect.t
    | Send : Meta.uid * bytes list -> unit Effect.t

  module Nic : Nic = struct
    type nic = {
      uid : Meta.uid;
    }
    let create () = { uid = perform Create }
    let send self payload = perform (Send (self.uid, payload))
  end

  type device = {
    uid : Meta.uid;
    rx : bytes list;
  } [@@deriving show]

  let run rtb f = 
    let devices : device list ref = ref [] in
    try f (); !devices with
      | effect Create, k -> 
          let uid = Meta.issue () in
          devices := !devices @ [{ uid; rx = [] }];
          continue k uid
      | effect Send (src, payload), k -> 
          let targets =
            List.filter_map (fun (rt : Meta.route) -> 
              if rt.p1 == src then 
                Some rt.p1
              else if rt.p2 == src then 
                Some rt.p2 
              else
                None
            ) rtb
          in 
          devices := List.map (fun dev -> 
            if List.exists (fun p -> p == dev.uid) targets then 
              { dev with rx = dev.rx @ payload} 
            else 
              dev
          ) !devices;
          continue k ()
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
  let nic = Phy.Nic.create () in
  Phy.Nic.send nic []

let _ = 
  let devices = Phy.run [] main in
  List.iter (fun (dev : Phy.device) -> Printf.printf "%s\n%!" (Phy.show_device dev)) devices;

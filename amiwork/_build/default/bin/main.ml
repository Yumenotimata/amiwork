open Fibra

type tcp_segment = {
  data : bytes;
} [@@deriving show]

type ip_addr = string [@@deriving show]

type ip_packet = {
  src : ip_addr;
  dst : ip_addr;
  payload : tcp_segment;
} [@@deriving show]

type mac = string [@@deriving show]

(* 本来はDatalinkで定義したいが、frameからbytesのシリアライザを書くのは面倒なためいったんframeをストリームに直接流す *)
type ethernet_frame = {
  src : mac;
  dst : mac;
  payload : ip_packet;
} [@@deriving show]

let tcp_segment_default () = {
  data = Bytes.empty;
} [@@deriving show]

let ip_packet_default : unit -> ip_packet = fun () -> {
  src = "";
  dst = "";
  payload = tcp_segment_default ();
} [@@deriving show]

let ethernet_frame_default () = {
  src = "";
  dst = "";
  payload = ip_packet_default ();
} [@@deriving show]

module type Meta = sig
  type uid
  type route
  type device [@@deriving show]
  val issue : unit -> uid
  val connect : uid -> uid -> unit
  val run : (((unit -> 'a) ref) -> device list) 
end

(*MEMO: なぜか抽象型だとrouteのフィールドにアクセスできない*)
(* もしかしなくてもMetaとFibraを統合すべき? *)
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
    rx : ethernet_frame list;
  } [@@deriving show]
  
  type _ Effect.t +=
    | Create : uid Effect.t
    | Connect : uid * uid -> unit Effect.t
    | Send : uid * ethernet_frame -> unit Effect.t
    | Recv : uid -> (ethernet_frame list Fibra.promise) Effect.t

  let counter = ref 0
  let issue () = 
    let uid = !counter in
    counter := !counter + 1;
    uid

  let connect src dst = perform (Connect (src, dst))
  let run f =
    let devices : device list ref = ref [] in
    let q = ref [] in
    let rtb : route list ref = ref [] in 
    let handler m = ref (try m (); with 
      | effect Connect (p1, p2), k -> 
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
              { dev with rx = dev.rx @ [payload]} 
            else 
              dev
          ) !devices;
          List.iter (fun (uid, waker) -> (
              List.iter (fun target -> (
                if uid == target then
                  (* すでに受信済みのものも返すべき。あとはバッファを殻にすべき *)
                  waker [payload]
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
    ) in 
    let a = handler (fun () -> (f handler)) in
    !devices
end

module type Nic = sig
  type nic
  val create : mac -> nic
  val send : nic -> ethernet_frame -> unit
  val recv : nic -> ethernet_frame
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
      mac : mac;
    }
    let create mac = { uid = perform Create; mac }
    let send self payload = perform (Send (self.uid, payload))
    let recv self = Fibra.await (perform (Recv self.uid))
  end

  let run f =
    f ()  
end

module type Ethernet = sig
  type ethernet
  val create : unit -> ethernet
  val send : ethernet -> ethernet_frame -> mac -> unit
  val recv : ethernet -> ethernet_frame
end

module type Datalink = sig
  module Ethernet : Ethernet
end

module Datalink = struct
  module Ethernet = struct
    type ethernet = {
      nic : Phy.Nic.nic;
    }
    let create nic = { nic }
    let send self payload dst =
      let payload' = {
        src = self.nic.mac;
        dst = dst;
        payload;
      } in
      Phy.Nic.send self.nic payload'
    let recv self = Phy.Nic.recv self.nic
  end
end
(* 
module type Ip = sig
  type ip
  val create : Phy.Nic.nic -> ip_addr -> ip
  val send : ip_addr -> ip_packet -> ip_addr -> unit
end

module type Network = sig
  module Ip : Ip
  val run : (unit -> unit) -> unit
end

module Network = struct
  open Effect
  open Effect.Deep

  module Ip = struct
    type ip = {
      eth : Datalink.Ethernet.ethernet
    }
    let create nic = { eth = Datalink.Ethernet.create nic }
    let send self payload dst =
      (* Datalink.Ethernet.send self.eth payload *)
      Datalink.Ethernet.send self.eth (ip_packet_default ()) dst
    let recv self = Datalink.Ethernet.recv self.eth
  end
end *)

module type Repeater = sig
  type repeater
  val create : mac -> repeater
end

module Repeater = struct 
  type repeater = {
    nic : Phy.Nic.nic
  }

  let create mac = {
    nic = Phy.Nic.create mac
  }

  let run self handler = Fibra.async @@ fun () -> 
    let rec loop () =
      Printf.printf "loop\n%!";
      let frame = Phy.Nic.recv self.nic in
      Printf.printf "Repeater: %s\n%!" (String.concat "," (List.map show_ethernet_frame frame));
      List.iter (fun fr -> Phy.Nic.send self.nic fr) frame;
      ()
    in handler loop
end

module Router = struct
  type router = {
    nic : Phy.Nic.nic;
    ip : ip_addr;
  }

  let create ip mac = {
    nic = Phy.Nic.create mac;
    ip;
  }

  let run self prog handler = Fibra.async @@ fun () ->
    let rec loop () =
      ()
      (* try prog () with
        | effect Recv _, k -> () *)
    in handler loop
end

module Machine = struct 
  type machine = {
    eth : Datalink.Ethernet.ethernet;
    ip : ip_addr;
  }

  open Effect
  open Effect.Deep

  type _ Effect.t +=
    Recv : unit -> unit Effect.t

  let create ip mac = {
    eth = Datalink.Ethernet.create (Phy.Nic.create mac);
    ip;
  }

  let run self prog handler = Fibra.async @@ fun () ->
    let self_handler f =
      let rec loop () =
        try f () with
          | effect Recv _, k -> 
              Printf.printf "machien received handler\n%!";
              let rx = Datalink.Ethernet.recv self.eth in
              continue k ()
          in handler loop
    in self_handler (fun () -> prog self_handler)
end

module SocketLib = struct
  open Effect
  open Effect.Deep

  type 'a future =
    | Waiting of unit
    | Done of unit

  type socketlib = unit
  type port = int
  type fd = int
  type socket = {
    fd : fd;
  }

  type _ Effect.t +=
    | Bind : socket * port -> unit Effect.t
    | Listen : socket -> unit Effect.t
    | Accept : socket -> unit future Effect.t

  let socket () = {
    fd = 0;
  }
  let bind self port = perform (Bind (self, port))
  let listen self = perform (Listen self)
  let accept self = perform (Accept self)
  (* let  *)
  
  let kqueue = Queue.create ()
  let run prog eff_handler = 
    
    let q = ref [] in
    let rec handler = 
      try
        prog ()
      with
        | effect Bind (self, port), k ->
            if List.exists (fun (_, p) -> port == p) !q then
              q := !q @ [(self, port)];
            continue k ()
        | effect Listen self, k -> continue k ()
        | effect Accept self, k ->
            continue k (Done ())
    in
    Fibra.async @@ fun () -> handler;
    Fibra.async @@ fun () ->
      eff_handler @@ fun () ->
      let rx = perform (Machine.Recv ()) in
      Printf.printf "returned\n%!";
      ()
end

let main () = Fibra.await @@ Fibra.async @@ fun () -> Meta.run @@ fun handler -> 
  let machine0 = Machine.create "0.0.0.0" "00:00:00" in
  let lib machine_handler =
    let prog () = 
      let socket = SocketLib.socket () in
      let _ = SocketLib.bind socket 0 in
      let fd = SocketLib.accept socket in
      Printf.printf "received \n%!";
      ()
    in 
    let _ = SocketLib.run prog machine_handler in ()
  in
  let _ = Machine.run machine0 lib handler in
  let nic0 = Phy.Nic.create "00:00:01" in
  Meta.connect machine0.eth.nic.uid nic0.uid;
  Phy.Nic.send nic0 (ethernet_frame_default ());
  ()

let _ = 
  let devices = Fibra.run main in
  List.iter (fun (dev : Meta.device) -> Printf.printf "%s\n%!" ("Device: " ^ (Meta.show_device dev))) devices;
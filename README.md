```ocaml
open Fibra

type machine = {

};;

// 1マシンに1つ
type socket_lib = {
  listenings : socket list;
};;

type tcp-header = {
  src-port : int;
  dst-port : int;
  data : byte list;
}

// recvを非同期にwakeしたいが、そこに直接Fibraを持ち込むのはまだ早い
// もっと遅らせたい
// ここの復帰はFibraではなくmachineで管理？
// 結局やることは同じだからここにFibraを入れて、グローバルキューに持ち込む？
// しかしこの役割は本来machineのOSが行うべきで、、、
// いやそこを全部分離するのはたぶん不可能だから、いっそのこと全部グローバルにしてしまうのがいい？
// いったん全部Fibraにしよう
let socket_lib_loop = 
  let segment = await Transport.recv()
  listenings.find(fun listen -> listen == segment.dst-port)
     socket_wake machine listen segment

// ソケットが走るマシンが必要
type socket = {
  port : int;
};;

let socket_open port lib = 
  { port = port; }

let socket_bind sock lib = 
  ()

let socket_listen sock lib = 
  lib.listenings.append sock.port

let socket_recv sock lib = 
  wake_by(fun waker -> (
    register sock waker
  ))

let main () = ()

let _ = Fibra.run main
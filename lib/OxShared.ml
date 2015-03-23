open Core.Std
open Async.Std

open Packet
open OpenFlow0x01

type to_sw = switchId * xid * Message.t

let (pkt_out : (switchId * xid * Message.t) Pipe.Reader.t), 
    (defer : (switchId * xid * Message.t) option -> unit) =
  let r, w = Pipe.create () in
  r, function
      | None -> ()
      | Some v -> Pipe.write_without_pushback w v

let munge_exns thk = 
  Monitor.try_with (fun () -> return (thk ()))
  >>> function
    | Ok () -> 
      ()
    | Error exn ->
      Printf.eprintf 
        "unhandled exception raised by a callback\n%s"
        (Exn.to_string exn)

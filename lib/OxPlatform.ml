open Async.Std

open Printf
open Packet
open OpenFlow0x04_Core
open OpenFlow0x04
open Message
open OxShared

let send_packet_out sw xid pktOut =
  defer (Some (sw, xid, PacketOutMsg pktOut))
	  
let send_flow_mod sw xid flowMod =
  defer (Some (sw, xid, FlowModMsg flowMod))
	  
let send_barrier_request sw xid =
  defer (Some (sw, xid, BarrierRequest))
    
let send_msg sw xid msg =
  defer (Some (sw, xid, msg))

(* TODO(arjun): I'm not happy about this. I want an exception to terminate
   the right swich, unless we have exceptions kill the controller. *)
let timeout (n : float) (thk : unit -> unit) : unit = 
  after (Core.Std.Time.Span.of_sec n)
  >>> fun () -> munge_exns thk

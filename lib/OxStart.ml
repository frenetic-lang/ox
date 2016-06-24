open Printf
open Frenetic_Packet
open Frenetic_OpenFlow0x01
open OxShared
open Frenetic_OpenFlow_Header

module Controller = Frenetic_OpenFlow0x01_Controller

module type OXMODULE = sig
  val switch_connected : switchId -> SwitchFeatures.t -> unit
  val switch_disconnected : switchId -> unit
  val packet_in : switchId -> xid -> packetIn -> unit
  val barrier_reply : switchId -> xid -> unit
  val stats_reply : switchId -> xid -> StatsReply.t -> unit
  val cleanup : unit -> unit
end

module DefaultTutorialHandlers = struct

  (* NB: packet_in is intentionally omitted from this structure. *)

  let switch_disconnected (sw : switchId) : unit = ()

  let switch_connected (sw : switchId) (feats : SwitchFeatures.t) : unit = ()

  let barrier_reply _ _ = ()

  let stats_reply _ _ _ = ()

  let cleanup _ = ()

end


module Make (Handlers:OXMODULE) = struct
  open Async.Std
  open Core.Std

  let send_pkt_out ((sw, xid, msg) : switchId * xid * Message.t) : unit Deferred.t =
    Controller.send sw xid msg >>= function 
      | `Ok -> 
        return ()
      | `Eof -> 
        return ()

  let handler event =
    let open Message in
    match event with
    | `Connect (sw, feats) ->
      begin 
        Controller.send sw 0l (FlowModMsg delete_all_flows) >>= function
        | `Ok ->
           begin Controller.send sw 1l BarrierRequest >>= function
                 | `Ok ->
                    return (Handlers.switch_connected sw feats)
                 | `Eof ->
                    return ()
           end
        | `Eof ->
           return ()
      end
    | `Message (sw, hdr, msg) ->
      let xid = hdr.xid in 
      return (match msg with
	| PacketInMsg pktIn -> Handlers.packet_in sw xid pktIn
	| BarrierReply -> Handlers.barrier_reply sw xid
	| StatsReplyMsg rep -> Handlers.stats_reply sw xid rep
	| msg -> 
          Printf.eprintf "ignored a message from %Ld" sw)
    | `Disconnect sw ->
      Printf.eprintf "switch %Ld disconnected\n%!" sw;
      return ()

  let start_controller () : unit =
    Controller.init 6633; 
    (Deferred.don't_wait_for
       (Monitor.try_with ~name:"controller" (fun () ->
         let d1 = Pipe.iter pkt_out send_pkt_out in 
         let d2 = Pipe.iter Controller.events handler in 
         d1 >>= fun () -> d2)
         >>= function
           | Ok () ->
             exit 0
           | Error exn ->
             Printf.eprintf "Unexpected exception: %s\n%!"
               (Exn.to_string exn);
             exit 1))

  let run () : unit =
    let open Core.Std in
    (* intentionally on stdout *)
    Printf.eprintf "Ox controller launching...\n%!";
    Sys.catch_break true;
    ignore (start_controller ());
    Core.Std.never_returns (Async.Std.Scheduler.go ())
end

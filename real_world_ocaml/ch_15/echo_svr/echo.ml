open Core
open Async

(** Copy data from reader to writer, using buffer as scratch *)
let rec copy_blocks buffer r w =
    Reader.read r buffer 
    >>= function
    | `Eof -> return ()
    | `Ok bytes_read -> 
            Writer.write w (Bytes.to_string buffer) ~len:bytes_read;
            Writer.flushed w
            >>= fun () ->
            copy_blocks buffer r w

(** Starts a TCP server, which listens on the specified port,
    invoking copy_blocks every time a client connects. *)
let run () = 
    let host_and_port =
        Tcp.Server.create
            ~on_handler_error:`Raise
            (Tcp.Where_to_listen.of_port 8765)
            (fun _addr r w -> 
                let buffer = Bytes.create (16*1024) in 
                copy_blocks buffer r w)
    in 
    ignore (host_and_port: (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t)

let () = 
    run ();
    never_returns (Scheduler.go ())

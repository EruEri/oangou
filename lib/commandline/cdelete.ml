(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of oangou: a content encryptor                                           *)
(* Copyright (C) 2024 Yves Ndiaye                                                             *)
(*                                                                                            *)
(* oangou is free software: you can redistribute it and/or modify it under the terms          *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* oangou is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;        *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with oangou.       *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)

module Make
    (AEAD : Mirage_crypto.AEAD)
    (Dh_dsa : Mirage_crypto_ec.Dh_dsa)
    (Hash : Mirage_crypto.Hash.S) =
struct
  open Cmdliner
  module LibangouI = Libangou.Make (AEAD) (Dh_dsa) (Hash)

  let name = "delete"

  type t = { peers : string list }

  let term_peers =
    Arg.(
      value & pos_all string [] & info ~docv:"<PEERS>" ~doc:"Peers to delete" []
    )

  let term_cmd run =
    let combine peers = run { peers } in
    Term.(const combine $ term_peers)

  let doc = "Delete peers"
  let man = []

  let cmd run =
    let info = Cmd.info ~exits:Ccommon.exits ~doc ~man name in
    Cmd.v info @@ term_cmd run

  let exec ~key peers angou =
    let { peers } = peers in
    let deleted, angou =
      LibangouI.Peers.partition (fun s _ -> List.mem s peers) angou
    in
    match LibangouI.Peers.PeersMap.is_empty deleted with
    | true ->
        Printf.eprintf "No peers deleted"
    | false ->
        LibangouI.Peers.save ~key angou

  let run t =
    let () = Libangou.Config.check_initialized () in
    let password = Libangou.Input.ask_password () in
    let angou = LibangouI.Peers.load ~key:password () in
    let result = Result.map (exec ~key:password t) angou in
    let () =
      match result with
      | Ok () ->
          ()
      | Error e ->
          Libangou.Error.Exn.angou_error_raise e
    in
    ()

  let command = cmd run
end

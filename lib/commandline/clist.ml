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

  let name = "list"

  type t = unit

  let term_cmd run =
    let combine () = run () in
    Term.(const combine $ const ())

  let doc = "List known peers"
  let man = []

  let cmd run =
    let info = Cmd.info ~doc ~man name in
    Cmd.v info @@ term_cmd run

  let run () =
    let () = Libangou.Config.check_initialized () in
    let password = Libangou.Input.ask_password () in
    let angou = LibangouI.Peers.load ~key:password () in
    let result =
      Result.map
        (fun angou ->
          let names = LibangouI.Peers.peers_name angou in
          List.iter (Printf.printf "- %s\n") names
        )
        angou
    in
    let () =
      match result with
      | Error e ->
          Libangou.Error.Exn.angou_error_raise e
      | Ok () ->
          ()
    in
    ()

  let command = cmd run
end

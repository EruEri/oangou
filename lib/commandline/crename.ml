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

  let name = "rename"

  type t = { old_name : string; new_name : string }

  let term_old_name =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"<OLD_NAME>" ~doc:"Name to rename" []
    )

  let term_new_name =
    Arg.(
      required
      & pos 1 (some string) None
      & info ~docv:"<NEW_NAME>" ~doc:"New name of the peer" []
    )

  let term_cmd run =
    let combine old_name new_name = run { old_name; new_name } in
    Term.(const combine $ term_old_name $ term_new_name)

  let doc = "Rename peer"
  let man = []

  let cmd run =
    let info = Cmd.info ~doc ~man name in
    Cmd.v info @@ term_cmd run

  let exec ~key old_name new_name angou =
    let angou, change = LibangouI.Peers.rename old_name new_name angou in
    let () =
      if change then
        let () = Printf.eprintf "%s ---> %s\n" old_name new_name in
        LibangouI.Peers.save ~key angou
      else
        Printf.eprintf "%s is unchanged\n" Libangou.Config.angou_name
    in
    ()

  let run t =
    let () = Libangou.Config.check_initialized () in
    let { old_name; new_name } = t in
    let password = Libangou.Input.ask_password () in
    let angou = LibangouI.Peers.load ~key:password () in
    let result = Result.map (exec ~key:password old_name new_name) angou in
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

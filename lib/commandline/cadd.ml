(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of oangou: a file encryptor                                              *)
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

  let name = "add"

  type t = { public_key : string option; peer : string }

  let term_peer =
    Arg.(
      required
      & opt (some string) None
      & info ~docv:"<PEER>" ~doc:"Add $(docv) to known peers" [ "p" ]
    )

  let term_public_key =
    Arg.(
      value
      & opt (some non_dir_file) None
      & info ~docv:"<PUBLIC_KEY>" ~doc:"Associate $(docv) to <PEER>" [ "k" ]
    )

  let term_cmd run =
    let combine public_key peer = run { public_key; peer } in
    Term.(const combine $ term_public_key $ term_peer)

  let doc = "Add peers"
  let man = []

  let cmd run =
    let info = Cmd.info ~doc ~man name in
    Cmd.v info @@ term_cmd run

  let run t =
    let () = Libangou.Config.check_initialized () in
    let { public_key; peer } = t in
    let password = Libangou.Input.ask_password () in
    let public_key = Util.Io.read_content ?file:public_key () in
    let ( >>= ) = Result.bind in
    let angou =
      LibangouI.Peers.load ~key:password ()
      >>= LibangouI.Peers.add peer public_key
    in
    let angou =
      match angou with
      | Ok angou ->
          angou
      | Error mirage_error ->
          Libangou.Error.Exn.mirage_crypto_error mirage_error
    in
    let _ = LibangouI.Peers.save ~key:password angou in
    ()

  let command = cmd run
end

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

  let name = "decrypt"

  type t = { infile : string option; outfile : string option; peer : string }

  let term_infile =
    Arg.(
      value
      & opt (some non_dir_file) None
      & info ~docv:"<FILE>" ~doc:"Encrypt a specific file" ~absent:"stdin"
          [ "f" ]
    )

  let term_outfile =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"<OUTFILE>" ~doc:"Output the encrypt file to $(docv)"
          ~absent:"stdout" [ "o" ]
    )

  let term_peer =
    Arg.(
      required
      & opt (some string) None
      & info ~docv:"<PEER>" ~doc:"Encrypt the file for $(docv)" [ "p" ]
    )

  let term_cmd run =
    let combine infile outfile peer = run { infile; outfile; peer } in
    Term.(const combine $ term_infile $ term_outfile $ term_peer)

  let doc = "Decrypt data"
  let man = []

  let cmd run =
    let info = Cmd.info ~doc ~man name in
    Cmd.v info @@ term_cmd run

  let run t =
    let () = Libangou.Config.check_initialized () in
    let { infile; outfile; peer } = t in
    let password = Libangou.Input.ask_password () in
    let angou = LibangouI.Peers.load ~key:password () in
    let result =
      Result.map
        (fun angou ->
          let shared_secret =
            match LibangouI.Peers.shared_secret peer angou with
            | None ->
                Libangou.Error.Exn.unknown_peer peer
            | Some share ->
                share
          in
          let cypher = Util.Io.read_content ?file:infile () in
          Option.map (fun cplaintext ->
              Util.Io.write_content ?file:outfile
              @@ Cstruct.to_string cplaintext
          )
          @@ LibangouI.Crypto.decrypt ~key:shared_secret cypher
        )
        angou
    in
    let () =
      match result with
      | Error e ->
          Libangou.Error.Exn.mirage_crypto_error e
      | Ok None ->
          failwith "Fail to decrypt"
      | Ok (Some ()) ->
          ()
    in
    ()

  let command = cmd run
end

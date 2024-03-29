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

  let name = "keys"

  type t = { hexa : bool; private_key : bool; outfile : string option }

  let term_outfile =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"<OUTFILE>" ~doc:"Output the key to $(docv)" ~absent:"stdout"
          [ "o" ]
    )

  let term_private_keys =
    Arg.(
      value & flag
      & info ~doc:"Output the private key instead" ~absent:"public"
          [ "private" ]
    )

  let term_hexadecimal =
    Arg.(
      value & flag
      & info ~doc:"Output in a hexadecimal string representation" [ "x" ]
    )

  let term_cmd run =
    let combine hexa outfile private_key = run { hexa; outfile; private_key } in
    Term.(const combine $ term_hexadecimal $ term_outfile $ term_private_keys)

  let doc = "Export keys from $(mname)"
  let man = []

  let cmd run =
    let info = Cmd.info ~doc ~man name in
    Cmd.v info @@ term_cmd run

  let run t =
    let () = Libangou.Config.check_initialized () in
    let { hexa; private_key; outfile } = t in
    let password = Libangou.Input.ask_password () in
    let angou = LibangouI.Peers.load ~key:password () in
    let result =
      Result.map
        (fun angou ->
          let raw_key = LibangouI.Peers.raw_key ~hexa ~private_key angou in
          Util.Io.write_content ?file:outfile raw_key
        )
        angou
    in
    let () =
      match result with
      | Ok () ->
          ()
      | Error e ->
          Libangou.Error.Exn.mirage_crypto_error e
    in
    ()

  let command = cmd run
end

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

  type t = { change_master_password : bool } [@@unboxed]

  let term_change_master_password =
    Arg.(
      value & flag
      & info [ "change-master-password" ] ~doc:"Change the master password"
    )

  let term_cmd run =
    let combine change_master_password = run { change_master_password } in
    Term.(const combine $ term_change_master_password)

  let run t =
    let { change_master_password = _ } = t in
    (* let () = Libcithare.Manager.check_initialized () in
       let () =
         match change_master_password with
         | false ->
             ()
         | true ->
             let master_password = Libcithare.Input.ask_password_encrypted () in
             let manager = Libcithare.Manager.decrypt master_password in
             let pass1 =
               Libcithare.Input.ask_password
                 ~prompt:Libcithare.Input.Prompt.master_new_password ()
             in
             let pass2 =
               Libcithare.Input.ask_password
                 ~prompt:Libcithare.Input.Prompt.master_confirm_new_password ()
             in
             let pass =
               match pass1 = pass2 with
               | true ->
                   pass1
               | false ->
                   raise @@ Libcithare.Error.unmatched_password
             in
             let () = Libcithare.Manager.encrypt ~encrypt_key:true pass manager in
             let () = Printf.printf "Master password sucessfully changed\n%!" in
             ()
       in *)
    ()

  let default = term_cmd run
  let name = Libangou.Config.angou_name
  let version = Libangou.Config.version
  let doc = "A file encryptor"

  let man =
    [
      `S Manpage.s_description;
      `P "$(mname) is a file encryptor";
      `P
        "It allows you to share encrypted file on one hand and to decrypt it \
         on the other.";
    ]

  let info = Cmd.info ~doc ~version ~man name

  let subcommands =
    let module Cinit = Cinit.Make (AEAD) (Dh_dsa) (Hash) in
    let module Centrypt = Centrypt.Make (AEAD) (Dh_dsa) (Hash) in
    let module Cadd = Cadd.Make (AEAD) (Dh_dsa) (Hash) in
    Cmd.group ~default info [ Cinit.command; Centrypt.command; Cadd.command ]

  let eval () = Cmd.eval ~catch:false subcommands
end

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

  let name = "init"

  type t = { force : bool }

  let term_force =
    Arg.(value & flag & info [ "f"; "force" ] ~doc:"Force the initialisation")

  let term_cmd run =
    let combine force = run @@ { force } in
    Term.(const combine $ term_force)

  let p e = `P e
  let i a b = `I (a, b)
  let noblank = `Noblank
  let doc = "Initialize $(mname)"
  let pre s = `Pre s

  let man =
    [
      `S Manpage.s_description;
      p
      @@ Printf.sprintf
           "Initialize $(mname) by creating $(b,XDG_DATA_HOME/%s/%s) file"
           Libangou.Config.angou_name Libangou.Config.angou_keys_file_name;
      p
        "If $(mname) has already been initialized, $(iname) will raise an \
         exception unless the $(b,--force) option is given which will delete \
         the existing $(mname) installation";
    ]

  let cmd run =
    let info = Cmd.info ~doc ~man name in
    Cmd.v info @@ term_cmd run

  let run t =
    let { force } = t in
    let citharecf_exist = Libangou.Config.is_angou_initialized () in
    let () =
      match force with
      | false when citharecf_exist ->
          Libangou.Error.Exn.angou_already_configured ()
      | true | false ->
          ()
    in
    let () =
      match
        Util.FileSys.mkdirp Libangou.Config.xdg_data
          [ Libangou.Config.angou_name ]
      with
      | Ok () ->
          ()
      | Error s ->
          failwith s
    in
    let () =
      match force with
      | true when citharecf_exist ->
          Sys.remove Libangou.Config.angou_keys_file
      | true | false ->
          ()
    in
    let pass1 =
      Libangou.Input.ask_password
        ~prompt:Libangou.Input.Prompt.master_new_password ()
    in
    let pass2 =
      Libangou.Input.ask_password
        ~prompt:Libangou.Input.Prompt.master_confirm_new_password ()
    in
    let pass =
      match pass1 = pass2 with
      | true ->
          pass1
      | false ->
          raise @@ Libangou.Error.Exn.password_not_matched ()
    in
    let angou = LibangouI.Peers.create () in
    let () = LibangouI.Peers.save ~key:pass angou in
    (* let () =  Libcithare.Manager.encrypt ~encrypt_key:true pass manager in *)
    let () = Printf.printf "%s initiliazed\n%!" Libangou.Config.angou_name in
    ()

  let command = cmd run
end

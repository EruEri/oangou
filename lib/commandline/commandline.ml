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
  module LibangouI = Libangou.Make (AEAD) (Dh_dsa) (Hash)
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
    let { change_master_password } = t in
    let () = Libangou.Config.check_initialized () in
    match change_master_password with
    | false ->
        ()
    | true ->
        let master_password = Libangou.Input.ask_password () in
        let angou = LibangouI.Peers.load ~key:master_password () in
        let angou =
          match angou with
          | Ok angou ->
              angou
          | Error e ->
              Libangou.Error.Exn.angou_error_raise e
        in
        let pass1 =
          Libangou.Input.ask_password
            ~prompt:Libangou.Input.Prompt.master_new_password ()
        in
        let pass2 =
          Libangou.Input.ask_password
            ~prompt:Libangou.Input.Prompt.master_confirm_new_password ()
        in
        let new_password =
          match pass1 = pass2 with
          | true ->
              pass1
          | false ->
              Libangou.Error.Exn.password_not_matched ()
        in
        let _ = LibangouI.Peers.save ~key:new_password angou in
        Printf.printf "Master password sucessfully changed\n%!"

  let default = term_cmd run
  let name = Libangou.Config.angou_name
  let version = Libangou.Config.version
  let doc = "A content encryptor"

  let man =
    [
      `S Manpage.s_description;
      `P "$(mname) is a content encryptor";
      `P
        "It allows you to share encrypted file on one hand and to decrypt it \
         on the other.";
      `P
        "$(mname) works by using public and private keys and the \
         Diffie–Hellman key-exchange protocol";
      `P
        "To use $(mname), your need to initialize it. Use the $(mname) init \
         subcommand";
      `P
        "To encrypt and decrypt messages, you need add the public key of the \
         peer. The key can be exported with $(b,oangou-export-keys)(1) and be \
         added thanks to $(b,oangou-add)(1)";
      `S Manpage.s_examples;
      `I
        ( "To encrypt the message \"$(b,Hello alice)\" for a peer name \
           $(b,alice), from stdin, in a hexadecimal string",
          "$(b,echo \"Hello alice\" | oangou encrypt -x -p alice)"
        );
      `I
        ( "To decrypt a message send by a peer named $(b,chloe), stored in a \
           file name $(b,secret.txt)",
          "$(b,oangou decrypt -f secret.txt -p chloe)"
        );
    ]

  let exits =
    [
      Cmd.Exit.info ~doc:"on success." Libangou.ExitCode.angou_success;
      Cmd.Exit.info ~doc:"on $(mname) errors." Libangou.ExitCode.angou_error;
      Cmd.Exit.info ~doc:"on internal errors." Libangou.ExitCode.any_other_error;
      Cmd.Exit.info ~doc:"on command line parsing errors."
        Libangou.ExitCode.angou_parsing_options_error;
    ]

  let info = Cmd.info ~exits ~doc ~version ~man name

  let subcommands =
    let module Cadd = Cadd.Make (AEAD) (Dh_dsa) (Hash) in
    let module Cdecrypt = Cdecrypt.Make (AEAD) (Dh_dsa) (Hash) in
    let module Cdelete = Cdelete.Make (AEAD) (Dh_dsa) (Hash) in
    let module Centrypt = Centrypt.Make (AEAD) (Dh_dsa) (Hash) in
    let module Cexport = Cexport.Make (AEAD) (Dh_dsa) (Hash) in
    let module Clist = Clist.Make (AEAD) (Dh_dsa) (Hash) in
    let module Cinit = Cinit.Make (AEAD) (Dh_dsa) (Hash) in
    let module Crename = Crename.Make (AEAD) (Dh_dsa) (Hash) in
    Cmd.group ~default info
      [
        Cinit.command;
        Centrypt.command;
        Cadd.command;
        Cdecrypt.command;
        Cexport.command;
        Clist.command;
        Cdelete.command;
        Crename.command;
      ]

  let eval () =
    match Cmd.eval_value ~catch:false subcommands with
    | Ok _ ->
        0
    | Error (`Parse | `Term) ->
        Libangou.ExitCode.angou_parsing_options_error
    | Error `Exn ->
        Libangou.ExitCode.any_other_error
    | exception Libangou.Error.AngouError e ->
        let () =
          prerr_endline @@ Libangou.Print.string_of_color_oangou_error e
        in
        Libangou.ExitCode.angou_error
    | exception e ->
        let massage = Printexc.to_string_default e in
        let () =
          Printf.eprintf "%s: %s\n%!"
            (Util.Repr.sprintf Util.Repr.fg_red "Interal error")
            massage
        in
        Libangou.ExitCode.any_other_error
end
